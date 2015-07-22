#if defined(_WIN32)
#include "AL/al.h"
#include "AL/alc.h"
#else
#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <pthread.h>
#include <stdbool.h>

#include "z_libpd.h"
#include "Sound/Pd/Internal_stub.h"



#define NUM_SOURCES 4
#define NUM_BUFFERS 3
#define BUFFER_SIZE 1024
#define SAMPLE_RATE 44100
#define FORMAT AL_FORMAT_MONO16

#define SAMPLE_TIME ((double)1/(double)SAMPLE_RATE)
#define BLOCK_TIME (BUFFER_SIZE*SAMPLE_TIME)

#define PD_NUM_INPUTS 0
#define PD_BLOCK_SIZE 64
#define PD_TICKS (BUFFER_SIZE/PD_BLOCK_SIZE)

#define PD_BUFFER_SIZE (BUFFER_SIZE * NUM_SOURCES)

short tempBuffer[BUFFER_SIZE];
short pdBuffer[PD_BUFFER_SIZE];

typedef struct {
  HsStablePtr pdChan;
  ALuint *allSourceIDs;
  int numSourceIDs;
} OpenALThreadData;

// Function prototypes
bool check_source_ready(ALuint sourceID);
ALuint create_source();
bool tick_source_stream(ALuint sourceID, int sourceNum);
ALuint* startAudio(HsStablePtr pdChan);
void *openal_thread_loop(void *threadArg);

// Checks if an OpenAL source has finished processing any of its streaming buffers
bool check_source_ready(ALuint sourceID) {
  ALint numBuffersToFill;
  alGetSourcei(sourceID, AL_BUFFERS_PROCESSED, &numBuffersToFill);
  return numBuffersToFill > 0;
}

ALuint create_source() {
  int sourceBuffer[BUFFER_SIZE] = {0};
  ALuint sourceID;
  ALuint sourceBufferIDs[NUM_BUFFERS];

  // Create our buffers and source
  alGenBuffers(NUM_BUFFERS, sourceBufferIDs);
  alGenSources(1, &sourceID);
  if(alGetError() != AL_NO_ERROR) {
    fprintf(stderr, "Error generating :(\n");
    return 1;
  }

  // Fill the buffers with initial data
  for (int i = 0; i < NUM_BUFFERS; ++i) {
    alBufferData(sourceBufferIDs[i], FORMAT, sourceBuffer, BUFFER_SIZE * sizeof(short), SAMPLE_RATE);
  }
  
  if(alGetError() != AL_NO_ERROR) {
    fprintf(stderr, "Error loading :(\n");
    return 1;
  }

  // Queue the initial buffers so we have something to dequeue and begin playing
  alSourceQueueBuffers(sourceID, NUM_BUFFERS, sourceBufferIDs);
  alSourcePlay(sourceID);

  if(alGetError() != AL_NO_ERROR) {
    fprintf(stderr, "Error starting :(\n");
    return 1;
  }
  return sourceID;
}

bool tick_source_stream(ALuint sourceID, int sourceNum) {
  
  ALint numBuffersToFill;
  alGetSourcei(sourceID, AL_BUFFERS_PROCESSED, &numBuffersToFill);
  if(numBuffersToFill <= 0) {
    return false;
  }

  // printf("%i Getting %i buffers\n", sourceID, numBuffersToFill);
  
  // NOTE: I'm not paying attention to numBuffersToFill; we only fill one buffer each tick.

  // Copy interleaved output of pd into OpenAL buffer for this source
  // TODO: SIMD this
  for (int n = 0; n < BUFFER_SIZE; ++n) {
    tempBuffer[n] = pdBuffer[sourceNum + (n * NUM_SOURCES)];
  }
  
  // sine_into_buffer(tempBuffer, BUFFER_SIZE, freq, tOffset);
  ALuint bufferID;
  alSourceUnqueueBuffers(sourceID, 1, &bufferID);
  alBufferData(bufferID, FORMAT, tempBuffer, BUFFER_SIZE * sizeof(short), SAMPLE_RATE);
  alSourceQueueBuffers(sourceID, 1, &bufferID);
  if(alGetError() != AL_NO_ERROR) {
    fprintf(stderr, "Error buffering :(\n");
    return false;
  }
  
  ALint isPlaying;
  alGetSourcei(sourceID, AL_SOURCE_STATE, &isPlaying);
  if(isPlaying != AL_PLAYING) {
    printf("Restarting play\n");
    alSourcePlay(sourceID);
  }
  return true;
}



ALuint* startAudio(HsStablePtr pdChan) {

  
  libpd_init();
  libpd_init_audio(PD_NUM_INPUTS, NUM_SOURCES, SAMPLE_RATE);

  libpd_start_message(1); // one entry in list
  libpd_add_float(1.0f);
  libpd_finish_message("pd", "dsp");

  // Open the default device and create an OpenAL context
  ALCdevice *device;
  ALCcontext *context;
  ALuint *allSourceIDs = (ALuint*)malloc(NUM_SOURCES * sizeof(ALuint));

  device = alcOpenDevice(NULL);
  if(!device) {
    fprintf(stderr, "Couldn't open OpenAL device :-(\n");
    return allSourceIDs;
  }
  context = alcCreateContext(device, NULL);
  alcMakeContextCurrent(context);
  if(!context) {
    fprintf(stderr, "Couldn't create OpenAL context :-(\n");
    return allSourceIDs;
  }

  
  for (int i = 0; i < NUM_SOURCES; ++i) {
    allSourceIDs[i] = create_source();
  }

  OpenALThreadData *threadData = (OpenALThreadData *)malloc(sizeof(OpenALThreadData));
  threadData->allSourceIDs = allSourceIDs;
  threadData->pdChan = pdChan;
  threadData->numSourceIDs = NUM_SOURCES;

  // Spread the sources out
  for (int i = 0; i < NUM_SOURCES; ++i) {
    float pan = ((float)i/(float)NUM_SOURCES) * 2 - 1;

    ALuint sourceID = allSourceIDs[i];
    
    ALfloat sourcePos[] = {pan,0,-1};
    alSourcefv(sourceID, AL_POSITION, sourcePos);
  }

  pthread_t thread;
  pthread_create(&thread, NULL, openal_thread_loop, (void *)threadData);

  return allSourceIDs;
}

void *openal_thread_loop(void *threadArg) {
  OpenALThreadData *threadData = (OpenALThreadData *)threadArg;
  HsStablePtr pdChan = threadData->pdChan;
  ALuint* allSourceIDs = threadData->allSourceIDs;
  int numSourceIDs = threadData->numSourceIDs;
  while (1) {
    // We want to wait until *all* sources are ready for new data, 
    // not just one, so we can fill them all at once.

    // Continuously check each source to see if it has buffers ready to fill
    int numSourcesReady = 0;
    for (int i = 0; i < numSourceIDs; ++i) {
      ALuint sourceID = allSourceIDs[i];
      if (check_source_ready(sourceID)) {
        numSourcesReady++;
      }
    }
    // Once all have reported they're ready, fill them all.
    // We fill them here with varying enveloped sine frequencies.
    if (numSourcesReady == numSourceIDs) {

      // buffer size is num channels * num ticks * num samples per tick (libpd_blocksize(), default 64)
      // samples are interleaved, so 
      // l0 r0 f0 l1 r1 f1 l2 r2 f2
      // sample(n) = pdBuffer[sourceNum + (n * numSources)]

      // libpd_process_short(PD_TICKS, 0, pdBuffer);
      // Call into Haskell to ensure processing occurs 
      // on the dedicated thread we create for libpd
      processShort(pdChan, PD_TICKS, 0, pdBuffer);

      for (int i = 0; i < numSourceIDs; ++i) {
        
        ALuint sourceID = allSourceIDs[i];
        tick_source_stream(sourceID, i);
      
      }
    }
  }
}


// Expose API simply to Haskell
void setOpenALSourcePositionRaw(ALuint sourceID, ALfloat *values) {
  alSourcefv(sourceID, AL_POSITION, values);
}
void setOpenALSourceOrientationRaw(ALuint sourceID, ALfloat *values) {
  alSourcefv(sourceID, AL_ORIENTATION, values);
}
void setOpenALListenerPositionRaw(ALuint sourceID, ALfloat *values) {
  alListenerfv(AL_POSITION, values);
}
void setOpenALListenerOrientationRaw(ALuint sourceID, ALfloat *values) {
  alListenerfv(AL_ORIENTATION, values);
}