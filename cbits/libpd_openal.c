#if defined(_WIN32)
#include "AL/al.h"
#include "AL/alc.h"
#else
#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#include <OpenAL/MacOSX_OALExtensions.h>
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <pthread.h>
#include <time.h>
#include <stdbool.h>

#include "z_libpd.h"
#include "Sound/Pd/Internal_stub.h"

// From openal_soft_reverb.c or openal_mac_reverb.c
// (we link one or the other per platform)
int add_reverb(ALuint *allSourceIDs, int numSourceIDs);

#define SAMPLE_RATE 44100.0

#define FORMAT AL_FORMAT_MONO16

#define PD_NUM_INPUTS 1
#define PD_BLOCK_SIZE 64


#define NSEC_PER_SEC 1000000000

#define NUM_BUFFERS_PER_SOURCE 16

typedef struct {
  HsStablePtr pdChan;
  ALCdevice *inputDevice;
  ALuint *allSourceIDs;
  int numSources;
  int bufferSize;
  short *tempBuffer;
  short *pdInBuffer;
  short *pdOutBuffer;
  int pdTicks;
  int threadSleepNsec;
} OpenALThreadData;

// Function prototypes
ALuint* startAudio(int numSources, int bufferSize, HsStablePtr pdChan);
bool check_source_ready(ALuint sourceID);
ALuint create_source(int bufferSize);
bool tick_source_stream(ALuint sourceID, int sourceNum, OpenALThreadData *threadData);
void *openal_thread_start(void *threadArg);
void openal_thread_loop(OpenALThreadData *threadData);
void checkALError(void);
void checkALCError(ALCdevice *device);


pthread_mutex_t audioThreadIsRunningMutex;
pthread_t audioThread;
ALCdevice *audioDevice;
ALCdevice *inputDevice;
ALCcontext *audioContext;

/* Returns 1 (true) if the mutex is unlocked, which is the
 * thread's signal to terminate. 
 */
bool audioThreadIsRunning()
{
  if (pthread_mutex_trylock(&audioThreadIsRunningMutex) == 0) {
    pthread_mutex_unlock(&audioThreadIsRunningMutex);
    return 0;
  }
  return 1;
}

void stopAudio() {
  pthread_mutex_unlock(&audioThreadIsRunningMutex);
  pthread_join(audioThread, NULL);
  alcCaptureStop(inputDevice);

  alcMakeContextCurrent(NULL);
  alcDestroyContext(audioContext);

  alcCaptureCloseDevice(inputDevice);
  alcCloseDevice(audioDevice);

}

ALuint* startAudio(int numSources, int bufferSize, HsStablePtr pdChan) {
  const int pdOutBufferSize = bufferSize * numSources;
  const int pdInBufferSize  = bufferSize * PD_NUM_INPUTS;
  
  libpd_init();
  libpd_init_audio(PD_NUM_INPUTS, numSources, SAMPLE_RATE);

  libpd_start_message(1); // one entry in list
  libpd_add_float(1.0f);
  libpd_finish_message("pd", "dsp");

  // Open the default device and create an OpenAL context
  ALuint *allSourceIDs = (ALuint*)malloc(numSources * sizeof(ALuint));

  audioDevice = alcOpenDevice(NULL);
  if(!audioDevice) {
    checkALCError(audioDevice);
    fprintf(stderr, "Couldn't open OpenAL device :-(\n");
    return allSourceIDs;
  }
  audioContext = alcCreateContext(audioDevice, NULL);
  checkALCError(audioDevice);
  if(!audioContext) {
    fprintf(stderr, "Couldn't create OpenAL context :-(\n");
    return allSourceIDs;
  }

  alcMakeContextCurrent(audioContext);
  checkALCError(audioDevice);

  // Open the input device for capturing microphone/line-in


  // Get list of available Capture Devices
  // const ALchar *deviceList = alcGetString(NULL, ALC_CAPTURE_DEVICE_SPECIFIER);
  // if (deviceList)
  // {
  //     printf("\nAvailable Capture Devices are:\n");

  //     while (*deviceList)
  //     {
  //         printf("%s\n", deviceList);
  //         deviceList += strlen(deviceList) + 1;
  //     }
  // }
  const ALchar *defaultCaptureDevice = alcGetString(NULL, ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER);
  // printf("\nDefault Capture Device is '%s'\n\n", defaultCaptureDevice);

  inputDevice = alcCaptureOpenDevice(NULL, SAMPLE_RATE, FORMAT, SAMPLE_RATE);
  if (!inputDevice) {
    fprintf(stderr, "Couldn't get an input device :-(\n");
  }
  checkALCError(inputDevice);
  alcCaptureStart(inputDevice);
  checkALCError(inputDevice);

  // Enable HRTF spatialization
  #if defined(_WIN32)
  // TODO OpenAL-soft HRTF enable here
  #else
  static alcMacOSXRenderingQualityProcPtr alcMacOSXRenderingQuality = NULL;
  alcMacOSXRenderingQuality = 
    (alcMacOSXRenderingQualityProcPtr)alcGetProcAddress(NULL, (const ALCchar*)"alcMacOSXRenderingQuality");
  alcMacOSXRenderingQuality(ALC_MAC_OSX_SPATIAL_RENDERING_QUALITY_HIGH);
  #endif

  // Create the number of requested OpenAL sourceIDs
  for (int i = 0; i < numSources; ++i) {
    allSourceIDs[i] = create_source(bufferSize);
    //printf("Created source with ID: %i\n", allSourceIDs[i]);
  }

  add_reverb(allSourceIDs, numSources);

  OpenALThreadData *threadData = (OpenALThreadData *)malloc(sizeof(OpenALThreadData));
  threadData->allSourceIDs    = allSourceIDs;
  threadData->inputDevice     = inputDevice;
  threadData->pdChan          = pdChan;
  threadData->numSources      = numSources;
  threadData->bufferSize      = bufferSize;
  threadData->tempBuffer      = calloc(bufferSize,      sizeof(short));
  threadData->pdOutBuffer     = calloc(pdOutBufferSize, sizeof(short));
  threadData->pdInBuffer      = calloc(pdInBufferSize,  sizeof(short));
  threadData->pdTicks         = bufferSize/PD_BLOCK_SIZE;
  threadData->threadSleepNsec = ((double)bufferSize/SAMPLE_RATE) * NSEC_PER_SEC;
  // printf("OpenAL thread sleep time is %f seconds\n", (double)threadData->threadSleepNsec / NSEC_PER_SEC);

  // Spread the sources out
  for (int i = 0; i < numSources; ++i) {
    float pan = ((float)i/(float)numSources) * 2 - 1;

    ALuint sourceID = allSourceIDs[i];
    
    ALfloat sourcePos[] = {pan,0,-1};
    alSourcefv(sourceID, AL_POSITION, sourcePos);
  }

  // Create a locked mutex to serve as an indicator that we should
  // keep running the audio thread. When it's unlocked, we'll stop.
  pthread_mutex_init(&audioThreadIsRunningMutex, NULL);
  pthread_mutex_lock(&audioThreadIsRunningMutex);
  pthread_create(&audioThread, NULL, openal_thread_start, (void *)threadData);

  return allSourceIDs;
}

ALuint create_source(int bufferSize) {
  ALuint sourceID;
  ALuint sourceBufferIDs[NUM_BUFFERS_PER_SOURCE];

  // Create our buffers and source
  alGenBuffers(NUM_BUFFERS_PER_SOURCE, sourceBufferIDs);
  alGenSources(1, &sourceID);
  if(alGetError() != AL_NO_ERROR) {
    fprintf(stderr, "Error generating :(\n");
    return 1;
  }

  // Fill the buffers with initial data
  short *emptyBuffer = calloc(bufferSize, sizeof(short));
  for (int i = 0; i < NUM_BUFFERS_PER_SOURCE; i++) {
    alBufferData(sourceBufferIDs[i], FORMAT, emptyBuffer, bufferSize * sizeof(short), SAMPLE_RATE);
  }
  free(emptyBuffer);
  
  if(alGetError() != AL_NO_ERROR) {
    fprintf(stderr, "Error loading :(\n");
    return 1;
  }

  // Assign the source's bufferIDs to its sourceID to be dequeued later and begin playing
  alSourceQueueBuffers(sourceID, NUM_BUFFERS_PER_SOURCE, sourceBufferIDs);
  alSourcePlay(sourceID);

  if(alGetError() != AL_NO_ERROR) {
    fprintf(stderr, "Error starting :(\n");
    return 1;
  }
  return sourceID;
}

bool allSourcesReady(const ALuint* allSourceIDs, const int numSources) {
  // Continuously check each source to see if it has buffers ready to fill
  int numSourcesReady = 0;
  for (int i = 0; i < numSources; ++i) {
    ALuint sourceID = allSourceIDs[i];
    if (check_source_ready(sourceID)) {
      numSourcesReady++;
    }
  }
  // printf("Num sources ready: %i\n", numSourcesReady);
  return numSourcesReady >= numSources;
}

void *openal_thread_start(void *threadArg) {
  OpenALThreadData *threadData = (OpenALThreadData *)threadArg;
  while (audioThreadIsRunning()) {
    openal_thread_loop(threadData);
  }
  
  // On finish, free the sources
  const ALuint* allSourceIDs = threadData->allSourceIDs;
  const int numSources = threadData->numSources;
  for (int i = 0; i < numSources; ++i) {
    ALuint sourceID = allSourceIDs[i];
    alSourceStop(sourceID);    
  }
  alDeleteSources(numSources, allSourceIDs);
  return NULL;
}

void openal_thread_loop(OpenALThreadData *threadData) {
  const ALuint* allSourceIDs = threadData->allSourceIDs;
  const int numSources = threadData->numSources;

  // Pd processes all our channels at once, so we need all OpenAL sources
  // to be ready to receive the next Pd tick's worth of audio data.
  // Thus we wait for all OpenAL sources to have at least one buffer ready to fill
  // before performing a Pd tick.

  // Once all have reported they're ready, fill them all.
  bool shouldFill = allSourcesReady(allSourceIDs, numSources);
  if (!shouldFill)
  {
    // printf("Thread woken but no sources ready\n");
  }
  int numberOfFills = 0;
  while (shouldFill) {
    numberOfFills++;
    // printf("Beginning tick...\n");
    
    // If, after performing a tick, all sources are still ready, then immediately fill
    // them again without the thread sleep so as to not fall further and further behind.
      
    // printf("Filling...\n");
    
    alcCaptureSamples(threadData->inputDevice, threadData->pdInBuffer, threadData->bufferSize);

    /*
       buffer size is num channels * num ticks * num samples per tick (libpd_blocksize(), default 64)
       samples are interleaved, so 
       l0 r0 f0 l1 r1 f1 l2 r2 f2
       sample(n) = pdBuffer[sourceNum + (n * numSources)]
    */

    // Call into Haskell to ensure processing occurs 
    // on the dedicated thread we create for libpd
    processShort(
        threadData->pdChan, 
        threadData->pdTicks,
        threadData->pdInBuffer,
        threadData->pdOutBuffer);

    // Copy Pd's output to each OpenAL source
    for (int i = 0; i < numSources; ++i) {
      ALuint sourceID = allSourceIDs[i];
      tick_source_stream(sourceID, i, threadData);
    }

    shouldFill = allSourcesReady(allSourceIDs, numSources);
  }
  // Print how many times we filled up
  if (numberOfFills > 1) {
    // printf("Refilled %i times\n", numberOfFills);
  }
  
  // Grab excess microphone data
  // int samplesIn;
  // alcGetIntegerv(threadData->inputDevice, ALC_CAPTURE_SAMPLES, 1, &samplesIn);
  // if (samplesIn >= threadData->bufferSize) {
  //   printf("Purging mic samples: %i\n", samplesIn);
  //   alcCaptureSamples(threadData->inputDevice, threadData->pdInBuffer, threadData->bufferSize);
  // }
  
  // Wait a portion of the buffer size so as to not peg CPU
  nanosleep((struct timespec[]){{0, threadData->threadSleepNsec}}, NULL);
}

// Checks if an OpenAL source has finished processing any of its streaming buffers
bool check_source_ready(ALuint sourceID) {
  ALint numBuffersToFill;
  alGetSourcei(sourceID, AL_BUFFERS_PROCESSED, &numBuffersToFill);
  return numBuffersToFill > 0;
}

bool tick_source_stream(ALuint sourceID, int sourceNum, OpenALThreadData *threadData) {
  int numSources = threadData->numSources;
  int bufferSize = threadData->bufferSize;
  ALint numBuffersToFill;
  alGetSourcei(sourceID, AL_BUFFERS_PROCESSED, &numBuffersToFill);
  if(numBuffersToFill <= 0) {
    return false;
  }

  // printf("%i Getting %i buffers\n", sourceID, numBuffersToFill);
  
  // NOTE: I'm not paying attention to numBuffersToFill; we only fill one buffer each tick.

  // Copy interleaved output of pd into OpenAL buffer for this source
  // TODO: SIMD this
  for (int n = 0; n < bufferSize; ++n) {
    threadData->tempBuffer[n] = threadData->pdOutBuffer[sourceNum + (n * numSources)];
  }
    
  ALuint bufferID;
  alSourceUnqueueBuffers(sourceID, 1, &bufferID);
  alBufferData(bufferID, FORMAT, threadData->tempBuffer, bufferSize * sizeof(short), SAMPLE_RATE);
  alSourceQueueBuffers(sourceID, 1, &bufferID);
  if(alGetError() != AL_NO_ERROR) {
    fprintf(stderr, "Error buffering :(\n");
    return false;
  }
  
  ALint isPlaying;
  alGetSourcei(sourceID, AL_SOURCE_STATE, &isPlaying);
  if(isPlaying != AL_PLAYING) {
    // printf("Restarting play\n");
    alSourcePlay(sourceID);
  }
  return true;
}



// Wrap OpenAL API for easier use with Haskell's FFI
void setOpenALSourcePositionRaw(ALuint sourceID, ALfloat *values) {
  // printf("Source %i Position: (%f %f %f)\n", sourceID, values[0],values[1],values[2]);
  alSourcefv(sourceID, AL_POSITION, values);
  checkALError();
}

void setOpenALListenerPositionRaw(ALfloat *values) {
  // printf("Listener Position (note: we are inverting x in Haskell binding): (%f %f %f)\n", values[0],values[1],values[2]);
  alListenerfv(AL_POSITION, values);
}
void setOpenALListenerOrientationRaw(ALfloat *values) {
  // printf("Orientation UP: (%f %f %f) AT: (%f %f %f)\n", values[0],values[1],values[2],values[3],values[4],values[5]);
  alListenerfv(AL_ORIENTATION, values);
}

void setOpenALListenerGainRaw(ALfloat value) {
  alListenerf(AL_GAIN, value);
}

void setOpenALDistanceModelInverse() {
  alDistanceModel(AL_INVERSE_DISTANCE);
}

void setOpenALDistanceModelLinear() {
  alDistanceModel(AL_LINEAR_DISTANCE);
}

void setOpenALDistanceModelExponent() {
  alDistanceModel(AL_EXPONENT_DISTANCE);
}

void checkALError(void) {
  
  ALenum error = alGetError();
  switch(error) {
    case AL_NO_ERROR:
      break;                              

    case AL_INVALID_NAME:
      printf("OpenAL Error: Invalid name\n");
      break;                          

    case AL_INVALID_ENUM:
      printf("OpenAL Error: Invalid enum\n");
      break;                          

    case AL_INVALID_VALUE:
      printf("OpenAL Error: Invalid value\n");
      break;                         

    case AL_INVALID_OPERATION:
      printf("OpenAL Error: Invalid operation\n");
      break;                     

    case AL_OUT_OF_MEMORY:
      printf("OpenAL Error: Out of memory\n");
      break;                         
  }
}

void checkALCError(ALCdevice *device) {
  ALCenum error = alcGetError(device);
  switch (error) {
    case ALC_NO_ERROR:
      break;

    case ALC_INVALID_DEVICE:
      printf("OpenALC Error: Invalid Device\n");

    case ALC_INVALID_CONTEXT:
      printf("OpenALC Error: Invalid Context\n");

    case ALC_INVALID_ENUM:
      printf("OpenALC Error: Invalid Enum\n");

    case ALC_INVALID_VALUE:
      printf("OpenALC Error: Invalid Value\n");

    case ALC_OUT_OF_MEMORY:
      printf("OpenALC Error: Out of memory\n");
  }
}
