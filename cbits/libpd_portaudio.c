#include <stdio.h>
#include <errno.h>
// #include <libproc.h>
#include <unistd.h>
#include "z_libpd.h"
#include <math.h>
#include "portaudio.h"

#include "Sound/Pd/Internal_stub.h"

#define SAMPLE_RATE   (44100)
#define NUM_CHANNELS_OUT (2)
#define NUM_CHANNELS_IN (0)

// Adapted from http://robertesler.com/libpd-xcode-projects/

typedef struct
{
    double inbuf[64 * NUM_CHANNELS_IN];
    // block size 64, one tick per buffer
    double outbuf[64 * NUM_CHANNELS_OUT]; //stereo outputs are interlaced, s[0] = RIGHT, s[1] = LEFT, etc..
    HsStablePtr pdChan;
}
PortAudioBuffers;

void logPortAudioError(PaError err) {
    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
}

//globals
static PortAudioBuffers data;

/* This routine will be called by the PortAudio engine when audio is needed.
 ** It may called at interrupt level on some machines so don't do anything
 ** that could mess up the system like calling malloc() or free().
 */
static int portAudioCallback( const void *inputBuffer, void *outputBuffer,
                              unsigned long framesPerBuffer,
                              const PaStreamCallbackTimeInfo* timeInfo,
                              PaStreamCallbackFlags statusFlags,
                              void *userData ) {
    /* Cast data passed through stream to our structure. */
    PortAudioBuffers *data = (PortAudioBuffers*)userData;
    float *out = (float*)outputBuffer;
    unsigned int i;

    (void) inputBuffer; /* Prevent unused variable warning. */

    static int ticks = 1;
    processFloat(data->pdChan, ticks, data->inbuf, data->outbuf);

    //dsp perform routine
    for( i=0; i<framesPerBuffer*NUM_CHANNELS_OUT; i++ )
    {
        if(i % 2) {
            *out++ = data->outbuf[i]; // right channel
        }
        else {
            *out++ = data->outbuf[i]; // left channel
        }
    }

    return 0;
}


PaStream *startAudio(HsStablePtr pdChan) {

    printf("PortAudio LibPd: Starting.\n");

    // init pd
    int sampleRate = SAMPLE_RATE;
    int blcksize = libpd_blocksize();

    libpd_init();
    libpd_init_audio(NUM_CHANNELS_IN, NUM_CHANNELS_OUT, sampleRate); //one channel in, two channels out


    // compute audio    [; pd dsp 1(
    libpd_start_message(1); // one entry in list
    libpd_add_float(1.0f);
    libpd_finish_message("pd", "dsp");

    PaStream *stream; //opens the audio stream
    PaError err;

    /* Initialize our data for use by callback. */
    data.pdChan = pdChan;
    for (int i = 0; i < blcksize; i++) {
        data.outbuf[i] = 0;
    }
    printf("PortAudio LibPd: init.\n");
    /* Initialize library before making any other calls. */
    err = Pa_Initialize();
    if( err != paNoError ) goto error;

    printf("PortAudio LibPd: open.\n");
    /* Open an audio I/O stream. */
    err = Pa_OpenDefaultStream( &stream,
                               NUM_CHANNELS_IN,          /* input channels */
                               NUM_CHANNELS_OUT,          /* output channels */
                               paFloat32,  /* 32 bit floating point output */
                               SAMPLE_RATE,
                               (long)blcksize,        /* frames per buffer */
                               portAudioCallback,
                               &data );
    if( err != paNoError ) goto error;

    printf("PortAudio LibPd: start.\n");
    err = Pa_StartStream( stream );
    if( err != paNoError ) goto error;

    printf("PortAudio LibPd: Returning stream.\n");
    return stream;

error:
    Pa_Terminate();
    logPortAudioError(err);

    return 0;
}

void finishAudio(void *stream_) {
    printf("PortAudio LibPd: Shutting down.\n");
    PaStream *stream = (PaStream *)stream_;
    PaError err;
    err = Pa_StopStream( stream );
    if( err != paNoError ) logPortAudioError(err);
    printf("PortAudio LibPd: Close stream.\n");
    err = Pa_CloseStream( stream );
    if( err != paNoError ) logPortAudioError(err);
    printf("PortAudio LibPd: Terminate.\n");
    Pa_Terminate();
}
