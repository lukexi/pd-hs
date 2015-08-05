#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#include <OpenAL/MacOSX_OALExtensions.h>

// See
// https://developer.apple.com/library/mac/samplecode/OpenALExample/Listings/Scene_m.html

#define NULL 0

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ALenum  alcASASetSourceProc(const ALuint property, ALuint source, ALvoid *data, ALuint dataSize)
{
    ALenum    err = 0;
    static  alcASASetSourceProcPtr  proc = NULL;
    
    if (proc == NULL) {
        proc = (alcASASetSourceProcPtr) alcGetProcAddress(NULL, (const ALCchar*) "alcASASetSource");
    }
    
    if (proc)
        err = proc(property, source, data, dataSize);
    return (err);
}
 
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ALenum  alcASASetListenerProc(const ALuint property, ALvoid *data, ALuint dataSize)
{
    ALenum    err = 0;
    static  alcASASetListenerProcPtr    proc = NULL;
    
    if (proc == NULL) {
        proc = (alcASASetListenerProcPtr) alcGetProcAddress(NULL, "alcASASetListener");
    }
    
    if (proc)
        err = proc(property, data, dataSize);
    return (err);
}

/* For reference:
#define ALC_ASA_REVERB_ROOM_TYPE_SmallRoom      0
#define ALC_ASA_REVERB_ROOM_TYPE_MediumRoom     1
#define ALC_ASA_REVERB_ROOM_TYPE_LargeRoom      2
#define ALC_ASA_REVERB_ROOM_TYPE_MediumHall     3
#define ALC_ASA_REVERB_ROOM_TYPE_LargeHall      4
#define ALC_ASA_REVERB_ROOM_TYPE_Plate        5
#define ALC_ASA_REVERB_ROOM_TYPE_MediumChamber    6
#define ALC_ASA_REVERB_ROOM_TYPE_LargeChamber   7
#define ALC_ASA_REVERB_ROOM_TYPE_Cathedral      8
#define ALC_ASA_REVERB_ROOM_TYPE_LargeRoom2     9
#define ALC_ASA_REVERB_ROOM_TYPE_MediumHall2    10
#define ALC_ASA_REVERB_ROOM_TYPE_MediumHall3    11
#define ALC_ASA_REVERB_ROOM_TYPE_LargeHall2     12

#define ALC_ASA_REVERB_QUALITY_Max          0x7F
#define ALC_ASA_REVERB_QUALITY_High         0x60
#define ALC_ASA_REVERB_QUALITY_Medium       0x40
#define ALC_ASA_REVERB_QUALITY_Low          0x20
#define ALC_ASA_REVERB_QUALITY_Min          0
*/

int add_reverb(ALuint* allSourceIDs, int numSourceIDs) {

  ALuint setting = 1;
  alcASASetListenerProc(alcGetEnumValue(NULL, "ALC_ASA_REVERB_ON"), &setting, sizeof(setting));

  ALuint roomtype = ALC_ASA_REVERB_ROOM_TYPE_LargeHall;
  alcASASetListenerProc(alcGetEnumValue(NULL, "ALC_ASA_REVERB_ROOM_TYPE"), &roomtype, sizeof(roomtype));

  ALuint quality = ALC_ASA_REVERB_QUALITY_High;
  alcASASetListenerProc(alcGetEnumValue(NULL, "ALC_ASA_REVERB_QUALITY"), &quality, sizeof(quality));

  // Global
  ALfloat level = 0.5;
  alcASASetListenerProc(alcGetEnumValue(NULL, "ALC_ASA_REVERB_GLOBAL_LEVEL"), &level, sizeof(level));

  // Per-source
  // ALfloat level = 0.5;
  // alcASASetSourceProc(alcGetEnumValue(NULL, "ALC_ASA_REVERB_SEND_LEVEL"), sourceID, &level, sizeof(level));

  // ALfloat occlusion = -50; // -100 (most occlusion) to 0 (no occlusion)
  // alcASASetSourceProc(alcGetEnumValue(NULL, "ALC_ASA_OCCLUSION"), sourceID, &occlusion, sizeof(occlusion));
  // ALfloat obstruction = -50; // -100 (most obstruction) to 0 (no obstruction)
  // alcASASetSourceProc(alcGetEnumValue(NULL, "ALC_ASA_OBSTRUCTION"), sourceID, &obstruction, sizeof(obstruction));

  return 0;
}
