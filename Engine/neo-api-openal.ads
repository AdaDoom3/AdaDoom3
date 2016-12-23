
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

-- Custom binding to the OpenAL Soft API: https://web.archive.org/web/20160409112902/http://www.openal.org/documentation/openal-1.1-specification.pdf
package Neo.API.OpenAL is

  -----------
  -- Types --
  -----------

  -- ALvoid     Ptr
  -- ALint      Int_C
  -- ALsizei    Int_Size_C
  -- ALenum     Int_32_Unsigned_C
  -- ALboolean  Int_8_Unsigned_C
  -- ALbyte     Int_8_Signed_C
  -- ALubyte    Int_8_Unsigned_C
  -- ALshort    Int_16_Signed_C
  -- ALushort   Int_16_Unsigned_C
  -- ALuint     Int_32_Unsigned_C
  -- ALfloat    Real_32_C
  -- ALdouble   Real_64_C
  -- ALint*     Ptr_Int_C,             Ptr_Array_Int_C
  -- ALboolean* Ptr_Int_8_Unsigned_C,  Ptr_Array_Int_8_Unsigned_C
  -- ALbyte*    Ptr_Int_8_Signed_C,    Ptr_Array_Int_8_Signed_C
  -- ALubyte*   Ptr_Int_8_Unsigned_C,  Ptr_Array_Int_8_Unsigned_C
  -- ALshort*   Ptr_Int_16_Signed_C,   Ptr_Array_Int_16_Signed_C
  -- ALushort*  Ptr_Int_16_Unsigned_C, Ptr_Array_Int_16_Unsigned_C
  -- ALuint*    Ptr_Int_32_Unsigned_C, Ptr_Array_Int_32_Unsigned_C
  -- ALfloat*   Ptr_Real_32_C,         Ptr_Array_Real_32_C
  -- ALdouble*  Ptr_Real_64_C,         Ptr_Array_Real_64_C
  -- ALCdevice* Ptr

  ---------------
  -- Constants --
  ---------------

  -- "no distance model" or "no buffer"
  AL_NONE  : constant Int_32_Unsigned_C := 0; -- 0

  -- AL Boolean
  AL_FALSE : constant Int_8_Unsigned_C := 0; -- 0
  AL_TRUE  : constant Int_8_Unsigned_C := 1; -- 0

  -- 2.7. AL Errors
  AL_NO_ERROR          : constant Int_32_Unsigned_C := 16#0000_0000#; -- 0x0000
  AL_INVALID_NAME      : constant Int_32_Unsigned_C := 16#0000_A001#; -- 0x0000
  AL_INVALID_ENUM      : constant Int_32_Unsigned_C := 16#0000_A002#; -- 0x0000
  AL_INVALID_VALUE     : constant Int_32_Unsigned_C := 16#0000_A003#; -- 0x0000
  AL_INVALID_OPERATION : constant Int_32_Unsigned_C := 16#0000_A004#; -- 0x0000
  AL_OUT_OF_MEMORY     : constant Int_32_Unsigned_C := 16#0000_A005#; -- 0x0000

  -- 3.1.1. Simple Queries
  AL_DOPPLER_FACTOR : constant Int_32_Unsigned_C := 16#0000_C000#; -- 0x0000
  AL_SPEED_OF_SOUND : constant Int_32_Unsigned_C := 16#0000_C003#; -- 0x0000
  AL_DISTANCE_MODEL : constant Int_32_Unsigned_C := 16#0000_D000#; -- 0x0000

  -- 3.1.2. String Queries
  AL_VENDOR     : constant Int_32_Unsigned_C := 16#0000_B001#; -- 0x0000
  AL_VERSION    : constant Int_32_Unsigned_C := 16#0000_B002#; -- 0x0000
  AL_RENDERER   : constant Int_32_Unsigned_C := 16#0000_B003#; -- 0x0000
  AL_EXTENSIONS : constant Int_32_Unsigned_C := 16#0000_B004#; -- 0x0000

  -- 3.4. Attenuation By Distance
  AL_INVERSE_DISTANCE          : constant Int_32_Unsigned_C := 16#0000_D001#; -- 0x0000
  AL_INVERSE_DISTANCE_CLAMPED  : constant Int_32_Unsigned_C := 16#0000_D002#; -- 0x0000
  AL_LINEAR_DISTANCE           : constant Int_32_Unsigned_C := 16#0000_D003#; -- 0x0000
  AL_LINEAR_DISTANCE_CLAMPED   : constant Int_32_Unsigned_C := 16#0000_D004#; -- 0x0000
  AL_EXPONENT_DISTANCE         : constant Int_32_Unsigned_C := 16#0000_D005#; -- 0x0000
  AL_EXPONENT_DISTANCE_CLAMPED : constant Int_32_Unsigned_C := 16#0000_D006#; -- 0x0000

  -- 4.1. Basic Listener and Source Attributes
  AL_POSITION : constant Int_32_Unsigned_C := 16#0000_1004#; -- 0x0000
  AL_VELOCITY : constant Int_32_Unsigned_C := 16#0000_1006#; -- 0x0000
  AL_GAIN     : constant Int_32_Unsigned_C := 16#0000_100A#; -- 0x0000

  -- 4.2.1. Listener Attributes
  AL_ORIENTATION : constant Int_32_Unsigned_C := 16#0000_100F#; -- 0x0000

  -- 4.3.2. Source Attributes
  AL_SOURCE_RELATIVE    : constant Int_32_Unsigned_C := 16#0000_0202#; -- 0x000
  AL_SOURCE_TYPE        : constant Int_32_Unsigned_C := 16#0000_1027#; -- 0x0000
  AL_UNDETERMINED       : constant Int_32_Unsigned_C := 16#0000_1030#; -- 0x0000
  AL_STATIC             : constant Int_32_Unsigned_C := 16#0000_1028#; -- 0x0000
  AL_STREAMING          : constant Int_32_Unsigned_C := 16#0000_1029#; -- 0x0000
  AL_LOOPING            : constant Int_32_Unsigned_C := 16#0000_1007#; -- 0x0000
  AL_BUFFER             : constant Int_32_Unsigned_C := 16#0000_1009#; -- 0x0000
  AL_BUFFERS_QUEUED     : constant Int_32_Unsigned_C := 16#0000_1015#; -- 0x0000
  AL_BUFFERS_PROCESSED  : constant Int_32_Unsigned_C := 16#0000_1016#; -- 0x0000
  AL_MIN_GAIN           : constant Int_32_Unsigned_C := 16#0000_100D#; -- 0x0000
  AL_MAX_GAIN           : constant Int_32_Unsigned_C := 16#0000_100E#; -- 0x0000
  AL_REFERENCE_DISTANCE : constant Int_32_Unsigned_C := 16#0000_1020#; -- 0x0000
  AL_ROLLOFF_FACTOR     : constant Int_32_Unsigned_C := 16#0000_1021#; -- 0x0000
  AL_MAX_DISTANCE       : constant Int_32_Unsigned_C := 16#0000_1023#; -- 0x0000
  AL_PITCH              : constant Int_32_Unsigned_C := 16#0000_1003#; -- 0x0000
  AL_DIRECTION          : constant Int_32_Unsigned_C := 16#0000_1005#; -- 0x0000
  AL_CONE_INNER_ANGLE   : constant Int_32_Unsigned_C := 16#0000_1001#; -- 0x0000
  AL_CONE_OUTER_ANGLE   : constant Int_32_Unsigned_C := 16#0000_1002#; -- 0x0000
  AL_CONE_OUTER_GAIN    : constant Int_32_Unsigned_C := 16#0000_1022#; -- 0x0000
  AL_SEC_OFFSET         : constant Int_32_Unsigned_C := 16#0000_1024#; -- 0x0000
  AL_SAMPLE_OFFSET      : constant Int_32_Unsigned_C := 16#0000_1025#; -- 0x0000
  AL_BYTE_OFFSET        : constant Int_32_Unsigned_C := 16#0000_1026#; -- 0x0000

  -- 4.3.6. Managing Source Execution
  AL_SOURCE_STATE : constant Int_32_Unsigned_C := 16#0000_1010#; -- 0x0000
  AL_INITIAL      : constant Int_32_Unsigned_C := 16#0000_1011#; -- 0x0000
  AL_PLAYING      : constant Int_32_Unsigned_C := 16#0000_1012#; -- 0x0000
  AL_PAUSED       : constant Int_32_Unsigned_C := 16#0000_1013#; -- 0x0000
  AL_STOPPED      : constant Int_32_Unsigned_C := 16#0000_1014#; -- 0x0000

  -- 5.3.1. Buffer Attributes
  AL_FREQUENCY : constant Int_32_Unsigned_C := 16#0000_2001#; -- 0x0000
  AL_BITS      : constant Int_32_Unsigned_C := 16#0000_2002#; -- 0x0000
  AL_CHANNELS  : constant Int_32_Unsigned_C := 16#0000_2003#; -- 0x0000
  AL_SIZE      : constant Int_32_Unsigned_C := 16#0000_2004#; -- 0x0000

  -- 5.3.4. Specifying Buffer Content
  AL_FORMAT_MONO8    : constant Int_32_Unsigned_C := 16#0000_1100#; -- 0x0000
  AL_FORMAT_MONO16   : constant Int_32_Unsigned_C := 16#0000_1101#; -- 0x0000
  AL_FORMAT_STEREO8  : constant Int_32_Unsigned_C := 16#0000_1102#; -- 0x0000
  AL_FORMAT_STEREO16 : constant Int_32_Unsigned_C := 16#0000_1103#; -- 0x0000

  -- ALC Boolean
  ALC_FALSE : constant Int_8_Unsigned_C := 0; -- 0
  ALC_TRUE  : constant Int_8_Unsigned_C := 1; -- 0

  -- 6.2.1. Context Attributes
  ALC_FREQUENCY      : constant Int_32_Unsigned_C := 16#0000_1007#; -- 0x0000
  ALC_REFRESH        : constant Int_32_Unsigned_C := 16#0000_1008#; -- 0x0000
  ALC_SYNC           : constant Int_32_Unsigned_C := 16#0000_1009#; -- 0x0000
  ALC_MONO_SOURCES   : constant Int_32_Unsigned_C := 16#0000_1010#; -- 0x0000
  ALC_STEREO_SOURCES : constant Int_32_Unsigned_C := 16#0000_1011#; -- 0x0000

  -- 6.3.6. Query for Error Conditions
  ALC_NO_ERROR        : constant Int_32_Unsigned_C := 16#0000_0000#; -- 0x0000
  ALC_INVALID_DEVICE  : constant Int_32_Unsigned_C := 16#0000_A001#; -- 0x0000
  ALC_INVALID_CONTEXT : constant Int_32_Unsigned_C := 16#0000_A002#; -- 0x0000
  ALC_INVALID_ENUM    : constant Int_32_Unsigned_C := 16#0000_A003#; -- 0x0000
  ALC_INVALID_VALUE   : constant Int_32_Unsigned_C := 16#0000_A004#; -- 0x0000
  ALC_OUT_OF_MEMORY   : constant Int_32_Unsigned_C := 16#0000_A005#; -- 0x0000

  -- 6.3.7. String Query
  ALC_DEFAULT_DEVICE_SPECIFIER : constant Int_32_Unsigned_C := 16#0000_1004#; -- 0x0000
  ALC_DEVICE_SPECIFIER         : constant Int_32_Unsigned_C := 16#0000_1005#; -- 0x0000
  ALC_EXTENSIONS               : constant Int_32_Unsigned_C := 16#0000_1006#; -- 0x0000

  -- 6.3.8. Integer Query
  ALC_ATTRIBUTES_SIZE : constant Int_32_Unsigned_C := 16#0000_1002#; -- 0x0000
  ALC_ALL_ATTRIBUTES  : constant Int_32_Unsigned_C := 16#0000_1003#; -- 0x0000
  ALC_MAJOR_VERSION   : constant Int_32_Unsigned_C := 16#0000_1000#; -- 0x0000
  ALC_MINOR_VERSION   : constant Int_32_Unsigned_C := 16#0000_1001#; -- 0x0000

  -- 6.4.2. Capture
  ALC_CAPTURE_DEVICE_SPECIFIER         : constant Int_32_Unsigned_C := 16#0000_310#; -- 0x0000
  ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER : constant Int_32_Unsigned_C := 16#0000_311#; -- 0x0000
  ALC_CAPTURE_SAMPLES                  : constant Int_32_Unsigned_C := 16#0000_312#; -- 0x0000

#ifndef AL_LOKI_IMA_ADPCM_format
#define AL_LOKI_IMA_ADPCM_format 1
#define AL_FORMAT_IMA_ADPCM_MONO16_EXT           0x10000
#define AL_FORMAT_IMA_ADPCM_STEREO16_EXT         0x10001
#endif

#ifndef AL_LOKI_WAVE_format
#define AL_LOKI_WAVE_format 1
#define AL_FORMAT_WAVE_EXT                       0x10002
#endif

#ifndef AL_EXT_vorbis
#define AL_EXT_vorbis 1
#define AL_FORMAT_VORBIS_EXT                     0x10003
#endif

#ifndef AL_LOKI_quadriphonic
#define AL_LOKI_quadriphonic 1
#define AL_FORMAT_QUAD8_LOKI                     0x10004
#define AL_FORMAT_QUAD16_LOKI                    0x10005
#endif

#ifndef AL_EXT_float32
#define AL_EXT_float32 1
#define AL_FORMAT_MONO_FLOAT32                   0x10010
#define AL_FORMAT_STEREO_FLOAT32                 0x10011
#endif

#ifndef AL_EXT_double
#define AL_EXT_double 1
#define AL_FORMAT_MONO_DOUBLE_EXT                0x10012
#define AL_FORMAT_STEREO_DOUBLE_EXT              0x10013
#endif

#ifndef AL_EXT_MULAW
#define AL_EXT_MULAW 1
#define AL_FORMAT_MONO_MULAW_EXT                 0x10014
#define AL_FORMAT_STEREO_MULAW_EXT               0x10015
#endif

#ifndef AL_EXT_ALAW
#define AL_EXT_ALAW 1
#define AL_FORMAT_MONO_ALAW_EXT                  0x10016
#define AL_FORMAT_STEREO_ALAW_EXT                0x10017
#endif

#ifndef ALC_LOKI_audio_channel
#define ALC_LOKI_audio_channel 1
#define ALC_CHAN_MAIN_LOKI                       0x500001
#define ALC_CHAN_PCM_LOKI                        0x500002
#define ALC_CHAN_CD_LOKI                         0x500003
#endif

#ifndef AL_EXT_MCFORMATS
#define AL_EXT_MCFORMATS 1
#define AL_FORMAT_QUAD8                          0x1204
#define AL_FORMAT_QUAD16                         0x1205
#define AL_FORMAT_QUAD32                         0x1206
#define AL_FORMAT_REAR8                          0x1207
#define AL_FORMAT_REAR16                         0x1208
#define AL_FORMAT_REAR32                         0x1209
#define AL_FORMAT_51CHN8                         0x120A
#define AL_FORMAT_51CHN16                        0x120B
#define AL_FORMAT_51CHN32                        0x120C
#define AL_FORMAT_61CHN8                         0x120D
#define AL_FORMAT_61CHN16                        0x120E
#define AL_FORMAT_61CHN32                        0x120F
#define AL_FORMAT_71CHN8                         0x1210
#define AL_FORMAT_71CHN16                        0x1211
#define AL_FORMAT_71CHN32                        0x1212
#endif

#ifndef AL_EXT_MULAW_MCFORMATS
#define AL_EXT_MULAW_MCFORMATS 1
#define AL_FORMAT_MONO_MULAW                     0x10014
#define AL_FORMAT_STEREO_MULAW                   0x10015
#define AL_FORMAT_QUAD_MULAW                     0x10021
#define AL_FORMAT_REAR_MULAW                     0x10022
#define AL_FORMAT_51CHN_MULAW                    0x10023
#define AL_FORMAT_61CHN_MULAW                    0x10024
#define AL_FORMAT_71CHN_MULAW                    0x10025
#endif

#ifndef AL_EXT_IMA4
#define AL_EXT_IMA4 1
#define AL_FORMAT_MONO_IMA4                      0x1300
#define AL_FORMAT_STEREO_IMA4                    0x1301
#endif

#ifndef AL_EXT_STATIC_BUFFER
#define AL_EXT_STATIC_BUFFER 1


#ifndef AL_EXT_source_distance_model
#define AL_EXT_source_distance_model 1
#define AL_SOURCE_DISTANCE_MODEL                 0x200
#endif

#ifndef AL_SOFT_buffer_sub_data
#define AL_SOFT_buffer_sub_data 1
#define AL_BYTE_RW_OFFSETS_SOFT                  0x1031
#define AL_SAMPLE_RW_OFFSETS_SOFT                0x1032

#ifndef ALC_EXT_EFX
#define ALC_EXT_EFX 1
#include "efx.h"
#endif

#ifndef ALC_EXT_disconnect
#define ALC_EXT_disconnect 1
#define ALC_CONNECTED                            0x313
#endif

#ifndef ALC_EXT_thread_local_context
#define ALC_EXT_thread_local_context 1


#define ALC_EFX_MAJOR_VERSION                    0x20001
#define ALC_EFX_MINOR_VERSION                    0x20002
#define ALC_MAX_AUXILIARY_SENDS                  0x20003




#ifndef AL_SOFT_direct_channels
#define AL_SOFT_direct_channels 1
#define AL_DIRECT_CHANNELS_SOFT                  0x1033
#endif

#ifndef ALC_SOFT_loopback
#define ALC_SOFT_loopback 1
#define ALC_FORMAT_CHANNELS_SOFT                 0x1990
#define ALC_FORMAT_TYPE_SOFT                     0x1991

/* Sample types */
#define ALC_BYTE_SOFT                            0x1400
#define ALC_UNSIGNED_BYTE_SOFT                   0x1401
#define ALC_SHORT_SOFT                           0x1402
#define ALC_UNSIGNED_SHORT_SOFT                  0x1403
#define ALC_INT_SOFT                             0x1404
#define ALC_UNSIGNED_INT_SOFT                    0x1405
#define ALC_FLOAT_SOFT                           0x1406

/* Channel configurations */
#define ALC_MONO_SOFT                            0x1500
#define ALC_STEREO_SOFT                          0x1501
#define ALC_QUAD_SOFT                            0x1503
#define ALC_5POINT1_SOFT                         0x1504
#define ALC_6POINT1_SOFT                         0x1505
#define ALC_7POINT1_SOFT                         0x1506


#ifndef AL_EXT_STEREO_ANGLES
#define AL_EXT_STEREO_ANGLES 1
#define AL_STEREO_ANGLES                         0x1030
#endif

#ifndef AL_EXT_SOURCE_RADIUS
#define AL_EXT_SOURCE_RADIUS 1
#define AL_SOURCE_RADIUS                         0x1031
#endif

#ifndef AL_SOFT_source_latency
#define AL_SOFT_source_latency 1
#define AL_SAMPLE_OFFSET_LATENCY_SOFT            0x1200
#define AL_SEC_OFFSET_LATENCY_SOFT               0x1201
typedef int64_t ALint64SOFT;
typedef uint64_t ALuint64SOFT;





/* Listener properties. */
#define AL_METERS_PER_UNIT                       0x20004

/* Source properties. */
#define AL_DIRECT_FILTER                         0x20005
#define AL_AUXILIARY_SEND_FILTER                 0x20006
#define AL_AIR_ABSORPTION_FACTOR                 0x20007
#define AL_ROOM_ROLLOFF_FACTOR                   0x20008
#define AL_CONE_OUTER_GAINHF                     0x20009
#define AL_DIRECT_FILTER_GAINHF_AUTO             0x2000A
#define AL_AUXILIARY_SEND_FILTER_GAIN_AUTO       0x2000B
#define AL_AUXILIARY_SEND_FILTER_GAINHF_AUTO     0x2000C


/* Effect properties. */

/* Reverb effect parameters */
#define AL_REVERB_DENSITY                        0x0001
#define AL_REVERB_DIFFUSION                      0x0002
#define AL_REVERB_GAIN                           0x0003
#define AL_REVERB_GAINHF                         0x0004
#define AL_REVERB_DECAY_TIME                     0x0005
#define AL_REVERB_DECAY_HFRATIO                  0x0006
#define AL_REVERB_REFLECTIONS_GAIN               0x0007
#define AL_REVERB_REFLECTIONS_DELAY              0x0008
#define AL_REVERB_LATE_REVERB_GAIN               0x0009
#define AL_REVERB_LATE_REVERB_DELAY              0x000A
#define AL_REVERB_AIR_ABSORPTION_GAINHF          0x000B
#define AL_REVERB_ROOM_ROLLOFF_FACTOR            0x000C
#define AL_REVERB_DECAY_HFLIMIT                  0x000D

/* EAX Reverb effect parameters */
#define AL_EAXREVERB_DENSITY                     0x0001
#define AL_EAXREVERB_DIFFUSION                   0x0002
#define AL_EAXREVERB_GAIN                        0x0003
#define AL_EAXREVERB_GAINHF                      0x0004
#define AL_EAXREVERB_GAINLF                      0x0005
#define AL_EAXREVERB_DECAY_TIME                  0x0006
#define AL_EAXREVERB_DECAY_HFRATIO               0x0007
#define AL_EAXREVERB_DECAY_LFRATIO               0x0008
#define AL_EAXREVERB_REFLECTIONS_GAIN            0x0009
#define AL_EAXREVERB_REFLECTIONS_DELAY           0x000A
#define AL_EAXREVERB_REFLECTIONS_PAN             0x000B
#define AL_EAXREVERB_LATE_REVERB_GAIN            0x000C
#define AL_EAXREVERB_LATE_REVERB_DELAY           0x000D
#define AL_EAXREVERB_LATE_REVERB_PAN             0x000E
#define AL_EAXREVERB_ECHO_TIME                   0x000F
#define AL_EAXREVERB_ECHO_DEPTH                  0x0010
#define AL_EAXREVERB_MODULATION_TIME             0x0011
#define AL_EAXREVERB_MODULATION_DEPTH            0x0012
#define AL_EAXREVERB_AIR_ABSORPTION_GAINHF       0x0013
#define AL_EAXREVERB_HFREFERENCE                 0x0014
#define AL_EAXREVERB_LFREFERENCE                 0x0015
#define AL_EAXREVERB_ROOM_ROLLOFF_FACTOR         0x0016
#define AL_EAXREVERB_DECAY_HFLIMIT               0x0017

/* Chorus effect parameters */
#define AL_CHORUS_WAVEFORM                       0x0001
#define AL_CHORUS_PHASE                          0x0002
#define AL_CHORUS_RATE                           0x0003
#define AL_CHORUS_DEPTH                          0x0004
#define AL_CHORUS_FEEDBACK                       0x0005
#define AL_CHORUS_DELAY                          0x0006

/* Distortion effect parameters */
#define AL_DISTORTION_EDGE                       0x0001
#define AL_DISTORTION_GAIN                       0x0002
#define AL_DISTORTION_LOWPASS_CUTOFF             0x0003
#define AL_DISTORTION_EQCENTER                   0x0004
#define AL_DISTORTION_EQBANDWIDTH                0x0005

/* Echo effect parameters */
#define AL_ECHO_DELAY                            0x0001
#define AL_ECHO_LRDELAY                          0x0002
#define AL_ECHO_DAMPING                          0x0003
#define AL_ECHO_FEEDBACK                         0x0004
#define AL_ECHO_SPREAD                           0x0005

/* Flanger effect parameters */
#define AL_FLANGER_WAVEFORM                      0x0001
#define AL_FLANGER_PHASE                         0x0002
#define AL_FLANGER_RATE                          0x0003
#define AL_FLANGER_DEPTH                         0x0004
#define AL_FLANGER_FEEDBACK                      0x0005
#define AL_FLANGER_DELAY                         0x0006

/* Frequency shifter effect parameters */
#define AL_FREQUENCY_SHIFTER_FREQUENCY           0x0001
#define AL_FREQUENCY_SHIFTER_LEFT_DIRECTION      0x0002
#define AL_FREQUENCY_SHIFTER_RIGHT_DIRECTION     0x0003

/* Vocal morpher effect parameters */
#define AL_VOCAL_MORPHER_PHONEMEA                0x0001
#define AL_VOCAL_MORPHER_PHONEMEA_COARSE_TUNING  0x0002
#define AL_VOCAL_MORPHER_PHONEMEB                0x0003
#define AL_VOCAL_MORPHER_PHONEMEB_COARSE_TUNING  0x0004
#define AL_VOCAL_MORPHER_WAVEFORM                0x0005
#define AL_VOCAL_MORPHER_RATE                    0x0006

/* Pitchshifter effect parameters */
#define AL_PITCH_SHIFTER_COARSE_TUNE             0x0001
#define AL_PITCH_SHIFTER_FINE_TUNE               0x0002

/* Ringmodulator effect parameters */
#define AL_RING_MODULATOR_FREQUENCY              0x0001
#define AL_RING_MODULATOR_HIGHPASS_CUTOFF        0x0002
#define AL_RING_MODULATOR_WAVEFORM               0x0003

/* Autowah effect parameters */
#define AL_AUTOWAH_ATTACK_TIME                   0x0001
#define AL_AUTOWAH_RELEASE_TIME                  0x0002
#define AL_AUTOWAH_RESONANCE                     0x0003
#define AL_AUTOWAH_PEAK_GAIN                     0x0004

/* Compressor effect parameters */
#define AL_COMPRESSOR_ONOFF                      0x0001

/* Equalizer effect parameters */
#define AL_EQUALIZER_LOW_GAIN                    0x0001
#define AL_EQUALIZER_LOW_CUTOFF                  0x0002
#define AL_EQUALIZER_MID1_GAIN                   0x0003
#define AL_EQUALIZER_MID1_CENTER                 0x0004
#define AL_EQUALIZER_MID1_WIDTH                  0x0005
#define AL_EQUALIZER_MID2_GAIN                   0x0006
#define AL_EQUALIZER_MID2_CENTER                 0x0007
#define AL_EQUALIZER_MID2_WIDTH                  0x0008
#define AL_EQUALIZER_HIGH_GAIN                   0x0009
#define AL_EQUALIZER_HIGH_CUTOFF                 0x000A

/* Effect type */
#define AL_EFFECT_FIRST_PARAMETER                0x0000
#define AL_EFFECT_LAST_PARAMETER                 0x8000
#define AL_EFFECT_TYPE                           0x8001

/* Effect types, used with the AL_EFFECT_TYPE property */
#define AL_EFFECT_NULL                           0x0000
#define AL_EFFECT_REVERB                         0x0001
#define AL_EFFECT_CHORUS                         0x0002
#define AL_EFFECT_DISTORTION                     0x0003
#define AL_EFFECT_ECHO                           0x0004
#define AL_EFFECT_FLANGER                        0x0005
#define AL_EFFECT_FREQUENCY_SHIFTER              0x0006
#define AL_EFFECT_VOCAL_MORPHER                  0x0007
#define AL_EFFECT_PITCH_SHIFTER                  0x0008
#define AL_EFFECT_RING_MODULATOR                 0x0009
#define AL_EFFECT_AUTOWAH                        0x000A
#define AL_EFFECT_COMPRESSOR                     0x000B
#define AL_EFFECT_EQUALIZER                      0x000C
#define AL_EFFECT_EAXREVERB                      0x8000

/* Auxiliary Effect Slot properties. */
#define AL_EFFECTSLOT_EFFECT                     0x0001
#define AL_EFFECTSLOT_GAIN                       0x0002
#define AL_EFFECTSLOT_AUXILIARY_SEND_AUTO        0x0003

/* NULL Auxiliary Slot ID to disable a source send. */
#define AL_EFFECTSLOT_NULL                       0x0000


/* Filter properties. */

/* Lowpass filter parameters */
#define AL_LOWPASS_GAIN                          0x0001
#define AL_LOWPASS_GAINHF                        0x0002

/* Highpass filter parameters */
#define AL_HIGHPASS_GAIN                         0x0001
#define AL_HIGHPASS_GAINLF                       0x0002

/* Bandpass filter parameters */
#define AL_BANDPASS_GAIN                         0x0001
#define AL_BANDPASS_GAINLF                       0x0002
#define AL_BANDPASS_GAINHF                       0x0003

/* Filter type */
#define AL_FILTER_FIRST_PARAMETER                0x0000
#define AL_FILTER_LAST_PARAMETER                 0x8000
#define AL_FILTER_TYPE                           0x8001

/* Filter types, used with the AL_FILTER_TYPE property */
#define AL_FILTER_NULL                           0x0000
#define AL_FILTER_LOWPASS                        0x0001
#define AL_FILTER_HIGHPASS                       0x0002
#define AL_FILTER_BANDPASS                       0x0003

/* Filter ranges and defaults. */

/* Lowpass filter */
#define AL_LOWPASS_MIN_GAIN                      (0.0f)
#define AL_LOWPASS_MAX_GAIN                      (1.0f)
#define AL_LOWPASS_DEFAULT_GAIN                  (1.0f)

#define AL_LOWPASS_MIN_GAINHF                    (0.0f)
#define AL_LOWPASS_MAX_GAINHF                    (1.0f)
#define AL_LOWPASS_DEFAULT_GAINHF                (1.0f)

/* Highpass filter */
#define AL_HIGHPASS_MIN_GAIN                     (0.0f)
#define AL_HIGHPASS_MAX_GAIN                     (1.0f)
#define AL_HIGHPASS_DEFAULT_GAIN                 (1.0f)

#define AL_HIGHPASS_MIN_GAINLF                   (0.0f)
#define AL_HIGHPASS_MAX_GAINLF                   (1.0f)
#define AL_HIGHPASS_DEFAULT_GAINLF               (1.0f)

/* Bandpass filter */
#define AL_BANDPASS_MIN_GAIN                     (0.0f)
#define AL_BANDPASS_MAX_GAIN                     (1.0f)
#define AL_BANDPASS_DEFAULT_GAIN                 (1.0f)

#define AL_BANDPASS_MIN_GAINHF                   (0.0f)
#define AL_BANDPASS_MAX_GAINHF                   (1.0f)
#define AL_BANDPASS_DEFAULT_GAINHF               (1.0f)

#define AL_BANDPASS_MIN_GAINLF                   (0.0f)
#define AL_BANDPASS_MAX_GAINLF                   (1.0f)
#define AL_BANDPASS_DEFAULT_GAINLF               (1.0f)


/* Effect parameter ranges and defaults. */

/* Standard reverb effect */
#define AL_REVERB_MIN_DENSITY                    (0.0f)
#define AL_REVERB_MAX_DENSITY                    (1.0f)
#define AL_REVERB_DEFAULT_DENSITY                (1.0f)

#define AL_REVERB_MIN_DIFFUSION                  (0.0f)
#define AL_REVERB_MAX_DIFFUSION                  (1.0f)
#define AL_REVERB_DEFAULT_DIFFUSION              (1.0f)

#define AL_REVERB_MIN_GAIN                       (0.0f)
#define AL_REVERB_MAX_GAIN                       (1.0f)
#define AL_REVERB_DEFAULT_GAIN                   (0.32f)

#define AL_REVERB_MIN_GAINHF                     (0.0f)
#define AL_REVERB_MAX_GAINHF                     (1.0f)
#define AL_REVERB_DEFAULT_GAINHF                 (0.89f)

#define AL_REVERB_MIN_DECAY_TIME                 (0.1f)
#define AL_REVERB_MAX_DECAY_TIME                 (20.0f)
#define AL_REVERB_DEFAULT_DECAY_TIME             (1.49f)

#define AL_REVERB_MIN_DECAY_HFRATIO              (0.1f)
#define AL_REVERB_MAX_DECAY_HFRATIO              (2.0f)
#define AL_REVERB_DEFAULT_DECAY_HFRATIO          (0.83f)

#define AL_REVERB_MIN_REFLECTIONS_GAIN           (0.0f)
#define AL_REVERB_MAX_REFLECTIONS_GAIN           (3.16f)
#define AL_REVERB_DEFAULT_REFLECTIONS_GAIN       (0.05f)

#define AL_REVERB_MIN_REFLECTIONS_DELAY          (0.0f)
#define AL_REVERB_MAX_REFLECTIONS_DELAY          (0.3f)
#define AL_REVERB_DEFAULT_REFLECTIONS_DELAY      (0.007f)

#define AL_REVERB_MIN_LATE_REVERB_GAIN           (0.0f)
#define AL_REVERB_MAX_LATE_REVERB_GAIN           (10.0f)
#define AL_REVERB_DEFAULT_LATE_REVERB_GAIN       (1.26f)

#define AL_REVERB_MIN_LATE_REVERB_DELAY          (0.0f)
#define AL_REVERB_MAX_LATE_REVERB_DELAY          (0.1f)
#define AL_REVERB_DEFAULT_LATE_REVERB_DELAY      (0.011f)

#define AL_REVERB_MIN_AIR_ABSORPTION_GAINHF      (0.892f)
#define AL_REVERB_MAX_AIR_ABSORPTION_GAINHF      (1.0f)
#define AL_REVERB_DEFAULT_AIR_ABSORPTION_GAINHF  (0.994f)

#define AL_REVERB_MIN_ROOM_ROLLOFF_FACTOR        (0.0f)
#define AL_REVERB_MAX_ROOM_ROLLOFF_FACTOR        (10.0f)
#define AL_REVERB_DEFAULT_ROOM_ROLLOFF_FACTOR    (0.0f)

#define AL_REVERB_MIN_DECAY_HFLIMIT              AL_FALSE
#define AL_REVERB_MAX_DECAY_HFLIMIT              AL_TRUE
#define AL_REVERB_DEFAULT_DECAY_HFLIMIT          AL_TRUE

/* EAX reverb effect */
#define AL_EAXREVERB_MIN_DENSITY                 (0.0f)
#define AL_EAXREVERB_MAX_DENSITY                 (1.0f)
#define AL_EAXREVERB_DEFAULT_DENSITY             (1.0f)

#define AL_EAXREVERB_MIN_DIFFUSION               (0.0f)
#define AL_EAXREVERB_MAX_DIFFUSION               (1.0f)
#define AL_EAXREVERB_DEFAULT_DIFFUSION           (1.0f)

#define AL_EAXREVERB_MIN_GAIN                    (0.0f)
#define AL_EAXREVERB_MAX_GAIN                    (1.0f)
#define AL_EAXREVERB_DEFAULT_GAIN                (0.32f)

#define AL_EAXREVERB_MIN_GAINHF                  (0.0f)
#define AL_EAXREVERB_MAX_GAINHF                  (1.0f)
#define AL_EAXREVERB_DEFAULT_GAINHF              (0.89f)

#define AL_EAXREVERB_MIN_GAINLF                  (0.0f)
#define AL_EAXREVERB_MAX_GAINLF                  (1.0f)
#define AL_EAXREVERB_DEFAULT_GAINLF              (1.0f)

#define AL_EAXREVERB_MIN_DECAY_TIME              (0.1f)
#define AL_EAXREVERB_MAX_DECAY_TIME              (20.0f)
#define AL_EAXREVERB_DEFAULT_DECAY_TIME          (1.49f)

#define AL_EAXREVERB_MIN_DECAY_HFRATIO           (0.1f)
#define AL_EAXREVERB_MAX_DECAY_HFRATIO           (2.0f)
#define AL_EAXREVERB_DEFAULT_DECAY_HFRATIO       (0.83f)

#define AL_EAXREVERB_MIN_DECAY_LFRATIO           (0.1f)
#define AL_EAXREVERB_MAX_DECAY_LFRATIO           (2.0f)
#define AL_EAXREVERB_DEFAULT_DECAY_LFRATIO       (1.0f)

#define AL_EAXREVERB_MIN_REFLECTIONS_GAIN        (0.0f)
#define AL_EAXREVERB_MAX_REFLECTIONS_GAIN        (3.16f)
#define AL_EAXREVERB_DEFAULT_REFLECTIONS_GAIN    (0.05f)

#define AL_EAXREVERB_MIN_REFLECTIONS_DELAY       (0.0f)
#define AL_EAXREVERB_MAX_REFLECTIONS_DELAY       (0.3f)
#define AL_EAXREVERB_DEFAULT_REFLECTIONS_DELAY   (0.007f)

#define AL_EAXREVERB_DEFAULT_REFLECTIONS_PAN_XYZ (0.0f)

#define AL_EAXREVERB_MIN_LATE_REVERB_GAIN        (0.0f)
#define AL_EAXREVERB_MAX_LATE_REVERB_GAIN        (10.0f)
#define AL_EAXREVERB_DEFAULT_LATE_REVERB_GAIN    (1.26f)

#define AL_EAXREVERB_MIN_LATE_REVERB_DELAY       (0.0f)
#define AL_EAXREVERB_MAX_LATE_REVERB_DELAY       (0.1f)
#define AL_EAXREVERB_DEFAULT_LATE_REVERB_DELAY   (0.011f)

#define AL_EAXREVERB_DEFAULT_LATE_REVERB_PAN_XYZ (0.0f)

#define AL_EAXREVERB_MIN_ECHO_TIME               (0.075f)
#define AL_EAXREVERB_MAX_ECHO_TIME               (0.25f)
#define AL_EAXREVERB_DEFAULT_ECHO_TIME           (0.25f)

#define AL_EAXREVERB_MIN_ECHO_DEPTH              (0.0f)
#define AL_EAXREVERB_MAX_ECHO_DEPTH              (1.0f)
#define AL_EAXREVERB_DEFAULT_ECHO_DEPTH          (0.0f)

#define AL_EAXREVERB_MIN_MODULATION_TIME         (0.04f)
#define AL_EAXREVERB_MAX_MODULATION_TIME         (4.0f)
#define AL_EAXREVERB_DEFAULT_MODULATION_TIME     (0.25f)

#define AL_EAXREVERB_MIN_MODULATION_DEPTH        (0.0f)
#define AL_EAXREVERB_MAX_MODULATION_DEPTH        (1.0f)
#define AL_EAXREVERB_DEFAULT_MODULATION_DEPTH    (0.0f)

#define AL_EAXREVERB_MIN_AIR_ABSORPTION_GAINHF   (0.892f)
#define AL_EAXREVERB_MAX_AIR_ABSORPTION_GAINHF   (1.0f)
#define AL_EAXREVERB_DEFAULT_AIR_ABSORPTION_GAINHF (0.994f)

#define AL_EAXREVERB_MIN_HFREFERENCE             (1000.0f)
#define AL_EAXREVERB_MAX_HFREFERENCE             (20000.0f)
#define AL_EAXREVERB_DEFAULT_HFREFERENCE         (5000.0f)

#define AL_EAXREVERB_MIN_LFREFERENCE             (20.0f)
#define AL_EAXREVERB_MAX_LFREFERENCE             (1000.0f)
#define AL_EAXREVERB_DEFAULT_LFREFERENCE         (250.0f)

#define AL_EAXREVERB_MIN_ROOM_ROLLOFF_FACTOR     (0.0f)
#define AL_EAXREVERB_MAX_ROOM_ROLLOFF_FACTOR     (10.0f)
#define AL_EAXREVERB_DEFAULT_ROOM_ROLLOFF_FACTOR (0.0f)

#define AL_EAXREVERB_MIN_DECAY_HFLIMIT           AL_FALSE
#define AL_EAXREVERB_MAX_DECAY_HFLIMIT           AL_TRUE
#define AL_EAXREVERB_DEFAULT_DECAY_HFLIMIT       AL_TRUE

/* Chorus effect */
#define AL_CHORUS_WAVEFORM_SINUSOID              (0)
#define AL_CHORUS_WAVEFORM_TRIANGLE              (1)

#define AL_CHORUS_MIN_WAVEFORM                   (0)
#define AL_CHORUS_MAX_WAVEFORM                   (1)
#define AL_CHORUS_DEFAULT_WAVEFORM               (1)

#define AL_CHORUS_MIN_PHASE                      (-180)
#define AL_CHORUS_MAX_PHASE                      (180)
#define AL_CHORUS_DEFAULT_PHASE                  (90)

#define AL_CHORUS_MIN_RATE                       (0.0f)
#define AL_CHORUS_MAX_RATE                       (10.0f)
#define AL_CHORUS_DEFAULT_RATE                   (1.1f)

#define AL_CHORUS_MIN_DEPTH                      (0.0f)
#define AL_CHORUS_MAX_DEPTH                      (1.0f)
#define AL_CHORUS_DEFAULT_DEPTH                  (0.1f)

#define AL_CHORUS_MIN_FEEDBACK                   (-1.0f)
#define AL_CHORUS_MAX_FEEDBACK                   (1.0f)
#define AL_CHORUS_DEFAULT_FEEDBACK               (0.25f)

#define AL_CHORUS_MIN_DELAY                      (0.0f)
#define AL_CHORUS_MAX_DELAY                      (0.016f)
#define AL_CHORUS_DEFAULT_DELAY                  (0.016f)

/* Distortion effect */
#define AL_DISTORTION_MIN_EDGE                   (0.0f)
#define AL_DISTORTION_MAX_EDGE                   (1.0f)
#define AL_DISTORTION_DEFAULT_EDGE               (0.2f)

#define AL_DISTORTION_MIN_GAIN                   (0.01f)
#define AL_DISTORTION_MAX_GAIN                   (1.0f)
#define AL_DISTORTION_DEFAULT_GAIN               (0.05f)

#define AL_DISTORTION_MIN_LOWPASS_CUTOFF         (80.0f)
#define AL_DISTORTION_MAX_LOWPASS_CUTOFF         (24000.0f)
#define AL_DISTORTION_DEFAULT_LOWPASS_CUTOFF     (8000.0f)

#define AL_DISTORTION_MIN_EQCENTER               (80.0f)
#define AL_DISTORTION_MAX_EQCENTER               (24000.0f)
#define AL_DISTORTION_DEFAULT_EQCENTER           (3600.0f)

#define AL_DISTORTION_MIN_EQBANDWIDTH            (80.0f)
#define AL_DISTORTION_MAX_EQBANDWIDTH            (24000.0f)
#define AL_DISTORTION_DEFAULT_EQBANDWIDTH        (3600.0f)

/* Echo effect */
#define AL_ECHO_MIN_DELAY                        (0.0f)
#define AL_ECHO_MAX_DELAY                        (0.207f)
#define AL_ECHO_DEFAULT_DELAY                    (0.1f)

#define AL_ECHO_MIN_LRDELAY                      (0.0f)
#define AL_ECHO_MAX_LRDELAY                      (0.404f)
#define AL_ECHO_DEFAULT_LRDELAY                  (0.1f)

#define AL_ECHO_MIN_DAMPING                      (0.0f)
#define AL_ECHO_MAX_DAMPING                      (0.99f)
#define AL_ECHO_DEFAULT_DAMPING                  (0.5f)

#define AL_ECHO_MIN_FEEDBACK                     (0.0f)
#define AL_ECHO_MAX_FEEDBACK                     (1.0f)
#define AL_ECHO_DEFAULT_FEEDBACK                 (0.5f)

#define AL_ECHO_MIN_SPREAD                       (-1.0f)
#define AL_ECHO_MAX_SPREAD                       (1.0f)
#define AL_ECHO_DEFAULT_SPREAD                   (-1.0f)

/* Flanger effect */
#define AL_FLANGER_WAVEFORM_SINUSOID             (0)
#define AL_FLANGER_WAVEFORM_TRIANGLE             (1)

#define AL_FLANGER_MIN_WAVEFORM                  (0)
#define AL_FLANGER_MAX_WAVEFORM                  (1)
#define AL_FLANGER_DEFAULT_WAVEFORM              (1)

#define AL_FLANGER_MIN_PHASE                     (-180)
#define AL_FLANGER_MAX_PHASE                     (180)
#define AL_FLANGER_DEFAULT_PHASE                 (0)

#define AL_FLANGER_MIN_RATE                      (0.0f)
#define AL_FLANGER_MAX_RATE                      (10.0f)
#define AL_FLANGER_DEFAULT_RATE                  (0.27f)

#define AL_FLANGER_MIN_DEPTH                     (0.0f)
#define AL_FLANGER_MAX_DEPTH                     (1.0f)
#define AL_FLANGER_DEFAULT_DEPTH                 (1.0f)

#define AL_FLANGER_MIN_FEEDBACK                  (-1.0f)
#define AL_FLANGER_MAX_FEEDBACK                  (1.0f)
#define AL_FLANGER_DEFAULT_FEEDBACK              (-0.5f)

#define AL_FLANGER_MIN_DELAY                     (0.0f)
#define AL_FLANGER_MAX_DELAY                     (0.004f)
#define AL_FLANGER_DEFAULT_DELAY                 (0.002f)

/* Frequency shifter effect */
#define AL_FREQUENCY_SHIFTER_MIN_FREQUENCY       (0.0f)
#define AL_FREQUENCY_SHIFTER_MAX_FREQUENCY       (24000.0f)
#define AL_FREQUENCY_SHIFTER_DEFAULT_FREQUENCY   (0.0f)

#define AL_FREQUENCY_SHIFTER_MIN_LEFT_DIRECTION  (0)
#define AL_FREQUENCY_SHIFTER_MAX_LEFT_DIRECTION  (2)
#define AL_FREQUENCY_SHIFTER_DEFAULT_LEFT_DIRECTION (0)

#define AL_FREQUENCY_SHIFTER_DIRECTION_DOWN      (0)
#define AL_FREQUENCY_SHIFTER_DIRECTION_UP        (1)
#define AL_FREQUENCY_SHIFTER_DIRECTION_OFF       (2)

#define AL_FREQUENCY_SHIFTER_MIN_RIGHT_DIRECTION (0)
#define AL_FREQUENCY_SHIFTER_MAX_RIGHT_DIRECTION (2)
#define AL_FREQUENCY_SHIFTER_DEFAULT_RIGHT_DIRECTION (0)

/* Vocal morpher effect */
#define AL_VOCAL_MORPHER_MIN_PHONEMEA            (0)
#define AL_VOCAL_MORPHER_MAX_PHONEMEA            (29)
#define AL_VOCAL_MORPHER_DEFAULT_PHONEMEA        (0)

#define AL_VOCAL_MORPHER_MIN_PHONEMEA_COARSE_TUNING (-24)
#define AL_VOCAL_MORPHER_MAX_PHONEMEA_COARSE_TUNING (24)
#define AL_VOCAL_MORPHER_DEFAULT_PHONEMEA_COARSE_TUNING (0)

#define AL_VOCAL_MORPHER_MIN_PHONEMEB            (0)
#define AL_VOCAL_MORPHER_MAX_PHONEMEB            (29)
#define AL_VOCAL_MORPHER_DEFAULT_PHONEMEB        (10)

#define AL_VOCAL_MORPHER_MIN_PHONEMEB_COARSE_TUNING (-24)
#define AL_VOCAL_MORPHER_MAX_PHONEMEB_COARSE_TUNING (24)
#define AL_VOCAL_MORPHER_DEFAULT_PHONEMEB_COARSE_TUNING (0)

#define AL_VOCAL_MORPHER_PHONEME_A               (0)
#define AL_VOCAL_MORPHER_PHONEME_E               (1)
#define AL_VOCAL_MORPHER_PHONEME_I               (2)
#define AL_VOCAL_MORPHER_PHONEME_O               (3)
#define AL_VOCAL_MORPHER_PHONEME_U               (4)
#define AL_VOCAL_MORPHER_PHONEME_AA              (5)
#define AL_VOCAL_MORPHER_PHONEME_AE              (6)
#define AL_VOCAL_MORPHER_PHONEME_AH              (7)
#define AL_VOCAL_MORPHER_PHONEME_AO              (8)
#define AL_VOCAL_MORPHER_PHONEME_EH              (9)
#define AL_VOCAL_MORPHER_PHONEME_ER              (10)
#define AL_VOCAL_MORPHER_PHONEME_IH              (11)
#define AL_VOCAL_MORPHER_PHONEME_IY              (12)
#define AL_VOCAL_MORPHER_PHONEME_UH              (13)
#define AL_VOCAL_MORPHER_PHONEME_UW              (14)
#define AL_VOCAL_MORPHER_PHONEME_B               (15)
#define AL_VOCAL_MORPHER_PHONEME_D               (16)
#define AL_VOCAL_MORPHER_PHONEME_F               (17)
#define AL_VOCAL_MORPHER_PHONEME_G               (18)
#define AL_VOCAL_MORPHER_PHONEME_J               (19)
#define AL_VOCAL_MORPHER_PHONEME_K               (20)
#define AL_VOCAL_MORPHER_PHONEME_L               (21)
#define AL_VOCAL_MORPHER_PHONEME_M               (22)
#define AL_VOCAL_MORPHER_PHONEME_N               (23)
#define AL_VOCAL_MORPHER_PHONEME_P               (24)
#define AL_VOCAL_MORPHER_PHONEME_R               (25)
#define AL_VOCAL_MORPHER_PHONEME_S               (26)
#define AL_VOCAL_MORPHER_PHONEME_T               (27)
#define AL_VOCAL_MORPHER_PHONEME_V               (28)
#define AL_VOCAL_MORPHER_PHONEME_Z               (29)

#define AL_VOCAL_MORPHER_WAVEFORM_SINUSOID       (0)
#define AL_VOCAL_MORPHER_WAVEFORM_TRIANGLE       (1)
#define AL_VOCAL_MORPHER_WAVEFORM_SAWTOOTH       (2)

#define AL_VOCAL_MORPHER_MIN_WAVEFORM            (0)
#define AL_VOCAL_MORPHER_MAX_WAVEFORM            (2)
#define AL_VOCAL_MORPHER_DEFAULT_WAVEFORM        (0)

#define AL_VOCAL_MORPHER_MIN_RATE                (0.0f)
#define AL_VOCAL_MORPHER_MAX_RATE                (10.0f)
#define AL_VOCAL_MORPHER_DEFAULT_RATE            (1.41f)

/* Pitch shifter effect */
#define AL_PITCH_SHIFTER_MIN_COARSE_TUNE         (-12)
#define AL_PITCH_SHIFTER_MAX_COARSE_TUNE         (12)
#define AL_PITCH_SHIFTER_DEFAULT_COARSE_TUNE     (12)

#define AL_PITCH_SHIFTER_MIN_FINE_TUNE           (-50)
#define AL_PITCH_SHIFTER_MAX_FINE_TUNE           (50)
#define AL_PITCH_SHIFTER_DEFAULT_FINE_TUNE       (0)

/* Ring modulator effect */
#define AL_RING_MODULATOR_MIN_FREQUENCY          (0.0f)
#define AL_RING_MODULATOR_MAX_FREQUENCY          (8000.0f)
#define AL_RING_MODULATOR_DEFAULT_FREQUENCY      (440.0f)

#define AL_RING_MODULATOR_MIN_HIGHPASS_CUTOFF    (0.0f)
#define AL_RING_MODULATOR_MAX_HIGHPASS_CUTOFF    (24000.0f)
#define AL_RING_MODULATOR_DEFAULT_HIGHPASS_CUTOFF (800.0f)

#define AL_RING_MODULATOR_SINUSOID               (0)
#define AL_RING_MODULATOR_SAWTOOTH               (1)
#define AL_RING_MODULATOR_SQUARE                 (2)

#define AL_RING_MODULATOR_MIN_WAVEFORM           (0)
#define AL_RING_MODULATOR_MAX_WAVEFORM           (2)
#define AL_RING_MODULATOR_DEFAULT_WAVEFORM       (0)

/* Autowah effect */
#define AL_AUTOWAH_MIN_ATTACK_TIME               (0.0001f)
#define AL_AUTOWAH_MAX_ATTACK_TIME               (1.0f)
#define AL_AUTOWAH_DEFAULT_ATTACK_TIME           (0.06f)

#define AL_AUTOWAH_MIN_RELEASE_TIME              (0.0001f)
#define AL_AUTOWAH_MAX_RELEASE_TIME              (1.0f)
#define AL_AUTOWAH_DEFAULT_RELEASE_TIME          (0.06f)

#define AL_AUTOWAH_MIN_RESONANCE                 (2.0f)
#define AL_AUTOWAH_MAX_RESONANCE                 (1000.0f)
#define AL_AUTOWAH_DEFAULT_RESONANCE             (1000.0f)

#define AL_AUTOWAH_MIN_PEAK_GAIN                 (0.00003f)
#define AL_AUTOWAH_MAX_PEAK_GAIN                 (31621.0f)
#define AL_AUTOWAH_DEFAULT_PEAK_GAIN             (11.22f)

/* Compressor effect */
#define AL_COMPRESSOR_MIN_ONOFF                  (0)
#define AL_COMPRESSOR_MAX_ONOFF                  (1)
#define AL_COMPRESSOR_DEFAULT_ONOFF              (1)

/* Equalizer effect */
#define AL_EQUALIZER_MIN_LOW_GAIN                (0.126f)
#define AL_EQUALIZER_MAX_LOW_GAIN                (7.943f)
#define AL_EQUALIZER_DEFAULT_LOW_GAIN            (1.0f)

#define AL_EQUALIZER_MIN_LOW_CUTOFF              (50.0f)
#define AL_EQUALIZER_MAX_LOW_CUTOFF              (800.0f)
#define AL_EQUALIZER_DEFAULT_LOW_CUTOFF          (200.0f)

#define AL_EQUALIZER_MIN_MID1_GAIN               (0.126f)
#define AL_EQUALIZER_MAX_MID1_GAIN               (7.943f)
#define AL_EQUALIZER_DEFAULT_MID1_GAIN           (1.0f)

#define AL_EQUALIZER_MIN_MID1_CENTER             (200.0f)
#define AL_EQUALIZER_MAX_MID1_CENTER             (3000.0f)
#define AL_EQUALIZER_DEFAULT_MID1_CENTER         (500.0f)

#define AL_EQUALIZER_MIN_MID1_WIDTH              (0.01f)
#define AL_EQUALIZER_MAX_MID1_WIDTH              (1.0f)
#define AL_EQUALIZER_DEFAULT_MID1_WIDTH          (1.0f)

#define AL_EQUALIZER_MIN_MID2_GAIN               (0.126f)
#define AL_EQUALIZER_MAX_MID2_GAIN               (7.943f)
#define AL_EQUALIZER_DEFAULT_MID2_GAIN           (1.0f)

#define AL_EQUALIZER_MIN_MID2_CENTER             (1000.0f)
#define AL_EQUALIZER_MAX_MID2_CENTER             (8000.0f)
#define AL_EQUALIZER_DEFAULT_MID2_CENTER         (3000.0f)

#define AL_EQUALIZER_MIN_MID2_WIDTH              (0.01f)
#define AL_EQUALIZER_MAX_MID2_WIDTH              (1.0f)
#define AL_EQUALIZER_DEFAULT_MID2_WIDTH          (1.0f)

#define AL_EQUALIZER_MIN_HIGH_GAIN               (0.126f)
#define AL_EQUALIZER_MAX_HIGH_GAIN               (7.943f)
#define AL_EQUALIZER_DEFAULT_HIGH_GAIN           (1.0f)

#define AL_EQUALIZER_MIN_HIGH_CUTOFF             (4000.0f)
#define AL_EQUALIZER_MAX_HIGH_CUTOFF             (16000.0f)
#define AL_EQUALIZER_DEFAULT_HIGH_CUTOFF         (6000.0f)


/* Source parameter value ranges and defaults. */
#define AL_MIN_AIR_ABSORPTION_FACTOR             (0.0f)
#define AL_MAX_AIR_ABSORPTION_FACTOR             (10.0f)
#define AL_DEFAULT_AIR_ABSORPTION_FACTOR         (0.0f)

#define AL_MIN_ROOM_ROLLOFF_FACTOR               (0.0f)
#define AL_MAX_ROOM_ROLLOFF_FACTOR               (10.0f)
#define AL_DEFAULT_ROOM_ROLLOFF_FACTOR           (0.0f)

#define AL_MIN_CONE_OUTER_GAINHF                 (0.0f)
#define AL_MAX_CONE_OUTER_GAINHF                 (1.0f)
#define AL_DEFAULT_CONE_OUTER_GAINHF             (1.0f)

#define AL_MIN_DIRECT_FILTER_GAINHF_AUTO         AL_FALSE
#define AL_MAX_DIRECT_FILTER_GAINHF_AUTO         AL_TRUE
#define AL_DEFAULT_DIRECT_FILTER_GAINHF_AUTO     AL_TRUE

#define AL_MIN_AUXILIARY_SEND_FILTER_GAIN_AUTO   AL_FALSE
#define AL_MAX_AUXILIARY_SEND_FILTER_GAIN_AUTO   AL_TRUE
#define AL_DEFAULT_AUXILIARY_SEND_FILTER_GAIN_AUTO AL_TRUE

#define AL_MIN_AUXILIARY_SEND_FILTER_GAINHF_AUTO AL_FALSE
#define AL_MAX_AUXILIARY_SEND_FILTER_GAINHF_AUTO AL_TRUE
#define AL_DEFAULT_AUXILIARY_SEND_FILTER_GAINHF_AUTO AL_TRUE

/* Listener parameter value ranges and defaults. */
#define AL_MIN_METERS_PER_UNIT                   FLT_MIN
#define AL_MAX_METERS_PER_UNIT                   FLT_MAX
#define AL_DEFAULT_METERS_PER_UNIT               (1.0f)

  -------------
  -- Presets --
  -------------

typedef struct {
    float flDensity;
    float flDiffusion;
    float flGain;
    float flGainHF;
    float flGainLF;
    float flDecayTime;
    float flDecayHFRatio;
    float flDecayLFRatio;
    float flReflectionsGain;
    float flReflectionsDelay;
    float flReflectionsPan[3];
    float flLateReverbGain;
    float flLateReverbDelay;
    float flLateReverbPan[3];
    float flEchoTime;
    float flEchoDepth;
    float flModulationTime;
    float flModulationDepth;
    float flAirAbsorptionGainHF;
    float flHFReference;
    float flLFReference;
    float flRoomRolloffFactor;
    int   iDecayHFLimit;
} EFXEAXREVERBPROPERTIES, *LPEFXEAXREVERBPROPERTIES;
#define EFX_REVERB_PRESET_GENERIC \
    { 1.0000f, 1.0000f, 0.3162f, 0.8913f, 1.0000f, 1.4900f, 0.8300f, 1.0000f, 0.0500f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0110f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_PADDEDCELL \
    { 0.1715f, 1.0000f, 0.3162f, 0.0010f, 1.0000f, 0.1700f, 0.1000f, 1.0000f, 0.2500f, 0.0010f, { 0.0000f, 0.0000f, 0.0000f }, 1.2691f, 0.0020f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_ROOM \
    { 0.4287f, 1.0000f, 0.3162f, 0.5929f, 1.0000f, 0.4000f, 0.8300f, 1.0000f, 0.1503f, 0.0020f, { 0.0000f, 0.0000f, 0.0000f }, 1.0629f, 0.0030f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_BATHROOM \
    { 0.1715f, 1.0000f, 0.3162f, 0.2512f, 1.0000f, 1.4900f, 0.5400f, 1.0000f, 0.6531f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 3.2734f, 0.0110f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_LIVINGROOM \
    { 0.9766f, 1.0000f, 0.3162f, 0.0010f, 1.0000f, 0.5000f, 0.1000f, 1.0000f, 0.2051f, 0.0030f, { 0.0000f, 0.0000f, 0.0000f }, 0.2805f, 0.0040f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_STONEROOM \
    { 1.0000f, 1.0000f, 0.3162f, 0.7079f, 1.0000f, 2.3100f, 0.6400f, 1.0000f, 0.4411f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 1.1003f, 0.0170f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_AUDITORIUM \
    { 1.0000f, 1.0000f, 0.3162f, 0.5781f, 1.0000f, 4.3200f, 0.5900f, 1.0000f, 0.4032f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 0.7170f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CONCERTHALL \
    { 1.0000f, 1.0000f, 0.3162f, 0.5623f, 1.0000f, 3.9200f, 0.7000f, 1.0000f, 0.2427f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 0.9977f, 0.0290f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CAVE \
    { 1.0000f, 1.0000f, 0.3162f, 1.0000f, 1.0000f, 2.9100f, 1.3000f, 1.0000f, 0.5000f, 0.0150f, { 0.0000f, 0.0000f, 0.0000f }, 0.7063f, 0.0220f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_ARENA \
    { 1.0000f, 1.0000f, 0.3162f, 0.4477f, 1.0000f, 7.2400f, 0.3300f, 1.0000f, 0.2612f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 1.0186f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_HANGAR \
    { 1.0000f, 1.0000f, 0.3162f, 0.3162f, 1.0000f, 10.0500f, 0.2300f, 1.0000f, 0.5000f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 1.2560f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CARPETEDHALLWAY \
    { 0.4287f, 1.0000f, 0.3162f, 0.0100f, 1.0000f, 0.3000f, 0.1000f, 1.0000f, 0.1215f, 0.0020f, { 0.0000f, 0.0000f, 0.0000f }, 0.1531f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_HALLWAY \
    { 0.3645f, 1.0000f, 0.3162f, 0.7079f, 1.0000f, 1.4900f, 0.5900f, 1.0000f, 0.2458f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 1.6615f, 0.0110f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_STONECORRIDOR \
    { 1.0000f, 1.0000f, 0.3162f, 0.7612f, 1.0000f, 2.7000f, 0.7900f, 1.0000f, 0.2472f, 0.0130f, { 0.0000f, 0.0000f, 0.0000f }, 1.5758f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_ALLEY \
    { 1.0000f, 0.3000f, 0.3162f, 0.7328f, 1.0000f, 1.4900f, 0.8600f, 1.0000f, 0.2500f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 0.9954f, 0.0110f, { 0.0000f, 0.0000f, 0.0000f }, 0.1250f, 0.9500f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_FOREST \
    { 1.0000f, 0.3000f, 0.3162f, 0.0224f, 1.0000f, 1.4900f, 0.5400f, 1.0000f, 0.0525f, 0.1620f, { 0.0000f, 0.0000f, 0.0000f }, 0.7682f, 0.0880f, { 0.0000f, 0.0000f, 0.0000f }, 0.1250f, 1.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CITY \
    { 1.0000f, 0.5000f, 0.3162f, 0.3981f, 1.0000f, 1.4900f, 0.6700f, 1.0000f, 0.0730f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 0.1427f, 0.0110f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_MOUNTAINS \
    { 1.0000f, 0.2700f, 0.3162f, 0.0562f, 1.0000f, 1.4900f, 0.2100f, 1.0000f, 0.0407f, 0.3000f, { 0.0000f, 0.0000f, 0.0000f }, 0.1919f, 0.1000f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 1.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_QUARRY \
    { 1.0000f, 1.0000f, 0.3162f, 0.3162f, 1.0000f, 1.4900f, 0.8300f, 1.0000f, 0.0000f, 0.0610f, { 0.0000f, 0.0000f, 0.0000f }, 1.7783f, 0.0250f, { 0.0000f, 0.0000f, 0.0000f }, 0.1250f, 0.7000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_PLAIN \
    { 1.0000f, 0.2100f, 0.3162f, 0.1000f, 1.0000f, 1.4900f, 0.5000f, 1.0000f, 0.0585f, 0.1790f, { 0.0000f, 0.0000f, 0.0000f }, 0.1089f, 0.1000f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 1.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_PARKINGLOT \
    { 1.0000f, 1.0000f, 0.3162f, 1.0000f, 1.0000f, 1.6500f, 1.5000f, 1.0000f, 0.2082f, 0.0080f, { 0.0000f, 0.0000f, 0.0000f }, 0.2652f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_SEWERPIPE \
    { 0.3071f, 0.8000f, 0.3162f, 0.3162f, 1.0000f, 2.8100f, 0.1400f, 1.0000f, 1.6387f, 0.0140f, { 0.0000f, 0.0000f, 0.0000f }, 3.2471f, 0.0210f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_UNDERWATER \
    { 0.3645f, 1.0000f, 0.3162f, 0.0100f, 1.0000f, 1.4900f, 0.1000f, 1.0000f, 0.5963f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 7.0795f, 0.0110f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 1.1800f, 0.3480f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_DRUGGED \
    { 0.4287f, 0.5000f, 0.3162f, 1.0000f, 1.0000f, 8.3900f, 1.3900f, 1.0000f, 0.8760f, 0.0020f, { 0.0000f, 0.0000f, 0.0000f }, 3.1081f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 1.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_DIZZY \
    { 0.3645f, 0.6000f, 0.3162f, 0.6310f, 1.0000f, 17.2300f, 0.5600f, 1.0000f, 0.1392f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 0.4937f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 1.0000f, 0.8100f, 0.3100f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_PSYCHOTIC \
    { 0.0625f, 0.5000f, 0.3162f, 0.8404f, 1.0000f, 7.5600f, 0.9100f, 1.0000f, 0.4864f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 2.4378f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 4.0000f, 1.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

/* Castle Presets */

#define EFX_REVERB_PRESET_CASTLE_SMALLROOM \
    { 1.0000f, 0.8900f, 0.3162f, 0.3981f, 0.1000f, 1.2200f, 0.8300f, 0.3100f, 0.8913f, 0.0220f, { 0.0000f, 0.0000f, 0.0000f }, 1.9953f, 0.0110f, { 0.0000f, 0.0000f, 0.0000f }, 0.1380f, 0.0800f, 0.2500f, 0.0000f, 0.9943f, 5168.6001f, 139.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CASTLE_SHORTPASSAGE \
    { 1.0000f, 0.8900f, 0.3162f, 0.3162f, 0.1000f, 2.3200f, 0.8300f, 0.3100f, 0.8913f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0230f, { 0.0000f, 0.0000f, 0.0000f }, 0.1380f, 0.0800f, 0.2500f, 0.0000f, 0.9943f, 5168.6001f, 139.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CASTLE_MEDIUMROOM \
    { 1.0000f, 0.9300f, 0.3162f, 0.2818f, 0.1000f, 2.0400f, 0.8300f, 0.4600f, 0.6310f, 0.0220f, { 0.0000f, 0.0000f, 0.0000f }, 1.5849f, 0.0110f, { 0.0000f, 0.0000f, 0.0000f }, 0.1550f, 0.0300f, 0.2500f, 0.0000f, 0.9943f, 5168.6001f, 139.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CASTLE_LARGEROOM \
    { 1.0000f, 0.8200f, 0.3162f, 0.2818f, 0.1259f, 2.5300f, 0.8300f, 0.5000f, 0.4467f, 0.0340f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0160f, { 0.0000f, 0.0000f, 0.0000f }, 0.1850f, 0.0700f, 0.2500f, 0.0000f, 0.9943f, 5168.6001f, 139.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CASTLE_LONGPASSAGE \
    { 1.0000f, 0.8900f, 0.3162f, 0.3981f, 0.1000f, 3.4200f, 0.8300f, 0.3100f, 0.8913f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 1.4125f, 0.0230f, { 0.0000f, 0.0000f, 0.0000f }, 0.1380f, 0.0800f, 0.2500f, 0.0000f, 0.9943f, 5168.6001f, 139.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CASTLE_HALL \
    { 1.0000f, 0.8100f, 0.3162f, 0.2818f, 0.1778f, 3.1400f, 0.7900f, 0.6200f, 0.1778f, 0.0560f, { 0.0000f, 0.0000f, 0.0000f }, 1.1220f, 0.0240f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5168.6001f, 139.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CASTLE_CUPBOARD \
    { 1.0000f, 0.8900f, 0.3162f, 0.2818f, 0.1000f, 0.6700f, 0.8700f, 0.3100f, 1.4125f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 3.5481f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 0.1380f, 0.0800f, 0.2500f, 0.0000f, 0.9943f, 5168.6001f, 139.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CASTLE_COURTYARD \
    { 1.0000f, 0.4200f, 0.3162f, 0.4467f, 0.1995f, 2.1300f, 0.6100f, 0.2300f, 0.2239f, 0.1600f, { 0.0000f, 0.0000f, 0.0000f }, 0.7079f, 0.0360f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.3700f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_CASTLE_ALCOVE \
    { 1.0000f, 0.8900f, 0.3162f, 0.5012f, 0.1000f, 1.6400f, 0.8700f, 0.3100f, 1.0000f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 1.4125f, 0.0340f, { 0.0000f, 0.0000f, 0.0000f }, 0.1380f, 0.0800f, 0.2500f, 0.0000f, 0.9943f, 5168.6001f, 139.5000f, 0.0000f, 0x1 }

/* Factory Presets */

#define EFX_REVERB_PRESET_FACTORY_SMALLROOM \
    { 0.3645f, 0.8200f, 0.3162f, 0.7943f, 0.5012f, 1.7200f, 0.6500f, 1.3100f, 0.7079f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 1.7783f, 0.0240f, { 0.0000f, 0.0000f, 0.0000f }, 0.1190f, 0.0700f, 0.2500f, 0.0000f, 0.9943f, 3762.6001f, 362.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_FACTORY_SHORTPASSAGE \
    { 0.3645f, 0.6400f, 0.2512f, 0.7943f, 0.5012f, 2.5300f, 0.6500f, 1.3100f, 1.0000f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0380f, { 0.0000f, 0.0000f, 0.0000f }, 0.1350f, 0.2300f, 0.2500f, 0.0000f, 0.9943f, 3762.6001f, 362.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_FACTORY_MEDIUMROOM \
    { 0.4287f, 0.8200f, 0.2512f, 0.7943f, 0.5012f, 2.7600f, 0.6500f, 1.3100f, 0.2818f, 0.0220f, { 0.0000f, 0.0000f, 0.0000f }, 1.4125f, 0.0230f, { 0.0000f, 0.0000f, 0.0000f }, 0.1740f, 0.0700f, 0.2500f, 0.0000f, 0.9943f, 3762.6001f, 362.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_FACTORY_LARGEROOM \
    { 0.4287f, 0.7500f, 0.2512f, 0.7079f, 0.6310f, 4.2400f, 0.5100f, 1.3100f, 0.1778f, 0.0390f, { 0.0000f, 0.0000f, 0.0000f }, 1.1220f, 0.0230f, { 0.0000f, 0.0000f, 0.0000f }, 0.2310f, 0.0700f, 0.2500f, 0.0000f, 0.9943f, 3762.6001f, 362.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_FACTORY_LONGPASSAGE \
    { 0.3645f, 0.6400f, 0.2512f, 0.7943f, 0.5012f, 4.0600f, 0.6500f, 1.3100f, 1.0000f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0370f, { 0.0000f, 0.0000f, 0.0000f }, 0.1350f, 0.2300f, 0.2500f, 0.0000f, 0.9943f, 3762.6001f, 362.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_FACTORY_HALL \
    { 0.4287f, 0.7500f, 0.3162f, 0.7079f, 0.6310f, 7.4300f, 0.5100f, 1.3100f, 0.0631f, 0.0730f, { 0.0000f, 0.0000f, 0.0000f }, 0.8913f, 0.0270f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0700f, 0.2500f, 0.0000f, 0.9943f, 3762.6001f, 362.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_FACTORY_CUPBOARD \
    { 0.3071f, 0.6300f, 0.2512f, 0.7943f, 0.5012f, 0.4900f, 0.6500f, 1.3100f, 1.2589f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 1.9953f, 0.0320f, { 0.0000f, 0.0000f, 0.0000f }, 0.1070f, 0.0700f, 0.2500f, 0.0000f, 0.9943f, 3762.6001f, 362.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_FACTORY_COURTYARD \
    { 0.3071f, 0.5700f, 0.3162f, 0.3162f, 0.6310f, 2.3200f, 0.2900f, 0.5600f, 0.2239f, 0.1400f, { 0.0000f, 0.0000f, 0.0000f }, 0.3981f, 0.0390f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.2900f, 0.2500f, 0.0000f, 0.9943f, 3762.6001f, 362.5000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_FACTORY_ALCOVE \
    { 0.3645f, 0.5900f, 0.2512f, 0.7943f, 0.5012f, 3.1400f, 0.6500f, 1.3100f, 1.4125f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 1.0000f, 0.0380f, { 0.0000f, 0.0000f, 0.0000f }, 0.1140f, 0.1000f, 0.2500f, 0.0000f, 0.9943f, 3762.6001f, 362.5000f, 0.0000f, 0x1 }

/* Ice Palace Presets */

#define EFX_REVERB_PRESET_ICEPALACE_SMALLROOM \
    { 1.0000f, 0.8400f, 0.3162f, 0.5623f, 0.2818f, 1.5100f, 1.5300f, 0.2700f, 0.8913f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 1.4125f, 0.0110f, { 0.0000f, 0.0000f, 0.0000f }, 0.1640f, 0.1400f, 0.2500f, 0.0000f, 0.9943f, 12428.5000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_ICEPALACE_SHORTPASSAGE \
    { 1.0000f, 0.7500f, 0.3162f, 0.5623f, 0.2818f, 1.7900f, 1.4600f, 0.2800f, 0.5012f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 1.1220f, 0.0190f, { 0.0000f, 0.0000f, 0.0000f }, 0.1770f, 0.0900f, 0.2500f, 0.0000f, 0.9943f, 12428.5000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_ICEPALACE_MEDIUMROOM \
    { 1.0000f, 0.8700f, 0.3162f, 0.5623f, 0.4467f, 2.2200f, 1.5300f, 0.3200f, 0.3981f, 0.0390f, { 0.0000f, 0.0000f, 0.0000f }, 1.1220f, 0.0270f, { 0.0000f, 0.0000f, 0.0000f }, 0.1860f, 0.1200f, 0.2500f, 0.0000f, 0.9943f, 12428.5000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_ICEPALACE_LARGEROOM \
    { 1.0000f, 0.8100f, 0.3162f, 0.5623f, 0.4467f, 3.1400f, 1.5300f, 0.3200f, 0.2512f, 0.0390f, { 0.0000f, 0.0000f, 0.0000f }, 1.0000f, 0.0270f, { 0.0000f, 0.0000f, 0.0000f }, 0.2140f, 0.1100f, 0.2500f, 0.0000f, 0.9943f, 12428.5000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_ICEPALACE_LONGPASSAGE \
    { 1.0000f, 0.7700f, 0.3162f, 0.5623f, 0.3981f, 3.0100f, 1.4600f, 0.2800f, 0.7943f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0250f, { 0.0000f, 0.0000f, 0.0000f }, 0.1860f, 0.0400f, 0.2500f, 0.0000f, 0.9943f, 12428.5000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_ICEPALACE_HALL \
    { 1.0000f, 0.7600f, 0.3162f, 0.4467f, 0.5623f, 5.4900f, 1.5300f, 0.3800f, 0.1122f, 0.0540f, { 0.0000f, 0.0000f, 0.0000f }, 0.6310f, 0.0520f, { 0.0000f, 0.0000f, 0.0000f }, 0.2260f, 0.1100f, 0.2500f, 0.0000f, 0.9943f, 12428.5000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_ICEPALACE_CUPBOARD \
    { 1.0000f, 0.8300f, 0.3162f, 0.5012f, 0.2239f, 0.7600f, 1.5300f, 0.2600f, 1.1220f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 1.9953f, 0.0160f, { 0.0000f, 0.0000f, 0.0000f }, 0.1430f, 0.0800f, 0.2500f, 0.0000f, 0.9943f, 12428.5000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_ICEPALACE_COURTYARD \
    { 1.0000f, 0.5900f, 0.3162f, 0.2818f, 0.3162f, 2.0400f, 1.2000f, 0.3800f, 0.3162f, 0.1730f, { 0.0000f, 0.0000f, 0.0000f }, 0.3162f, 0.0430f, { 0.0000f, 0.0000f, 0.0000f }, 0.2350f, 0.4800f, 0.2500f, 0.0000f, 0.9943f, 12428.5000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_ICEPALACE_ALCOVE \
    { 1.0000f, 0.8400f, 0.3162f, 0.5623f, 0.2818f, 2.7600f, 1.4600f, 0.2800f, 1.1220f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 0.8913f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 0.1610f, 0.0900f, 0.2500f, 0.0000f, 0.9943f, 12428.5000f, 99.6000f, 0.0000f, 0x1 }

/* Space Station Presets */

#define EFX_REVERB_PRESET_SPACESTATION_SMALLROOM \
    { 0.2109f, 0.7000f, 0.3162f, 0.7079f, 0.8913f, 1.7200f, 0.8200f, 0.5500f, 0.7943f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 1.4125f, 0.0130f, { 0.0000f, 0.0000f, 0.0000f }, 0.1880f, 0.2600f, 0.2500f, 0.0000f, 0.9943f, 3316.1001f, 458.2000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SPACESTATION_SHORTPASSAGE \
    { 0.2109f, 0.8700f, 0.3162f, 0.6310f, 0.8913f, 3.5700f, 0.5000f, 0.5500f, 1.0000f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 1.1220f, 0.0160f, { 0.0000f, 0.0000f, 0.0000f }, 0.1720f, 0.2000f, 0.2500f, 0.0000f, 0.9943f, 3316.1001f, 458.2000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SPACESTATION_MEDIUMROOM \
    { 0.2109f, 0.7500f, 0.3162f, 0.6310f, 0.8913f, 3.0100f, 0.5000f, 0.5500f, 0.3981f, 0.0340f, { 0.0000f, 0.0000f, 0.0000f }, 1.1220f, 0.0350f, { 0.0000f, 0.0000f, 0.0000f }, 0.2090f, 0.3100f, 0.2500f, 0.0000f, 0.9943f, 3316.1001f, 458.2000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SPACESTATION_LARGEROOM \
    { 0.3645f, 0.8100f, 0.3162f, 0.6310f, 0.8913f, 3.8900f, 0.3800f, 0.6100f, 0.3162f, 0.0560f, { 0.0000f, 0.0000f, 0.0000f }, 0.8913f, 0.0350f, { 0.0000f, 0.0000f, 0.0000f }, 0.2330f, 0.2800f, 0.2500f, 0.0000f, 0.9943f, 3316.1001f, 458.2000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SPACESTATION_LONGPASSAGE \
    { 0.4287f, 0.8200f, 0.3162f, 0.6310f, 0.8913f, 4.6200f, 0.6200f, 0.5500f, 1.0000f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0310f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.2300f, 0.2500f, 0.0000f, 0.9943f, 3316.1001f, 458.2000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SPACESTATION_HALL \
    { 0.4287f, 0.8700f, 0.3162f, 0.6310f, 0.8913f, 7.1100f, 0.3800f, 0.6100f, 0.1778f, 0.1000f, { 0.0000f, 0.0000f, 0.0000f }, 0.6310f, 0.0470f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.2500f, 0.2500f, 0.0000f, 0.9943f, 3316.1001f, 458.2000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SPACESTATION_CUPBOARD \
    { 0.1715f, 0.5600f, 0.3162f, 0.7079f, 0.8913f, 0.7900f, 0.8100f, 0.5500f, 1.4125f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 1.7783f, 0.0180f, { 0.0000f, 0.0000f, 0.0000f }, 0.1810f, 0.3100f, 0.2500f, 0.0000f, 0.9943f, 3316.1001f, 458.2000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SPACESTATION_ALCOVE \
    { 0.2109f, 0.7800f, 0.3162f, 0.7079f, 0.8913f, 1.1600f, 0.8100f, 0.5500f, 1.4125f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 1.0000f, 0.0180f, { 0.0000f, 0.0000f, 0.0000f }, 0.1920f, 0.2100f, 0.2500f, 0.0000f, 0.9943f, 3316.1001f, 458.2000f, 0.0000f, 0x1 }

/* Wooden Galleon Presets */

#define EFX_REVERB_PRESET_WOODEN_SMALLROOM \
    { 1.0000f, 1.0000f, 0.3162f, 0.1122f, 0.3162f, 0.7900f, 0.3200f, 0.8700f, 1.0000f, 0.0320f, { 0.0000f, 0.0000f, 0.0000f }, 0.8913f, 0.0290f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 4705.0000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_WOODEN_SHORTPASSAGE \
    { 1.0000f, 1.0000f, 0.3162f, 0.1259f, 0.3162f, 1.7500f, 0.5000f, 0.8700f, 0.8913f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 0.6310f, 0.0240f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 4705.0000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_WOODEN_MEDIUMROOM \
    { 1.0000f, 1.0000f, 0.3162f, 0.1000f, 0.2818f, 1.4700f, 0.4200f, 0.8200f, 0.8913f, 0.0490f, { 0.0000f, 0.0000f, 0.0000f }, 0.8913f, 0.0290f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 4705.0000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_WOODEN_LARGEROOM \
    { 1.0000f, 1.0000f, 0.3162f, 0.0891f, 0.2818f, 2.6500f, 0.3300f, 0.8200f, 0.8913f, 0.0660f, { 0.0000f, 0.0000f, 0.0000f }, 0.7943f, 0.0490f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 4705.0000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_WOODEN_LONGPASSAGE \
    { 1.0000f, 1.0000f, 0.3162f, 0.1000f, 0.3162f, 1.9900f, 0.4000f, 0.7900f, 1.0000f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 0.4467f, 0.0360f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 4705.0000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_WOODEN_HALL \
    { 1.0000f, 1.0000f, 0.3162f, 0.0794f, 0.2818f, 3.4500f, 0.3000f, 0.8200f, 0.8913f, 0.0880f, { 0.0000f, 0.0000f, 0.0000f }, 0.7943f, 0.0630f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 4705.0000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_WOODEN_CUPBOARD \
    { 1.0000f, 1.0000f, 0.3162f, 0.1413f, 0.3162f, 0.5600f, 0.4600f, 0.9100f, 1.1220f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 1.1220f, 0.0280f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 4705.0000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_WOODEN_COURTYARD \
    { 1.0000f, 0.6500f, 0.3162f, 0.0794f, 0.3162f, 1.7900f, 0.3500f, 0.7900f, 0.5623f, 0.1230f, { 0.0000f, 0.0000f, 0.0000f }, 0.1000f, 0.0320f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 4705.0000f, 99.6000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_WOODEN_ALCOVE \
    { 1.0000f, 1.0000f, 0.3162f, 0.1259f, 0.3162f, 1.2200f, 0.6200f, 0.9100f, 1.1220f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 0.7079f, 0.0240f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 4705.0000f, 99.6000f, 0.0000f, 0x1 }

/* Sports Presets */

#define EFX_REVERB_PRESET_SPORT_EMPTYSTADIUM \
    { 1.0000f, 1.0000f, 0.3162f, 0.4467f, 0.7943f, 6.2600f, 0.5100f, 1.1000f, 0.0631f, 0.1830f, { 0.0000f, 0.0000f, 0.0000f }, 0.3981f, 0.0380f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SPORT_SQUASHCOURT \
    { 1.0000f, 0.7500f, 0.3162f, 0.3162f, 0.7943f, 2.2200f, 0.9100f, 1.1600f, 0.4467f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 0.7943f, 0.0110f, { 0.0000f, 0.0000f, 0.0000f }, 0.1260f, 0.1900f, 0.2500f, 0.0000f, 0.9943f, 7176.8999f, 211.2000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SPORT_SMALLSWIMMINGPOOL \
    { 1.0000f, 0.7000f, 0.3162f, 0.7943f, 0.8913f, 2.7600f, 1.2500f, 1.1400f, 0.6310f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 0.7943f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 0.1790f, 0.1500f, 0.8950f, 0.1900f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_SPORT_LARGESWIMMINGPOOL \
    { 1.0000f, 0.8200f, 0.3162f, 0.7943f, 1.0000f, 5.4900f, 1.3100f, 1.1400f, 0.4467f, 0.0390f, { 0.0000f, 0.0000f, 0.0000f }, 0.5012f, 0.0490f, { 0.0000f, 0.0000f, 0.0000f }, 0.2220f, 0.5500f, 1.1590f, 0.2100f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_SPORT_GYMNASIUM \
    { 1.0000f, 0.8100f, 0.3162f, 0.4467f, 0.8913f, 3.1400f, 1.0600f, 1.3500f, 0.3981f, 0.0290f, { 0.0000f, 0.0000f, 0.0000f }, 0.5623f, 0.0450f, { 0.0000f, 0.0000f, 0.0000f }, 0.1460f, 0.1400f, 0.2500f, 0.0000f, 0.9943f, 7176.8999f, 211.2000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SPORT_FULLSTADIUM \
    { 1.0000f, 1.0000f, 0.3162f, 0.0708f, 0.7943f, 5.2500f, 0.1700f, 0.8000f, 0.1000f, 0.1880f, { 0.0000f, 0.0000f, 0.0000f }, 0.2818f, 0.0380f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SPORT_STADIUMTANNOY \
    { 1.0000f, 0.7800f, 0.3162f, 0.5623f, 0.5012f, 2.5300f, 0.8800f, 0.6800f, 0.2818f, 0.2300f, { 0.0000f, 0.0000f, 0.0000f }, 0.5012f, 0.0630f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.2000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

/* Prefab Presets */

#define EFX_REVERB_PRESET_PREFAB_WORKSHOP \
    { 0.4287f, 1.0000f, 0.3162f, 0.1413f, 0.3981f, 0.7600f, 1.0000f, 1.0000f, 1.0000f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 1.1220f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_PREFAB_SCHOOLROOM \
    { 0.4022f, 0.6900f, 0.3162f, 0.6310f, 0.5012f, 0.9800f, 0.4500f, 0.1800f, 1.4125f, 0.0170f, { 0.0000f, 0.0000f, 0.0000f }, 1.4125f, 0.0150f, { 0.0000f, 0.0000f, 0.0000f }, 0.0950f, 0.1400f, 0.2500f, 0.0000f, 0.9943f, 7176.8999f, 211.2000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_PREFAB_PRACTISEROOM \
    { 0.4022f, 0.8700f, 0.3162f, 0.3981f, 0.5012f, 1.1200f, 0.5600f, 0.1800f, 1.2589f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 1.4125f, 0.0110f, { 0.0000f, 0.0000f, 0.0000f }, 0.0950f, 0.1400f, 0.2500f, 0.0000f, 0.9943f, 7176.8999f, 211.2000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_PREFAB_OUTHOUSE \
    { 1.0000f, 0.8200f, 0.3162f, 0.1122f, 0.1585f, 1.3800f, 0.3800f, 0.3500f, 0.8913f, 0.0240f, { 0.0000f, 0.0000f, -0.0000f }, 0.6310f, 0.0440f, { 0.0000f, 0.0000f, 0.0000f }, 0.1210f, 0.1700f, 0.2500f, 0.0000f, 0.9943f, 2854.3999f, 107.5000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_PREFAB_CARAVAN \
    { 1.0000f, 1.0000f, 0.3162f, 0.0891f, 0.1259f, 0.4300f, 1.5000f, 1.0000f, 1.0000f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 1.9953f, 0.0120f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

/* Dome and Pipe Presets */

#define EFX_REVERB_PRESET_DOME_TOMB \
    { 1.0000f, 0.7900f, 0.3162f, 0.3548f, 0.2239f, 4.1800f, 0.2100f, 0.1000f, 0.3868f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 1.6788f, 0.0220f, { 0.0000f, 0.0000f, 0.0000f }, 0.1770f, 0.1900f, 0.2500f, 0.0000f, 0.9943f, 2854.3999f, 20.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_PIPE_SMALL \
    { 1.0000f, 1.0000f, 0.3162f, 0.3548f, 0.2239f, 5.0400f, 0.1000f, 0.1000f, 0.5012f, 0.0320f, { 0.0000f, 0.0000f, 0.0000f }, 2.5119f, 0.0150f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 2854.3999f, 20.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_DOME_SAINTPAULS \
    { 1.0000f, 0.8700f, 0.3162f, 0.3548f, 0.2239f, 10.4800f, 0.1900f, 0.1000f, 0.1778f, 0.0900f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0420f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.1200f, 0.2500f, 0.0000f, 0.9943f, 2854.3999f, 20.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_PIPE_LONGTHIN \
    { 0.2560f, 0.9100f, 0.3162f, 0.4467f, 0.2818f, 9.2100f, 0.1800f, 0.1000f, 0.7079f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 0.7079f, 0.0220f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 2854.3999f, 20.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_PIPE_LARGE \
    { 1.0000f, 1.0000f, 0.3162f, 0.3548f, 0.2239f, 8.4500f, 0.1000f, 0.1000f, 0.3981f, 0.0460f, { 0.0000f, 0.0000f, 0.0000f }, 1.5849f, 0.0320f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 2854.3999f, 20.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_PIPE_RESONANT \
    { 0.1373f, 0.9100f, 0.3162f, 0.4467f, 0.2818f, 6.8100f, 0.1800f, 0.1000f, 0.7079f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 1.0000f, 0.0220f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 2854.3999f, 20.0000f, 0.0000f, 0x0 }

/* Outdoors Presets */

#define EFX_REVERB_PRESET_OUTDOORS_BACKYARD \
    { 1.0000f, 0.4500f, 0.3162f, 0.2512f, 0.5012f, 1.1200f, 0.3400f, 0.4600f, 0.4467f, 0.0690f, { 0.0000f, 0.0000f, -0.0000f }, 0.7079f, 0.0230f, { 0.0000f, 0.0000f, 0.0000f }, 0.2180f, 0.3400f, 0.2500f, 0.0000f, 0.9943f, 4399.1001f, 242.9000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_OUTDOORS_ROLLINGPLAINS \
    { 1.0000f, 0.0000f, 0.3162f, 0.0112f, 0.6310f, 2.1300f, 0.2100f, 0.4600f, 0.1778f, 0.3000f, { 0.0000f, 0.0000f, -0.0000f }, 0.4467f, 0.0190f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 1.0000f, 0.2500f, 0.0000f, 0.9943f, 4399.1001f, 242.9000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_OUTDOORS_DEEPCANYON \
    { 1.0000f, 0.7400f, 0.3162f, 0.1778f, 0.6310f, 3.8900f, 0.2100f, 0.4600f, 0.3162f, 0.2230f, { 0.0000f, 0.0000f, -0.0000f }, 0.3548f, 0.0190f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 1.0000f, 0.2500f, 0.0000f, 0.9943f, 4399.1001f, 242.9000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_OUTDOORS_CREEK \
    { 1.0000f, 0.3500f, 0.3162f, 0.1778f, 0.5012f, 2.1300f, 0.2100f, 0.4600f, 0.3981f, 0.1150f, { 0.0000f, 0.0000f, -0.0000f }, 0.1995f, 0.0310f, { 0.0000f, 0.0000f, 0.0000f }, 0.2180f, 0.3400f, 0.2500f, 0.0000f, 0.9943f, 4399.1001f, 242.9000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_OUTDOORS_VALLEY \
    { 1.0000f, 0.2800f, 0.3162f, 0.0282f, 0.1585f, 2.8800f, 0.2600f, 0.3500f, 0.1413f, 0.2630f, { 0.0000f, 0.0000f, -0.0000f }, 0.3981f, 0.1000f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.3400f, 0.2500f, 0.0000f, 0.9943f, 2854.3999f, 107.5000f, 0.0000f, 0x0 }

/* Mood Presets */

#define EFX_REVERB_PRESET_MOOD_HEAVEN \
    { 1.0000f, 0.9400f, 0.3162f, 0.7943f, 0.4467f, 5.0400f, 1.1200f, 0.5600f, 0.2427f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0290f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0800f, 2.7420f, 0.0500f, 0.9977f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_MOOD_HELL \
    { 1.0000f, 0.5700f, 0.3162f, 0.3548f, 0.4467f, 3.5700f, 0.4900f, 2.0000f, 0.0000f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 1.4125f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 0.1100f, 0.0400f, 2.1090f, 0.5200f, 0.9943f, 5000.0000f, 139.5000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_MOOD_MEMORY \
    { 1.0000f, 0.8500f, 0.3162f, 0.6310f, 0.3548f, 4.0600f, 0.8200f, 0.5600f, 0.0398f, 0.0000f, { 0.0000f, 0.0000f, 0.0000f }, 1.1220f, 0.0000f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.4740f, 0.4500f, 0.9886f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

/* Driving Presets */

#define EFX_REVERB_PRESET_DRIVING_COMMENTATOR \
    { 1.0000f, 0.0000f, 3.1623f, 0.5623f, 0.5012f, 2.4200f, 0.8800f, 0.6800f, 0.1995f, 0.0930f, { 0.0000f, 0.0000f, 0.0000f }, 0.2512f, 0.0170f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 1.0000f, 0.2500f, 0.0000f, 0.9886f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_DRIVING_PITGARAGE \
    { 0.4287f, 0.5900f, 0.3162f, 0.7079f, 0.5623f, 1.7200f, 0.9300f, 0.8700f, 0.5623f, 0.0000f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0160f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.1100f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_DRIVING_INCAR_RACER \
    { 0.0832f, 0.8000f, 0.3162f, 1.0000f, 0.7943f, 0.1700f, 2.0000f, 0.4100f, 1.7783f, 0.0070f, { 0.0000f, 0.0000f, 0.0000f }, 0.7079f, 0.0150f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 10268.2002f, 251.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_DRIVING_INCAR_SPORTS \
    { 0.0832f, 0.8000f, 0.3162f, 0.6310f, 1.0000f, 0.1700f, 0.7500f, 0.4100f, 1.0000f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 0.5623f, 0.0000f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 10268.2002f, 251.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_DRIVING_INCAR_LUXURY \
    { 0.2560f, 1.0000f, 0.3162f, 0.1000f, 0.5012f, 0.1300f, 0.4100f, 0.4600f, 0.7943f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 1.5849f, 0.0100f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 10268.2002f, 251.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_DRIVING_FULLGRANDSTAND \
    { 1.0000f, 1.0000f, 0.3162f, 0.2818f, 0.6310f, 3.0100f, 1.3700f, 1.2800f, 0.3548f, 0.0900f, { 0.0000f, 0.0000f, 0.0000f }, 0.1778f, 0.0490f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 10420.2002f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_DRIVING_EMPTYGRANDSTAND \
    { 1.0000f, 1.0000f, 0.3162f, 1.0000f, 0.7943f, 4.6200f, 1.7500f, 1.4000f, 0.2082f, 0.0900f, { 0.0000f, 0.0000f, 0.0000f }, 0.2512f, 0.0490f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.0000f, 0.9943f, 10420.2002f, 250.0000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_DRIVING_TUNNEL \
    { 1.0000f, 0.8100f, 0.3162f, 0.3981f, 0.8913f, 3.4200f, 0.9400f, 1.3100f, 0.7079f, 0.0510f, { 0.0000f, 0.0000f, 0.0000f }, 0.7079f, 0.0470f, { 0.0000f, 0.0000f, 0.0000f }, 0.2140f, 0.0500f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 155.3000f, 0.0000f, 0x1 }

/* City Presets */

#define EFX_REVERB_PRESET_CITY_STREETS \
    { 1.0000f, 0.7800f, 0.3162f, 0.7079f, 0.8913f, 1.7900f, 1.1200f, 0.9100f, 0.2818f, 0.0460f, { 0.0000f, 0.0000f, 0.0000f }, 0.1995f, 0.0280f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.2000f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CITY_SUBWAY \
    { 1.0000f, 0.7400f, 0.3162f, 0.7079f, 0.8913f, 3.0100f, 1.2300f, 0.9100f, 0.7079f, 0.0460f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0280f, { 0.0000f, 0.0000f, 0.0000f }, 0.1250f, 0.2100f, 0.2500f, 0.0000f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CITY_MUSEUM \
    { 1.0000f, 0.8200f, 0.3162f, 0.1778f, 0.1778f, 3.2800f, 1.4000f, 0.5700f, 0.2512f, 0.0390f, { 0.0000f, 0.0000f, -0.0000f }, 0.8913f, 0.0340f, { 0.0000f, 0.0000f, 0.0000f }, 0.1300f, 0.1700f, 0.2500f, 0.0000f, 0.9943f, 2854.3999f, 107.5000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_CITY_LIBRARY \
    { 1.0000f, 0.8200f, 0.3162f, 0.2818f, 0.0891f, 2.7600f, 0.8900f, 0.4100f, 0.3548f, 0.0290f, { 0.0000f, 0.0000f, -0.0000f }, 0.8913f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 0.1300f, 0.1700f, 0.2500f, 0.0000f, 0.9943f, 2854.3999f, 107.5000f, 0.0000f, 0x0 }

#define EFX_REVERB_PRESET_CITY_UNDERPASS \
    { 1.0000f, 0.8200f, 0.3162f, 0.4467f, 0.8913f, 3.5700f, 1.1200f, 0.9100f, 0.3981f, 0.0590f, { 0.0000f, 0.0000f, 0.0000f }, 0.8913f, 0.0370f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.1400f, 0.2500f, 0.0000f, 0.9920f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CITY_ABANDONED \
    { 1.0000f, 0.6900f, 0.3162f, 0.7943f, 0.8913f, 3.2800f, 1.1700f, 0.9100f, 0.4467f, 0.0440f, { 0.0000f, 0.0000f, 0.0000f }, 0.2818f, 0.0240f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.2000f, 0.2500f, 0.0000f, 0.9966f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

/* Misc. Presets */

#define EFX_REVERB_PRESET_DUSTYROOM \
    { 0.3645f, 0.5600f, 0.3162f, 0.7943f, 0.7079f, 1.7900f, 0.3800f, 0.2100f, 0.5012f, 0.0020f, { 0.0000f, 0.0000f, 0.0000f }, 1.2589f, 0.0060f, { 0.0000f, 0.0000f, 0.0000f }, 0.2020f, 0.0500f, 0.2500f, 0.0000f, 0.9886f, 13046.0000f, 163.3000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_CHAPEL \
    { 1.0000f, 0.8400f, 0.3162f, 0.5623f, 1.0000f, 4.6200f, 0.6400f, 1.2300f, 0.4467f, 0.0320f, { 0.0000f, 0.0000f, 0.0000f }, 0.7943f, 0.0490f, { 0.0000f, 0.0000f, 0.0000f }, 0.2500f, 0.0000f, 0.2500f, 0.1100f, 0.9943f, 5000.0000f, 250.0000f, 0.0000f, 0x1 }

#define EFX_REVERB_PRESET_SMALLWATERROOM \
    { 1.0000f, 0.7000f, 0.3162f, 0.4477f, 1.0000f, 1.5100f, 1.2500f, 1.1400f, 0.8913f, 0.0200f, { 0.0000f, 0.0000f, 0.0000f }, 1.4125f, 0.0300f, { 0.0000f, 0.0000f, 0.0000f }, 0.1790f, 0.1500f, 0.8950f, 0.1900f, 0.9920f, 5000.0000f, 250.0000f, 0.0000f, 0x0 }

  -----------------
  -- Subprograms --
  -----------------

  -- 2.7. AL Errors
  function alGetError return Int_32_Unsigned_C -- ALenum
                      with Import => True, Convention => StdCall, External_Name => "alGetError"; 

  -- 2.8. Controlling AL Execution
  procedure alEnable (target : Int_32_Unsigned_C) -- ALenum
                      with Import => True, Convention => StdCall, External_Name => "alEnable"; 

  -- 2.8. Controlling AL Execution
  procedure alDisable (target : Int_32_Unsigned_C) -- ALenum
                       with Import => True, Convention => StdCall, External_Name => "alDisable"; 

  -- 2.8. Controlling AL Execution
  function alIsEnabled (target : Int_32_Unsigned_C) -- ALenum
                        return Int_8_Unsigned_C     -- ALboolean
                        with Import => True, Convention => StdCall, External_Name => "alIsEnabled"; 

  -- 2.12. Requesting Object Names
  procedure alGenBuffers (n       : Int_Size_C;                  -- ALsizei
                          buffers : Ptr_Array_Int_32_Unsigned_C) -- ALuint*
                          with Import => True, Convention => StdCall, External_Name => "alGenBuffers"; 

  -- 2.13. Releasing Object Names
  procedure alDeleteBuffers (n       : Int_Size_C;                  -- ALsizei
                             buffers : Ptr_Array_Int_32_Unsigned_C) -- ALuint*
                             with Import => True, Convention => StdCall, External_Name => "alDeleteBuffers"; 

  -- 2.14. Validating an Object Name
  function alIsBuffer (bid : Int_32_Unsigned_C) -- ALuint 
                       return Int_8_Unsigned_C  -- ALboolean
                       with Import => True, Convention => StdCall, External_Name => "alIsBuffer"; 

  -- 2.14. Validating an Object Name
  function alIsSource (sid : Int_32_Unsigned_C) -- ALuint
                       return Int_8_Unsigned_C  -- ALboolean
                       with Import => True, Convention => StdCall, External_Name => "alIsSource"; 

  -- 3.1.1. Simple Queries
  procedure alGetBooleanv (paramName : Int_32_Unsigned_C;           -- ALenum
                           dest      : Ptr_Array_Int_8_Unsigned_C)  -- ALboolean*
                           with Import => True, Convention => StdCall, External_Name => "alGetBooleanv"; 

  -- 3.1.1. Simple Queries
  function alGetBoolean (paramName : Int_32_Unsigned_C) -- ALenum
                         return Int_8_Unsigned_C        -- ALboolean
                         with Import => True, Convention => StdCall, External_Name => "alGetBoolean"; 

  -- 3.1.1. Simple Queries
  procedure alGetIntegerv (paramName : Int_32_Unsigned_C;         -- ALenum
                           dest      : Ptr_Array_Int_32_Signed_C) -- ALint*
                           with Import => True, Convention => StdCall, External_Name => "alGetIntegerv"; 

  -- 3.1.1. Simple Queries
  function alGetInteger (paramName : Int_32_Unsigned_C) -- ALenum
                         return Int_C                   -- ALint
                         with Import => True, Convention => StdCall, External_Name => "alGetInteger"; 

  -- 3.1.1. Simple Queries
  procedure alGetFloatv (paramName : Int_32_Unsigned_C;   -- ALenum
                         dest      : Ptr_Array_Real_32_C) -- ALfloat*
                         with Import => True, Convention => StdCall, External_Name => "alGetFloatv"; 

  -- 3.1.1. Simple Queries
  function alGetFloat (paramName : Int_32_Unsigned_C) -- ALenum
                       return Real_32_C               -- ALfloat
                       with Import => True, Convention => StdCall, External_Name => "alGetFloat"; 

  -- 3.1.1. Simple Queries
  procedure alGetDoublev (paramName : Int_32_Unsigned_C;   -- ALenum
                          dest      : Ptr_Array_Real_64_C) -- ALdouble*
                          with Import => True, Convention => StdCall, External_Name => "alGetDoublev"; 

  -- 3.1.1. Simple Queries
  function alGetDouble (paramName : Int_32_Unsigned_C) -- ALenum
                        return Real_64_C               -- ALdouble
                        with Import => True, Convention => StdCall, External_Name => "alGetDouble"; 

  -- 3.4. Attenuation By Distance
  procedure alDistanceModel (distanceModel : Int_32_Unsigned_C) -- ALenum 
                             with Import => True, Convention => StdCall, External_Name => "alDistanceModel"; 

  -- 3.5.2. Velocity Dependent Doppler Effect
  procedure alDopplerFactor (Value : Real_32_C) -- ALfloat
                             with Import => True, Convention => StdCall, External_Name => "alDopplerFactor";  

  -- 3.5.2. Velocity Dependent Doppler Effect
  procedure alSpeedOfSound (Value : Real_32_C) -- ALfloat
                            with Import => True, Convention => StdCall, External_Name => "alSpeedOfSound"; 

  -- 4.2.2. Changing Listener Attributes
  procedure alListenerf (param : Int_32_Unsigned_C; -- ALenum
                         Value : Real_32_C)         -- ALfloat
                         with Import => True, Convention => StdCall, External_Name => "alListenerf"; 

  -- 4.2.2. Changing Listener Attributes
  procedure alListener3f (param  : Int_32_Unsigned_C; -- ALenum
                          value1 : Real_32_C;         -- ALfloat
                          value2 : Real_32_C;         -- ALfloat
                          value3 : Real_32_C)         -- ALfloat
                          with Import => True, Convention => StdCall, External_Name => "alListener3f"; 

  -- 4.2.2. Changing Listener Attributes
  procedure alListeneri (param : Int_32_Unsigned_C; -- ALenum
                         Value : Int_C)             -- ALint 
                         with Import => True, Convention => StdCall, External_Name => "alListeneri"; 

  -- 4.2.2. Changing Listener Attributes
  procedure alListenerfv (param  : Int_32_Unsigned_C; -- ALenum
                          value1 : Int_C;             -- ALint
                          value2 : Int_C;             -- ALint
                          value3 : Int_C)             -- ALint 
                          with Import => True, Convention => StdCall, External_Name => "alListener3i"; 

  -- 4.2.2. Changing Listener Attributes
  procedure alListenerfv (param  : Int_32_Unsigned_C;   -- ALenum
                          values : Ptr_Array_Real_32_C) -- ALfloat*
                          with Import => True, Convention => StdCall, External_Name => "alListenerfv"; 

  -- 4.2.2. Changing Listener Attributes
  procedure alListeneriv (param  : Int_32_Unsigned_C;         -- ALenum
                          values : Ptr_Array_Int_32_Signed_C) -- ALint*
                          with Import => True, Convention => StdCall, External_Name => "alListeneriv"; 

  -- 4.2.3. Querying Listener Attributes
  procedure alGetListenerf (param : Int_32_Unsigned_C; -- ALenum 
                            Value : Pointer) -- 
                            with Import => True, Convention => StdCall, External_Name => "alGetListenerf"; 

  -- 4.2.3. Querying Listener Attributes
  procedure alGetListener3f (param : Int_32_Unsigned_C; -- ALenum
                             value1 : Pointer; -- 
                             value2 : Pointer; -- 
                             value3 : Pointer) -- 
                             with Import => True, Convention => StdCall, External_Name => "alGetListener3f"; 

  -- 4.2.3. Querying Listener Attributes
  procedure alGetListenerfv (param : Int_32_Unsigned_C; -- ALenum
                             values : Pointer) -- 
                             with Import => True, Convention => StdCall, External_Name => "alGetListenerfv"; 

  -- 4.2.3. Querying Listener Attributes
  procedure alGetListeneri (param : Int_32_Unsigned_C; -- ALenum
                            Value : Pointer) -- 
                            with Import => True, Convention => StdCall, External_Name => "alGetListeneri"; 
 
  -- 4.2.3. Querying Listener Attributes
  procedure alGetListener3i (param  : Int_32_Unsigned_C; -- ALenum
                             value1 : Pointer; -- 
                             value2 : Pointer; -- 
                             value3 : Pointer) -- 
                             with Import => True, Convention => StdCall, External_Name => "alGetListener3i"; 

  -- 4.2.3. Querying Listener Attributes
  procedure alGetListeneriv (param : Int_32_Unsigned_C; -- ALenum
                             values : Pointer) -- 
                             with Import => True, Convention => StdCall, External_Name => "alGetListeneriv"; 

  -- 4.3.1. Managing Source Names
  procedure alGenSources (n       : Int_Size_C; -- ALsizei
                          sources : Pointer) -- 
                           with Import => True, Convention => StdCall, External_Name => "alGenSources"; 

  -- 4.3.1. Managing Source Names
  procedure alDeleteSources (n       : Int_Size_C; -- ALsizei
                             Sources : Pointer) -- 
                             with Import => True, Convention => StdCall, External_Name => "alDeleteSources"; 

  -- 4.3.3. Changing Source Attributes
  procedure alSourcef (sid   : Int_32_Unsigned_C; -- ALuint 
                       param : Int_32_Unsigned_C; -- ALenum
                       Value : Real_32_C)         -- ALfloat
                       with Import => True, Convention => StdCall, External_Name => "alSourcef"; 

  -- 4.3.3. Changing Source Attributes
  procedure alSource3f (sid    : Int_32_Unsigned_C; -- ALuint 
                        param  : Int_32_Unsigned_C; -- ALenum
                        value1 : Real_32_C;         -- ALfloat
                        value2 : Real_32_C;         -- ALfloat
                        value3 : Real_32_C)         -- ALfloat
                        with Import => True, Convention => StdCall, External_Name => "alSource3f"; 

  -- 4.3.3. Changing Source Attributes
  procedure alSourcei (sid   : Int_32_Unsigned_C; -- ALuint 
                       param : Int_32_Unsigned_C; -- ALenum
                       Value : Int_C)             -- ALint 
                       with Import => True, Convention => StdCall, External_Name => "alSourcei"; 

  -- 4.3.3. Changing Source Attributes
  procedure alSource3i (sid    : Int_32_Unsigned_C; -- ALuint 
                        param  : Int_32_Unsigned_C; -- ALenum
                        value1 : Int_C; -- ALint
                        value2 : Int_C; -- ALint
                        value3 : Int_C) -- ALint 
                        with Import => True, Convention => StdCall, External_Name => "alSource3i"; 

  -- 4.3.3. Changing Source Attributes
  procedure alSourcefv (sid    : Int_32_Unsigned_C; -- ALuint 
                        param  : Int_32_Unsigned_C; -- ALenum
                        values : Pointer) -- 
                        with Import => True, Convention => StdCall, External_Name => "alSourcefv"; 

  -- 4.3.3. Changing Source Attributes
  procedure alSourceiv (sid : Int_32_Unsigned_C; -- ALuint 
                        param : Int_32_Unsigned_C; -- ALenum
                        values : Pointer) -- 
                        with Import => True, Convention => StdCall, External_Name => "alSourceiv"; 

  -- 4.3.4. Querying Source Attributes
  procedure alGetSourcef (sid   : Int_32_Unsigned_C; -- ALuint 
                          param : Int_32_Unsigned_C; -- ALenum
                          value : Poin) -- 
                          with Import => True, Convention => StdCall, External_Name => "alGetSourcef"; 

  -- 4.3.4. Querying Source Attributes
  procedure alGetSource3f (sid    : Int_32_Unsigned_C; -- ALuint 
                           param  : Int_32_Unsigned_C; -- ALenum
                           value1 : Pointer; -- 
                           value2 : Pointer; -- 
                           value3 : Pointer) -- 
                           with Import => True, Convention => StdCall, External_Name => "alGetSource3f"; 

  -- 4.3.4. Querying Source Attributes
  procedure alGetSourcefv (sid    : Int_32_Unsigned_C; -- ALuint 
                           param  : Int_32_Unsigned_C; -- ALenum
                           values : Pointer) -- 
                           with Import => True, Convention => StdCall, External_Name => "alGetSourcefv"; 

  -- 4.3.4. Querying Source Attributes
  procedure alGetSourcei (sid   : Int_32_Unsigned_C; -- ALuint 
                          param : Int_32_Unsigned_C; -- ALenum
                          Value : Pointer) -- 
                          with Import => True, Convention => StdCall, External_Name => "alGetSourcei"; 

  -- 4.3.4. Querying Source Attributes
  procedure alGetSource3i (sid    : Int_32_Unsigned_C; -- ALuint 
                           param  : Int_32_Unsigned_C; -- ALenum
                           value1 : Pointer; -- 
                           value2 : Pointer; -- 
                           value3 : Pointer) -- 
                           with Import => True, Convention => StdCall, External_Name => "alGetSource3i"; 

  -- 4.3.4. Querying Source Attributes
  procedure alGetSourceiv (sid    : Int_32_Unsigned_C; -- ALuint 
                           param  : Int_32_Unsigned_C; -- ALenum
                           values : Pointer) -- 
                           with Import => True, Convention => StdCall, External_Name => "alGetSourceiv"; 

  -- 4.3.5. Queuing Buffers with a Source
  procedure alSourceQueueBuffers (sid        : Int_32_Unsigned_C; -- ALuint 
                                  numEntries : Int_Size_C; -- ALsizei
                                  bids       : Pointer) -- 
                                  with Import => True, Convention => StdCall, External_Name => "alSourceQueueBuffers"; 
  
  -- 4.3.5. Queuing Buffers with a Source
  procedure alSourceUnqueueBuffers (sid        : Int_32_Unsigned_C; -- ALuint 
                                    numEntries : Int_Size_C; -- ALsizei
                                    bids       : Pointer) -- 
                                    with Import => True, Convention => StdCall, External_Name => "alSourceUnqueueBuffers"; 

  -- 4.3.6. Managing Source Execution
  procedure alSourcePlayv (ns   : Int_Size_C; -- ALsizei
                           sids : Pointer) -- 
                           with Import => True, Convention => StdCall, External_Name => "alSourcePlayv"; 

  -- 4.3.6. Managing Source Execution
  procedure alSourcePlay (sid : UInt) -- 
                          with Import => True, Convention => StdCall, External_Name => "alSourcePlay"; 

  -- 4.3.6. Managing Source Execution
  procedure alSourceStopv (ns   : Int_Size_C; -- ALsizei
                           sids : Pointer) -- 
                           with Import => True, Convention => StdCall, External_Name => "alSourceStopv"; 

  -- 4.3.6. Managing Source Execution
  procedure alSourceStop (sid : UInt) -- 
                          with Import => True, Convention => StdCall, External_Name => "alSourceStop"; 

  -- 4.3.6. Managing Source Execution
  procedure alSourceRewindv (ns   : Int_Size_C; -- ALsizei
                             sids : Pointer) -- 
                             with Import => True, Convention => StdCall, External_Name => "alSourceRewindv"; 

  -- 4.3.6. Managing Source Execution
  procedure alSourceRewind (sid : UInt) -- 
                            with Import => True, Convention => StdCall, External_Name => "alSourceRewind"; 

  -- 4.3.6. Managing Source Execution
  procedure alSourcePausev (ns : Int_Size_C; -- ALsizei
                            SIds : Pointer) -- 
                            with Import => True, Convention => StdCall, External_Name => "alSourcePausev"; 

  -- 4.3.6. Managing Source Execution
  procedure alSourcePause (sid : UInt) -- 
                           with Import => True, Convention => StdCall, External_Name => "alSourcePause"; 

  -- 5.1. Buffer States
  procedure alBufferData (bid    : Int_32_Unsigned_C; -- ALuint 
                          format : Int_32_Unsigned_C; -- ALenum
                          data   : Ptr;               -- const ALvoid*
                          size   : Int_Size_C;        -- ALsizei
                          freq   : Int_Size_C)        -- ALsizei
                          with Import => True, Convention => StdCall, External_Name => "alBufferData"; 

  -- 5.3.2. Changing Buffer Attributes
  procedure alBufferf (bid   : Int_32_Unsigned_C; -- ALuint 
                       param : Int_32_Unsigned_C; -- ALenum
                       Value : Real_32_C)         -- ALfloat
                       with Import => True, Convention => StdCall, External_Name => "alBufferf"; 

  -- 5.3.2. Changing Buffer Attributes
  procedure alBuffer3f (bid    : Int_32_Unsigned_C; -- ALuint 
                        param  : Int_32_Unsigned_C; -- ALenum
                        value1 : Real_32_C;         -- ALfloat
                        value2 : Real_32_C;         -- ALfloat
                        value3 : Real_32_C)         -- ALfloat
                        with Import => True, Convention => StdCall, External_Name => "alBuffer3f"; 

  -- 5.3.2. Changing Buffer Attributes
  procedure alBufferi (bid : Int_32_Unsigned_C; -- ALuint 
                       param : Int_32_Unsigned_C; -- ALenum
                       Value : Int_C) -- ALint 
                       with Import => True, Convention => StdCall, External_Name => "alBufferi"; 

  -- 5.3.2. Changing Buffer Attributes
  procedure alBuffer3i (bid : Int_32_Unsigned_C; -- ALuint 
                        param : Int_32_Unsigned_C; -- ALenum
                        value1 : Int_C; -- ALint
                        value2 : Int_C; -- ALint
                        value3 : Int_C) -- ALint 
                        with Import => True, Convention => StdCall, External_Name => "alBuffer3i"; 

  -- 5.3.2. Changing Buffer Attributes
  procedure alBufferfv (bid : Int_32_Unsigned_C; -- ALuint 
                        param : Int_32_Unsigned_C; -- ALenum
                        values : Pointer) -- 
                        with Import => True, Convention => StdCall, External_Name => "alBufferfv"; 

  -- 5.3.2. Changing Buffer Attributes
  procedure alBufferiv (bid : Int_32_Unsigned_C; -- ALuint 
                        param : Int_32_Unsigned_C; -- ALenum
                        values : Pointer) -- 
                        with Import => True, Convention => StdCall, External_Name => "alBufferiv"; 

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBufferf (bid : Int_32_Unsigned_C; -- ALuint 
                          param : Int_32_Unsigned_C; -- ALenum
                          Value : Pointer) -- 
                          with Import => True, Convention => StdCall, External_Name => "alGetBufferf"; 

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBuffer3f (bid : Int_32_Unsigned_C; -- ALuint 
                           param : Int_32_Unsigned_C; -- ALenum
                           value1 : Pointer; -- 
                           value2 : Pointer; -- 
                           value3 : Pointer) -- 
                           with Import => True, Convention => StdCall, External_Name => "alGetBuffer3f"; 

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBufferfv (bid : Int_32_Unsigned_C; -- ALuint 
                           param : Int_32_Unsigned_C; -- ALenum
                           values : Real_32_C) -- ALfloat
                           with Import => True, Convention => StdCall, External_Name => "alGetBufferfv"; 

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBufferi (bid : Int_32_Unsigned_C; -- ALuint 
                          param : Int_32_Unsigned_C; -- ALenum
                          Value : Pointer) -- 
                          with Import => True, Convention => StdCall, External_Name => "alGetBufferi"; 

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBuffer3i (bid : Int_32_Unsigned_C; -- ALuint 
                           param : Int_32_Unsigned_C; -- ALenum
                           value1 : Pointer; -- 
                           value2 : Pointer; -- 
                           value3 : Pointer) -- 
                           with Import => True, Convention => StdCall, External_Name => "alGetBuffer3i"; 

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBufferiv (bid    : Int_32_Unsigned_C; -- ALuint 
                           param  : Int_32_Unsigned_C; -- ALCenum
                           values : Pointer) -- ALint*
                           with Import => True, Convention => StdCall, External_Name => "alGetBufferiv"; 

  -- 6.1.1. Connecting to a Device
  function alcOpenDevice (devicename : String) -- ALCchar*
                          return Ptr -- ALCdevice*
                          with Import => True, Convention => StdCall, External_Name => "alcOpenDevice"; 

  -- 6.1.2. Disconnecting from a Device
  function alcCloseDevice (device : Ptr)           -- ALCdevice*
                           return Int_8_Unsigned_C -- ALCboolean
                           with Import => True, Convention => StdCall, External_Name => "alcCloseDevice"; 
                           
  -- 6.2.2. Creating a Context
  function alcCreateContext (device   : Ptr; -- ALCdevice*
                             attrlist : Pointer) -- ALCint*
                             return Ptr -- ALCcontext* 
                             with Import => True, Convention => StdCall, External_Name => "alcCreateContext"; 

  -- 6.2.3. Selecting a Context for Operation
  function alcMakeContextCurrent (context : Ptr)          -- ALCcontext* 
                                  return Int_8_Unsigned_C -- ALCboolean
                                  with Import => True, Convention => StdCall, External_Name => "alcMakeContextCurrent"; 

  -- 6.2.4. Initiate Context Processing
  procedure alcProcessContext (context : Ptr) -- ALCcontext* 
                               with Import => True, Convention => StdCall, External_Name => "alcProcessContext"; 

  -- 6.2.5. Suspend Context Processing
  procedure alcSuspendContext (context : Ptr) -- ALCcontext* 
                               with Import => True, Convention => StdCall, External_Name => "alcSuspendContext"; 

  -- 6.2.6. Destroying a Context
  procedure alcDestroyContext (context : Ptr) -- ALCcontext* 
                               with Import => True, Convention => StdCall, External_Name => "alcDestroyContext"; 

  -- 6.3.1. Query for Current Context
  function alcGetCurrentContext return Ptr -- ALCcontext* 
                                with Import => True, Convention => StdCall, External_Name => "alcGetCurrentContext"; 

  -- 6.3.2. Query for a Context's Device
  function alcGetContextsDevice (context : Ptr) -- ALCcontext* 
                                 return Ptr     -- ALCdevice*
                                 with Import => True, Convention => StdCall, External_Name => "alcGetContextsDevice"; 

  -- 6.3.6. Query for Error Conditions
  function alcGetError (device : Ptr)            -- ALCdevice*
                        return Int_32_Unsigned_C -- ALCenum
                        with Import => True, Convention => StdCall, External_Name => "alcGetError"; 

  -- 6.3.8. Integer Query
  procedure alcGetIntegerv (device : Ptr;                       -- ALCdevice*
                            param  : Int_32_Unsigned_C;         -- ALCenum
                            size   : Int_Size_C;                -- ALCsizei
                            data   : Ptr_Array_Int_32_Signed_C) -- ALCint*
                            with Import => True, Convention => StdCall, External_Name => "alcGetIntegerv"; 

  -- 6.4.2. Capture
  function alcCaptureCloseDevice (device : Ptr)           -- ALCdevice*
                                  return Int_8_Unsigned_C -- ALCboolean
                                  with Import => True, Convention => StdCall, External_Name => "alcCaptureCloseDevice"; 

  -- 6.4.2. Capture
  procedure alcCaptureStart (device : Ptr) -- ALCdevice*
                             with Import => True, Convention => StdCall, External_Name => "alcCaptureStart"; 

  -- 6.4.2. Capture
  procedure alcCaptureStop (device : Ptr) -- ALCdevice*
                            with Import => True, Convention => StdCall, External_Name => "alcCaptureStop"; 

  -- 6.4.2. Capture
  procedure alcCaptureSamples (device : Ptr;        -- ALCdevice*
                               bug    : Ptr;        -- ALCvoid*
                               samps  : Int_Size_C) -- ALCsizei 
                               with Import => True, Convention => StdCall, External_Name => "alcCaptureSamples"; 

typedef ALvoid (AL_APIENTRY*PFNALBUFFERDATASTATICPROC)(const ALint,ALenum,ALvoid*,ALsizei,ALsizei);
#ifdef AL_ALEXT_PROTOTYPES
AL_API ALvoid AL_APIENTRY alBufferDataStatic(const ALint buffer, ALenum format, ALvoid *data, ALsizei len, ALsizei freq);
typedef void (AL_APIENTRY*LPALFOLDBACKCALLBACK)(ALenum,ALsizei);
typedef void (AL_APIENTRY*LPALREQUESTFOLDBACKSTART)(ALenum,ALsizei,ALsizei,ALfloat*,LPALFOLDBACKCALLBACK);
typedef void (AL_APIENTRY*LPALREQUESTFOLDBACKSTOP)(void);
#ifdef AL_ALEXT_PROTOTYPES
AL_API void AL_APIENTRY alRequestFoldbackStart(ALenum mode,ALsizei count,ALsizei length,ALfloat *mem,LPALFOLDBACKCALLBACK callback);
AL_API void AL_APIENTRY alRequestFoldbackStop(void);
typedef void (AL_APIENTRY*LPALFOLDBACKCALLBACK)(ALenum,ALsizei);
typedef void (AL_APIENTRY*LPALREQUESTFOLDBACKSTART)(ALenum,ALsizei,ALsizei,ALfloat*,LPALFOLDBACKCALLBACK);
typedef void (AL_APIENTRY*LPALREQUESTFOLDBACKSTOP)(void);
#ifdef AL_ALEXT_PROTOTYPES
AL_API void AL_APIENTRY alRequestFoldbackStart(ALenum mode,ALsizei count,ALsizei length,ALfloat *mem,LPALFOLDBACKCALLBACK callback);
AL_API void AL_APIENTRY alRequestFoldbackStop(void);
typedef void (AL_APIENTRY*LPALBUFFERSAMPLESSOFT)(ALuint,ALuint,ALenum,ALsizei,ALenum,ALenum,const ALvoid*);
typedef void (AL_APIENTRY*LPALBUFFERSUBSAMPLESSOFT)(ALuint,ALsizei,ALsizei,ALenum,ALenum,const ALvoid*);
typedef void (AL_APIENTRY*LPALGETBUFFERSAMPLESSOFT)(ALuint,ALsizei,ALsizei,ALenum,ALenum,ALvoid*);
typedef ALboolean (AL_APIENTRY*LPALISBUFFERFORMATSUPPORTEDSOFT)(ALenum);
#ifdef AL_ALEXT_PROTOTYPES
AL_API void AL_APIENTRY alBufferSamplesSOFT(ALuint buffer, ALuint samplerate, ALenum internalformat, ALsizei samples, ALenum channels, ALenum type, const ALvoid *data);
AL_API void AL_APIENTRY alBufferSubSamplesSOFT(ALuint buffer, ALsizei offset, ALsizei samples, ALenum channels, ALenum type, const ALvoid *data);
AL_API void AL_APIENTRY alGetBufferSamplesSOFT(ALuint buffer, ALsizei offset, ALsizei samples, ALenum channels, ALenum type, ALvoid *data);
AL_API ALboolean AL_APIENTRY alIsBufferFormatSupportedSOFT(ALenum format);
#endif
typedef void (AL_APIENTRY*LPALSOURCEDSOFT)(ALuint,ALenum,ALdouble);
typedef void (AL_APIENTRY*LPALSOURCE3DSOFT)(ALuint,ALenum,ALdouble,ALdouble,ALdouble);
typedef void (AL_APIENTRY*LPALSOURCEDVSOFT)(ALuint,ALenum,const ALdouble*);
typedef void (AL_APIENTRY*LPALGETSOURCEDSOFT)(ALuint,ALenum,ALdouble*);
typedef void (AL_APIENTRY*LPALGETSOURCE3DSOFT)(ALuint,ALenum,ALdouble*,ALdouble*,ALdouble*);
typedef void (AL_APIENTRY*LPALGETSOURCEDVSOFT)(ALuint,ALenum,ALdouble*);
typedef void (AL_APIENTRY*LPALSOURCEI64SOFT)(ALuint,ALenum,ALint64SOFT);
typedef void (AL_APIENTRY*LPALSOURCE3I64SOFT)(ALuint,ALenum,ALint64SOFT,ALint64SOFT,ALint64SOFT);
typedef void (AL_APIENTRY*LPALSOURCEI64VSOFT)(ALuint,ALenum,const ALint64SOFT*);
typedef void (AL_APIENTRY*LPALGETSOURCEI64SOFT)(ALuint,ALenum,ALint64SOFT*);
typedef void (AL_APIENTRY*LPALGETSOURCE3I64SOFT)(ALuint,ALenum,ALint64SOFT*,ALint64SOFT*,ALint64SOFT*);
typedef void (AL_APIENTRY*LPALGETSOURCEI64VSOFT)(ALuint,ALenum,ALint64SOFT*);
#ifdef AL_ALEXT_PROTOTYPES
AL_API void AL_APIENTRY alSourcedSOFT(ALuint source, ALenum param, ALdouble value);
AL_API void AL_APIENTRY alSource3dSOFT(ALuint source, ALenum param, ALdouble value1, ALdouble value2, ALdouble value3);
AL_API void AL_APIENTRY alSourcedvSOFT(ALuint source, ALenum param, const ALdouble *values);
AL_API void AL_APIENTRY alGetSourcedSOFT(ALuint source, ALenum param, ALdouble *value);
AL_API void AL_APIENTRY alGetSource3dSOFT(ALuint source, ALenum param, ALdouble *value1, ALdouble *value2, ALdouble *value3);
AL_API void AL_APIENTRY alGetSourcedvSOFT(ALuint source, ALenum param, ALdouble *values);
AL_API void AL_APIENTRY alSourcei64SOFT(ALuint source, ALenum param, ALint64SOFT value);
AL_API void AL_APIENTRY alSource3i64SOFT(ALuint source, ALenum param, ALint64SOFT value1, ALint64SOFT value2, ALint64SOFT value3);
AL_API void AL_APIENTRY alSourcei64vSOFT(ALuint source, ALenum param, const ALint64SOFT *values);
AL_API void AL_APIENTRY alGetSourcei64SOFT(ALuint source, ALenum param, ALint64SOFT *value);
AL_API void AL_APIENTRY alGetSource3i64SOFT(ALuint source, ALenum param, ALint64SOFT *value1, ALint64SOFT *value2, ALint64SOFT *value3);
AL_API void AL_APIENTRY alGetSourcei64vSOFT(ALuint source, ALenum param, ALint64SOFT *values);

/* Effect object function types. */
typedef void (AL_APIENTRY *LPALGENEFFECTS)(ALsizei, ALuint*);
typedef void (AL_APIENTRY *LPALDELETEEFFECTS)(ALsizei, const ALuint*);
typedef ALboolean (AL_APIENTRY *LPALISEFFECT)(ALuint);
typedef void (AL_APIENTRY *LPALEFFECTI)(ALuint, ALenum, ALint);
typedef void (AL_APIENTRY *LPALEFFECTIV)(ALuint, ALenum, const ALint*);
typedef void (AL_APIENTRY *LPALEFFECTF)(ALuint, ALenum, ALfloat);
typedef void (AL_APIENTRY *LPALEFFECTFV)(ALuint, ALenum, const ALfloat*);
typedef void (AL_APIENTRY *LPALGETEFFECTI)(ALuint, ALenum, ALint*);
typedef void (AL_APIENTRY *LPALGETEFFECTIV)(ALuint, ALenum, ALint*);
typedef void (AL_APIENTRY *LPALGETEFFECTF)(ALuint, ALenum, ALfloat*);
typedef void (AL_APIENTRY *LPALGETEFFECTFV)(ALuint, ALenum, ALfloat*);

/* Filter object function types. */
typedef void (AL_APIENTRY *LPALGENFILTERS)(ALsizei, ALuint*);
typedef void (AL_APIENTRY *LPALDELETEFILTERS)(ALsizei, const ALuint*);
typedef ALboolean (AL_APIENTRY *LPALISFILTER)(ALuint);
typedef void (AL_APIENTRY *LPALFILTERI)(ALuint, ALenum, ALint);
typedef void (AL_APIENTRY *LPALFILTERIV)(ALuint, ALenum, const ALint*);
typedef void (AL_APIENTRY *LPALFILTERF)(ALuint, ALenum, ALfloat);
typedef void (AL_APIENTRY *LPALFILTERFV)(ALuint, ALenum, const ALfloat*);
typedef void (AL_APIENTRY *LPALGETFILTERI)(ALuint, ALenum, ALint*);
typedef void (AL_APIENTRY *LPALGETFILTERIV)(ALuint, ALenum, ALint*);
typedef void (AL_APIENTRY *LPALGETFILTERF)(ALuint, ALenum, ALfloat*);
typedef void (AL_APIENTRY *LPALGETFILTERFV)(ALuint, ALenum, ALfloat*);

/* Auxiliary Effect Slot object function types. */
typedef void (AL_APIENTRY *LPALGENAUXILIARYEFFECTSLOTS)(ALsizei, ALuint*);
typedef void (AL_APIENTRY *LPALDELETEAUXILIARYEFFECTSLOTS)(ALsizei, const ALuint*);
typedef ALboolean (AL_APIENTRY *LPALISAUXILIARYEFFECTSLOT)(ALuint);
typedef void (AL_APIENTRY *LPALAUXILIARYEFFECTSLOTI)(ALuint, ALenum, ALint);
typedef void (AL_APIENTRY *LPALAUXILIARYEFFECTSLOTIV)(ALuint, ALenum, const ALint*);
typedef void (AL_APIENTRY *LPALAUXILIARYEFFECTSLOTF)(ALuint, ALenum, ALfloat);
typedef void (AL_APIENTRY *LPALAUXILIARYEFFECTSLOTFV)(ALuint, ALenum, const ALfloat*);
typedef void (AL_APIENTRY *LPALGETAUXILIARYEFFECTSLOTI)(ALuint, ALenum, ALint*);
typedef void (AL_APIENTRY *LPALGETAUXILIARYEFFECTSLOTIV)(ALuint, ALenum, ALint*);
typedef void (AL_APIENTRY *LPALGETAUXILIARYEFFECTSLOTF)(ALuint, ALenum, ALfloat*);
typedef void (AL_APIENTRY *LPALGETAUXILIARYEFFECTSLOTFV)(ALuint, ALenum, ALfloat*);

#ifdef AL_ALEXT_PROTOTYPES
AL_API ALvoid AL_APIENTRY alGenEffects(ALsizei n, ALuint *effects);
AL_API ALvoid AL_APIENTRY alDeleteEffects(ALsizei n, const ALuint *effects);
AL_API ALboolean AL_APIENTRY alIsEffect(ALuint effect);
AL_API ALvoid AL_APIENTRY alEffecti(ALuint effect, ALenum param, ALint iValue);
AL_API ALvoid AL_APIENTRY alEffectiv(ALuint effect, ALenum param, const ALint *piValues);
AL_API ALvoid AL_APIENTRY alEffectf(ALuint effect, ALenum param, ALfloat flValue);
AL_API ALvoid AL_APIENTRY alEffectfv(ALuint effect, ALenum param, const ALfloat *pflValues);
AL_API ALvoid AL_APIENTRY alGetEffecti(ALuint effect, ALenum param, ALint *piValue);
AL_API ALvoid AL_APIENTRY alGetEffectiv(ALuint effect, ALenum param, ALint *piValues);
AL_API ALvoid AL_APIENTRY alGetEffectf(ALuint effect, ALenum param, ALfloat *pflValue);
AL_API ALvoid AL_APIENTRY alGetEffectfv(ALuint effect, ALenum param, ALfloat *pflValues);

AL_API ALvoid AL_APIENTRY alGenFilters(ALsizei n, ALuint *filters);
AL_API ALvoid AL_APIENTRY alDeleteFilters(ALsizei n, const ALuint *filters);
AL_API ALboolean AL_APIENTRY alIsFilter(ALuint filter);
AL_API ALvoid AL_APIENTRY alFilteri(ALuint filter, ALenum param, ALint iValue);
AL_API ALvoid AL_APIENTRY alFilteriv(ALuint filter, ALenum param, const ALint *piValues);
AL_API ALvoid AL_APIENTRY alFilterf(ALuint filter, ALenum param, ALfloat flValue);
AL_API ALvoid AL_APIENTRY alFilterfv(ALuint filter, ALenum param, const ALfloat *pflValues);
AL_API ALvoid AL_APIENTRY alGetFilteri(ALuint filter, ALenum param, ALint *piValue);
AL_API ALvoid AL_APIENTRY alGetFilteriv(ALuint filter, ALenum param, ALint *piValues);
AL_API ALvoid AL_APIENTRY alGetFilterf(ALuint filter, ALenum param, ALfloat *pflValue);
AL_API ALvoid AL_APIENTRY alGetFilterfv(ALuint filter, ALenum param, ALfloat *pflValues);

AL_API ALvoid AL_APIENTRY alGenAuxiliaryEffectSlots(ALsizei n, ALuint *effectslots);
AL_API ALvoid AL_APIENTRY alDeleteAuxiliaryEffectSlots(ALsizei n, const ALuint *effectslots);
AL_API ALboolean AL_APIENTRY alIsAuxiliaryEffectSlot(ALuint effectslot);
AL_API ALvoid AL_APIENTRY alAuxiliaryEffectSloti(ALuint effectslot, ALenum param, ALint iValue);
AL_API ALvoid AL_APIENTRY alAuxiliaryEffectSlotiv(ALuint effectslot, ALenum param, const ALint *piValues);
AL_API ALvoid AL_APIENTRY alAuxiliaryEffectSlotf(ALuint effectslot, ALenum param, ALfloat flValue);
AL_API ALvoid AL_APIENTRY alAuxiliaryEffectSlotfv(ALuint effectslot, ALenum param, const ALfloat *pflValues);
AL_API ALvoid AL_APIENTRY alGetAuxiliaryEffectSloti(ALuint effectslot, ALenum param, ALint *piValue);
AL_API ALvoid AL_APIENTRY alGetAuxiliaryEffectSlotiv(ALuint effectslot, ALenum param, ALint *piValues);
AL_API ALvoid AL_APIENTRY alGetAuxiliaryEffectSlotf(ALuint effectslot, ALenum param, ALfloat *pflValue);
AL_API ALvoid AL_APIENTRY alGetAuxiliaryEffectSlotfv(ALuint effectslot, ALenum param, ALfloat *pflValues);

end;