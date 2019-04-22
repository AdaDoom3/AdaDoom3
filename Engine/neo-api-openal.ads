
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

-- Custom binding to the OpenAL Soft API:
--   https://web.archive.org/web/20160409112902/http://www.openal.org/documentation/openal-1.1-specification.pdf
package Neo.API.OpenAL is

  -----------
  -- Types --
  -----------

  -- ALvoid     Ptr
  -- ALint      Int_C
  -- ALsizei    Int_Size_C
  -- ALenum     Int_Unsigned_C
  -- ALboolean  Int_8_Unsigned_C
  -- ALbyte     Int_8_Signed_C
  -- ALubyte    Int_8_Unsigned_C
  -- ALshort    Int_16_Signed_C
  -- ALushort   Int_16_Unsigned_C
  -- ALuint     Int_Unsigned_C
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
  AL_NONE  : constant Int_Unsigned_C := 0; -- 0

  -- AL Boolean
  AL_FALSE : constant Int_8_Unsigned_C := 0; -- 0
  AL_TRUE  : constant Int_8_Unsigned_C := 1; -- 0

  -- 2.7. AL Errors
  AL_NO_ERROR          : constant Int_Unsigned_C := 16#0000_0000#; -- 0000
  AL_INVALID_NAME      : constant Int_Unsigned_C := 16#0000_A001#; -- 0000
  AL_INVALID_ENUM      : constant Int_Unsigned_C := 16#0000_A002#; -- 0000
  AL_INVALID_VALUE     : constant Int_Unsigned_C := 16#0000_A003#; -- 0000
  AL_INVALID_OPERATION : constant Int_Unsigned_C := 16#0000_A004#; -- 0000
  AL_OUT_OF_MEMORY     : constant Int_Unsigned_C := 16#0000_A005#; -- 0000

  -- 3.1.1. Simple Queries
  AL_DOPPLER_FACTOR : constant Int_Unsigned_C := 16#0000_C000#; -- 0000
  AL_SPEED_OF_SOUND : constant Int_Unsigned_C := 16#0000_C003#; -- 0000
  AL_DISTANCE_MODEL : constant Int_Unsigned_C := 16#0000_D000#; -- 0000

  -- 3.1.2. String Queries
  AL_VENDOR     : constant Int_Unsigned_C := 16#0000_B001#; -- 0000
  AL_VERSION    : constant Int_Unsigned_C := 16#0000_B002#; -- 0000
  AL_RENDERER   : constant Int_Unsigned_C := 16#0000_B003#; -- 0000
  AL_EXTENSIONS : constant Int_Unsigned_C := 16#0000_B004#; -- 0000

  -- 3.4. Attenuation By Distance
  AL_INVERSE_DISTANCE          : constant Int_Unsigned_C := 16#0000_D001#; -- 0000
  AL_INVERSE_DISTANCE_CLAMPED  : constant Int_Unsigned_C := 16#0000_D002#; -- 0000
  AL_LINEAR_DISTANCE           : constant Int_Unsigned_C := 16#0000_D003#; -- 0000
  AL_LINEAR_DISTANCE_CLAMPED   : constant Int_Unsigned_C := 16#0000_D004#; -- 0000
  AL_EXPONENT_DISTANCE         : constant Int_Unsigned_C := 16#0000_D005#; -- 0000
  AL_EXPONENT_DISTANCE_CLAMPED : constant Int_Unsigned_C := 16#0000_D006#; -- 0000

  -- 4.1. Basic Listener and Source Attributes
  AL_POSITION : constant Int_Unsigned_C := 16#0000_1004#; -- 0000
  AL_VELOCITY : constant Int_Unsigned_C := 16#0000_1006#; -- 0000
  AL_GAIN     : constant Int_Unsigned_C := 16#0000_100A#; -- 0000

  -- 4.2.1. Listener Attributes
  AL_ORIENTATION : constant Int_Unsigned_C := 16#0000_10#; -- 0000

  -- 4.3.2. Source Attributes
  AL_SOURCE_RELATIVE    : constant Int_Unsigned_C := 16#0000_0202#; -- 000
  AL_SOURCE_TYPE        : constant Int_Unsigned_C := 16#0000_1027#; -- 0000
  AL_UNDETERMINED       : constant Int_Unsigned_C := 16#0000_1030#; -- 0000
  AL_STATIC             : constant Int_Unsigned_C := 16#0000_1028#; -- 0000
  AL_STREAMING          : constant Int_Unsigned_C := 16#0000_1029#; -- 0000
  AL_LOOPING            : constant Int_Unsigned_C := 16#0000_1007#; -- 0000
  AL_BUFFER             : constant Int_Unsigned_C := 16#0000_1009#; -- 0000
  AL_BUFFERS_QUEUED     : constant Int_Unsigned_C := 16#0000_1015#; -- 0000
  AL_BUFFERS_PROCESSED  : constant Int_Unsigned_C := 16#0000_1016#; -- 0000
  AL_MIN_GAIN           : constant Int_Unsigned_C := 16#0000_100D#; -- 0000
  AL_MAX_GAIN           : constant Int_Unsigned_C := 16#0000_100E#; -- 0000
  AL_REFERENCE_DISTANCE : constant Int_Unsigned_C := 16#0000_1020#; -- 0000
  AL_ROLLOFF_FACTOR     : constant Int_Unsigned_C := 16#0000_1021#; -- 0000
  AL_MAX_DISTANCE       : constant Int_Unsigned_C := 16#0000_1023#; -- 0000
  AL_PITCH              : constant Int_Unsigned_C := 16#0000_1003#; -- 0000
  AL_DIRECTION          : constant Int_Unsigned_C := 16#0000_1005#; -- 0000
  AL_CONE_INNER_ANGLE   : constant Int_Unsigned_C := 16#0000_1001#; -- 0000
  AL_CONE_OUTER_ANGLE   : constant Int_Unsigned_C := 16#0000_1002#; -- 0000
  AL_CONE_OUTER_GAIN    : constant Int_Unsigned_C := 16#0000_1022#; -- 0000
  AL_SEC_OFFSET         : constant Int_Unsigned_C := 16#0000_1024#; -- 0000
  AL_SAMPLE_OFFSET      : constant Int_Unsigned_C := 16#0000_1025#; -- 0000
  AL_BYTE_OFFSET        : constant Int_Unsigned_C := 16#0000_1026#; -- 0000

  -- 4.3.6. Managing Source Execution
  AL_SOURCE_STATE : constant Int_Unsigned_C := 16#0000_1010#; -- 0000
  AL_INITIAL      : constant Int_Unsigned_C := 16#0000_1011#; -- 0000
  AL_PLAYING      : constant Int_Unsigned_C := 16#0000_1012#; -- 0000
  AL_PAUSED       : constant Int_Unsigned_C := 16#0000_1013#; -- 0000
  AL_STOPPED      : constant Int_Unsigned_C := 16#0000_1014#; -- 0000

  -- 5.3.1. Buffer Attributes
  AL_FREQUENCY : constant Int_Unsigned_C := 16#0000_2001#; -- 0000
  AL_BITS      : constant Int_Unsigned_C := 16#0000_2002#; -- 0000
  AL_CHANNELS  : constant Int_Unsigned_C := 16#0000_2003#; -- 0000
  AL_SIZE      : constant Int_Unsigned_C := 16#0000_2004#; -- 0000

  -- 5.3.4. Specifying Buffer Content
  AL_FORMAT_MONO8    : constant Int_Unsigned_C := 16#0000_1100#; -- 0000
  AL_FORMAT_MONO16   : constant Int_Unsigned_C := 16#0000_1101#; -- 0000
  AL_FORMAT_STEREO8  : constant Int_Unsigned_C := 16#0000_1102#; -- 0000
  AL_FORMAT_STEREO16 : constant Int_Unsigned_C := 16#0000_1103#; -- 0000

  -- ALC Boolean
  ALC_FALSE : constant Int_8_Unsigned_C := 0; -- 0
  ALC_TRUE  : constant Int_8_Unsigned_C := 1; -- 0

  -- 6.2.1. Context Attributes
  ALC_FREQUENCY      : constant Int_Unsigned_C := 16#0000_1007#; -- 0000
  ALC_REFRESH        : constant Int_Unsigned_C := 16#0000_1008#; -- 0000
  ALC_SYNC           : constant Int_Unsigned_C := 16#0000_1009#; -- 0000
  ALC_MONO_SOURCES   : constant Int_Unsigned_C := 16#0000_1010#; -- 0000
  ALC_STEREO_SOURCES : constant Int_Unsigned_C := 16#0000_1011#; -- 0000

  -- 6.3.6. Query for Error Conditions
  ALC_NO_ERROR        : constant Int_Unsigned_C := 16#0000_0000#; -- 0000
  ALC_INVALID_DEVICE  : constant Int_Unsigned_C := 16#0000_A001#; -- 0000
  ALC_INVALID_CONTEXT : constant Int_Unsigned_C := 16#0000_A002#; -- 0000
  ALC_INVALID_ENUM    : constant Int_Unsigned_C := 16#0000_A003#; -- 0000
  ALC_INVALID_VALUE   : constant Int_Unsigned_C := 16#0000_A004#; -- 0000
  ALC_OUT_OF_MEMORY   : constant Int_Unsigned_C := 16#0000_A005#; -- 0000

  -- 6.3.7. String Query
  ALC_DEFAULT_DEVICE_SPECIFIER : constant Int_Unsigned_C := 16#0000_1004#; -- 0000
  ALC_DEVICE_SPECIFIER         : constant Int_Unsigned_C := 16#0000_1005#; -- 0000
  ALC_EXTENSIONS               : constant Int_Unsigned_C := 16#0000_1006#; -- 0000

  -- 6.3.8. Integer Query
  ALC_ATTRIBUTES_SIZE : constant Int_Unsigned_C := 16#0000_1002#; -- 0000
  ALC_ALL_ATTRIBUTES  : constant Int_Unsigned_C := 16#0000_1003#; -- 0000
  ALC_MAJOR_VERSION   : constant Int_Unsigned_C := 16#0000_1000#; -- 0000
  ALC_MINOR_VERSION   : constant Int_Unsigned_C := 16#0000_1001#; -- 0000

  -- 6.4.2. Capture
  ALC_CAPTURE_DEVICE_SPECIFIER         : constant Int_Unsigned_C := 16#0000_310#; -- 0000
  ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER : constant Int_Unsigned_C := 16#0000_311#; -- 0000
  ALC_CAPTURE_SAMPLES                  : constant Int_Unsigned_C := 16#0000_312#; -- 0000

  --
  AL_LOKI_IMA_ADPCM_format     : constant Int_Unsigned_C := 1
  AL_FORMAT_IMA_ADPCM_MONO16_EXT      : constant Int_Unsigned_C :=      10000
  AL_FORMAT_IMA_ADPCM_STEREO16_EXT    : constant Int_Unsigned_C :=      10001

  --
  AL_LOKI_WAVE_format: constant Int_Unsigned_C :=  1
  AL_FORMAT_WAVE_EXT                 : constant Int_Unsigned_C :=       10002

  --
  AL_EXT_vorbis 1
  AL_FORMAT_VORBIS_EXT                 : constant Int_Unsigned_C :=     10003

  --
  AL_LOKI_quadriphonic 1
  AL_FORMAT_QUAD8_LOKI                  : constant Int_Unsigned_C :=    10004
  AL_FORMAT_QUAD16_LOKI             : constant Int_Unsigned_C :=        10005

  --
  AL_EXT_float32 1
  AL_FORMAT_MONO_FLOAT32          : constant Int_Unsigned_C :=          10010
  AL_FORMAT_STEREO_FLOAT32        : constant Int_Unsigned_C :=          10011

  --
  AL_EXT_double 1
  AL_FORMAT_MONO_DOUBLE_EXT       : constant Int_Unsigned_C :=          10012
  AL_FORMAT_STEREO_DOUBLE_EXT     : constant Int_Unsigned_C :=          10013

  --
  AL_EXT_MULAW 1
  AL_FORMAT_MONO_MULAW_EXT   : constant Int_Unsigned_C :=               10014
  AL_FORMAT_STEREO_MULAW_EXT : constant Int_Unsigned_C :=               10015

  --
  AL_EXT_ALAW : constant Int_Unsigned_C :=  1
  AL_FORMAT_MONO_ALAW_EXT    : constant Int_Unsigned_C :=               10016
  AL_FORMAT_STEREO_ALAW_EXT : constant Int_Unsigned_C :=                10017

  --
  ALC_LOKI_audio_channel: constant Int_Unsigned_C :=  1
  ALC_CHAN_MAIN_LOKI     : constant Int_Unsigned_C :=                   0x500001
  ALC_CHAN_PCM_LOKI      : constant Int_Unsigned_C :=                   0x500002
  ALC_CHAN_CD_LOKI       : constant Int_Unsigned_C :=                   0x500003

  --
  AL_EXT_MCFORMATS : constant Int_Unsigned_C := 1
  AL_FORMAT_QUAD8   : constant Int_Unsigned_C :=                        1204
  AL_FORMAT_QUAD16   : constant Int_Unsigned_C :=                       1205
  AL_FORMAT_QUAD32  : constant Int_Unsigned_C :=                        1206
  AL_FORMAT_REAR8    : constant Int_Unsigned_C :=                       1207
  AL_FORMAT_REAR16   : constant Int_Unsigned_C :=                       1208
  AL_FORMAT_REAR32    : constant Int_Unsigned_C :=                      1209
  AL_FORMAT_51CHN8    : constant Int_Unsigned_C :=                      120A
  AL_FORMAT_51CHN16   : constant Int_Unsigned_C :=                      120B
  AL_FORMAT_51CHN32   : constant Int_Unsigned_C :=                      120C
  AL_FORMAT_61CHN8    : constant Int_Unsigned_C :=                      120D
  AL_FORMAT_61CHN16   : constant Int_Unsigned_C :=                      120E
  AL_FORMAT_61CHN32   : constant Int_Unsigned_C :=                      120
  AL_FORMAT_71CHN8    : constant Int_Unsigned_C :=                      1210
  AL_FORMAT_71CHN16    : constant Int_Unsigned_C :=                     1211
  AL_FORMAT_71CHN32    : constant Int_Unsigned_C :=                     1212

  --
  AL_EXT_MULAW_MCFORMATS : constant Int_Unsigned_C := 1
  AL_FORMAT_MONO_MULAW    : constant Int_Unsigned_C :=                  10014
  AL_FORMAT_STEREO_MULAW  : constant Int_Unsigned_C :=                  10015
  AL_FORMAT_QUAD_MULAW    : constant Int_Unsigned_C :=                  10021
  AL_FORMAT_REAR_MULAW    : constant Int_Unsigned_C :=                  10022
  AL_FORMAT_51CHN_MULAW   : constant Int_Unsigned_C :=                  10023
  AL_FORMAT_61CHN_MULAW   : constant Int_Unsigned_C :=                  10024
  AL_FORMAT_71CHN_MULAW    : constant Int_Unsigned_C :=                 10025

  --
  AL_EXT_IMA4  : constant Int_Unsigned_C := 1
  AL_FORMAT_MONO_IMA4   : constant Int_Unsigned_C :=                    1300
  AL_FORMAT_STEREO_IMA4 : constant Int_Unsigned_C :=                    1301

  --
  AL_EXT_STATIC_BUFFER : constant Int_Unsigned_C :=  1

  --
  AL_EXT_source_distance_model  : constant Int_Unsigned_C := 1
  AL_SOURCE_DISTANCE_MODEL     : constant Int_Unsigned_C :=              0x200

  --
  AL_SOFT_buffer_sub_data  : constant Int_Unsigned_C := 1
  AL_BYTE_RW_OFFSETS_SOFT  : constant Int_Unsigned_C :=                 1031
  AL_SAMPLE_RW_OFFSETS_SOFT : constant Int_Unsigned_C :=                 1032

  --
  ALC_EXT_EFX : constant Int_Unsigned_C :=  1

  --
  ALC_EXT_disconnect : constant Int_Unsigned_C :=  1
  ALC_CONNECTED   : constant Int_Unsigned_C :=                          0x313

  --
  ALC_EXT_thread_local_context: constant Int_Unsigned_C :=  1

  --
  ALC_EFX_MAJOR_VERSION    : constant Int_Unsigned_C :=                 0x20001
  ALC_EFX_MINOR_VERSION    : constant Int_Unsigned_C :=                 0x20002
  ALC_MAX_AUXILIARY_SENDS  : constant Int_Unsigned_C :=                 0x20003

  --
  AL_SOFT_direct_channels  : constant Int_Unsigned_C :=  1
  AL_DIRECT_CHANNELS_SOFT : constant Int_Unsigned_C :=                  1033
#endif

#ifndef ALC_SOFT_loopback
  ALC_SOFT_loopback : constant Int_Unsigned_C :=  1
  ALC_FORMAT_CHANNELS_SOFT : constant Int_Unsigned_C :=                 1990
  ALC_FORMAT_TYPE_SOFT     : constant Int_Unsigned_C :=                 1991

  -- Sample types
  ALC_BYTE_SOFT           : constant Int_Unsigned_C :=                  1400
  ALC_UNSIGNED_BYTE_SOFT : constant Int_Unsigned_C :=                   1401
  ALC_SHORT_SOFT         : constant Int_Unsigned_C :=                   1402
  ALC_UNSIGNED_SHORT_SOFT: constant Int_Unsigned_C :=                   1403
  ALC_INT_SOFT           : constant Int_Unsigned_C :=                   1404
  ALC_UNSIGNED_INT_SOFT  : constant Int_Unsigned_C :=                   1405
  ALC_FLOAT_SOFT         : constant Int_Unsigned_C :=                   1406

  -- Channel configurations
  ALC_MONO_SOFT         : constant Int_Unsigned_C :=                    1500
  ALC_STEREO_SOFT       : constant Int_Unsigned_C :=                    1501
  ALC_QUAD_SOFT        : constant Int_Unsigned_C :=                     1503
  ALC_5POINT1_SOFT     : constant Int_Unsigned_C :=                     1504
  ALC_6POINT1_SOFT    : constant Int_Unsigned_C :=                      1505
  ALC_7POINT1_SOFT     : constant Int_Unsigned_C :=                     1506


#ifndef AL_EXT_STEREO_ANGLES
  AL_EXT_STEREO_ANGLES : constant Int_Unsigned_C := 1
  AL_STEREO_ANGLES     : constant Int_Unsigned_C :=                     1030
#endif

#ifndef AL_EXT_SOURCE_RADIUS
  AL_EXT_SOURCE_RADIUS : constant Int_Unsigned_C := 1
  AL_SOURCE_RADIUS    : constant Int_Unsigned_C :=                      1031
#endif

#ifndef AL_SOFT_source_latency
  AL_SOFT_source_latency 1
  AL_SAMPLE_OFFSET_LATENCY_SOFT : constant Int_Unsigned_C :=            1200
  AL_SEC_OFFSET_LATENCY_SOFT    : constant Int_Unsigned_C :=            1201
typedef int64_t ALint64SOFT;
typedef uint64_t ALuint64SOFT;

  -- Listener properties
  AL_METERS_PER_UNIT                       0x20004

  -- Source properties
  AL_DIRECT_FILTER                         0x20005
  AL_AUXILIARY_SEND_FILTER                 0x20006
  AL_AIR_ABSORPTION_FACTOR                 0x20007
  AL_ROOM_ROLLOFF_FACTOR                   0x20008
  AL_CONE_OUTER_GAINHF                     0x20009
  AL_DIRECT_FILTER_GAINHF_AUTO             0x2000A
  AL_AUXILIARY_SEND_FILTER_GAIN_AUTO       0x2000B
  AL_AUXILIARY_SEND_FILTER_GAINHF_AUTO     0x2000C


  -- Effect properties

  -- Reverb effect parameters
  AL_REVERB_DENSITY                        0001
  AL_REVERB_DIFFUSION                      0002
  AL_REVERB_GAIN                           0003
  AL_REVERB_GAINHF                         0004
  AL_REVERB_DECAY_TIME                     0005
  AL_REVERB_DECAY_HFRATIO                  0006
  AL_REVERB_REFLECTIONS_GAIN               0007
  AL_REVERB_REFLECTIONS_DELAY              0008
  AL_REVERB_LATE_REVERB_GAIN               0009
  AL_REVERB_LATE_REVERB_DELAY              000A
  AL_REVERB_AIR_ABSORPTION_GAINHF          000B
  AL_REVERB_ROOM_ROLLOFF_FACTOR            000C
  AL_REVERB_DECAY_HFLIMIT                  000D

  -- EAX Reverb effect parameters
  AL_EAXREVERB_DENSITY                     0001
  AL_EAXREVERB_DIFFUSION                   0002
  AL_EAXREVERB_GAIN                        0003
  AL_EAXREVERB_GAINHF                      0004
  AL_EAXREVERB_GAINLF                      0005
  AL_EAXREVERB_DECAY_TIME                  0006
  AL_EAXREVERB_DECAY_HFRATIO               0007
  AL_EAXREVERB_DECAY_LFRATIO               0008
  AL_EAXREVERB_REFLECTIONS_GAIN            0009
  AL_EAXREVERB_REFLECTIONS_DELAY           000A
  AL_EAXREVERB_REFLECTIONS_PAN             000B
  AL_EAXREVERB_LATE_REVERB_GAIN            000C
  AL_EAXREVERB_LATE_REVERB_DELAY           000D
  AL_EAXREVERB_LATE_REVERB_PAN             000E
  AL_EAXREVERB_ECHO_TIME                   00
  AL_EAXREVERB_ECHO_DEPTH                  0010
  AL_EAXREVERB_MODULATION_TIME             0011
  AL_EAXREVERB_MODULATION_DEPTH            0012
  AL_EAXREVERB_AIR_ABSORPTION_GAINHF       0013
  AL_EAXREVERB_HFREFERENCE                 0014
  AL_EAXREVERB_LFREFERENCE                 0015
  AL_EAXREVERB_ROOM_ROLLOFF_FACTOR         0016
  AL_EAXREVERB_DECAY_HFLIMIT               0017

  -- Chorus effect parameters
  AL_CHORUS_WAVEFORM                       0001
  AL_CHORUS_PHASE                          0002
  AL_CHORUS_RATE                           0003
  AL_CHORUS_DEPTH                          0004
  AL_CHORUS_FEEDBACK                       0005
  AL_CHORUS_DELAY                          0006

  -- Distortion effect parameters
  AL_DISTORTION_EDGE                       0001
  AL_DISTORTION_GAIN                       0002
  AL_DISTORTION_LOWPASS_CUTOFF             0003
  AL_DISTORTION_EQCENTER                   0004
  AL_DISTORTION_EQBANDWIDTH                0005

  -- Echo effect parameters
  AL_ECHO_DELAY                            0001
  AL_ECHO_LRDELAY                          0002
  AL_ECHO_DAMPING                          0003
  AL_ECHO_FEEDBACK                         0004
  AL_ECHO_SPREAD                           0005

  -- Flanger effect parameters
  AL_FLANGER_WAVEFORM                      0001
  AL_FLANGER_PHASE                         0002
  AL_FLANGER_RATE                          0003
  AL_FLANGER_DEPTH                         0004
  AL_FLANGER_FEEDBACK                      0005
  AL_FLANGER_DELAY                         0006

  -- Frequency shifter effect parameters
  AL_FREQUENCY_SHIFTER_FREQUENCY           0001
  AL_FREQUENCY_SHIFTER_LEFT_DIRECTION      0002
  AL_FREQUENCY_SHIFTER_RIGHT_DIRECTION     0003

  -- Vocal morpher effect parameters
  AL_VOCAL_MORPHER_PHONEMEA               : constant Int_Unsigned_C := 0001
  AL_VOCAL_MORPHER_PHONEMEA_COARSE_TUNING : constant Int_Unsigned_C := 0002
  AL_VOCAL_MORPHER_PHONEMEB              : constant Int_Unsigned_C :=  0003
  AL_VOCAL_MORPHER_PHONEMEB_COARSE_TUNING : constant Int_Unsigned_C := 0004
  AL_VOCAL_MORPHER_WAVEFORM               : constant Int_Unsigned_C := 0005
  AL_VOCAL_MORPHER_RATE                   : constant Int_Unsigned_C := 0006

  -- Pitchshifter effect parameters
  AL_PITCH_SHIFTER_COARSE_TUNE          : constant Int_Unsigned_C :=   0001
  AL_PITCH_SHIFTER_FINE_TUNE           : constant Int_Unsigned_C :=    0002

  -- Ringmodulator effect parameters
  AL_RING_MODULATOR_FREQUENCY          : constant Int_Unsigned_C :=    0001
  AL_RING_MODULATOR_HIGHPASS_CUTOFF    : constant Int_Unsigned_C :=    0002
  AL_RING_MODULATOR_WAVEFORM           : constant Int_Unsigned_C :=    0003

  -- Autowah effect parameters
  AL_AUTOWAH_ATTACK_TIME              : constant Int_Unsigned_C :=     0001
  AL_AUTOWAH_RELEASE_TIME            : constant Int_Unsigned_C :=      0002
  AL_AUTOWAH_RESONANCE               : constant Int_Unsigned_C :=      0003
  AL_AUTOWAH_PEAK_GAIN              : constant Int_Unsigned_C :=       0004

  -- Compressor effect parameters
  AL_COMPRESSOR_ONOFF               : constant Int_Unsigned_C :=       0001

  -- Equalizer effect parameters
  AL_EQUALIZER_LOW_GAIN            : constant Int_Unsigned_C :=        0001
  AL_EQUALIZER_LOW_CUTOFF          : constant Int_Unsigned_C :=        0002
  AL_EQUALIZER_MID1_GAIN           : constant Int_Unsigned_C :=        0003
  AL_EQUALIZER_MID1_CENTER         : constant Int_Unsigned_C :=        0004
  AL_EQUALIZER_MID1_WIDTH          : constant Int_Unsigned_C :=        0005
  AL_EQUALIZER_MID2_GAIN           : constant Int_Unsigned_C :=        0006
  AL_EQUALIZER_MID2_CENTER         : constant Int_Unsigned_C :=        0007
  AL_EQUALIZER_MID2_WIDTH          : constant Int_Unsigned_C :=        0008
  AL_EQUALIZER_HIGH_GAIN           : constant Int_Unsigned_C :=        0009
  AL_EQUALIZER_HIGH_CUTOFF         : constant Int_Unsigned_C :=        000A

  -- Effect type
  AL_EFFECT_FIRST_PARAMETER        : constant Int_Unsigned_C :=        0000
  AL_EFFECT_LAST_PARAMETER         : constant Int_Unsigned_C :=        0x8000
  AL_EFFECT_TYPE                   : constant Int_Unsigned_C :=        0x8001

  -- Effect types, used with the AL_EFFECT_TYPE property
  AL_EFFECT_NULL                     : constant Int_Unsigned_C :=      0000
  AL_EFFECT_REVERB                   : constant Int_Unsigned_C :=      0001
  AL_EFFECT_CHORUS                   : constant Int_Unsigned_C :=      0002
  AL_EFFECT_DISTORTION               : constant Int_Unsigned_C :=      0003
  AL_EFFECT_ECHO                     : constant Int_Unsigned_C :=      0004
  AL_EFFECT_FLANGER                  : constant Int_Unsigned_C :=      0005
  AL_EFFECT_FREQUENCY_SHIFTER        : constant Int_Unsigned_C :=      0006
  AL_EFFECT_VOCAL_MORPHER            : constant Int_Unsigned_C :=      0007
  AL_EFFECT_PITCH_SHIFTER            : constant Int_Unsigned_C :=      0008
  AL_EFFECT_RING_MODULATOR           : constant Int_Unsigned_C :=      0009
  AL_EFFECT_AUTOWAH                  : constant Int_Unsigned_C :=      000A
  AL_EFFECT_COMPRESSOR               : constant Int_Unsigned_C :=      000B
  AL_EFFECT_EQUALIZER                 : constant Int_Unsigned_C :=     000C
  AL_EFFECT_EAXREVERB                  : constant Int_Unsigned_C :=    0x8000

  -- Auxiliary Effect Slot properties
  AL_EFFECTSLOT_EFFECT                   : constant Int_Unsigned_C :=  0001
  AL_EFFECTSLOT_GAIN                     : constant Int_Unsigned_C :=  0002
  AL_EFFECTSLOT_AUXILIARY_SEND_AUTO      : constant Int_Unsigned_C :=  0003

  -- NULL Auxiliary Slot ID to disable a source send
  AL_EFFECTSLOT_NULL                     : constant Int_Unsigned_C :=  0000


  -- Filter properties

  -- Lowpass filter parameters
  AL_LOWPASS_GAIN                        : constant Int_Unsigned_C :=  0001
  AL_LOWPASS_GAINHF                      : constant Int_Unsigned_C :=  0002

  -- Highpass filter parameters
  AL_HIGHPASS_GAIN                      : constant Int_Unsigned_C :=   0001
  AL_HIGHPASS_GAINLF                    : constant Int_Unsigned_C :=   0002

  -- Bandpass filter parameters
  AL_BANDPASS_GAIN                      : constant Int_Unsigned_C :=   0001
  AL_BANDPASS_GAINLF                    : constant Int_Unsigned_C :=   0002
  AL_BANDPASS_GAINHF                    : constant Int_Unsigned_C :=   0003

  -- Filter type
  AL_FILTER_FIRST_PARAMETER             : constant Int_Unsigned_C :=   0000
  AL_FILTER_LAST_PARAMETER              : constant Int_Unsigned_C :=   0x8000
  AL_FILTER_TYPE                        : constant Int_Unsigned_C :=   0x8001

  -- Filter types, used with the AL_FILTER_TYPE property
  AL_FILTER_NULL                       : constant Int_Unsigned_C :=    0000
  AL_FILTER_LOWPASS                    : constant Int_Unsigned_C :=    0001
  AL_FILTER_HIGHPASS                   : constant Int_Unsigned_C :=    0002
  AL_FILTER_BANDPASS                   : constant Int_Unsigned_C :=    0003

  -- Filter ranges and defaults

  -- Lowpass filter
  AL_LOWPASS_MIN_GAIN                      (0.0)
  AL_LOWPASS_MAX_GAIN                      (1.0)
  AL_LOWPASS_DEFAULT_GAIN                  (1.0)

  AL_LOWPASS_MIN_GAINHF                    (0.0)
  AL_LOWPASS_MAX_GAINHF                    (1.0)
  AL_LOWPASS_DEFAULT_GAINHF                (1.0)

  -- Highpass filter
  AL_HIGHPASS_MIN_GAIN                     (0.0)
  AL_HIGHPASS_MAX_GAIN                     (1.0)
  AL_HIGHPASS_DEFAULT_GAIN                 (1.0)

  AL_HIGHPASS_MIN_GAINLF                   (0.0)
  AL_HIGHPASS_MAX_GAINLF                   (1.0)
  AL_HIGHPASS_DEFAULT_GAINLF               (1.0)

  -- Bandpass filter
  AL_BANDPASS_MIN_GAIN                     (0.0)
  AL_BANDPASS_MAX_GAIN                     (1.0)
  AL_BANDPASS_DEFAULT_GAIN                 (1.0)

  AL_BANDPASS_MIN_GAINHF                   (0.0)
  AL_BANDPASS_MAX_GAINHF                   (1.0)
  AL_BANDPASS_DEFAULT_GAINHF               (1.0)

  AL_BANDPASS_MIN_GAINLF                   (0.0)
  AL_BANDPASS_MAX_GAINLF                   (1.0)
  AL_BANDPASS_DEFAULT_GAINLF               (1.0)


  -- Effect parameter ranges and defaults

  -- Standard reverb effect
  AL_REVERB_MIN_DENSITY                    (0.0)
  AL_REVERB_MAX_DENSITY                    (1.0)
  AL_REVERB_DEFAULT_DENSITY                (1.0)

  AL_REVERB_MIN_DIFFUSION                  (0.0)
  AL_REVERB_MAX_DIFFUSION                  (1.0)
  AL_REVERB_DEFAULT_DIFFUSION              (1.0)

  AL_REVERB_MIN_GAIN                       (0.0)
  AL_REVERB_MAX_GAIN                       (1.0)
  AL_REVERB_DEFAULT_GAIN                   (0.32f)

  AL_REVERB_MIN_GAINHF                     (0.0)
  AL_REVERB_MAX_GAINHF                     (1.0)
  AL_REVERB_DEFAULT_GAINHF                 (0.89f)

  AL_REVERB_MIN_DECAY_TIME                 (0.1f)
  AL_REVERB_MAX_DECAY_TIME                 (20.0)
  AL_REVERB_DEFAULT_DECAY_TIME             (1.49f)

  AL_REVERB_MIN_DECAY_HFRATIO              (0.1f)
  AL_REVERB_MAX_DECAY_HFRATIO              (2.0)
  AL_REVERB_DEFAULT_DECAY_HFRATIO          (0.83f)

  AL_REVERB_MIN_REFLECTIONS_GAIN           (0.0)
  AL_REVERB_MAX_REFLECTIONS_GAIN           (3.16f)
  AL_REVERB_DEFAULT_REFLECTIONS_GAIN       (0.05f)

  AL_REVERB_MIN_REFLECTIONS_DELAY          (0.0)
  AL_REVERB_MAX_REFLECTIONS_DELAY          (0.3f)
  AL_REVERB_DEFAULT_REFLECTIONS_DELAY      (0.007f)

  AL_REVERB_MIN_LATE_REVERB_GAIN           (0.0)
  AL_REVERB_MAX_LATE_REVERB_GAIN           (10.0)
  AL_REVERB_DEFAULT_LATE_REVERB_GAIN       (1.26f)

  AL_REVERB_MIN_LATE_REVERB_DELAY          (0.0)
  AL_REVERB_MAX_LATE_REVERB_DELAY          (0.1f)
  AL_REVERB_DEFAULT_LATE_REVERB_DELAY      (0.011f)

  AL_REVERB_MIN_AIR_ABSORPTION_GAINHF      (0.892f)
  AL_REVERB_MAX_AIR_ABSORPTION_GAINHF      (1.0)
  AL_REVERB_DEFAULT_AIR_ABSORPTION_GAINHF  (0.994f)

  AL_REVERB_MIN_ROOM_ROLLOFF_FACTOR        (0.0)
  AL_REVERB_MAX_ROOM_ROLLOFF_FACTOR        (10.0)
  AL_REVERB_DEFAULT_ROOM_ROLLOFF_FACTOR    (0.0)

  AL_REVERB_MIN_DECAY_HFLIMIT              AL_FALSE
  AL_REVERB_MAX_DECAY_HFLIMIT              AL_TRUE
  AL_REVERB_DEFAULT_DECAY_HFLIMIT          AL_TRUE

  -- EAX reverb effect
  AL_EAXREVERB_MIN_DENSITY                 (0.0)
  AL_EAXREVERB_MAX_DENSITY                 (1.0)
  AL_EAXREVERB_DEFAULT_DENSITY             (1.0)

  AL_EAXREVERB_MIN_DIFFUSION               (0.0)
  AL_EAXREVERB_MAX_DIFFUSION               (1.0)
  AL_EAXREVERB_DEFAULT_DIFFUSION           (1.0)

  AL_EAXREVERB_MIN_GAIN                    (0.0)
  AL_EAXREVERB_MAX_GAIN                    (1.0)
  AL_EAXREVERB_DEFAULT_GAIN                (0.32f)

  AL_EAXREVERB_MIN_GAINHF                  (0.0)
  AL_EAXREVERB_MAX_GAINHF                  (1.0)
  AL_EAXREVERB_DEFAULT_GAINHF              (0.89f)

  AL_EAXREVERB_MIN_GAINLF                  (0.0)
  AL_EAXREVERB_MAX_GAINLF                  (1.0)
  AL_EAXREVERB_DEFAULT_GAINLF              (1.0)

  AL_EAXREVERB_MIN_DECAY_TIME              (0.1f)
  AL_EAXREVERB_MAX_DECAY_TIME              (20.0)
  AL_EAXREVERB_DEFAULT_DECAY_TIME          (1.49f)

  AL_EAXREVERB_MIN_DECAY_HFRATIO           (0.1f)
  AL_EAXREVERB_MAX_DECAY_HFRATIO           (2.0)
  AL_EAXREVERB_DEFAULT_DECAY_HFRATIO       (0.83f)

  AL_EAXREVERB_MIN_DECAY_LFRATIO           (0.1f)
  AL_EAXREVERB_MAX_DECAY_LFRATIO           (2.0)
  AL_EAXREVERB_DEFAULT_DECAY_LFRATIO       (1.0)

  AL_EAXREVERB_MIN_REFLECTIONS_GAIN        (0.0)
  AL_EAXREVERB_MAX_REFLECTIONS_GAIN        (3.16f)
  AL_EAXREVERB_DEFAULT_REFLECTIONS_GAIN    (0.05f)

  AL_EAXREVERB_MIN_REFLECTIONS_DELAY       (0.0)
  AL_EAXREVERB_MAX_REFLECTIONS_DELAY       (0.3f)
  AL_EAXREVERB_DEFAULT_REFLECTIONS_DELAY   (0.007f)

  AL_EAXREVERB_DEFAULT_REFLECTIONS_PAN_XYZ (0.0)

  AL_EAXREVERB_MIN_LATE_REVERB_GAIN        (0.0)
  AL_EAXREVERB_MAX_LATE_REVERB_GAIN        (10.0)
  AL_EAXREVERB_DEFAULT_LATE_REVERB_GAIN    (1.26f)

  AL_EAXREVERB_MIN_LATE_REVERB_DELAY       (0.0)
  AL_EAXREVERB_MAX_LATE_REVERB_DELAY       (0.1f)
  AL_EAXREVERB_DEFAULT_LATE_REVERB_DELAY   (0.011f)

  AL_EAXREVERB_DEFAULT_LATE_REVERB_PAN_XYZ (0.0)

  AL_EAXREVERB_MIN_ECHO_TIME               (0.075f)
  AL_EAXREVERB_MAX_ECHO_TIME               (0.25f)
  AL_EAXREVERB_DEFAULT_ECHO_TIME           (0.25f)

  AL_EAXREVERB_MIN_ECHO_DEPTH              (0.0)
  AL_EAXREVERB_MAX_ECHO_DEPTH              (1.0)
  AL_EAXREVERB_DEFAULT_ECHO_DEPTH          (0.0)

  AL_EAXREVERB_MIN_MODULATION_TIME         (0.04f)
  AL_EAXREVERB_MAX_MODULATION_TIME         (4.0)
  AL_EAXREVERB_DEFAULT_MODULATION_TIME     (0.25f)

  AL_EAXREVERB_MIN_MODULATION_DEPTH        (0.0)
  AL_EAXREVERB_MAX_MODULATION_DEPTH        (1.0)
  AL_EAXREVERB_DEFAULT_MODULATION_DEPTH    (0.0)

  AL_EAXREVERB_MIN_AIR_ABSORPTION_GAINHF   (0.892f)
  AL_EAXREVERB_MAX_AIR_ABSORPTION_GAINHF   (1.0)
  AL_EAXREVERB_DEFAULT_AIR_ABSORPTION_GAINHF (0.994f)

  AL_EAXREVERB_MIN_HFREFERENCE             (1000.0)
  AL_EAXREVERB_MAX_HFREFERENCE             (20000.0)
  AL_EAXREVERB_DEFAULT_HFREFERENCE         (5000.0)

  AL_EAXREVERB_MIN_LFREFERENCE             (20.0)
  AL_EAXREVERB_MAX_LFREFERENCE             (1000.0)
  AL_EAXREVERB_DEFAULT_LFREFERENCE         (250.0)

  AL_EAXREVERB_MIN_ROOM_ROLLOFF_FACTOR     (0.0)
  AL_EAXREVERB_MAX_ROOM_ROLLOFF_FACTOR     (10.0)
  AL_EAXREVERB_DEFAULT_ROOM_ROLLOFF_FACTOR (0.0)

  AL_EAXREVERB_MIN_DECAY_HFLIMIT         : constant Int_Unsigned_C :=  AL_FALSE
  AL_EAXREVERB_MAX_DECAY_HFLIMIT          : constant Int_Unsigned_C := AL_TRUE
  AL_EAXREVERB_DEFAULT_DECAY_HFLIMIT     : constant Int_Unsigned_C :=  AL_TRUE

  -- Chorus effect
  AL_CHORUS_WAVEFORM_SINUSOID           : constant Int_Unsigned_C :=   (0)
  AL_CHORUS_WAVEFORM_TRIANGLE           : constant Int_Unsigned_C :=   (1)

  AL_CHORUS_MIN_WAVEFORM                : constant Int_Unsigned_C :=   (0)
  AL_CHORUS_MAX_WAVEFORM                : constant Int_Unsigned_C :=   (1)
  AL_CHORUS_DEFAULT_WAVEFORM            : constant Int_Unsigned_C :=   (1)

  AL_CHORUS_MIN_PHASE                    : constant Int_Unsigned_C :=  (-180)
  AL_CHORUS_MAX_PHASE                   : constant Int_Unsigned_C :=   (180)
  AL_CHORUS_DEFAULT_PHASE                : constant Int_Unsigned_C :=  (90)

  AL_CHORUS_MIN_RATE                       (0.0)
  AL_CHORUS_MAX_RATE                       (10.0)
  AL_CHORUS_DEFAULT_RATE                   (1.1f)

  AL_CHORUS_MIN_DEPTH                      (0.0)
  AL_CHORUS_MAX_DEPTH                      (1.0)
  AL_CHORUS_DEFAULT_DEPTH                  (0.1f)

  AL_CHORUS_MIN_FEEDBACK                   (-1.0)
  AL_CHORUS_MAX_FEEDBACK                   (1.0)
  AL_CHORUS_DEFAULT_FEEDBACK               (0.25f)

  AL_CHORUS_MIN_DELAY                      (0.0)
  AL_CHORUS_MAX_DELAY                      (0.016f)
  AL_CHORUS_DEFAULT_DELAY                  (0.016f)

  -- Distortion effect
  AL_DISTORTION_MIN_EDGE                   (0.0)
  AL_DISTORTION_MAX_EDGE                   (1.0)
  AL_DISTORTION_DEFAULT_EDGE               (0.2f)

  AL_DISTORTION_MIN_GAIN                   (0.01f)
  AL_DISTORTION_MAX_GAIN                   (1.0)
  AL_DISTORTION_DEFAULT_GAIN               (0.05f)

  AL_DISTORTION_MIN_LOWPASS_CUTOFF         (80.0)
  AL_DISTORTION_MAX_LOWPASS_CUTOFF         (24000.0)
  AL_DISTORTION_DEFAULT_LOWPASS_CUTOFF     (8000.0)

  AL_DISTORTION_MIN_EQCENTER               (80.0)
  AL_DISTORTION_MAX_EQCENTER               (24000.0)
  AL_DISTORTION_DEFAULT_EQCENTER           (3600.0)

  AL_DISTORTION_MIN_EQBANDWIDTH            (80.0)
  AL_DISTORTION_MAX_EQBANDWIDTH            (24000.0)
  AL_DISTORTION_DEFAULT_EQBANDWIDTH        (3600.0)

  -- Echo effect
  AL_ECHO_MIN_DELAY                        (0.0)
  AL_ECHO_MAX_DELAY                        (0.207f)
  AL_ECHO_DEFAULT_DELAY                    (0.1f)

  AL_ECHO_MIN_LRDELAY                      (0.0)
  AL_ECHO_MAX_LRDELAY                      (0.404f)
  AL_ECHO_DEFAULT_LRDELAY                  (0.1f)

  AL_ECHO_MIN_DAMPING                      (0.0)
  AL_ECHO_MAX_DAMPING                      (0.99f)
  AL_ECHO_DEFAULT_DAMPING                  (0.5f)

  AL_ECHO_MIN_FEEDBACK                     (0.0)
  AL_ECHO_MAX_FEEDBACK                     (1.0)
  AL_ECHO_DEFAULT_FEEDBACK                 (0.5f)

  AL_ECHO_MIN_SPREAD                       (-1.0)
  AL_ECHO_MAX_SPREAD                       (1.0)
  AL_ECHO_DEFAULT_SPREAD                   (-1.0)

  -- Flanger effect
  AL_FLANGER_WAVEFORM_SINUSOID             (0)
  AL_FLANGER_WAVEFORM_TRIANGLE             (1)

  AL_FLANGER_MIN_WAVEFORM                  (0)
  AL_FLANGER_MAX_WAVEFORM                  (1)
  AL_FLANGER_DEFAULT_WAVEFORM              (1)

  AL_FLANGER_MIN_PHASE                     (-180)
  AL_FLANGER_MAX_PHASE                     (180)
  AL_FLANGER_DEFAULT_PHASE                 (0)

  AL_FLANGER_MIN_RATE                      (0.0)
  AL_FLANGER_MAX_RATE                      (10.0)
  AL_FLANGER_DEFAULT_RATE                  (0.27f)

  AL_FLANGER_MIN_DEPTH                     (0.0)
  AL_FLANGER_MAX_DEPTH                     (1.0)
  AL_FLANGER_DEFAULT_DEPTH                 (1.0)

  AL_FLANGER_MIN_FEEDBACK                  (-1.0)
  AL_FLANGER_MAX_FEEDBACK                  (1.0)
  AL_FLANGER_DEFAULT_FEEDBACK              (-0.5f)

  AL_FLANGER_MIN_DELAY                     (0.0)
  AL_FLANGER_MAX_DELAY                     (0.004f)
  AL_FLANGER_DEFAULT_DELAY                 (0.002f)

  -- Frequency shifter effect
  AL_FREQUENCY_SHIFTER_MIN_FREQUENCY       (0.0)
  AL_FREQUENCY_SHIFTER_MAX_FREQUENCY       (24000.0)
  AL_FREQUENCY_SHIFTER_DEFAULT_FREQUENCY   (0.0)

  AL_FREQUENCY_SHIFTER_MIN_LEFT_DIRECTION : constant Int_Unsigned_C := (0)
  AL_FREQUENCY_SHIFTER_MAX_LEFT_DIRECTION  : constant Int_Unsigned_C :=(2)
  AL_FREQUENCY_SHIFTER_DEFAULT_LEFT_DIRECTION : constant Int_Unsigned_C := (0)

  AL_FREQUENCY_SHIFTER_DIRECTION_DOWN    : constant Int_Unsigned_C :=  (0)
  AL_FREQUENCY_SHIFTER_DIRECTION_UP      : constant Int_Unsigned_C :=  (1)
  AL_FREQUENCY_SHIFTER_DIRECTION_OFF     : constant Int_Unsigned_C :=  (2)

  AL_FREQUENCY_SHIFTER_MIN_RIGHT_DIRECTION : constant Int_Unsigned_C := (0)
  AL_FREQUENCY_SHIFTER_MAX_RIGHT_DIRECTION : constant Int_Unsigned_C := (2)
  AL_FREQUENCY_SHIFTER_DEFAULT_RIGHT_DIRECTION : constant Int_Unsigned_C := (0)

  -- Vocal morpher effect
  AL_VOCAL_MORPHER_MIN_PHONEMEA       : constant Int_Unsigned_C :=     (0)
  AL_VOCAL_MORPHER_MAX_PHONEMEA       : constant Int_Unsigned_C :=     (29)
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEA   : constant Int_Unsigned_C :=     (0)

  AL_VOCAL_MORPHER_MIN_PHONEMEA_COARSE_TUNING : constant Int_Unsigned_C := (-24)
  AL_VOCAL_MORPHER_MAX_PHONEMEA_COARSE_TUNING : constant Int_Unsigned_C := (24)
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEA_COARSE_TUNING : constant Int_Unsigned_C := (0)

  AL_VOCAL_MORPHER_MIN_PHONEMEB          : constant Int_Unsigned_C :=  (0)
  AL_VOCAL_MORPHER_MAX_PHONEMEB          : constant Int_Unsigned_C :=  (29)
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEB      : constant Int_Unsigned_C :=  (10)

  AL_VOCAL_MORPHER_MIN_PHONEMEB_COARSE_TUNING : constant Int_Unsigned_C := (-24)
  AL_VOCAL_MORPHER_MAX_PHONEMEB_COARSE_TUNING : constant Int_Unsigned_C := (24)
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEB_COARSE_TUNING : constant Int_Unsigned_C := (0)

  AL_VOCAL_MORPHER_PHONEME_A            : constant Int_Unsigned_C :=   (0)
  AL_VOCAL_MORPHER_PHONEME_E            : constant Int_Unsigned_C :=   (1)
  AL_VOCAL_MORPHER_PHONEME_I            : constant Int_Unsigned_C :=   (2)
  AL_VOCAL_MORPHER_PHONEME_O            : constant Int_Unsigned_C :=   (3)
  AL_VOCAL_MORPHER_PHONEME_U            : constant Int_Unsigned_C :=   (4)
  AL_VOCAL_MORPHER_PHONEME_AA           : constant Int_Unsigned_C :=   (5)
  AL_VOCAL_MORPHER_PHONEME_AE           : constant Int_Unsigned_C :=   (6)
  AL_VOCAL_MORPHER_PHONEME_AH           : constant Int_Unsigned_C :=   (7)
  AL_VOCAL_MORPHER_PHONEME_AO           : constant Int_Unsigned_C :=   (8)
  AL_VOCAL_MORPHER_PHONEME_EH           : constant Int_Unsigned_C :=   (9)
  AL_VOCAL_MORPHER_PHONEME_ER           : constant Int_Unsigned_C :=   (10)
  AL_VOCAL_MORPHER_PHONEME_IH           : constant Int_Unsigned_C :=   (11)
  AL_VOCAL_MORPHER_PHONEME_IY           : constant Int_Unsigned_C :=   (12)
  AL_VOCAL_MORPHER_PHONEME_UH           : constant Int_Unsigned_C :=   (13)
  AL_VOCAL_MORPHER_PHONEME_UW           : constant Int_Unsigned_C :=   (14)
  AL_VOCAL_MORPHER_PHONEME_B            : constant Int_Unsigned_C :=   (15)
  AL_VOCAL_MORPHER_PHONEME_D            : constant Int_Unsigned_C :=   (16)
  AL_VOCAL_MORPHER_PHONEME_F            : constant Int_Unsigned_C :=   (17)
  AL_VOCAL_MORPHER_PHONEME_G            : constant Int_Unsigned_C :=   (18)
  AL_VOCAL_MORPHER_PHONEME_J            : constant Int_Unsigned_C :=   (19)
  AL_VOCAL_MORPHER_PHONEME_K            : constant Int_Unsigned_C :=   (20)
  AL_VOCAL_MORPHER_PHONEME_L            : constant Int_Unsigned_C :=   (21)
  AL_VOCAL_MORPHER_PHONEME_M            : constant Int_Unsigned_C :=   (22)
  AL_VOCAL_MORPHER_PHONEME_N            : constant Int_Unsigned_C :=   (23)
  AL_VOCAL_MORPHER_PHONEME_P            : constant Int_Unsigned_C :=   (24)
  AL_VOCAL_MORPHER_PHONEME_R            : constant Int_Unsigned_C :=   (25)
  AL_VOCAL_MORPHER_PHONEME_S            : constant Int_Unsigned_C :=   (26)
  AL_VOCAL_MORPHER_PHONEME_T            : constant Int_Unsigned_C :=   (27)
  AL_VOCAL_MORPHER_PHONEME_V            : constant Int_Unsigned_C :=   (28)
  AL_VOCAL_MORPHER_PHONEME_Z            : constant Int_Unsigned_C :=   (29)

  AL_VOCAL_MORPHER_WAVEFORM_SINUSOID    : constant Int_Unsigned_C :=   (0)
  AL_VOCAL_MORPHER_WAVEFORM_TRIANGLE    : constant Int_Unsigned_C :=   (1)
  AL_VOCAL_MORPHER_WAVEFORM_SAWTOOTH    : constant Int_Unsigned_C :=   (2)

  AL_VOCAL_MORPHER_MIN_WAVEFORM         : constant Int_Unsigned_C :=   (0)
  AL_VOCAL_MORPHER_MAX_WAVEFORM         : constant Int_Unsigned_C :=   (2)
  AL_VOCAL_MORPHER_DEFAULT_WAVEFORM     : constant Int_Unsigned_C :=   (0)

  AL_VOCAL_MORPHER_MIN_RATE                (0.0)
  AL_VOCAL_MORPHER_MAX_RATE                (10.0)
  AL_VOCAL_MORPHER_DEFAULT_RATE            (1.41f)

  -- Pitch shifter effect
  AL_PITCH_SHIFTER_MIN_COARSE_TUNE       : constant Int_Unsigned_C :=  (-12)
  AL_PITCH_SHIFTER_MAX_COARSE_TUNE       : constant Int_Unsigned_C :=  (12)
  AL_PITCH_SHIFTER_DEFAULT_COARSE_TUNE   : constant Int_Unsigned_C :=  (12)

  AL_PITCH_SHIFTER_MIN_FINE_TUNE         : constant Int_Unsigned_C :=  (-50)
  AL_PITCH_SHIFTER_MAX_FINE_TUNE         : constant Int_Unsigned_C :=  (50)
  AL_PITCH_SHIFTER_DEFAULT_FINE_TUNE     : constant Int_Unsigned_C :=  (0)

  -- Ring modulator effect
  AL_RING_MODULATOR_MIN_FREQUENCY          (0.0)
  AL_RING_MODULATOR_MAX_FREQUENCY          (8000.0)
  AL_RING_MODULATOR_DEFAULT_FREQUENCY      (440.0)

  AL_RING_MODULATOR_MIN_HIGHPASS_CUTOFF    (0.0)
  AL_RING_MODULATOR_MAX_HIGHPASS_CUTOFF    (24000.0)
  AL_RING_MODULATOR_DEFAULT_HIGHPASS_CUTOFF (800.0)

  AL_RING_MODULATOR_SINUSOID            : constant Int_Unsigned_C :=   (0)
  AL_RING_MODULATOR_SAWTOOTH            : constant Int_Unsigned_C :=   (1)
  AL_RING_MODULATOR_SQUARE              : constant Int_Unsigned_C :=   (2)

  AL_RING_MODULATOR_MIN_WAVEFORM         : constant Int_Unsigned_C :=  (0)
  AL_RING_MODULATOR_MAX_WAVEFORM         : constant Int_Unsigned_C :=  (2)
  AL_RING_MODULATOR_DEFAULT_WAVEFORM     : constant Int_Unsigned_C :=  (0)

  -- Autowah effect
  AL_AUTOWAH_MIN_ATTACK_TIME               (0.0001f)
  AL_AUTOWAH_MAX_ATTACK_TIME               (1.0)
  AL_AUTOWAH_DEFAULT_ATTACK_TIME           (0.06f)

  AL_AUTOWAH_MIN_RELEASE_TIME              (0.0001f)
  AL_AUTOWAH_MAX_RELEASE_TIME              (1.0)
  AL_AUTOWAH_DEFAULT_RELEASE_TIME          (0.06f)

  AL_AUTOWAH_MIN_RESONANCE                 (2.0)
  AL_AUTOWAH_MAX_RESONANCE                 (1000.0)
  AL_AUTOWAH_DEFAULT_RESONANCE             (1000.0)

  AL_AUTOWAH_MIN_PEAK_GAIN                 (0.00003f)
  AL_AUTOWAH_MAX_PEAK_GAIN                 (31621.0)
  AL_AUTOWAH_DEFAULT_PEAK_GAIN             (11.22f)

  -- Compressor effect
  AL_COMPRESSOR_MIN_ONOFF                : constant Int_Unsigned_C :=  (0)
  AL_COMPRESSOR_MAX_ONOFF                : constant Int_Unsigned_C :=  (1)
  AL_COMPRESSOR_DEFAULT_ONOFF            : constant Int_Unsigned_C :=  (1)

  -- Equalizer effect
  AL_EQUALIZER_MIN_LOW_GAIN                (0.126f)
  AL_EQUALIZER_MAX_LOW_GAIN                (7.943f)
  AL_EQUALIZER_DEFAULT_LOW_GAIN            (1.0)

  AL_EQUALIZER_MIN_LOW_CUTOFF              (50.0)
  AL_EQUALIZER_MAX_LOW_CUTOFF              (800.0)
  AL_EQUALIZER_DEFAULT_LOW_CUTOFF          (200.0)

  AL_EQUALIZER_MIN_MID1_GAIN               (0.126f)
  AL_EQUALIZER_MAX_MID1_GAIN               (7.943f)
  AL_EQUALIZER_DEFAULT_MID1_GAIN           (1.0)

  AL_EQUALIZER_MIN_MID1_CENTER             (200.0)
  AL_EQUALIZER_MAX_MID1_CENTER             (3000.0)
  AL_EQUALIZER_DEFAULT_MID1_CENTER         (500.0)

  AL_EQUALIZER_MIN_MID1_WIDTH              (0.01f)
  AL_EQUALIZER_MAX_MID1_WIDTH              (1.0)
  AL_EQUALIZER_DEFAULT_MID1_WIDTH          (1.0)

  AL_EQUALIZER_MIN_MID2_GAIN               (0.126f)
  AL_EQUALIZER_MAX_MID2_GAIN               (7.943f)
  AL_EQUALIZER_DEFAULT_MID2_GAIN           (1.0)

  AL_EQUALIZER_MIN_MID2_CENTER             (1000.0)
  AL_EQUALIZER_MAX_MID2_CENTER             (8000.0)
  AL_EQUALIZER_DEFAULT_MID2_CENTER         (3000.0)

  AL_EQUALIZER_MIN_MID2_WIDTH              (0.01f)
  AL_EQUALIZER_MAX_MID2_WIDTH              (1.0)
  AL_EQUALIZER_DEFAULT_MID2_WIDTH          (1.0)

  AL_EQUALIZER_MIN_HIGH_GAIN               (0.126f)
  AL_EQUALIZER_MAX_HIGH_GAIN               (7.943f)
  AL_EQUALIZER_DEFAULT_HIGH_GAIN           (1.0)

  AL_EQUALIZER_MIN_HIGH_CUTOFF             (4000.0)
  AL_EQUALIZER_MAX_HIGH_CUTOFF             (16000.0)
  AL_EQUALIZER_DEFAULT_HIGH_CUTOFF         (6000.0)


  -- Source parameter value ranges and defaults
  AL_MIN_AIR_ABSORPTION_FACTOR             (0.0)
  AL_MAX_AIR_ABSORPTION_FACTOR             (10.0)
  AL_DEFAULT_AIR_ABSORPTION_FACTOR         (0.0)

  AL_MIN_ROOM_ROLLOFF_FACTOR               (0.0)
  AL_MAX_ROOM_ROLLOFF_FACTOR               (10.0)
  AL_DEFAULT_ROOM_ROLLOFF_FACTOR           (0.0)

  AL_MIN_CONE_OUTER_GAINHF                 (0.0)
  AL_MAX_CONE_OUTER_GAINHF                 (1.0)
  AL_DEFAULT_CONE_OUTER_GAINHF             (1.0)

  AL_MIN_DIRECT_FILTER_GAINHF_AUTO       : constant Int_Unsigned_C :=  AL_FALSE
  AL_MAX_DIRECT_FILTER_GAINHF_AUTO        : constant Int_Unsigned_C := AL_TRUE
  AL_DEFAULT_DIRECT_FILTER_GAINHF_AUTO    : constant Int_Unsigned_C := AL_TRUE

  AL_MIN_AUXILIARY_SEND_FILTER_GAIN_AUTO : constant Int_Unsigned_C :=  AL_FALSE
  AL_MAX_AUXILIARY_SEND_FILTER_GAIN_AUTO   : constant Int_Unsigned_C := AL_TRUE
  AL_DEFAULT_AUXILIARY_SEND_FILTER_GAIN_AUTO : constant Int_Unsigned_C := AL_TRUE

  AL_MIN_AUXILIARY_SEND_FILTER_GAINHF_AUTO  : constant Int_Unsigned_C :=AL_FALSE
  AL_MAX_AUXILIARY_SEND_FILTER_GAINHF_AUTO : constant Int_Unsigned_C := AL_TRUE
  AL_DEFAULT_AUXILIARY_SEND_FILTER_GAINHF_AUTO : constant Int_Unsigned_C := AL_TRUE

  -- Listener parameter value ranges and defaults
  AL_MIN_METERS_PER_UNIT                : constant Int_Unsigned_C :=   FLT_MIN
  AL_MAX_METERS_PER_UNIT                 : constant Int_Unsigned_C :=  FLT_MAX
  AL_DEFAULT_METERS_PER_UNIT             : constant Int_Unsigned_C :=  (1.0)

  -------------
  -- Presets --
  -------------

  type EFXEAXREVERBPROPERTIES is record
      flDensity             : Real_32_C              := 0.0;
      flDiffusion           : Real_32_C              := 0.0;
      flGain                : Real_32_C              := 0.0;
      flGainHF              : Real_32_C              := 0.0;
      flGainLF              : Real_32_C              := 0.0;
      flDecayTime           : Real_32_C              := 0.0;
      flDecayHFRatio        : Real_32_C              := 0.0;
      flDecayLFRatio        : Real_32_C              := 0.0;
      flReflectionsGain     : Real_32_C              := 0.0;
      flReflectionsDelay    : Real_32_C              := 0.0;
      flReflectionsPan      : Array_Real_32_C (1..3) := 0.0;
      flLateReverbGain      : Real_32_C              := 0.0;
      flLateReverbDelay     : Real_32_C              := 0.0;
      flLateReverbPan       : Array_Real_32_C (1..3) := 0.0;
      flEchoTime            : Real_32_C              := 0.0;
      flEchoDepth           : Real_32_C              := 0.0;
      flModulationTime      : Real_32_C              := 0.0;
      flModulationDepth     : Real_32_C              := 0.0;
      flAirAbsorptionGainHF : Real_32_C              := 0.0;
      flHFReference         : Real_32_C              := 0.0;
      flLFReference         : Real_32_C              := 0.0;
      flRoomRolloffFactor   : Real_32_C              := 0.0;
      iDecayHFLimit         : Int_C                  := 0;
    end record with Convention => C;

  -- General
  EFX_REVERB_PRESET_GENERIC         : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.8913, 1.0000, 1.490, 0.830, 1.0000, 0.050, 0.0070, (0.0, 0.0, 0.0), 1.2589, 0.0110, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_PADDEDCELL      : constant EFXEAXREVERBPROPERTIES := (0.1715, 1.0000, 0.3162, 0.0010, 1.0000, 0.170, 0.100, 1.0000, 0.250, 0.0010, (0.0, 0.0, 0.0), 1.2691, 0.0020, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_ROOM            : constant EFXEAXREVERBPROPERTIES := (0.4287, 1.0000, 0.3162, 0.5929, 1.0000, 0.400, 0.830, 1.0000, 0.1503, 0.0020, (0.0, 0.0, 0.0), 1.0629, 0.0030, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_BATHROOM        : constant EFXEAXREVERBPROPERTIES := (0.1715, 1.0000, 0.3162, 0.2512, 1.0000, 1.490, 0.540, 1.0000, 0.6531, 0.0070, (0.0, 0.0, 0.0), 3.2734, 0.0110, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_LIVINGROOM      : constant EFXEAXREVERBPROPERTIES := (0.9766, 1.0000, 0.3162, 0.0010, 1.0000, 0.500, 0.100, 1.0000, 0.2051, 0.0030, (0.0, 0.0, 0.0), 0.2805, 0.0040, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_STONEROOM       : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.7079, 1.0000, 2.310, 0.640, 1.0000, 0.4411, 0.0120, (0.0, 0.0, 0.0), 1.1003, 0.0170, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_AUDITORIUM      : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.5781, 1.0000, 4.320, 0.590, 1.0000, 0.4032, 0.020, (0.0, 0.0, 0.0), 0.7170, 0.030, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_CONCERTHALL     : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.5623, 1.0000, 3.920, 0.700, 1.0000, 0.2427, 0.020, (0.0, 0.0, 0.0), 0.9977, 0.0290, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_CAVE            : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 1.0000, 1.0000, 2.910, 1.300, 1.0000, 0.500, 0.0150, (0.0, 0.0, 0.0), 0.7063, 0.0220, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_ARENA           : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.4477, 1.0000, 7.240, 0.330, 1.0000, 0.2612, 0.020, (0.0, 0.0, 0.0), 1.0186, 0.030, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_HANGAR          : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.3162, 1.0000, 10.050, 0.230, 1.0000, 0.500, 0.020, (0.0, 0.0, 0.0), 1.2560, 0.030, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_CARPETEDHALLWAY : constant EFXEAXREVERBPROPERTIES := (0.4287, 1.0000, 0.3162, 0.010, 1.0000, 0.300, 0.100, 1.0000, 0.1215, 0.0020, (0.0, 0.0, 0.0), 0.1531, 0.030, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_HALLWAY         : constant EFXEAXREVERBPROPERTIES := (0.3645, 1.0000, 0.3162, 0.7079, 1.0000, 1.490, 0.590, 1.0000, 0.2458, 0.0070, (0.0, 0.0, 0.0), 1.6615, 0.0110, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_STONECORRIDOR   : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.7612, 1.0000, 2.700, 0.790, 1.0000, 0.2472, 0.0130, (0.0, 0.0, 0.0), 1.5758, 0.020, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_ALLEY           : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.3000, 0.3162, 0.7328, 1.0000, 1.490, 0.860, 1.0000, 0.250, 0.0070, (0.0, 0.0, 0.0), 0.9954, 0.0110, (0.0, 0.0, 0.0), 0.1250, 0.950, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_FOREST          : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.3000, 0.3162, 0.0224, 1.0000, 1.490, 0.540, 1.0000, 0.0525, 0.1620, (0.0, 0.0, 0.0), 0.7682, 0.0880, (0.0, 0.0, 0.0), 0.1250, 1.0000, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_CITY            : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.5000, 0.3162, 0.3981, 1.0000, 1.490, 0.670, 1.0000, 0.0730, 0.0070, (0.0, 0.0, 0.0), 0.1427, 0.0110, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_MOUNTAINS       : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.2700, 0.3162, 0.0562, 1.0000, 1.490, 0.210, 1.0000, 0.0407, 0.300, (0.0, 0.0, 0.0), 0.1919, 0.100, (0.0, 0.0, 0.0), 0.250, 1.0000, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_QUARRY          : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.3162, 1.0000, 1.490, 0.830, 1.0000, 0.0, 0.0610, (0.0, 0.0, 0.0), 1.7783, 0.0250, (0.0, 0.0, 0.0), 0.1250, 0.700, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_PLAIN           : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.2100, 0.3162, 0.100, 1.0000, 1.490, 0.500, 1.0000, 0.0585, 0.1790, (0.0, 0.0, 0.0), 0.1089, 0.100, (0.0, 0.0, 0.0), 0.250, 1.0000, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_PARKINGLOT      : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 1.0000, 1.0000, 1.650, 1.500, 1.0000, 0.2082, 0.0080, (0.0, 0.0, 0.0), 0.2652, 0.0120, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_SEWERPIPE       : constant EFXEAXREVERBPROPERTIES := (0.3071, 0.8000, 0.3162, 0.3162, 1.0000, 2.810, 0.140, 1.0000, 1.6387, 0.0140, (0.0, 0.0, 0.0), 3.2471, 0.0210, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_UNDERWATER      : constant EFXEAXREVERBPROPERTIES := (0.3645, 1.0000, 0.3162, 0.010, 1.0000, 1.490, 0.100, 1.0000, 0.5963, 0.0070, (0.0, 0.0, 0.0), 7.0795, 0.0110, (0.0, 0.0, 0.0), 0.250, 0.0, 1.180, 0.3480, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_DRUGGED         : constant EFXEAXREVERBPROPERTIES := (0.4287, 0.5000, 0.3162, 1.0000, 1.0000, 8.390, 1.390, 1.0000, 0.8760, 0.0020, (0.0, 0.0, 0.0), 3.1081, 0.030, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 1.0000, 0.9943, 5000.0, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_DIZZY           : constant EFXEAXREVERBPROPERTIES := (0.3645, 0.6000, 0.3162, 0.6310, 1.0000, 17.230, 0.560, 1.0000, 0.1392, 0.020, (0.0, 0.0, 0.0), 0.4937, 0.030, (0.0, 0.0, 0.0), 0.250, 1.0000, 0.810, 0.310, 0.9943, 5000.0, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_PSYCHOTIC       : constant EFXEAXREVERBPROPERTIES := (0.0625, 0.5000, 0.3162, 0.8404, 1.0000, 7.560, 0.910, 1.0000, 0.4864, 0.020, (0.0, 0.0, 0.0), 2.4378, 0.030, (0.0, 0.0, 0.0), 0.250, 0.0, 4.0, 1.0000, 0.9943, 5000.0, 250.0, 0.0, 0)

  -- Castle Presets
  EFX_REVERB_PRESET_CASTLE_SMALLROOM : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.890, 0.3162, 0.3981, 0.100, 1.220, 0.830, 0.310, 0.8913, 0.0220, (0.0, 0.0, 0.0), 1.9953, 0.0110, (0.0, 0.0, 0.0), 0.1380, 0.080, 0.250, 0.0, 0.9943, 5168.6001, 139.500, 0.0, 1)
  EFX_REVERB_PRESET_CASTLE_SHORTPASSAGE : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.890, 0.3162, 0.3162, 0.100, 2.320, 0.830, 0.310, 0.8913, 0.0070, (0.0, 0.0, 0.0), 1.2589, 0.0230, (0.0, 0.0, 0.0), 0.1380, 0.080, 0.250, 0.0, 0.9943, 5168.6001, 139.500, 0.0, 1)
  EFX_REVERB_PRESET_CASTLE_MEDIUMROOM : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.930, 0.3162, 0.2818, 0.100, 2.040, 0.830, 0.460, 0.6310, 0.0220, (0.0, 0.0, 0.0), 1.5849, 0.0110, (0.0, 0.0, 0.0), 0.1550, 0.030, 0.250, 0.0, 0.9943, 5168.6001, 139.500, 0.0, 1)
  EFX_REVERB_PRESET_CASTLE_LARGEROOM : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.820, 0.3162, 0.2818, 0.1259, 2.530, 0.830, 0.500, 0.4467, 0.0340, (0.0, 0.0, 0.0), 1.2589, 0.0160, (0.0, 0.0, 0.0), 0.1850, 0.070, 0.250, 0.0, 0.9943, 5168.6001, 139.500, 0.0, 1)
  EFX_REVERB_PRESET_CASTLE_LONGPASSAGE : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.890, 0.3162, 0.3981, 0.100, 3.420, 0.830, 0.310, 0.8913, 0.0070, (0.0, 0.0, 0.0), 1.4125, 0.0230, (0.0, 0.0, 0.0), 0.1380, 0.080, 0.250, 0.0, 0.9943, 5168.6001, 139.500, 0.0, 1)
  EFX_REVERB_PRESET_CASTLE_HALL : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.810, 0.3162, 0.2818, 0.1778, 3.140, 0.790, 0.620, 0.1778, 0.0560, (0.0, 0.0, 0.0), 1.1220, 0.0240, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5168.6001, 139.500, 0.0, 1)
  EFX_REVERB_PRESET_CASTLE_CUPBOARD : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.890, 0.3162, 0.2818, 0.100, 0.670, 0.870, 0.310, 1.4125, 0.010, (0.0, 0.0, 0.0), 3.5481, 0.0070, (0.0, 0.0, 0.0), 0.1380, 0.080, 0.250, 0.0, 0.9943, 5168.6001, 139.500, 0.0, 1)
  EFX_REVERB_PRESET_CASTLE_COURTYARD : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.420, 0.3162, 0.4467, 0.1995, 2.130, 0.610, 0.230, 0.2239, 0.160, (0.0, 0.0, 0.0), 0.7079, 0.0360, (0.0, 0.0, 0.0), 0.250, 0.370, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_CASTLE_ALCOVE : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.890, 0.3162, 0.5012, 0.100, 1.640, 0.870, 0.310, 1.0000, 0.0070, (0.0, 0.0, 0.0), 1.4125, 0.0340, (0.0, 0.0, 0.0), 0.1380, 0.080, 0.250, 0.0, 0.9943, 5168.6001, 139.500, 0.0, 1)

  -- Factory Presets
  EFX_REVERB_PRESET_FACTORY_SMALLROOM : constant EFXEAXREVERBPROPERTIES := (0.3645, 0.820, 0.3162, 0.7943, 0.5012, 1.720, 0.650, 1.310, 0.7079, 0.010, (0.0, 0.0, 0.0), 1.7783, 0.0240, (0.0, 0.0, 0.0), 0.1190, 0.070, 0.250, 0.0, 0.9943, 3762.6001, 362.500, 0.0, 1)
  EFX_REVERB_PRESET_FACTORY_SHORTPASSAGE : constant EFXEAXREVERBPROPERTIES := (0.3645, 0.640, 0.2512, 0.7943, 0.5012, 2.530, 0.650, 1.310, 1.0000, 0.010, (0.0, 0.0, 0.0), 1.2589, 0.0380, (0.0, 0.0, 0.0), 0.1350, 0.230, 0.250, 0.0, 0.9943, 3762.6001, 362.500, 0.0, 1)
  EFX_REVERB_PRESET_FACTORY_MEDIUMROOM : constant EFXEAXREVERBPROPERTIES := (0.4287, 0.820, 0.2512, 0.7943, 0.5012, 2.760, 0.650, 1.310, 0.2818, 0.0220, (0.0, 0.0, 0.0), 1.4125, 0.0230, (0.0, 0.0, 0.0), 0.1740, 0.070, 0.250, 0.0, 0.9943, 3762.6001, 362.500, 0.0, 1)
  EFX_REVERB_PRESET_FACTORY_LARGEROOM : constant EFXEAXREVERBPROPERTIES := (0.4287, 0.750, 0.2512, 0.7079, 0.6310, 4.240, 0.510, 1.310, 0.1778, 0.0390, (0.0, 0.0, 0.0), 1.1220, 0.0230, (0.0, 0.0, 0.0), 0.2310, 0.070, 0.250, 0.0, 0.9943, 3762.6001, 362.500, 0.0, 1)
  EFX_REVERB_PRESET_FACTORY_LONGPASSAGE : constant EFXEAXREVERBPROPERTIES := (0.3645, 0.640, 0.2512, 0.7943, 0.5012, 4.060, 0.650, 1.310, 1.0000, 0.020, (0.0, 0.0, 0.0), 1.2589, 0.0370, (0.0, 0.0, 0.0), 0.1350, 0.230, 0.250, 0.0, 0.9943, 3762.6001, 362.500, 0.0, 1)
  EFX_REVERB_PRESET_FACTORY_HALL : constant EFXEAXREVERBPROPERTIES := (0.4287, 0.750, 0.3162, 0.7079, 0.6310, 7.430, 0.510, 1.310, 0.0631, 0.0730, (0.0, 0.0, 0.0), 0.8913, 0.0270, (0.0, 0.0, 0.0), 0.250, 0.070, 0.250, 0.0, 0.9943, 3762.6001, 362.500, 0.0, 1)
  EFX_REVERB_PRESET_FACTORY_CUPBOARD : constant EFXEAXREVERBPROPERTIES := (0.3071, 0.630, 0.2512, 0.7943, 0.5012, 0.490, 0.650, 1.310, 1.2589, 0.010, (0.0, 0.0, 0.0), 1.9953, 0.0320, (0.0, 0.0, 0.0), 0.1070, 0.070, 0.250, 0.0, 0.9943, 3762.6001, 362.500, 0.0, 1)
  EFX_REVERB_PRESET_FACTORY_COURTYARD : constant EFXEAXREVERBPROPERTIES := (0.3071, 0.570, 0.3162, 0.3162, 0.6310, 2.320, 0.290, 0.560, 0.2239, 0.140, (0.0, 0.0, 0.0), 0.3981, 0.0390, (0.0, 0.0, 0.0), 0.250, 0.290, 0.250, 0.0, 0.9943, 3762.6001, 362.500, 0.0, 1)
  EFX_REVERB_PRESET_FACTORY_ALCOVE : constant EFXEAXREVERBPROPERTIES := (0.3645, 0.590, 0.2512, 0.7943, 0.5012, 3.140, 0.650, 1.310, 1.4125, 0.010, (0.0, 0.0, 0.0), 1.0000, 0.0380, (0.0, 0.0, 0.0), 0.1140, 0.100, 0.250, 0.0, 0.9943, 3762.6001, 362.500, 0.0, 1)

  -- Ice Palace Presets
  EFX_REVERB_PRESET_ICEPALACE_SMALLROOM : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.840, 0.3162, 0.5623, 0.2818, 1.510, 1.530, 0.270, 0.8913, 0.010, (0.0, 0.0, 0.0), 1.4125, 0.0110, (0.0, 0.0, 0.0), 0.1640, 0.140, 0.250, 0.0, 0.9943, 12428.500, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_ICEPALACE_SHORTPASSAGE : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.750, 0.3162, 0.5623, 0.2818, 1.790, 1.460, 0.280, 0.5012, 0.010, (0.0, 0.0, 0.0), 1.1220, 0.0190, (0.0, 0.0, 0.0), 0.1770, 0.090, 0.250, 0.0, 0.9943, 12428.500, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_ICEPALACE_MEDIUMROOM : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.870, 0.3162, 0.5623, 0.4467, 2.220, 1.530, 0.320, 0.3981, 0.0390, (0.0, 0.0, 0.0), 1.1220, 0.0270, (0.0, 0.0, 0.0), 0.1860, 0.120, 0.250, 0.0, 0.9943, 12428.500, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_ICEPALACE_LARGEROOM : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.810, 0.3162, 0.5623, 0.4467, 3.140, 1.530, 0.320, 0.2512, 0.0390, (0.0, 0.0, 0.0), 1.0000, 0.0270, (0.0, 0.0, 0.0), 0.2140, 0.110, 0.250, 0.0, 0.9943, 12428.500, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_ICEPALACE_LONGPASSAGE : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.770, 0.3162, 0.5623, 0.3981, 3.010, 1.460, 0.280, 0.7943, 0.0120, (0.0, 0.0, 0.0), 1.2589, 0.0250, (0.0, 0.0, 0.0), 0.1860, 0.040, 0.250, 0.0, 0.9943, 12428.500, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_ICEPALACE_HALL : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.760, 0.3162, 0.4467, 0.5623, 5.490, 1.530, 0.380, 0.1122, 0.0540, (0.0, 0.0, 0.0), 0.6310, 0.0520, (0.0, 0.0, 0.0), 0.2260, 0.110, 0.250, 0.0, 0.9943, 12428.500, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_ICEPALACE_CUPBOARD : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.830, 0.3162, 0.5012, 0.2239, 0.760, 1.530, 0.260, 1.1220, 0.0120, (0.0, 0.0, 0.0), 1.9953, 0.0160, (0.0, 0.0, 0.0), 0.1430, 0.080, 0.250, 0.0, 0.9943, 12428.500, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_ICEPALACE_COURTYARD : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.590, 0.3162, 0.2818, 0.3162, 2.040, 1.200, 0.380, 0.3162, 0.1730, (0.0, 0.0, 0.0), 0.3162, 0.0430, (0.0, 0.0, 0.0), 0.2350, 0.480, 0.250, 0.0, 0.9943, 12428.500, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_ICEPALACE_ALCOVE : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.840, 0.3162, 0.5623, 0.2818, 2.760, 1.460, 0.280, 1.1220, 0.010, (0.0, 0.0, 0.0), 0.8913, 0.030, (0.0, 0.0, 0.0), 0.1610, 0.090, 0.250, 0.0, 0.9943, 12428.500, 99.600, 0.0, 1)

  -- Space Station Presets
  EFX_REVERB_PRESET_SPACESTATION_SMALLROOM : constant EFXEAXREVERBPROPERTIES := (0.2109, 0.700, 0.3162, 0.7079, 0.8913, 1.720, 0.820, 0.550, 0.7943, 0.0070, (0.0, 0.0, 0.0), 1.4125, 0.0130, (0.0, 0.0, 0.0), 0.1880, 0.260, 0.250, 0.0, 0.9943, 3316.1001, 458.200, 0.0, 1)
  EFX_REVERB_PRESET_SPACESTATION_SHORTPASSAGE : constant EFXEAXREVERBPROPERTIES := (0.2109, 0.870, 0.3162, 0.6310, 0.8913, 3.570, 0.500, 0.550, 1.0000, 0.0120, (0.0, 0.0, 0.0), 1.1220, 0.0160, (0.0, 0.0, 0.0), 0.1720, 0.200, 0.250, 0.0, 0.9943, 3316.1001, 458.200, 0.0, 1)
  EFX_REVERB_PRESET_SPACESTATION_MEDIUMROOM : constant EFXEAXREVERBPROPERTIES := (0.2109, 0.750, 0.3162, 0.6310, 0.8913, 3.010, 0.500, 0.550, 0.3981, 0.0340, (0.0, 0.0, 0.0), 1.1220, 0.0350, (0.0, 0.0, 0.0), 0.2090, 0.310, 0.250, 0.0, 0.9943, 3316.1001, 458.200, 0.0, 1)
  EFX_REVERB_PRESET_SPACESTATION_LARGEROOM : constant EFXEAXREVERBPROPERTIES := (0.3645, 0.810, 0.3162, 0.6310, 0.8913, 3.890, 0.380, 0.610, 0.3162, 0.0560, (0.0, 0.0, 0.0), 0.8913, 0.0350, (0.0, 0.0, 0.0), 0.2330, 0.280, 0.250, 0.0, 0.9943, 3316.1001, 458.200, 0.0, 1)
  EFX_REVERB_PRESET_SPACESTATION_LONGPASSAGE : constant EFXEAXREVERBPROPERTIES := (0.4287, 0.820, 0.3162, 0.6310, 0.8913, 4.620, 0.620, 0.550, 1.0000, 0.0120, (0.0, 0.0, 0.0), 1.2589, 0.0310, (0.0, 0.0, 0.0), 0.250, 0.230, 0.250, 0.0, 0.9943, 3316.1001, 458.200, 0.0, 1)
  EFX_REVERB_PRESET_SPACESTATION_HALL : constant EFXEAXREVERBPROPERTIES := (0.4287, 0.870, 0.3162, 0.6310, 0.8913, 7.110, 0.380, 0.610, 0.1778, 0.100, (0.0, 0.0, 0.0), 0.6310, 0.0470, (0.0, 0.0, 0.0), 0.250, 0.250, 0.250, 0.0, 0.9943, 3316.1001, 458.200, 0.0, 1)
  EFX_REVERB_PRESET_SPACESTATION_CUPBOARD : constant EFXEAXREVERBPROPERTIES := (0.1715, 0.560, 0.3162, 0.7079, 0.8913, 0.790, 0.810, 0.550, 1.4125, 0.0070, (0.0, 0.0, 0.0), 1.7783, 0.0180, (0.0, 0.0, 0.0), 0.1810, 0.310, 0.250, 0.0, 0.9943, 3316.1001, 458.200, 0.0, 1)
  EFX_REVERB_PRESET_SPACESTATION_ALCOVE : constant EFXEAXREVERBPROPERTIES := (0.2109, 0.780, 0.3162, 0.7079, 0.8913, 1.160, 0.810, 0.550, 1.4125, 0.0070, (0.0, 0.0, 0.0), 1.0000, 0.0180, (0.0, 0.0, 0.0), 0.1920, 0.210, 0.250, 0.0, 0.9943, 3316.1001, 458.200, 0.0, 1)

  -- Wooden Galleon Presets
  EFX_REVERB_PRESET_WOODEN_SMALLROOM : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.1122, 0.3162, 0.790, 0.320, 0.870, 1.0000, 0.0320, (0.0, 0.0, 0.0), 0.8913, 0.0290, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 4705.0, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_WOODEN_SHORTPASSAGE : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.1259, 0.3162, 1.750, 0.500, 0.870, 0.8913, 0.0120, (0.0, 0.0, 0.0), 0.6310, 0.0240, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 4705.0, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_WOODEN_MEDIUMROOM : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.100, 0.2818, 1.470, 0.420, 0.820, 0.8913, 0.0490, (0.0, 0.0, 0.0), 0.8913, 0.0290, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 4705.0, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_WOODEN_LARGEROOM : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.0891, 0.2818, 2.650, 0.330, 0.820, 0.8913, 0.0660, (0.0, 0.0, 0.0), 0.7943, 0.0490, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 4705.0, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_WOODEN_LONGPASSAGE : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.100, 0.3162, 1.990, 0.400, 0.790, 1.0000, 0.020, (0.0, 0.0, 0.0), 0.4467, 0.0360, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 4705.0, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_WOODEN_HALL : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.0794, 0.2818, 3.450, 0.300, 0.820, 0.8913, 0.0880, (0.0, 0.0, 0.0), 0.7943, 0.0630, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 4705.0, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_WOODEN_CUPBOARD : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.1413, 0.3162, 0.560, 0.460, 0.910, 1.1220, 0.0120, (0.0, 0.0, 0.0), 1.1220, 0.0280, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 4705.0, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_WOODEN_COURTYARD : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.650, 0.3162, 0.0794, 0.3162, 1.790, 0.350, 0.790, 0.5623, 0.1230, (0.0, 0.0, 0.0), 0.100, 0.0320, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 4705.0, 99.600, 0.0, 1)
  EFX_REVERB_PRESET_WOODEN_ALCOVE : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.1259, 0.3162, 1.220, 0.620, 0.910, 1.1220, 0.0120, (0.0, 0.0, 0.0), 0.7079, 0.0240, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 4705.0, 99.600, 0.0, 1)

  -- Sports Presets
  EFX_REVERB_PRESET_SPORT_EMPTYSTADIUM : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.4467, 0.7943, 6.260, 0.510, 1.100, 0.0631, 0.1830, (0.0, 0.0, 0.0), 0.3981, 0.0380, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_SPORT_SQUASHCOURT : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.750, 0.3162, 0.3162, 0.7943, 2.220, 0.910, 1.160, 0.4467, 0.0070, (0.0, 0.0, 0.0), 0.7943, 0.0110, (0.0, 0.0, 0.0), 0.1260, 0.190, 0.250, 0.0, 0.9943, 7176.8999, 211.200, 0.0, 1)
  EFX_REVERB_PRESET_SPORT_SMALLSWIMMINGPOOL : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.700, 0.3162, 0.7943, 0.8913, 2.760, 1.250, 1.140, 0.6310, 0.020, (0.0, 0.0, 0.0), 0.7943, 0.030, (0.0, 0.0, 0.0), 0.1790, 0.150, 0.8950, 0.190, 0.9943, 5000.0, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_SPORT_LARGESWIMMINGPOOL : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.820, 0.3162, 0.7943, 1.0000, 5.490, 1.310, 1.140, 0.4467, 0.0390, (0.0, 0.0, 0.0), 0.5012, 0.0490, (0.0, 0.0, 0.0), 0.2220, 0.550, 1.1590, 0.210, 0.9943, 5000.0, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_SPORT_GYMNASIUM : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.810, 0.3162, 0.4467, 0.8913, 3.140, 1.060, 1.350, 0.3981, 0.0290, (0.0, 0.0, 0.0), 0.5623, 0.0450, (0.0, 0.0, 0.0), 0.1460, 0.140, 0.250, 0.0, 0.9943, 7176.8999, 211.200, 0.0, 1)
  EFX_REVERB_PRESET_SPORT_FULLSTADIUM : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.0708, 0.7943, 5.250, 0.170, 0.800, 0.100, 0.1880, (0.0, 0.0, 0.0), 0.2818, 0.0380, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_SPORT_STADIUMTANNOY : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.780, 0.3162, 0.5623, 0.5012, 2.530, 0.880, 0.680, 0.2818, 0.230, (0.0, 0.0, 0.0), 0.5012, 0.0630, (0.0, 0.0, 0.0), 0.250, 0.200, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)

  -- Prefab Presets
  EFX_REVERB_PRESET_PREFAB_WORKSHOP : constant EFXEAXREVERBPROPERTIES := (0.4287, 1.0000, 0.3162, 0.1413, 0.3981, 0.760, 1.0000, 1.0000, 1.0000, 0.0120, (0.0, 0.0, 0.0), 1.1220, 0.0120, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_PREFAB_SCHOOLROOM : constant EFXEAXREVERBPROPERTIES := (0.4022, 0.690, 0.3162, 0.6310, 0.5012, 0.980, 0.450, 0.180, 1.4125, 0.0170, (0.0, 0.0, 0.0), 1.4125, 0.0150, (0.0, 0.0, 0.0), 0.0950, 0.140, 0.250, 0.0, 0.9943, 7176.8999, 211.200, 0.0, 1)
  EFX_REVERB_PRESET_PREFAB_PRACTISEROOM : constant EFXEAXREVERBPROPERTIES := (0.4022, 0.870, 0.3162, 0.3981, 0.5012, 1.120, 0.560, 0.180, 1.2589, 0.010, (0.0, 0.0, 0.0), 1.4125, 0.0110, (0.0, 0.0, 0.0), 0.0950, 0.140, 0.250, 0.0, 0.9943, 7176.8999, 211.200, 0.0, 1)
  EFX_REVERB_PRESET_PREFAB_OUTHOUSE : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.820, 0.3162, 0.1122, 0.1585, 1.380, 0.380, 0.350, 0.8913, 0.0240, (0.0, 0.0, -0.0), 0.6310, 0.0440, (0.0, 0.0, 0.0), 0.1210, 0.170, 0.250, 0.0, 0.9943, 2854.3999, 107.500, 0.0, 0)
  EFX_REVERB_PRESET_PREFAB_CARAVAN : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.0891, 0.1259, 0.430, 1.500, 1.0000, 1.0000, 0.0120, (0.0, 0.0, 0.0), 1.9953, 0.0120, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 0)

  -- Dome and Pipe Presets
  EFX_REVERB_PRESET_DOME_TOMB : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.790, 0.3162, 0.3548, 0.2239, 4.180, 0.210, 0.100, 0.3868, 0.030, (0.0, 0.0, 0.0), 1.6788, 0.0220, (0.0, 0.0, 0.0), 0.1770, 0.190, 0.250, 0.0, 0.9943, 2854.3999, 20.0, 0.0, 0)
  EFX_REVERB_PRESET_PIPE_SMALL : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.3548, 0.2239, 5.040, 0.100, 0.100, 0.5012, 0.0320, (0.0, 0.0, 0.0), 2.5119, 0.0150, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 2854.3999, 20.0, 0.0, 1)
  EFX_REVERB_PRESET_DOME_SAINTPAULS : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.870, 0.3162, 0.3548, 0.2239, 10.480, 0.190, 0.100, 0.1778, 0.090, (0.0, 0.0, 0.0), 1.2589, 0.0420, (0.0, 0.0, 0.0), 0.250, 0.120, 0.250, 0.0, 0.9943, 2854.3999, 20.0, 0.0, 1)
  EFX_REVERB_PRESET_PIPE_LONGTHIN : constant EFXEAXREVERBPROPERTIES := (0.2560, 0.910, 0.3162, 0.4467, 0.2818, 9.210, 0.180, 0.100, 0.7079, 0.010, (0.0, 0.0, 0.0), 0.7079, 0.0220, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 2854.3999, 20.0, 0.0, 0)
  EFX_REVERB_PRESET_PIPE_LARGE : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.3548, 0.2239, 8.450, 0.100, 0.100, 0.3981, 0.0460, (0.0, 0.0, 0.0), 1.5849, 0.0320, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 2854.3999, 20.0, 0.0, 1)
  EFX_REVERB_PRESET_PIPE_RESONANT : constant EFXEAXREVERBPROPERTIES := (0.1373, 0.910, 0.3162, 0.4467, 0.2818, 6.810, 0.180, 0.100, 0.7079, 0.010, (0.0, 0.0, 0.0), 1.0000, 0.0220, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 2854.3999, 20.0, 0.0, 0)

  -- Outdoors Presets
  EFX_REVERB_PRESET_OUTDOORS_BACKYARD : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.450, 0.3162, 0.2512, 0.5012, 1.120, 0.340, 0.460, 0.4467, 0.0690, (0.0, 0.0, -0.0), 0.7079, 0.0230, (0.0, 0.0, 0.0), 0.2180, 0.340, 0.250, 0.0, 0.9943, 4399.1001, 242.900, 0.0, 0)
  EFX_REVERB_PRESET_OUTDOORS_ROLLINGPLAINS : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.0, 0.3162, 0.0112, 0.6310, 2.130, 0.210, 0.460, 0.1778, 0.300, (0.0, 0.0, -0.0), 0.4467, 0.0190, (0.0, 0.0, 0.0), 0.250, 1.0000, 0.250, 0.0, 0.9943, 4399.1001, 242.900, 0.0, 0)
  EFX_REVERB_PRESET_OUTDOORS_DEEPCANYON : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.740, 0.3162, 0.1778, 0.6310, 3.890, 0.210, 0.460, 0.3162, 0.2230, (0.0, 0.0, -0.0), 0.3548, 0.0190, (0.0, 0.0, 0.0), 0.250, 1.0000, 0.250, 0.0, 0.9943, 4399.1001, 242.900, 0.0, 0)
  EFX_REVERB_PRESET_OUTDOORS_CREEK : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.350, 0.3162, 0.1778, 0.5012, 2.130, 0.210, 0.460, 0.3981, 0.1150, (0.0, 0.0, -0.0), 0.1995, 0.0310, (0.0, 0.0, 0.0), 0.2180, 0.340, 0.250, 0.0, 0.9943, 4399.1001, 242.900, 0.0, 0)
  EFX_REVERB_PRESET_OUTDOORS_VALLEY : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.280, 0.3162, 0.0282, 0.1585, 2.880, 0.260, 0.350, 0.1413, 0.2630, (0.0, 0.0, -0.0), 0.3981, 0.100, (0.0, 0.0, 0.0), 0.250, 0.340, 0.250, 0.0, 0.9943, 2854.3999, 107.500, 0.0, 0)

  -- Mood Presets
  EFX_REVERB_PRESET_MOOD_HEAVEN : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.940, 0.3162, 0.7943, 0.4467, 5.040, 1.120, 0.560, 0.2427, 0.020, (0.0, 0.0, 0.0), 1.2589, 0.0290, (0.0, 0.0, 0.0), 0.250, 0.080, 2.7420, 0.050, 0.9977, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_MOOD_HELL : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.570, 0.3162, 0.3548, 0.4467, 3.570, 0.490, 2.0, 0.0, 0.020, (0.0, 0.0, 0.0), 1.4125, 0.030, (0.0, 0.0, 0.0), 0.110, 0.040, 2.1090, 0.520, 0.9943, 5000.0, 139.500, 0.0, 0)
  EFX_REVERB_PRESET_MOOD_MEMORY : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.850, 0.3162, 0.6310, 0.3548, 4.060, 0.820, 0.560, 0.0398, 0.0, (0.0, 0.0, 0.0), 1.1220, 0.0, (0.0, 0.0, 0.0), 0.250, 0.0, 0.4740, 0.450, 0.9886, 5000.0, 250.0, 0.0, 0)

  -- Driving Presets
  EFX_REVERB_PRESET_DRIVING_COMMENTATOR     : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.0, 3.1623, 0.5623, 0.5012, 2.420, 0.880, 0.680, 0.1995, 0.0930, (0.0, 0.0, 0.0), 0.2512, 0.0170, (0.0, 0.0, 0.0), 0.250, 1.0000, 0.250, 0.0, 0.9886, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_DRIVING_PITGARAGE       : constant EFXEAXREVERBPROPERTIES := (0.4287, 0.590, 0.3162, 0.7079, 0.5623, 1.720, 0.930, 0.870, 0.5623, 0.0, (0.0, 0.0, 0.0), 1.2589, 0.0160, (0.0, 0.0, 0.0), 0.250, 0.110, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_DRIVING_INCAR_RACER     : constant EFXEAXREVERBPROPERTIES := (0.0832, 0.800, 0.3162, 1.0000, 0.7943, 0.170, 2.0, 0.410, 1.7783, 0.0070, (0.0, 0.0, 0.0), 0.7079, 0.0150, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 10268.2002, 251.0000, 0.0, 1)
  EFX_REVERB_PRESET_DRIVING_INCAR_SPORTS    : constant EFXEAXREVERBPROPERTIES := (0.0832, 0.800, 0.3162, 0.6310, 1.0000, 0.170, 0.750, 0.410, 1.0000, 0.010, (0.0, 0.0, 0.0), 0.5623, 0.0, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 10268.2002, 251.0000, 0.0, 1)
  EFX_REVERB_PRESET_DRIVING_INCAR_LUXURY    : constant EFXEAXREVERBPROPERTIES := (0.2560, 1.0000, 0.3162, 0.100, 0.5012, 0.130, 0.410, 0.460, 0.7943, 0.010, (0.0, 0.0, 0.0), 1.5849, 0.010, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 10268.2002, 251.0000, 0.0, 1)
  EFX_REVERB_PRESET_DRIVING_FULLGRANDSTAND  : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 0.2818, 0.6310, 3.010, 1.370, 1.280, 0.3548, 0.090, (0.0, 0.0, 0.0), 0.1778, 0.0490, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 10420.2002, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_DRIVING_EMPTYGRANDSTAND : constant EFXEAXREVERBPROPERTIES := (1.0000, 1.0000, 0.3162, 1.0000, 0.7943, 4.620, 1.750, 1.400, 0.2082, 0.090, (0.0, 0.0, 0.0), 0.2512, 0.0490, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.0, 0.9943, 10420.2002, 250.0, 0.0, 0)
  EFX_REVERB_PRESET_DRIVING_TUNNEL          : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.810, 0.3162, 0.3981, 0.8913, 3.420, 0.940, 1.310, 0.7079, 0.0510, (0.0, 0.0, 0.0), 0.7079, 0.0470, (0.0, 0.0, 0.0), 0.2140, 0.050, 0.250, 0.0, 0.9943, 5000.0, 155.300, 0.0, 1)

  -- City Presets
  EFX_REVERB_PRESET_CITY_STREETS   : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.780, 0.3162, 0.7079, 0.8913, 1.790, 1.120, 0.910, 0.2818, 0.0460, (0.0, 0.0, 0.0), 0.1995, 0.0280, (0.0, 0.0, 0.0), 0.250, 0.200, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_CITY_SUBWAY    : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.740, 0.3162, 0.7079, 0.8913, 3.010, 1.230, 0.910, 0.7079, 0.0460, (0.0, 0.0, 0.0), 1.2589, 0.0280, (0.0, 0.0, 0.0), 0.1250, 0.210, 0.250, 0.0, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_CITY_MUSEUM    : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.820, 0.3162, 0.1778, 0.1778, 3.280, 1.400, 0.570, 0.2512, 0.0390, (0.0, 0.0, -0.0), 0.8913, 0.0340, (0.0, 0.0, 0.0), 0.130, 0.170, 0.250, 0.0, 0.9943, 2854.3999, 107.500, 0.0, 0)
  EFX_REVERB_PRESET_CITY_LIBRARY   : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.820, 0.3162, 0.2818, 0.0891, 2.760, 0.890, 0.410, 0.3548, 0.0290, (0.0, 0.0, -0.0), 0.8913, 0.020, (0.0, 0.0, 0.0), 0.130, 0.170, 0.250, 0.0, 0.9943, 2854.3999, 107.500, 0.0, 0)
  EFX_REVERB_PRESET_CITY_UNDERPASS : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.820, 0.3162, 0.4467, 0.8913, 3.570, 1.120, 0.910, 0.3981, 0.0590, (0.0, 0.0, 0.0), 0.8913, 0.0370, (0.0, 0.0, 0.0), 0.250, 0.140, 0.250, 0.0, 0.9920, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_CITY_ABANDONED : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.690, 0.3162, 0.7943, 0.8913, 3.280, 1.170, 0.910, 0.4467, 0.0440, (0.0, 0.0, 0.0), 0.2818, 0.0240, (0.0, 0.0, 0.0), 0.250, 0.200, 0.250, 0.0, 0.9966, 5000.0, 250.0, 0.0, 1)

  -- Misc. Presets
  EFX_REVERB_PRESET_DUSTYROOM      : constant EFXEAXREVERBPROPERTIES := (0.3645, 0.560, 0.3162, 0.7943, 0.7079, 1.790, 0.380, 0.210, 0.5012, 0.0020, (0.0, 0.0, 0.0), 1.2589, 0.0060, (0.0, 0.0, 0.0), 0.2020, 0.050, 0.250, 0.0, 0.9886, 13046.0, 163.300, 0.0, 1)
  EFX_REVERB_PRESET_CHAPEL         : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.840, 0.3162, 0.5623, 1.0000, 4.620, 0.640, 1.230, 0.4467, 0.0320, (0.0, 0.0, 0.0), 0.7943, 0.0490, (0.0, 0.0, 0.0), 0.250, 0.0, 0.250, 0.110, 0.9943, 5000.0, 250.0, 0.0, 1)
  EFX_REVERB_PRESET_SMALLWATERROOM : constant EFXEAXREVERBPROPERTIES := (1.0000, 0.700, 0.3162, 0.4477, 1.0000, 1.510, 1.250, 1.140, 0.8913, 0.0200, (0.0, 0.0, 0.0), 1.4125, 0.0300, (0.0, 0.0, 0.0), 0.1790, 0.150, 0.8950, 0.190, 0.9920, 5000.0, 250.0, 0.0, 0)

  -----------------
  -- Subprograms --
  -----------------

  -- 2.7. AL Errors
  function alGetError return Int_Unsigned_C -- ALenum
                      with Import => True, Convention => StdCall, External_Name => "alGetError";

  -- 2.8. Controlling AL Execution
  procedure alEnable (target : Int_Unsigned_C) -- ALenum
                      with Import => True, Convention => StdCall, External_Name => "alEnable";

  -- 2.8. Controlling AL Execution
  procedure alDisable (target : Int_Unsigned_C) -- ALenum
                       with Import => True, Convention => StdCall, External_Name => "alDisable";

  -- 2.8. Controlling AL Execution
  function alIsEnabled (target : Int_Unsigned_C) -- ALenum
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
  function alIsBuffer (bid : Int_Unsigned_C) -- ALuint
                       return Int_8_Unsigned_C  -- ALboolean
                       with Import => True, Convention => StdCall, External_Name => "alIsBuffer";

  -- 2.14. Validating an Object Name
  function alIsSource (sid : Int_Unsigned_C) -- ALuint
                       return Int_8_Unsigned_C  -- ALboolean
                       with Import => True, Convention => StdCall, External_Name => "alIsSource";

  -- 3.1.1. Simple Queries
  procedure alGetBooleanv (paramName : Int_Unsigned_C;           -- ALenum
                           dest      : Ptr_Array_Int_8_Unsigned_C)  -- ALboolean*
                           with Import => True, Convention => StdCall, External_Name => "alGetBooleanv";

  -- 3.1.1. Simple Queries
  function alGetBoolean (paramName : Int_Unsigned_C) -- ALenum
                         return Int_8_Unsigned_C        -- ALboolean
                         with Import => True, Convention => StdCall, External_Name => "alGetBoolean";

  -- 3.1.1. Simple Queries
  procedure alGetIntegerv (paramName : Int_Unsigned_C;         -- ALenum
                           dest      : Ptr_Array_Int_32_Signed_C) -- ALint*
                           with Import => True, Convention => StdCall, External_Name => "alGetIntegerv";

  -- 3.1.1. Simple Queries
  function alGetInteger (paramName : Int_Unsigned_C) -- ALenum
                         return Int_C                   -- ALint
                         with Import => True, Convention => StdCall, External_Name => "alGetInteger";

  -- 3.1.1. Simple Queries
  procedure alGetFloatv (paramName : Int_Unsigned_C;   -- ALenum
                         dest      : Ptr_Array_Real_32_C) -- ALfloat*
                         with Import => True, Convention => StdCall, External_Name => "alGetFloatv";

  -- 3.1.1. Simple Queries
  function alGetFloat (paramName : Int_Unsigned_C) -- ALenum
                       return Real_32_C               -- ALfloat
                       with Import => True, Convention => StdCall, External_Name => "alGetFloat";

  -- 3.1.1. Simple Queries
  procedure alGetDoublev (paramName : Int_Unsigned_C;   -- ALenum
                          dest      : Ptr_Array_Real_64_C) -- ALdouble*
                          with Import => True, Convention => StdCall, External_Name => "alGetDoublev";

  -- 3.1.1. Simple Queries
  function alGetDouble (paramName : Int_Unsigned_C) -- ALenum
                        return Real_64_C               -- ALdouble
                        with Import => True, Convention => StdCall, External_Name => "alGetDouble";

  -- 3.4. Attenuation By Distance
  procedure alDistanceModel (distanceModel : Int_Unsigned_C) -- ALenum
                             with Import => True, Convention => StdCall, External_Name => "alDistanceModel";

  -- 3.5.2. Velocity Dependent Doppler Effect
  procedure alDopplerFactor (Value : Real_32_C) -- ALfloat
                             with Import => True, Convention => StdCall, External_Name => "alDopplerFactor";

  -- 3.5.2. Velocity Dependent Doppler Effect
  procedure alSpeedOfSound (Value : Real_32_C) -- ALfloat
                            with Import => True, Convention => StdCall, External_Name => "alSpeedOfSound";

  -- 4.2.2. Changing Listener Attributes
  procedure alListenerf (param : Int_Unsigned_C; -- ALenum
                         Value : Real_32_C)         -- ALfloat
                         with Import => True, Convention => StdCall, External_Name => "alListenerf";

  -- 4.2.2. Changing Listener Attributes
  procedure alListener3f (param  : Int_Unsigned_C; -- ALenum
                          value1 : Real_32_C;         -- ALfloat
                          value2 : Real_32_C;         -- ALfloat
                          value3 : Real_32_C)         -- ALfloat
                          with Import => True, Convention => StdCall, External_Name => "alListener3f";

  -- 4.2.2. Changing Listener Attributes
  procedure alListeneri (param : Int_Unsigned_C; -- ALenum
                         Value : Int_C)             -- ALint
                         with Import => True, Convention => StdCall, External_Name => "alListeneri";

  -- 4.2.2. Changing Listener Attributes
  procedure alListenerfv (param  : Int_Unsigned_C; -- ALenum
                          value1 : Int_C;             -- ALint
                          value2 : Int_C;             -- ALint
                          value3 : Int_C)             -- ALint
                          with Import => True, Convention => StdCall, External_Name => "alListener3i";

  -- 4.2.2. Changing Listener Attributes
  procedure alListenerfv (param  : Int_Unsigned_C;   -- ALenum
                          values : Ptr_Array_Real_32_C) -- ALfloat*
                          with Import => True, Convention => StdCall, External_Name => "alListenerfv";

  -- 4.2.2. Changing Listener Attributes
  procedure alListeneriv (param  : Int_Unsigned_C;         -- ALenum
                          values : Ptr_Array_Int_32_Signed_C) -- ALint*
                          with Import => True, Convention => StdCall, External_Name => "alListeneriv";

  -- 4.2.3. Querying Listener Attributes
  procedure alGetListenerf (param : Int_Unsigned_C; -- ALenum
                            Value : Pointer) --
                            with Import => True, Convention => StdCall, External_Name => "alGetListenerf";

  -- 4.2.3. Querying Listener Attributes
  procedure alGetListener3f (param : Int_Unsigned_C; -- ALenum
                             value1 : Pointer; --
                             value2 : Pointer; --
                             value3 : Pointer) --
                             with Import => True, Convention => StdCall, External_Name => "alGetListener3f";

  -- 4.2.3. Querying Listener Attributes
  procedure alGetListenerfv (param : Int_Unsigned_C; -- ALenum
                             values : Pointer) --
                             with Import => True, Convention => StdCall, External_Name => "alGetListenerfv";

  -- 4.2.3. Querying Listener Attributes
  procedure alGetListeneri (param : Int_Unsigned_C; -- ALenum
                            Value : Pointer) --
                            with Import => True, Convention => StdCall, External_Name => "alGetListeneri";

  -- 4.2.3. Querying Listener Attributes
  procedure alGetListener3i (param  : Int_Unsigned_C; -- ALenum
                             value1 : Pointer; --
                             value2 : Pointer; --
                             value3 : Pointer) --
                             with Import => True, Convention => StdCall, External_Name => "alGetListener3i";

  -- 4.2.3. Querying Listener Attributes
  procedure alGetListeneriv (param : Int_Unsigned_C; -- ALenum
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
  procedure alSourcef (sid   : Int_Unsigned_C; -- ALuint
                       param : Int_Unsigned_C; -- ALenum
                       Value : Real_32_C)         -- ALfloat
                       with Import => True, Convention => StdCall, External_Name => "alSourcef";

  -- 4.3.3. Changing Source Attributes
  procedure alSource3f (sid    : Int_Unsigned_C; -- ALuint
                        param  : Int_Unsigned_C; -- ALenum
                        value1 : Real_32_C;         -- ALfloat
                        value2 : Real_32_C;         -- ALfloat
                        value3 : Real_32_C)         -- ALfloat
                        with Import => True, Convention => StdCall, External_Name => "alSource3f";

  -- 4.3.3. Changing Source Attributes
  procedure alSourcei (sid   : Int_Unsigned_C; -- ALuint
                       param : Int_Unsigned_C; -- ALenum
                       Value : Int_C)             -- ALint
                       with Import => True, Convention => StdCall, External_Name => "alSourcei";

  -- 4.3.3. Changing Source Attributes
  procedure alSource3i (sid    : Int_Unsigned_C; -- ALuint
                        param  : Int_Unsigned_C; -- ALenum
                        value1 : Int_C; -- ALint
                        value2 : Int_C; -- ALint
                        value3 : Int_C) -- ALint
                        with Import => True, Convention => StdCall, External_Name => "alSource3i";

  -- 4.3.3. Changing Source Attributes
  procedure alSourcefv (sid    : Int_Unsigned_C; -- ALuint
                        param  : Int_Unsigned_C; -- ALenum
                        values : Pointer) --
                        with Import => True, Convention => StdCall, External_Name => "alSourcefv";

  -- 4.3.3. Changing Source Attributes
  procedure alSourceiv (sid : Int_Unsigned_C; -- ALuint
                        param : Int_Unsigned_C; -- ALenum
                        values : Pointer) --
                        with Import => True, Convention => StdCall, External_Name => "alSourceiv";

  -- 4.3.4. Querying Source Attributes
  procedure alGetSourcef (sid   : Int_Unsigned_C; -- ALuint
                          param : Int_Unsigned_C; -- ALenum
                          value : Poin) --
                          with Import => True, Convention => StdCall, External_Name => "alGetSourcef";

  -- 4.3.4. Querying Source Attributes
  procedure alGetSource3f (sid    : Int_Unsigned_C; -- ALuint
                           param  : Int_Unsigned_C; -- ALenum
                           value1 : Pointer; --
                           value2 : Pointer; --
                           value3 : Pointer) --
                           with Import => True, Convention => StdCall, External_Name => "alGetSource3f";

  -- 4.3.4. Querying Source Attributes
  procedure alGetSourcefv (sid    : Int_Unsigned_C; -- ALuint
                           param  : Int_Unsigned_C; -- ALenum
                           values : Pointer) --
                           with Import => True, Convention => StdCall, External_Name => "alGetSourcefv";

  -- 4.3.4. Querying Source Attributes
  procedure alGetSourcei (sid   : Int_Unsigned_C; -- ALuint
                          param : Int_Unsigned_C; -- ALenum
                          Value : Pointer) --
                          with Import => True, Convention => StdCall, External_Name => "alGetSourcei";

  -- 4.3.4. Querying Source Attributes
  procedure alGetSource3i (sid    : Int_Unsigned_C; -- ALuint
                           param  : Int_Unsigned_C; -- ALenum
                           value1 : Pointer; --
                           value2 : Pointer; --
                           value3 : Pointer) --
                           with Import => True, Convention => StdCall, External_Name => "alGetSource3i";

  -- 4.3.4. Querying Source Attributes
  procedure alGetSourceiv (sid    : Int_Unsigned_C; -- ALuint
                           param  : Int_Unsigned_C; -- ALenum
                           values : Pointer) --
                           with Import => True, Convention => StdCall, External_Name => "alGetSourceiv";

  -- 4.3.5. Queuing Buffers with a Source
  procedure alSourceQueueBuffers (sid        : Int_Unsigned_C; -- ALuint
                                  numEntries : Int_Size_C; -- ALsizei
                                  bids       : Pointer) --
                                  with Import => True, Convention => StdCall, External_Name => "alSourceQueueBuffers";

  -- 4.3.5. Queuing Buffers with a Source
  procedure alSourceUnqueueBuffers (sid        : Int_Unsigned_C; -- ALuint
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
  procedure alBufferData (bid    : Int_Unsigned_C; -- ALuint
                          format : Int_Unsigned_C; -- ALenum
                          data   : Ptr;               -- const ALvoid*
                          size   : Int_Size_C;        -- ALsizei
                          freq   : Int_Size_C)        -- ALsizei
                          with Import => True, Convention => StdCall, External_Name => "alBufferData";

  -- 5.3.2. Changing Buffer Attributes
  procedure alBufferf (bid   : Int_Unsigned_C; -- ALuint
                       param : Int_Unsigned_C; -- ALenum
                       Value : Real_32_C)         -- ALfloat
                       with Import => True, Convention => StdCall, External_Name => "alBufferf";

  -- 5.3.2. Changing Buffer Attributes
  procedure alBuffer3f (bid    : Int_Unsigned_C; -- ALuint
                        param  : Int_Unsigned_C; -- ALenum
                        value1 : Real_32_C;         -- ALfloat
                        value2 : Real_32_C;         -- ALfloat
                        value3 : Real_32_C)         -- ALfloat
                        with Import => True, Convention => StdCall, External_Name => "alBuffer3f";

  -- 5.3.2. Changing Buffer Attributes
  procedure alBufferi (bid : Int_Unsigned_C; -- ALuint
                       param : Int_Unsigned_C; -- ALenum
                       Value : Int_C) -- ALint
                       with Import => True, Convention => StdCall, External_Name => "alBufferi";

  -- 5.3.2. Changing Buffer Attributes
  procedure alBuffer3i (bid : Int_Unsigned_C; -- ALuint
                        param : Int_Unsigned_C; -- ALenum
                        value1 : Int_C; -- ALint
                        value2 : Int_C; -- ALint
                        value3 : Int_C) -- ALint
                        with Import => True, Convention => StdCall, External_Name => "alBuffer3i";

  -- 5.3.2. Changing Buffer Attributes
  procedure alBufferfv (bid : Int_Unsigned_C; -- ALuint
                        param : Int_Unsigned_C; -- ALenum
                        values : Pointer) --
                        with Import => True, Convention => StdCall, External_Name => "alBufferfv";

  -- 5.3.2. Changing Buffer Attributes
  procedure alBufferiv (bid : Int_Unsigned_C; -- ALuint
                        param : Int_Unsigned_C; -- ALenum
                        values : Pointer) --
                        with Import => True, Convention => StdCall, External_Name => "alBufferiv";

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBufferf (bid : Int_Unsigned_C; -- ALuint
                          param : Int_Unsigned_C; -- ALenum
                          Value : Pointer) --
                          with Import => True, Convention => StdCall, External_Name => "alGetBufferf";

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBuffer3f (bid : Int_Unsigned_C; -- ALuint
                           param : Int_Unsigned_C; -- ALenum
                           value1 : Pointer; --
                           value2 : Pointer; --
                           value3 : Pointer) --
                           with Import => True, Convention => StdCall, External_Name => "alGetBuffer3f";

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBufferfv (bid : Int_Unsigned_C; -- ALuint
                           param : Int_Unsigned_C; -- ALenum
                           values : Real_32_C) -- ALfloat
                           with Import => True, Convention => StdCall, External_Name => "alGetBufferfv";

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBufferi (bid : Int_Unsigned_C; -- ALuint
                          param : Int_Unsigned_C; -- ALenum
                          Value : Pointer) --
                          with Import => True, Convention => StdCall, External_Name => "alGetBufferi";

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBuffer3i (bid : Int_Unsigned_C; -- ALuint
                           param : Int_Unsigned_C; -- ALenum
                           value1 : Pointer; --
                           value2 : Pointer; --
                           value3 : Pointer) --
                           with Import => True, Convention => StdCall, External_Name => "alGetBuffer3i";

  -- 5.3.3. Querying Buffer Attributes
  procedure alGetBufferiv (bid    : Int_Unsigned_C; -- ALuint
                           param  : Int_Unsigned_C; -- ALCenum
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
                        return Int_Unsigned_C -- ALCenum
                        with Import => True, Convention => StdCall, External_Name => "alcGetError";

  -- 6.3.8. Integer Query
  procedure alcGetIntegerv (device : Ptr;                       -- ALCdevice*
                            param  : Int_Unsigned_C;         -- ALCenum
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

AL_API ALvoid AL_APIENTRY alBufferDataStatic(const ALint buffer, ALenum format, ALvoid *data, ALsizei len, ALsizei freq);
typedef void (AL_APIENTRY*LPALFOLDBACKCALLBACK)(ALenum,ALsizei);
typedef void (AL_APIENTRY*LPALREQUESTFOLDBACKSTART)(ALenum,ALsizei,ALsizei,ALfloat*,LPALFOLDBACKCALLBACK);
typedef void (AL_APIENTRY*LPALREQUESTFOLDBACKSTOP)(void);

AL_API void AL_APIENTRY alRequestFoldbackStart(ALenum mode,ALsizei count,ALsizei length,ALfloat *mem,LPALFOLDBACKCALLBACK callback);
AL_API void AL_APIENTRY alRequestFoldbackStop(void);
typedef void (AL_APIENTRY*LPALFOLDBACKCALLBACK)(ALenum,ALsizei);
typedef void (AL_APIENTRY*LPALREQUESTFOLDBACKSTART)(ALenum,ALsizei,ALsizei,ALfloat*,LPALFOLDBACKCALLBACK);
typedef void (AL_APIENTRY*LPALREQUESTFOLDBACKSTOP)(void);

AL_API void AL_APIENTRY alRequestFoldbackStart(ALenum mode,ALsizei count,ALsizei length,ALfloat *mem,LPALFOLDBACKCALLBACK callback);
AL_API void AL_APIENTRY alRequestFoldbackStop(void);
typedef void (AL_APIENTRY*LPALBUFFERSAMPLESSOFT)(ALuint,ALuint,ALenum,ALsizei,ALenum,ALenum,const ALvoid*);
typedef void (AL_APIENTRY*LPALBUFFERSUBSAMPLESSOFT)(ALuint,ALsizei,ALsizei,ALenum,ALenum,const ALvoid*);
typedef void (AL_APIENTRY*LPALGETBUFFERSAMPLESSOFT)(ALuint,ALsizei,ALsizei,ALenum,ALenum,ALvoid*);
typedef ALboolean (AL_APIENTRY*LPALISBUFFERFORMATSUPPORTEDSOFT)(ALenum);

AL_API void AL_APIENTRY alBufferSamplesSOFT(ALuint buffer, ALuint samplerate, ALenum internalformat, ALsizei samples, ALenum channels, ALenum type, const ALvoid *data);
AL_API void AL_APIENTRY alBufferSubSamplesSOFT(ALuint buffer, ALsizei offset, ALsizei samples, ALenum channels, ALenum type, const ALvoid *data);
AL_API void AL_APIENTRY alGetBufferSamplesSOFT(ALuint buffer, ALsizei offset, ALsizei samples, ALenum channels, ALenum type, ALvoid *data);
AL_API ALboolean AL_APIENTRY alIsBufferFormatSupportedSOFT(ALenum format);

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

  -- Effect object function types
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

  -- Filter object function types
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

  -- Auxiliary Effect Slot object function types
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