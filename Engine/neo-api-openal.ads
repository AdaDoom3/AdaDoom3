
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

-- Custom binding to the OpenAL API: https://web.archive.org/web/20160409112902/http://www.openal.org/documentation/openal-1.1-specification.pdf
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
end;