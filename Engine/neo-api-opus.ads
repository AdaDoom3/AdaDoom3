
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

-- Custom binding to the Opus codec API: https://web.archive.org/web/20151011202106/http://www.opus-codec.org/docs/opus_api-1.1.0.pdf
package Neo.API.Opus is

  -----------
  -- Types --
  -----------

  -- opus_uint16       Int_16_Unsigned_C
  -- opus_int16        Int_16_Signed_C
  -- opus_uint32       Int_32_Unsigned_C
  -- opus_int32        Int_C
  -- OpusEncoder*      Ptr
  -- OpusDecoder*      Ptr
  -- OpusMSEncoder*    Ptr
  -- OpusMSDecoder*    Ptr
  -- OpusRepacketizer* Ptr

  ---------------
  -- Constants --
  ---------------

  OPUS_MULTISTREAM_GET_ENCODER_STATE_REQUEST : constant Int_32_Unsigned_C := 5120; -- 0
  OPUS_MULTISTREAM_GET_DECODER_STATE_REQUEST : constant Int_32_Unsigned_C := 5122; -- 0

  ------------
  -- Macros --
  ------------

  -- #define OPUS_MULTISTREAM_GET_ENCODER_STATE (x,y) OPUS_MULTISTREAM_GET_ENCODER_STATE_REQUEST,
  --                                                  (((void)((x) == (opus_int32)0)),
  --                                                  (opus_int32)(x)),
  --                                                  ((y) + ((y) - (OpusEncoder**)(y)))

  -- #define PUS_MULTISTREAM_GET_DECODER_STATE (x,y) OPUS_MULTISTREAM_GET_DECODER_STATE_REQUEST,
  --                                                 (((void)((x) == (opus_int32)0)),
  --                                                 (opus_int32)(x)), __opus_check_int(x),
  --                                                 ((y) + ((y) - (OpusDecoder**)(y)))

  -----------------
  -- Subprograms --
  -----------------

  -- https://web.archive.org/web/20141013022344/http://opus-codec.org/docs/html_api/opus__multistream_8h.html#a8642aa9cf16115229a655574d832293b
  function opus_multistream_encoder_get_size (streams         : Int_C; -- int
                                              coupled_streams : Int_C) -- int
                                              return Int_C             -- opus_int32
                                              with Import => True, Convention => StdCall, External_Name => "opus_multistream_encoder_get_size"; 
                                              
  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#ga9bed3af8a080ab97b8af48007758fa14
  function opus_multistream_surround_encoder_get_size (channels : Int_C; -- int
                                                       mapping  : Int_C) -- int
                                                       return Int_C;     -- opus_int32
                                                       with Import => True, Convention => StdCall, External_Name => "opus_multistream_surround_encoder_get_size"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#gaeb64c648ed8155f824ca8d9a93ccecae
  function opus_multistream_encoder_create (Fs              : Int_C; -- opus_int32
                                            channels        : Int_C; -- int
                                            streams         : Int_C; -- int
                                            coupled_streams : Int_C; -- int
                                            mapping         : Ptr;   -- const unsigned char*
                                            application     : Int_C; -- int 
                                            error           : Int_C) -- int*
                                            return Ptr               -- OpusMSEncoder*
                                            with Import => True, Convention => StdCall, External_Name => "opus_multistream_encoder_create"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#ga8d87c08ecbe6ed4ce1ede4e58718b621
  function opus_multistream_surround_encoder_create (Fs              : Int_C;     -- opus_int32
                                                     channels        : Int_C;     -- int
                                                     mapping_family  : Int_C;     -- int
                                                     streams         : Ptr_Int_C; -- int*
                                                     coupled_streams : Ptr_Int_C; -- int*
                                                     mapping         : Ptr;       -- unsigned char*
                                                     application     : Int_C;     -- int 
                                                     error           : Ptr_Int_C) -- int*
                                                     return Ptr                   -- OpusMSEncoder*
                                                     with Import => True, Convention => StdCall, External_Name => "opus_multistream_surround_encoder_create"; 

  -- https://web.archive.org/web/20141013022344/http://opus-codec.org/docs/html_api/opus__multistream_8h.html#ac29b1055be0cc29af5729ad55b9ead53
  function opus_multistream_encoder_init (st              : Ptr;   -- OpusMSEncoder*
                                          Fs              : Int_C; -- opus_int32
                                          channels        : Int_C; -- int 
                                          streams         : Int_C; -- int 
                                          coupled_streams : Int_C; -- int 
                                          mapping         : Ptr;   -- const unsigned char*
                                          application     : Int_C) -- int 
                                          return Int_C             -- int
                                          with Import => True, Convention => StdCall, External_Name => "opus_multistream_encoder_init"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#ga993bfbae18b2dbfd641e50e51cef4484
  function opus_multistream_surround_encoder_init (st              : Ptr;       -- OpusMSEncoder*
                                                   Fs              : Int_C;     -- opus_int32 
                                                   channels        : Int_C;     -- int 
                                                   mapping_family  : Int_C;     -- int 
                                                   streams         : Ptr_Int_C; -- int*
                                                   coupled_streams : Ptr_Int_C; -- int*
                                                   mapping         : Ptr;       -- unsigned char*
                                                   application     : Int_C)     -- int
                                                   return Int_C                 -- int 
                                                   with Import => True, Convention => StdCall, External_Name => "opus_multistream_surround_encoder_init"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#ga9e1000f220d9872e9f3b6e4f2417b445
  function opus_multistream_encode (st             : Ptr;                 -- OpusMSEncoder*
                                    pcm            : Ptr_Int_16_Signed_C; -- const opus_int16*
                                    frame_size     : Int_C;               -- int
                                    data           : Ptr;                 -- unsigned char*
                                    max_data_bytes : Int_C)               -- opus_int32
                                    return Int_C                          -- int
                                    with Import => True, Convention => StdCall, External_Name => "opus_multistream_encode"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#gaff832211e572536941b9d6094f9f42ce
  function opus_multistream_encode_float (st             : Ptr;           -- OpusMSEncoder*
                                          pcm            : Ptr_Real_32_C; -- const float*
                                          frame_size     : Int_C;         -- int
                                          data           : Ptr;           -- unsigned char*
                                          max_data_bytes : Int_C)         -- opus_int32
                                          return Int_C                    -- int 
                                          with Import => True, Convention => StdCall, External_Name => "opus_multistream_encode_float"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#gaec819b8d4b38350aba6959cee7d33f94
  procedure opus_multistream_encoder_destroy (st : Ptr) -- OpusMSEncoder*
                                              with Import => True, Convention => StdCall, External_Name => "opus_multistream_encoder_destroy"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#gae14328330c548dede66c494f51e33707
  function opus_multistream_encoder_ctl (st      : Ptr;   -- OpusMSEncoder*
                                         request : Int_C) -- int
                                                          -- ...
                                         return Int_C     -- int 
                                         with Import => True, Convention => StdCall, External_Name => "opus_multistream_encoder_ctl"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#ga38d745963e7903890c80278bc2569c39
  function opus_multistream_decoder_get_size (streams         : Int_C; -- int
                                              coupled_streams : Int_C) -- int
                                              return Int_C             -- opus_int32 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#ga3c0e342774174c471e61cedba53755c9
  function opus_multistream_decoder_create (Fs              : Int_C;     -- opus_int32 
                                            channels        : Int_C;     -- int
                                            streams         : Int_C;     -- int 
                                            coupled_streams : Int_C;     -- int 
                                            mapping         : Ptr;       -- const unsigned char*
                                            error           : Ptr_Int_C) -- int*
                                            return Ptr                   -- OpusMSDecoder*
                                            with Import => True, Convention => StdCall, External_Name => "opus_multistream_decoder_create"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#ga09a4d14fc497d4f6fbe76bd1c5d45436
  function opus_multistream_decoder_init (st              : Ptr;   -- OpusMSDecoder*
                                          Fs              : Int_C; -- opus_int32 
                                          channels        : Int_C; -- int 
                                          streams         : Int_C; -- int 
                                          coupled_streams : Int_C; -- int 
                                          mapping         : Ptr;   -- const unsigned char*
                                          return Int_C             -- int
                                          with Import => True, Convention => StdCall, External_Name => "opus_multistream_decoder_init"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#gaa4b89541efe01970cf52e4a336db3ad0
  function opus_multistream_decode (st         : Ptr;                 -- OpusMSDecoder*
                                    data       : Ptr;                 -- const unsigned char*
                                    len        : Int_C;               -- opus_int32
                                    pcm        : Ptr_Int_16_Signed_C; -- opus_int16*
                                    frame_size : Int_C;               -- int
                                    decode_fec : Int_C)               -- int 
                                    return Int_C                      -- int
                                    with Import => True, Convention => StdCall, External_Name => "opus_multistream_decode"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#ga620e67c67872f8ea0b67a200c729630a
  function opus_multistream_decode_float (st         : Ptr;         -- OpusMSDecoder*
                                          data       : Ptr;         -- const unsigned char*
                                          len        : Int_C;       -- opus_int32
                                          pcm        : Ptr_Real_32; -- float*
                                          frame_size : Int_C;       -- int
                                          decode_fec : Int_C)       -- int
                                          return Int_C              -- int
                                          with Import => True, Convention => StdCall, External_Name => "opus_multistream_decode_float"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#ga4b3dca8d46e5868cc133f3f6d2b57688
  function opus_multistream_decoder_ctl (st      : Ptr;   -- OpusMSDecoder*
                                         request : Int_C) -- int
                                                          -- ... 
                                         return Int_C     -- int
                                         with Import => True, Convention => StdCall, External_Name => "opus_multistream_decoder_ctl"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__multistream.html#gaaec72b484eabc78d7869221c6d2ce080
  procedure opus_multistream_decoder_destroy (st : Ptr) -- OpusMSDecoder*
                                             with Import => True, Convention => StdCall, External_Name => "opus_multistream_decoder_destroy"; 

  -- https://web.archive.org/web/20150618131942/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__encoder.html#gaefeb7dc1d6e3b59dea5ea674c86e9c18
  function opus_encoder_get_size (channels : Int_C) -- int
                                  return Int_C      -- int
                                  with Import => True, Convention => StdCall, External_Name => "opus_encoder_get_size"; 

  -- https://web.archive.org/web/20150618131942/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__encoder.html#gaa89264fd93c9da70362a0c9b96b9ca88
  function opus_encoder_create (Fs          : Int_C;     -- opus_int32
                                channels    : Int_C;     -- int
                                application : Int_C;     -- int
                                error       : Ptr_Int_C) -- int*
                                return Ptr               -- OpusEncoder* 
                                with Import => True, Convention => StdCall, External_Name => "opus_encoder_create"; 

  -- https://web.archive.org/web/20150618131942/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__encoder.html#ga515db1c267a7421dacaad3610f79eb79
  function opus_encoder_init (st          : Ptr;   -- OpusEncoder*
                              Fs          : Int_C; -- opus_int32 
                              channels    : Int_C; -- int
                              application : Int_C) -- int
                              return Int_C         -- int
                              with Import => True, Convention => StdCall, External_Name => "opus_encoder_init"; 

  -- https://web.archive.org/web/20150618131942/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__encoder.html#gad2d6bf6a9ffb6674879d7605ed073e25
  function opus_encode (st             : Ptr;                 -- OpusEncoder*
                        pcm            : Ptr_Int_16_Signed_C; -- const opus_int16*
                        frame_size     : Int_C;               -- int 
                        data           : Ptr;                 -- unsigned char*
                        max_data_bytes : Int_C)               -- opus_int32
                        return Int_C                          -- opus_int32
                        with Import => True, Convention => StdCall, External_Name => "opus_encode"; 

  -- https://web.archive.org/web/20150618131942/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__encoder.html#ga4ae9905859cd241ef4bb5c59cd5e5309
  function opus_encode_float (st             : Ptr;           -- OpusEncoder*
                              pcm            : Ptr_Real_32_C; -- const float*
                              frame_size     : Int_C;         -- int 
                              data           : Ptr;           -- unsigned char*
                              max_data_bytes : Int_C)         -- opus_int32
                              return Int_C                    -- opus_int32
                              with Import => True, Convention => StdCall, External_Name => "opus_encode_float"; 

  -- https://web.archive.org/web/20150618131942/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__encoder.html#ga5f4c05b4b51cdffec5a55dbf17bbfa1c
  procedure opus_encoder_destroy (st : Ptr) -- OpusEncoder*
                                  with Import => True, Convention => StdCall, External_Name => "opus_encoder_destroy"; 

  -- https://web.archive.org/web/20150618131942/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__encoder.html#ga164cbb0425238961919adf1db67949df
  function opus_encoder_ctl (st      : Ptr;   -- OpusEncoder*
                             request : Int_C) -- int 
                                              -- ... 
                             return Int_C     -- int
                             with Import => True, Convention => StdCall, External_Name => "opus_encoder_ctl"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#gac918415b2ee21add75b7f867ce235011
  function opus_decoder_get_size (channels : Int_C) -- int  
                                  return Int_C      -- int
                                  with Import => True, Convention => StdCall, External_Name => "opus_decoder_get_size"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#ga753f6fe0b699c81cfd47d70c8e15a0bd
  function opus_decoder_create (Fs       : Int_C;     -- opus_int32 
                                channels : Int_C;     -- int  
                                error    : Ptr_Int_C) -- int*
                                return Ptr            -- OpusDecoder*
                                with Import => True, Convention => StdCall, External_Name => "opus_decoder_create"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#ga939156d1f561c4273d5c62fa9c235a01
  function opus_decoder_init (st       : Ptr;   -- OpusDecoder*
                              Fs       : Int_C; -- opus_int32 
                              channels : Int_C) -- int  
                              return Int_C      -- int
                              with Import => True, Convention => StdCall, External_Name => "opus_decoder_init"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#ga7d1111f64c36027ddcb81799df9b3fc9
  function opus_decode (st         : Ptr;                 -- OpusDecoder*
                        data       : Ptr;                 -- const unsigned char*
                        len        : Int_C;               -- opus_int32 
                        pcm        : Ptr_Int_16_Signed_C; -- opus_int16*
                        frame_size : Int_C;               -- int 
                        decode_fec : Int_C)               -- int 
                        return Int_C                      -- int
                        with Import => True, Convention => StdCall, External_Name => "opus_decode"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#ga9c554b8c0214e24733a299fe53bb3bd2
  function opus_decode_float (st         : Ptr;           -- OpusDecoder*
                              data       : Ptr;           -- const unsigned char*
                              len        : Int_C;         -- opus_int32 
                              pcm        : Ptr_Real_32_C; -- float*
                              frame_size : Int_C;         -- int 
                              decode_fec : Int_C)         -- int 
                              return Int_C                -- int
                              with Import => True, Convention => StdCall, External_Name => "opus_decode_float"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#ga2d492844b4f66e7f34d85870f13d4846
  function opus_decoder_ctl (st      : Ptr;   -- OpusDecoder*
                             request : Int_C) -- int 
                                              -- ... 
                             return Int_C     -- int
                             with Import => True, Convention => StdCall, External_Name => "opus_decoder_ctl"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#gafebf4cb3c29c9317cac385446a76e36e
  procedure opus_decoder_destroy (st : Ptr); -- OpusDecoder*
                                  with Import => True, Convention => StdCall, External_Name => "opus_decoder_destroy"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#ga9d0054936a3345865632b04729cd368c
  function opus_packet_parse (data           : Ptr;                           -- const unsigned char*
                              len            : Int_C;                         -- opus_int32 
                              out_toc        : Ptr;                           -- unsigned char*
                              frames         : Ptr;                           -- const unsigned char* [48]
                              size           : Array_Int_16_Signed_C (1..48); -- opus_int16 [48]
                              payload_offset : Ptr_Int_C)                     -- int*
                              return Int_C                                    -- int
                              with Import => True, Convention => StdCall, External_Name => "opus_packet_parse"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#ga89b9375b6ff5e15f07fdc5d0cf4f0053
  function opus_packet_get_bandwidth (data : Ptr)  -- const unsigned char*
                                      return Int_C -- int
                                      with Import => True, Convention => StdCall, External_Name => "opus_packet_get_bandwidth"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#gab2a955acced631c6cb7876bbdc7953d4
  function opus_packet_get_samples_per_frame (data : Ptr;   -- const unsigned char*
                                              Fs   : Int_C) -- opus_int32
                                              return Int_C  -- int
                                              with Import => True, Convention => StdCall, External_Name => "opus_packet_get_samples_per_frame";

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#ga4209376ddf8cc3379767e1749e1ef26d
  function opus_packet_get_nb_channels (data : Ptr)  -- const unsigned char*
                                        return Int_C -- int
                                        with Import => True, Convention => StdCall, External_Name => "opus_packet_get_nb_channels"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#ga064cb2ed9e77a934cd7db6c13b02c584
  function opus_packet_get_nb_frames (packet : Ptr;   -- const unsigned char[]
                                      len    : Int_C) -- opus_int32
                                      return Int_C    -- int
                                      with Import => True, Convention => StdCall, External_Name => "opus_packet_get_nb_frames"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#ga3cfec8b0bed7789ebd88c3b3370d337b
  function opus_packet_get_nb_samples (packet : Ptr;   -- const unsigned char[]
                                       len    : Int_C; -- opus_int32 
                                       Fs     : Int_C) -- opus_int32
                                       return Int_C    -- int
                                       with Import => True, Convention => StdCall, External_Name => "opus_packet_get_nb_samples"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#ga659135a16060f85908f63443a2325118
  function opus_decoder_get_nb_samples (dec    : Ptr;   -- const OpusDecoder*
                                        packet : Ptr;   -- const unsigned char[]
                                        len    : Int_C) -- opus_int32
                                        return Int_C    -- int
                                        with Import => True, Convention => StdCall, External_Name => "opus_decoder_get_nb_samples"; 

  -- https://web.archive.org/web/20160426200936/https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__decoder.html#gaff99598b352e8939dded08d96e125e0b
  procedure opus_pcm_soft_clip (pcm          : Ptr_Real_32_C; -- float*
                                frame_size   : Int_C;         -- int  
                                channels     : Int_C;         -- int 
                                softclip_mem : Ptr_Real_32_C) -- float*
                                with Import => True, Convention => StdCall, External_Name => "opus_pcm_soft_clip"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#ga35c8fc05764748d187c62fc50e812d06
  function opus_repacketizer_get_size return Int_C -- int
                                      with Import => True, Convention => StdCall, External_Name => "opus_repacketizer_get_size"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#gab42ff7c3f8a49ff5029fcf60f3b853f0
  function opus_repacketizer_init (rp : Ptr)  -- OpusRepacketizer*
                                   return Ptr -- OpusRepacketizer*
                                   with Import => True, Convention => StdCall, External_Name => "opus_repacketizer_init"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#ga6f8813666ef851550ecf8658a731ff7d
  function opus_repacketizer_create return Ptr -- OpusRepacketizer*
                                    with Import => True, Convention => StdCall, External_Name => "opus_repacketizer_create"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#gadb08b25d2a29a559c35774cfe2a1b886
  procedure opus_repacketizer_destroy (rp : Ptr) -- OpusRepacketizer*
                                       with Import => True, Convention => StdCall, External_Name => "opus_repacketizer_destroy"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#ga2840dd56bfa37f8c6874355b9ce8fb46
  function opus_repacketizer_cat (rp   : Ptr;   -- OpusRepacketizer*
                                  data : Ptr;   -- const unsigned char*
                                  len  : Int_C) -- opus_int32
                                  return Int_C  -- int
                                  with Import => True, Convention => StdCall, External_Name => "opus_repacketizer_cat"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#gac591b550d92125b4abfa11a4b609f51f
  function opus_repacketizer_out_range (rp        : Ptr;   -- OpusRepacketizer*
                                        beginning : Int_C; -- int 
                                        ending    : Int_C; -- int 
                                        data      : Ptr;   -- unsigned char*
                                        maxlen    : Int_C) -- opus_int32
                                        return Int_C       -- opus_int32
                                        with Import => True, Convention => StdCall, External_Name => "opus_repacketizer_out_range"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#ga2cad98d04458aafdf6bb9f22f34ea7c0
  function opus_repacketizer_get_nb_frames (rp : Ptr)    -- OpusRepacketizer*
                                            return Int_C -- int
                                            with Import => True, Convention => StdCall, External_Name => "opus_repacketizer_get_nb_frames";

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#ga19ff1e91a8fa652380f972a224a26481
  function opus_repacketizer_out (rp     : Ptr;   -- OpusRepacketizer*
                                  data   : Ptr;   -- unsigned char*
                                  maxlen : Int_C) -- opus_int32
                                  return Int_C    -- opus_int32
                                  with Import => True, Convention => StdCall, External_Name => "opus_repacketizer_out"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#ga62eb1a9b887de0ce50f52eacb3609f13
  function opus_packet_pad (data    : Ptr;   -- unsigned char*
                            len     : Int_C; -- opus_int32 
                            new_len : Int_C) -- opus_int32
                            return Int_C     -- int
                            with Import => True, Convention => StdCall, External_Name => "opus_packet_pad"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#ga0f0d860cce598033814071327b626ecf
  function opus_packet_unpad (data : Ptr;   -- unsigned char*
                              len  : Int_C) -- opus_int32
                              return Int_C  -- opus_int32
                              with Import => True, Convention => StdCall, External_Name => "opus_packet_unpad"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#gad3559b66569ca4561fe4ba8f3b1cb336
  function opus_multistream_packet_pad (data       : Ptr;   -- unsigned char*
                                        len        : Int_C; -- opus_int32 
                                        new_len    : Int_C; -- opus_int32  
                                        nb_streams : Int_C) -- int  
                                        return Int_C        -- int
                                        with Import => True, Convention => StdCall, External_Name => "opus_multistream_packet_pad"; 

  -- https://mf4.xiph.org/jenkins/view/opus/job/opus/ws/doc/html/group__opus__repacketizer.html#gaa0f0e05884b48b2f901977478e74e067
  function opus_multistream_packet_unpad (data       : Ptr;   -- unsigned char*
                                          len        : Int_C; -- opus_int32 
                                          nb_streams : Int_C) -- int 
                                          return Int_C        -- opus_int32
                                          with Import => True, Convention => StdCall, External_Name => "opus_multistream_packet_unpad"; 
end;