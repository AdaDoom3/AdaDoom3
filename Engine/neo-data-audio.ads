
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

-- Unified texture type definitions
package Neo.Data.Audio is

  -------------
  -- Formats --
  -------------

  type Format_Kind is (OPUS_Format); -- http://web.archive.org/web/20160811201320/https://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/

  ----------
  -- Clip --
  ----------

  type Clip_Kind (Foreground_Clip, Background_Clip, Music_Clip, Cinematic_Clip, Dialog_Clip);

  type Clip_State (Kind : Clip_Kind := Foreground_Clip; Do_Loop : Bool := False) is record
      Lead_In_Sample  : Str_Unbound := NULL_STR_UNBOUND;
      Lead_In_Volume,
      Max_Volume      : Real_Percent := 100.0;
      Screen_Shake    : Real_Percent := 10.0;
      Fade_Radius_Min : Real         := 1.0;
      Fade_Radius_Max : Real         := 2.0;
      Omnidirectional,
      No_Occlusion : Bool := False;
      Is_Global,
      Is_Private,
      Use_Center,
      Use_Left,
      Use_Right,
      Use_Back_Left,
      Use_Back_Right,
      Use_Subwoofer : Bool := True;
      case Do_Loop is
        when False =>
          Sample : Str_Unbound := NULL_STR_UNBOUND;
        when True =>
          Samples       : Vector_Str_Unbound.Unsafe.Vector;
          Same_Twice_OK : Bool := False; -- Must have more than 2 samples to be relevant
      end case;
    end record;
    
  --------
  -- IO --
  --------

  function Load (Path : Str) return Compressed_Image;
end;
