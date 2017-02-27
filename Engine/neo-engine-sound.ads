
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


-- 
package Neo.Engine.Sound is

  ----------
  -- Clip --
  ----------

  type Clip_Kind (Foreground_Clip, Background_Clip, Music_Clip, Cinematic_Clip, Dialog_Clip);

  type Clip_State (Kind : Clip_Kind := Foreground_Clip; Do_Loop : Bool := False) is record
      Lead_In_Sample   : Str_Unbound := NULL_STR_UNBOUND;
      Lead_In_Volume   : Percent     := 100.0;
      Max_Volume       : Percent     := 100.0;
      Screen_Shake     : Percent     := 10.0;
      Fade_Radius_Min  : Real        := 1.0;
      Fade_Radius_Max  : Real        := 2.0;
      Omnidirectional  : Bool        := False;
      No_Occlusion     : Bool        := False;
      Is_Global        : Bool        := True;
      Is_Private       : Bool        := True;
      Use_Center       : Bool        := True;
      Use_Left         : Bool        := True;
      Use_Right        : Bool        := True;
      Use_Back_Left    : Bool        := True;
      Use_Back_Right   : Bool        := True;
      Use_Subwoofer    : Bool        := True;
      case Do_Loop is
        when False =>
          Sample : Str_Unbound := NULL_STR_UNBOUND;
        when True =>
          Samples       : Vector_Str_Unbound.Unsafe.Vector;
          Same_Twice_OK : Bool := False; -- Must have more than 2 samples to be relevant
      end case;
    end record;

  -------------
  -- Emitter --
  -------------

  type Emitter_State (Kind : Emitter_Kind) is record
      Position       : Position_State := (others => <>);
      Is_Mute        : Bool           := False;
      Is_Paused      : Bool           := False;
      Is_Playing     : Bool           := False;
      Min_Distance   : Real           := 0.0;
      Max_Distance   : Real           := 0.0;
      Current_Volume : Percent        := 75.0;
      Current_Shake  : Percent        := 75.0;
    end record;

  package Vector_Emitter is new Vector (Emitter_State);
  Emitters : Vector_Emitter.Safe_Vector;

  --------------
  -- Listener --
  --------------

  

  package Music_Volume;

  package Effects_Volume;

  package Mute;

  Get_Shake 

  Get_Amplitude
end;