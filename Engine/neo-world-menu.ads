
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

package Neo.Engine.Menu is

  UI_Elements : Treed_UI_Element.Safe.Tree;
  
  ----------
  -- Menu --
  ----------
  -- 
  -- 
  -- 

  type Alignment_Kind (Left_Alignment, Middle_Alightment, Right_Alightment);

  type Casing_Kind (Title_Case, Upper_Case, Lower_Case);

  type Element_Kind (Simple_Element, World_Element, Edit_Element, Choice_Element, Cinematic_Element,
                     Slider_Element, List_Element,  Bind_Element, Render_Element);

  type Element_State (Kind : Element_Kind) is record
      Border           : Border_State   := (others => <>);
      Orientation      : Orientation_4D := (others => <>);
      Do_Events        : Bool           := True;
      Is_Visible       : Bool           := True;
      Material         : Str_Unbound    := NULL_STR_UNBOUND;
      Text_Font        : Str_Unbound    := NULL_STR_UNBOUND;
      Text_Scale       : Real_Percent   := 15.0;
      Text_Color       : Color_State    := BLACK_COLOR;
      Text_Hover_Color : Color_State    := GREY_COLOR;
      Border_Color     : Color_State    := COLOR_LIGHT_BLUE;
      Background_Color : Color_State    := COLOR_SKY_BLUE;
      case Kind is 
        when World_Element =>
          Camera_Entity : Str_Unbound := NULL_STR_UNBOUND;
          Start_Time : Time;
          Elapsed    : Duration;
          Level      : Level_State := (others => <>);
        when Render_Element =>
          Model        : Str_Unbound := NULL_STR_UNBOUND;
          Animation    : Str_Unbound := NULL_STR_UNBOUND;
          Light_Color  : Color_State := COLOR_WHITE;
          Light_Origin : Point_3D    := 
          Model_Origin :
          Model_Rotate : 
          View_Offset  :
        when others =>
          CVar : Str := NULL_STR_UNBOUND;
          case Kind is
            when Choice_Element =>
              Arrow_ : Str_Unbound := NULL_STR_UNBOUND;
            when List_Element =>
              Multiple_Select   : Bool := False;
              Tab_Offsets       : Vector_Positive.Unsafe.Vector;
              Tab_Aligns        : Vector_Alignment.Unsafe.Vector;
              Options           : Vector_Vector_Str_Unbound.Unsafe.Vector;
              Current_Selection : Vector_Positive.Unsafe.Vector;
            when Edit_Element =>
              Kind                : Edit_Kind := ;
              Maximum_Characters  : Positive  := 100;
              Is_Word_Wrapped     : Bool      := False;
              Is_Read_Only        : Bool      := False;
              Is_Bottom_Scrolling : Bool      := False;
              Is_Password         : Bool      := False;
            when Field_Element =>
            when Button_Element =>
            when Slider_Element =>
              Slider_Background : Str_Unbound := NULL_STR_UNBOUND;
          when others => null; end case;
      when others => null; end case;
    end record;

  -- Callbacks
  procedure On_Time           (Element : in out Element_State; Seconds : Duration) is abstract;
  procedure On_Frame          (Element : in out Element_State) is abstract;
  procedure On_Action         (Element : in out Element_State) is abstract;
  procedure On_Action_Release (Element : in out Element_State) is abstract;
  procedure On_Cursor_Enter   (Element : in out Element_State) is abstract;
  procedure On_Cursor_Exit    (Element : in out Element_State) is abstract;
  procedure On_Deactivate     (Element : in out Element_State) is abstract;
  procedure On_Activate       (Element : in out Element_State) is abstract;

  -- 
  
 
  procedure Rotate (Element : in out Element_State; To : Degrees;        Acceleration, Deceleration : Real_Percent := 0.0);
  procedure Move   (Element : in out Element_State; To : Point_2D;       Acceleration, Deceleration : Real_Percent := 0.0);
  procedure Move   (Element : in out Element_State; To : Border_State;   Acceleration, Deceleration : Real_Percent := 0.0);
  procedure Move   (Element : in out Element_State; To : Orientation_4D; Acceleration, Deceleration : Real_Percent := 0.0);
end;
