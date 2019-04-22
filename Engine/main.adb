
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

with Is_Debugging;
with System;                     use System;
with Neo;                        use Neo;
with Neo.Data.Game;              use Neo.Data.Game;
with Neo.Core.Strings;           use Neo.Core.Strings;
with Neo.Core.Console;           use Neo.Core.Console;
with Neo.Engine;                 use Neo.Engine;
with Neo.Engine.Renderer;        use Neo.Engine.Renderer;
with Neo.World.CVars;            use Neo.World.CVars;
with Neo.World.Impulses;         use Neo.World.Impulses;
with Ada.Calendar;               use Ada.Calendar;
with Ada.Calendar.Formatting;    use Ada.Calendar.Formatting;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;

procedure Main is

  -- Game task
  procedure Menu is separate;
  package Menu_Tasks is new Tasks (Menu);
  Menu_Task : Menu_Tasks.Safe_Task;

  -- Set the windowing mode based on the Mode CVar
  procedure Set_Windowing_Mode is
    begin
      case Mode.Get is
        when Multi_Monitor_Mode => Initialize_Multi_Monitor;
        when Windowed_Mode      => Restore;
        when Fullscreen_Mode    => Maximize;
      end case;
      Clip_Cursor (Mode.Get = Fullscreen_Mode);
    end;

  -- Start of Main
  begin

    -- Make sure only one game instance exists at any one time
    if not Only_Instance then return; end if;

    -- Process arguments
    for I in 1..Argument_Count loop Submit (Replace (To_Str (Argument (I)), ".", " ")); end loop;

    -- Dump info
    Line ("Started by " & OS_Info.Username & " on " & To_Str (Image (Get_Start_Time)));
    Line ("Game: " & OS_Info.App_Name & WORD_SIZE'Wide_Image & (if Is_Debugging then " w/ debugging" else NULL_STR));
    Line ("Engine: " & NAME_ID & " " & VERSION);
    Line ("Compiler: " & "GNAT " & To_Str (GNAT_Info.Version) & " for " & S (OS_Info.Version));
    Line ("CPU: " & Get_CPU.Vendor'Wide_Image & " w/ (" & Get_Extensions_Image (Get_CPU) & ")");

    -- Handle debugging
    if Is_Debugging then -- Linked directly to the "Debug" scenario variable
      --Use_Ada_Put;
      null;--Initialize_Console;
    end if; Initialize_Console;

    -- Initialize
    Initialize_Configuration (S (OS_Info.App_Path) & PATH_CONFIG);
    --Initialize_Localization  (S (OS_Info.App_Path) & PATH_LOCALE);
    Initialize_Windowing;
    Set_Windowing_Mode;
    Initialize_Drawing;
    Initialize_Input;
    Menu_Task.Initialize;

    -- Set window interaction bindings
    Fullscreen.Bindings.Append   (Keyboard (F11_Key));
    Exit_To_Menu.Bindings.Append (Keyboard (Escape_Key));

    -- Block to catch runtime exceptions and contain the main loop
    declare
    Last_Time         : Time           := Clock;
    Saved_Pos         : Cursor_State   := Get_Cursor;
    Current_Mode      : Mode_Kind      := Mode.Get;
    Current_Menu      : Bool           := In_Menu.Get;
    Current_Cursor    : Cursor_Kind    := Cursor.Get;
    Current_Width     : Positive       := Window_Width.Get;
    Current_Height    : Positive       := Window_Height.Get;
    Current_Activated : Activated_Kind := Activated.Get;
    begin
      Input_Status.Occupied (True);
      while Update_Windowing and then Menu_Task.Running loop

        -- Set cursor
        if Current_Cursor /= Cursor.Get then
          if In_Menu.Get then Set_Cursor_Style (Cursor.Get); end if;
          Current_Cursor := Cursor.Get;
        end if;

        -- Handle mode switching
        if Current_Mode /= Mode.Get then
          if Current_Mode = Multi_Monitor_Mode then Finalize_Multi_Monitor; end if;
          Set_Windowing_Mode;
          if Mode.Get /= Multi_Monitor_Mode then -- Decide to set or save cursor position
            if not In_Main_Window             then Set_Cursor (Main_Window_Center); end if;
            if not In_Main_Window (Saved_Pos) then Saved_Pos := Main_Window_Center; end if;
          end if;
          Current_Mode := Mode.Get;
        end if;

        -- Handle menu entry
        if Current_Menu /= In_Menu.Get or Game_Entry_Status.Occupied then
          Game_Entry_Status.Occupied (False);
          if In_Menu.Get then
            Cursor_Status.Occupied (False);
            Exit_To_Menu.Disable;
            if Mode.Get /= Fullscreen_Mode then Clip_Cursor (False); end if;
            Set_Cursor_Style (Cursor.Get);
            Set_Cursor (Saved_Pos);
            Hide_Cursor (False);
            Enter_Game.Enable;
          else
            Saved_Pos := Get_Cursor;
            Enter_Game.Disable;
            Hide_Cursor;
            Clip_Cursor;
            Cursor_Status.Occupied (True);
            Exit_To_Menu.Enable;
          end if;
          Current_Menu := In_Menu.Get;
        end if;

        -- Activation is complex and branching can easily lead to problems, so handle cases explicitly
        if Current_Activated /= Activated.Get then
          case Activated.Get is
            when Other_Activated =>
              Input_Status.Occupied (True);

              case Mode.Get is
                when Windowed_Mode =>
                  Input_Status.Occupied (True);

                  if not In_Menu.Get then
                    Cursor_Status.Occupied (False);
                    Exit_To_Menu.Disable;
                    Enter_Game.Enable;
                  else Set_Cursor_Style (Cursor.Get); end if;
                when Multi_Monitor_Mode | Fullscreen_Mode =>
                  Maximize;

                  if Mode.Get = Multi_Monitor_Mode then Initialize_Multi_Monitor; end if;
                  if In_Menu.Get then Set_Cursor_Style (Cursor.Get);
                  elsif Mode.Get = Multi_Monitor_Mode then
                    Hide_Cursor;
                    Set_Cursor (Saved_Pos);
                    Cursor_Status.Occupied (True);
                  else
                    Hide_Cursor;
                    Clip_Cursor;
                    Set_Cursor (Saved_Pos);
                    Cursor_Status.Occupied (True);
                  end if;
              end case;
            when Click_Activated =>

              case Mode.Get is
                when Windowed_Mode =>
                  Input_Status.Occupied (True);

                  if In_Menu.Get then Set_Cursor_Style (Cursor.Get);
                  elsif In_Main_Window then
                    Saved_Pos := Get_Cursor;
                    Hide_Cursor;
                    Clip_Cursor;
                    Cursor_Status.Occupied (True);

                  -- Title-bar click in "game" mode
                  else
                    Exit_To_Menu.Disable;
                    Enter_Game.Enable;
                  end if;
                when others => null; end case;
            when Other_Deactivated | Minimize_Deactivated =>

              if not In_Menu.Get and In_Main_Window then Set_Cursor (Saved_Pos); end if;

              Input_Status.Occupied (False);
              Clip_Cursor (False);
              Set_Cursor_Style (System_Cursor);
              Hide_Cursor (False);

              if Activated.Get = Other_Deactivated then
                case Mode.Get is
                  when Multi_Monitor_Mode => Finalize_Multi_Monitor; Minimize;
                  when Fullscreen_Mode    => Minimize;
                  when others => null; end case;

              elsif Mode.Get = Multi_Monitor_Mode then Finalize_Multi_Monitor; end if;
          end case;
          Current_Activated := Activated.Get;
        end if;

        -- Restart the framebuffer if our resolution changed suddenly
        if Window_Width.Get /= Current_Width or Window_Height.Get /= Current_Height then
          --Resize (Window_Width.Get, Window_Height.Get);
          Current_Width  := Window_Width.Get;
          Current_Height := Window_Height.Get;
          Restart_Framebuffer;

        -- Also perform a restart if the framebuffer itself triggers the need for one
        elsif not Framebuffer_Status.Occupied then Restart_Framebuffer; end if;

        -- Save some cycles
        delay WINDOW_POLLING_DURATION - (Clock - Last_Time); Last_Time := Clock;
      end loop;

    -- Handle runtime exceptions
    exception when Occurrence: others =>
        Handle (Occurrence);
        if not Running_Console and then Ok (Icon    => Error_Icon,
                                            Message => Localize ("An error has occurred, would you like to view more information?"),
                                            Buttons => Yes_No_Buttons) then Initialize_Console; end if;
    end;

    -- Finalize
    Finalize_Drawing;
    Menu_Task.Finalize;
    Finalize_Input;
    Finalize_Windowing;
    Finalize_Configuration (S (OS_Info.App_Path) & PATH_CONFIG);
  end;





