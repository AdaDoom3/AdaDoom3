
--                                                                                                                               --
--                                                      N E O  E N G I N E                                                       --
--                                                                                                                               --
--                                               Copyright (C) 2020 Justin Squirek                                               --
--                                                                                                                               --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                      --
--                                                                                                                               --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                     --
--                                                                                                                               --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                --
--                                                                                                                               --

with Ada.Locales;      use Ada.Locales;
with Neo.Core.Strings; use Neo.Core.Strings;
with Neo.Core.Arrays;  use Neo.Core.Arrays;
with Neo.Core.Maps;    use Neo.Core.Maps;
with Neo.Core.Hashed;

-- Task safe IO and console interfacing types
package Neo.Core.Console is

  -- Submission and query of commands and cvars
  procedure Submit;
  procedure Submit      (Text : Str);
  function Autocomplete (Text : Str) return Array_Str_Unbound;
  
  -- Configuration and localization
  procedure Finalize_Configuration;
  procedure Initialize_Configuration (Path : Str);
  procedure Initialize_Localization  (Path : Str);
  function Localize (Item : Str) return Str; -- An english text string acts as a key to other languages

  --------
  -- IO --
  --------
  
  function Lines        return Int_64_Unsigned;
  function Log          return Str_Unbound;
  function Log          return Str is (S (Log));
  function Input_Entry  return Str;
  function Line_Size    return Positive;
  procedure Put         (Item : Str_Unbound);
  procedure Put         (Item : Char);                  
  procedure Put         (Item : Str);
  procedure Line        (Item : Char);
  procedure Title       (Item : Str);
  procedure Error       (Item : Str);
  procedure Warn        (Item : Str);
  procedure Info        (Item : Str);
  procedure Line        (Item : Str := "");
  procedure Line        (Item : Str_Unbound);
  procedure Line_Size   (Val  : Positive);
  procedure Input_Entry (Val  : Str);
  procedure Set_Put     (Val  : Ptr_Procedure_Put);
  procedure Use_Ada_Put; -- For debugging purposes, using this may crash if compiled in non-debugging mode

  -------------
  -- Command --
  -------------
  --
  -- The command package creates an interface to a procedure with arguments accessable to a user through the command line (Submit
  -- procedure). The callback for this command takes strings as arguments and may change some internal state (e.g. binding a
  -- player's controls to actions like jump or crouch). The Save parameter controls how an internal state used by a command will be
  -- stored in a configuration file.
  --
  -- Ex.
  --   procedure Callback_Bind (Args : Array_Str_Unbound);
  --   function Save_Binds return Str;
  --   package Bind is new Command ("bind", Callback_Bind, Save_Binds'Access);
  --   ...
  --   Submit ("bind right_mouse_button jump");
  --

  generic
    Name    : Str;
    Info    : Str;
    Usage   : Str;
    Arg_Min : Natural := 0;
    Arg_Max : Natural := Arg_Min;
    with procedure Callback (Args : Array_Str_Unbound);
    Save : access function return Str := null;
  package Command is end; 

  ----------
  -- CVar --
  ----------
  --
  -- The cvar package or "console variable" is the core of a game engine. It represents a changable setting that can be loaded
  -- from a configuration file or set via an in-game or external console window. In the Neo engine this package allows
  -- communication between between tasks (Set and Get) and from a user's input into the console's command line (Submit procedure).
  -- It is limited to only discrete types (integers and enumerations).
  --
  -- Ex.
  --   type Graphics_Kind is (Low_Quality, Medium_Quanlity, High_Quality, Ultra_Quality);
  --   package Graphics_Settings is new CVar (Name     => "vidquality",
  --                                          Help     => "Quality of graphics",
  --                                          Var_T    => Graphics_Kind,
  --                                          Initial  => Medium_Quanlity,
  --                                          Settable => True);
  --   ...
  --   Graphics_Settings.Set (Ultra_Quality);
  --   ...
  --   if Graphics_Settings.Get = Low_Quality then
  --   ... 
  --   Submit ("vidquality low_quality");
  --

  generic
    Name     : Str;
    Help     : Str;
    type Var_T is (<>);
    Initial  : Var_T := Var_T'First;
    Settable : Bool  := True;
  package CVar is
      function Get return Var_T;
      procedure Set (Val : Var_T);
    end;

  generic
    Name     : Str;
    Help     : Str;
    type Var_T is digits <>;
    Initial  : Var_T := 0.0;
    Settable : Bool  := True;
  package CVar_Real is
      function Get return Var_T;
      procedure Set (Val : Var_T);
    end;

  generic
    Name     : Str;
    Help     : Str;
    Initial  : Str  := "?";
    Settable : Bool := True;
  package CVar_Str is
      function Get return Str;
      function Get return Str_Unbound;
      procedure Set (Val : Str);
      procedure Set (Val : Str_Unbound);
    end;
end;
