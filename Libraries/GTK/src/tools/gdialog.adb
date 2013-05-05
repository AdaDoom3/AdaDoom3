-----------------------------------------------------------------------
--                 GDialog - GtkAda Components                       --
--                                                                   --
--      Copyright (C) 2000 E. Briot, J. Brobecker and A. Charlet     --
--                  Copyright (C) 2009-2013, AdaCore                 --
--                                                                   --
-- Dialog is free software; you can redistribute it and/or modify it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  Display a GtkAda dialog on the screen with the contents of the standard
--  input file until an end of file is encountered.
--
--  Example using a unix-like shell:
--
--  $ cat << EOF | gtkada-dialog error justify_fill
--  > Merge of some changes failed. It usually means that some modified code
--  > is obsolete in the current project file.
--  >
--  > Files with the ".rej" extension have been generated to help merging
--  > manually if needed.
--  > EOF

with Gtk.Main;  use Gtk.Main;
with Gtk.Enums; use Gtk.Enums;

with Gtkada.Dialogs; use Gtkada.Dialogs;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;

with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure GDialog is
   Button        : Message_Dialog_Buttons;
   pragma Warnings (Off, Button);

   Buffer        : String (1 .. 8192);
   Last          : Natural := 0;
   Kind          : Message_Dialog_Type := Information;
   Justification : Gtk_Justification   := Justify_Center;

begin
   Init;

   if Argument_Count > 0 then
      begin
         Kind := Message_Dialog_Type'Value (Argument (1));
      exception
         when Constraint_Error =>
            Put_Line (Standard_Error, "Wrong message dialog type: " &
              Argument (1));
            Put_Line (Standard_Error, "Possible values are:");

            for J in Message_Dialog_Type'Range loop
               Put_Line (Standard_Error,
                 Translate (Message_Dialog_Type'Image (J), Lower_Case_Map));
            end loop;

            OS_Exit (1);
      end;
   end if;

   if Argument_Count > 1 then
      begin
         Justification := Gtk_Justification'Value (Argument (2));
      exception
         when Constraint_Error =>
            Put_Line (Standard_Error, "Wrong justification value: " &
              Argument (2));
            Put_Line (Standard_Error, "Possible values are:");

            for J in Gtk_Justification'Range loop
               Put_Line (Standard_Error,
                 Translate (Gtk_Justification'Image (J), Lower_Case_Map));
            end loop;

            OS_Exit (2);
      end;
   end if;

   begin
      loop
         Get_Line (Buffer (Last + 1 .. Buffer'Last), Last);
         Last := Last + 1;
         Buffer (Last) := ASCII.LF;
      end loop;
   exception
      when End_Error =>
         null;
   end;

   Button := Message_Dialog
     (Buffer (1 .. Last), Kind, Button_OK,
      Justification => Justification);
end GDialog;
