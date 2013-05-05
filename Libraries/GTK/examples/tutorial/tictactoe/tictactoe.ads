-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                        Copyright (C) 2000                         --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtk.Widget, Gtk.Box, Gtk.Toggle_Button, Gtk.Handlers;
with Glib;

package Tictactoe is

   type Button_Record is record
      Button : Gtk.Toggle_Button.Gtk_Toggle_Button;
      Id : Gtk.Handlers.Handler_Id;
   end record;
   --  Hold a check item button and the handler id of a connection to the
   --  "toggled" signal.

   type Gtk_Tictactoe_Buttons is array
     (Glib.Guint range 1 .. 3, Glib.Guint range 1 .. 3) of
      Button_Record;

   type Gtk_Tictactoe_Record is new Gtk.Box.Gtk_Vbox_Record with record
      Buttons : Gtk_Tictactoe_Buttons;
   end record;
   type Gtk_Tictactoe is access all Gtk_Tictactoe_Record'Class;

   --  Primitive functions

   procedure Gtk_New (Tictactoe : out Gtk_Tictactoe);
   procedure Initialize (Tictactoe : access Gtk_Tictactoe_Record);
   procedure Clear (Tictactoe : access Gtk_Tictactoe_Record);

   --  Callbacks

   procedure Win (Tictactoe : access Gtk_Tictactoe_Record'Class);
   procedure Quit (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   --  Signal handling

   package Tictactoe_Cb is new Gtk.Handlers.Callback (Gtk_Tictactoe_Record);

   package Widget_Cb is new Gtk.Handlers.Callback
     (Gtk.Widget.Gtk_Widget_Record);
end Tictactoe;
