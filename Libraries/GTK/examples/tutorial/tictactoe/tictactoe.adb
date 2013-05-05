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

with Glib.Object; use Glib.Object;
with Gtk.Handlers, Gtk.Main, Gtk.Table;
with Gtkada.Types; use Gtkada.Types;
with Ada.Text_IO; use Ada.Text_IO;

package body Tictactoe is

   use Gtk, Glib;
   use Widget, Toggle_Button, Table;

   procedure Toggle
     (Button : access Gtk_Toggle_Button_Record'Class;
      Tictactoe : Gtk_Tictactoe);
   --  Signal handler for "toggled" signal.

   Class_Record : GObject_Class := Uninitialized_Class;
   --  This pointer will keep a pointer to the C 'class record' for
   --  gtk. To avoid allocating memory for each widget, this may be done
   --  only once, and reused

   Signals : Chars_Ptr_Array := Null_Array + "tictactoe";
   --  Array of the signals created for this widget

   package Internal_Cb is new Handlers.Callback (Gtk_Tictactoe_Record);
   --  The type of callbacks for the signals above. This is used only to
   --  emit the signals.

   package Button_Cb is new Handlers.User_Callback
     (Gtk_Toggle_Button_Record, Gtk_Tictactoe);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Tictactoe : out Gtk_Tictactoe) is
      --  Used to create a new widget
   begin
      Tictactoe := new Gtk_Tictactoe_Record;
      Standard.Tictactoe.Initialize (Tictactoe);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Tictactoe : access Gtk_Tictactoe_Record) is
      Table : Gtk_Table;
   begin
      --  We need to call the ancestor's Initialize function to create
      --  the underlying C object.
      Gtk.Box.Initialize_Vbox
        (Gtk.Box.Gtk_Vbox_Record (Tictactoe.all)'Access);

      --  The following call is required to initialize the class record,
      --  and the new signals created for this widget.
      --  Note that we keep Class_Record, so that the memory allocation
      --  is done only once.
      Glib.Object.Initialize_Class_Record
        (Tictactoe, Signals, Class_Record, "TicTacToe");

      --  Now initialize the composite part

      Gtk_New (Table, 3, 3, True);
      Add (Tictactoe, Table);
      Show (Table);

      for J in Tictactoe.Buttons'Range (1) loop
         for K in Tictactoe.Buttons'Range (2) loop
            Gtk_New (Tictactoe.Buttons (J, K).Button);
            Attach_Defaults
              (Table, Tictactoe.Buttons (J, K).Button, J - 1, J, K - 1, K);
            Tictactoe.Buttons (J, K).Id := Button_Cb.Connect
              (Tictactoe.Buttons (J, K).Button, "toggled",
               Button_Cb.To_Marshaller (Toggle'Access),
               Gtk_Tictactoe (Tictactoe));
            Set_USize (Tictactoe.Buttons (J, K).Button, 20, 20);
            Show (Tictactoe.Buttons (J, K).Button);
         end loop;
      end loop;
   end Initialize;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tictactoe : access Gtk_Tictactoe_Record) is
   begin
      for J in Tictactoe.Buttons'Range (1) loop
         for K in Tictactoe.Buttons'Range (2) loop
            Handlers.Handler_Block (Tictactoe.Buttons (J, K).Button,
              Tictactoe.Buttons (J, K).Id);
            Set_Active (Tictactoe.Buttons (J, K).Button, False);
            Handlers.Handler_Unblock (Tictactoe.Buttons (J, K).Button,
              Tictactoe.Buttons (J, K).Id);
         end loop;
      end loop;
   end Clear;

   ------------
   -- Toggle --
   ------------

   type Win_Array is array (1 .. 8, 1 .. 3) of Guint;

   Rwins : constant Win_Array :=
     ((1, 1, 1), (2, 2, 2), (3, 3, 3),
      (1, 2, 3), (1, 2, 3), (1, 2, 3),
      (1, 2, 3), (1, 2, 3));

   Cwins : constant Win_Array :=
     ((1, 2, 3), (1, 2, 3), (1, 2, 3),
      (1, 1, 1), (2, 2, 2), (3, 3, 3),
      (1, 2, 3), (3, 2, 1));

   procedure Toggle
     (Button    : access Gtk_Toggle_Button_Record'Class;
      Tictactoe : Gtk_Tictactoe)
   is
      Success, Found : Boolean;
   begin
      for K in Win_Array'Range (1) loop
         Success := True;
         Found := False;

         for J in Win_Array'Range (2) loop
            Success := Success and then
              Get_Active
                (Tictactoe.Buttons (Rwins (K, J), Cwins (K, J)).Button);
            Found := Found or else
              Tictactoe.Buttons (Rwins (K, J), Cwins (K, J)).Button =
                Gtk_Toggle_Button (Button);
         end loop;

         if Success and then Found then
            Internal_Cb.Emit_By_Name (Tictactoe, "tictactoe");
            exit;
         end if;
      end loop;
   end Toggle;

   ---------
   -- Win --
   ---------

   procedure Win (Tictactoe : access Gtk_Tictactoe_Record'Class) is
   begin
      Put_Line ("Yay!");
      Clear (Tictactoe);
   end Win;

   ----------
   -- Quit --
   ----------

   procedure Quit (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      pragma Unreferenced (Widget);
   begin
      Main.Gtk_Exit (0);
   end Quit;

end Tictactoe;
