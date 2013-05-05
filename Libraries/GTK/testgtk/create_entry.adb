-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                    Copyright (C) 2010-2013, AdaCore               --
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

with Glib;             use Glib;
with Glib.Main;        use Glib.Main;
with Gtk.Box;          use Gtk.Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Combo;        use Gtk.Combo;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Separator;    use Gtk.Separator;
with Gtk.Widget;       use Gtk.Widget;
with Gtk;              use Gtk;

with Common;           use Common;

package body Create_Entry is

   package Entry_Cb is new Handlers.User_Callback
     (Gtk_Check_Button_Record, Gtk_Entry);

   package Time_Cb is new Glib.Main.Generic_Sources (Gtk_Entry);

   Timer1, Timer2 : G_Source_Id;
   --  This is stored at the library level so that the On_Destroy callback
   --  can refer to the results from the calls to Time_Cb.Timeout_Add in the
   --  Run subprogram.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo demonstrates several types of widgets.  The first"
        & " couple of @bGtk_Entry@B widgets demonstrate different kinds of"
        & " progress bars that may be embedded.  Then, there's a simple"
        & " @bGtk_Entry@B with user-selectable options.  Finally, the last"
        & " widget is a @bGtk_Combo@B that adds a window to facilitate the"
        & " insertion of the text.";
   end Help;

   ---------------------
   -- Toggle_Editable --
   ---------------------

   procedure Toggle_Editable
     (Button    : access Gtk_Check_Button_Record'Class;
      The_Entry : Gtk_Entry) is
   begin
      Set_Editable (The_Entry, Get_Active (Button));
   end Toggle_Editable;

   ----------------------
   -- Toggle_Overwrite --
   ----------------------

   procedure Toggle_Overwrite
     (Button    : access Gtk_Check_Button_Record'Class;
      The_Entry : Gtk_Entry)
   is
   begin
      Set_Overwrite_Mode (The_Entry, Get_Active (Button));
   end Toggle_Overwrite;

   ----------------------
   -- Toggle_Sensitive --
   ----------------------

   procedure Toggle_Sensitive
     (Button    : access Gtk_Check_Button_Record'Class;
      The_Entry : Gtk_Entry)
   is
   begin
      Set_Sensitive (The_Entry, Get_Active (Button));
   end Toggle_Sensitive;

   -----------------------
   -- Toggle_Visibility --
   -----------------------

   procedure Toggle_Visibility
     (Button    : access Gtk_Check_Button_Record'Class;
      The_Entry : Gtk_Entry)
   is
   begin
      Set_Visibility (The_Entry, Get_Active (Button));
   end Toggle_Visibility;

   -------------------
   -- Pulse_Timeout --
   -------------------

   function Pulse_Timeout (The_Entry : Gtk_Entry) return Boolean is
   begin
      Progress_Pulse (The_Entry);
      return True;
   end Pulse_Timeout;

   ------------------------
   -- Fractional_Timeout --
   ------------------------

   function Fractional_Timeout (The_Entry : Gtk_Entry) return Boolean is
      Progress : Gdouble := Get_Progress_Fraction (The_Entry);
   begin
      Progress := Progress + 0.005;
      if Progress > 1.0 then
         Progress := 0.0;
      end if;

      Set_Progress_Fraction (The_Entry, Progress);
      return True;
   end Fractional_Timeout;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Window);
   begin
      Remove (Timer1);
      Remove (Timer2);
   end On_Destroy;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      use String_List;

      List      : String_List.Glist;
      Box1,
        Box2    : Gtk_Box;
      The_Entry : Gtk_Entry;
      Combo     : Gtk_Combo;
      Check     : Gtk_Check_Button;
      Hsep      : Gtk_Hseparator;

   begin
      Append (List, "item0");
      Append (List, "item1 item1");
      Append (List, "item2 item2 item2");
      Append (List, "item3 item3 item3 item3");
      Append (List, "item4 item4 item4 item4 item4");
      Append (List, "item5 item5 item5 item5 item5 item5");
      Append (List, "item6 item6 item6 item6 item6");
      Append (List, "item7 item7 item7 item7");
      Append (List, "item8 item8 item8");
      Append (List, "item9 item9");

      Set_Label (Frame, "Entry");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Widget_Handler.Connect
        (Box1, "destroy", Widget_Handler.To_Marshaller (On_Destroy'Access));

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (The_Entry);
      Set_Text (The_Entry, "Pulsed Progress");
      Set_Sensitive (The_Entry, False);
      Set_Editable (The_Entry, False);
      Timer1 := Time_Cb.Timeout_Add
        (100, Pulse_Timeout'Access, The_Entry);
      Pack_Start (Box2, The_Entry, True, True, 0);

      Gtk_New (The_Entry);
      Set_Text (The_Entry, "Fractional Progress");
      Set_Sensitive (The_Entry, False);
      Set_Editable (The_Entry, False);
      Timer2 := Time_Cb.Timeout_Add
        (20, Fractional_Timeout'Access, The_Entry);
      Pack_Start (Box2, The_Entry, True, True, 0);

      Gtk_New_Hseparator (Hsep);
      Pack_Start (Box2, Hsep);

      Gtk_New (The_Entry);
      Set_Text (The_Entry, "Hello world");
      Pack_Start (Box2, The_Entry, True, True, 0);

      Gtk_New (Check, "Editable");
      Pack_Start (Box2, Check, False, True, 0);
      Entry_Cb.Connect
        (Check, "toggled",
         Entry_Cb.To_Marshaller (Toggle_Editable'Access), The_Entry);
      Set_Active (Check, True);

      Gtk_New (Check, "Overwrite");
      Pack_Start (Box2, Check, False, True, 0);
      Entry_Cb.Connect
        (Check, "toggled",
         Entry_Cb.To_Marshaller (Toggle_Overwrite'Access), The_Entry);
      Set_Active (Check, False);

      Gtk_New (Check, "Visible");
      Pack_Start (Box2, Check, False, True, 0);
      Entry_Cb.Connect
        (Check, "toggled",
         Entry_Cb.To_Marshaller (Toggle_Visibility'Access), The_Entry);
      Set_Active (Check, True);

      Gtk_New (Check, "Sensitive");
      Pack_Start (Box2, Check, False, True, 0);
      Entry_Cb.Connect
        (Check, "toggled",
         Entry_Cb.To_Marshaller (Toggle_Sensitive'Access), The_Entry);
      Set_Active (Check, True);

      Gtk_New_Hseparator (Hsep);
      Pack_Start (Box2, Hsep);

      Gtk_New (Combo);
      Set_Popdown_Strings (Combo, List);
      Set_Text (Gtk_Entry (Get_Entry (Combo)), "hello world");
      Pack_Start (Box2, Combo, True, True, 0);

      Show_All (Frame);
   end Run;

end Create_Entry;
