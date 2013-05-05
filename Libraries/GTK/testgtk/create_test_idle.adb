-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                     Copyright 2000-2006 AdaCore                   --
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
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Label;        use Gtk.Label;
with Glib.Main;        use Glib.Main;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Widget;       use Gtk.Widget;
with Gtk;              use Gtk;
with Common;           use Common;

package body Create_Test_Idle is

   package Label_Idle is new Glib.Main.Generic_Sources (Gtk_Label);

   type My_Button_Record is new Gtk_Radio_Button_Record with record
      Value : Gtk_Resize_Mode;
   end record;
   type My_Button is access all My_Button_Record'Class;

   package My_Button_Cb is new Handlers.User_Callback
     (My_Button_Record, Gtk_Box);

   Idle   : G_Source_Id;
   Count  : Integer := 0;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "An idle function is a function that is run every time GtkAda is"
        & " not busy processing an event. No commitment is made as to when"
        & " this will next happen.";
   end Help;

   ---------------
   -- Idle_Test --
   ---------------

   function Idle_Test (Label : in Gtk_Label) return Boolean is
   begin
      Count := Count + 1;
      Set_Text (Label, "count:" & Integer'Image (Count));
      return True;
   end Idle_Test;

   ---------------
   -- Stop_Idle --
   ---------------

   procedure Stop_Idle (Object : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Object);
   begin
      if Idle /= 0 then
         Remove (Idle);
         Idle := 0;
      end if;
   end Stop_Idle;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Window : access Gtk_Widget_Record'Class) is
   begin
      Stop_Idle (Window);
   end Destroy_Idle;

   ----------------
   -- Start_Idle --
   ----------------

   procedure Start_Idle (Label : access Gtk_Label_Record'Class) is
   begin
      if Idle = 0 then
         Idle := Label_Idle.Idle_Add (Idle_Test'Access, Gtk_Label (Label));
      end if;
   end Start_Idle;

   ----------------------
   -- Toggle_Container --
   ----------------------

   procedure Toggle_Container (Button : access My_Button_Record'Class;
                               Contain : in Gtk_Box) is
   begin
      Set_Resize_Mode (Contain, Button.Value);
   end Toggle_Container;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Button   : Gtk_Button;
      Box      : Gtk_Box;
      Label    : Gtk_Label;
      Container : Gtk_Box;
      Frame2   : Gtk_Frame;
      Myb      : My_Button;
      Gr       : Widget_SList.GSlist;
      Vbox     : Gtk_Box;

   begin
      Set_Label (Frame, "Test Idle");

      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Add (Frame, Vbox);

      Widget_Handler.Connect
        (Vbox, "destroy",
         Widget_Handler.To_Marshaller (Destroy_Idle'Access));

      Gtk_New (Label, "count : " & Integer'Image (Count));
      Set_Padding (Label, 10, 10);

      Gtk_New_Hbox (Container, False, 0);
      Pack_Start (Container, Label, False, False, 0);
      Pack_Start (Vbox, Container, False, False, 0);

      Gtk_New (Frame2);
      Set_Border_Width (Frame2, 5);
      Pack_Start (Vbox, Frame2, False, False);

      Gtk_New_Vbox (Box, False, 0);
      Add (Frame2, Box);

      Myb := new My_Button_Record;
      Initialize (Myb, Widget_SList.Null_List, "Resize-Parent");
      Myb.Value := Resize_Parent;
      My_Button_Cb.Connect
        (Myb, "clicked",
         My_Button_Cb.To_Marshaller (Toggle_Container'Access), Container);
      Pack_Start (Box, Myb, False, False, 0);

      Gr := Get_Group (Myb);
      Myb := new My_Button_Record;
      Initialize (Myb, Gr, "Resize-Queue");
      Myb.Value := Resize_Queue;
      My_Button_Cb.Connect
        (Myb, "clicked",
         My_Button_Cb.To_Marshaller (Toggle_Container'Access), Container);
      Pack_Start (Box, Myb, False, False, 0);

      Gr := Get_Group (Myb);
      Myb := new My_Button_Record;
      Initialize (Myb, Gr, "Resize-Immediate");
      Myb.Value := Resize_Immediate;
      My_Button_Cb.Connect
        (Myb, "clicked",
         My_Button_Cb.To_Marshaller (Toggle_Container'Access), Container);
      Pack_Start (Box, Myb, False, False, 0);

      Gtk_New (Button, "start");
      Label_Handler.Object_Connect
        (Button, "clicked",
         Label_Handler.To_Marshaller (Start_Idle'Access),
         Slot_Object => Label);
      Set_Flags (Button, Can_Default);
      Pack_Start (Vbox, Button, False, False, 0);

      Gtk_New (Button, "stop");
      Widget_Handler.Object_Connect
        (Button, "clicked",
         Widget_Handler.To_Marshaller (Destroy_Idle'Access),
         Slot_Object => Vbox);
      Set_Flags (Button, Can_Default);
      Pack_Start (Vbox, Button, False, False, 0);

      Show_All (Frame);
   end Run;

end Create_Test_Idle;

