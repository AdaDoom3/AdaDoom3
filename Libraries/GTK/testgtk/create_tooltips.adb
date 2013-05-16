-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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

with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Gtk.Arguments;     use Gtk.Arguments;
with Gtk.Box;           use Gtk.Box;
with Gtk.Button;        use Gtk.Button;
with Gtk.Handlers;      use Gtk.Handlers;
pragma Warnings (Off);  --  Gtk.Tips_Query is obsolescent
with Gtk.Tips_Query;    use Gtk.Tips_Query;
pragma Warnings (On);
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Tooltips;      use Gtk.Tooltips;
with Gtk.Widget;        use Gtk.Widget;
with Gtk;               use Gtk;
with Ada.Text_IO;
with Common;            use Common;

package body Create_Tooltips is

   package Tooltips_Data is new User_Data (Gtk_Tooltips);
   --  This is required to set tooltips for a widget.

   package Query_Cb is new Handlers.Callback (Gtk_Tips_Query_Record);
   package Entered_Cb is new Handlers.User_Callback
     (Gtk_Tips_Query_Record, Gtk_Toggle_Button);
   package Selected_Cb is new Handlers.Return_Callback
     (Gtk_Tips_Query_Record, Gint);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "@bGtk_Tooltips@B allow you to provide short help texts to the"
        & " user. This also requires a @bGtk_Tips_Query@B widget, that"
        & " displays tooltips and has a ""What's this"" functionnality."
        & " Through the @bwidget_entered@B and @bwidget_selected@B signals,"
        & " you can decide to display some extensive help.";
   end Help;

   --------------------
   -- Widget_Entered --
   --------------------

   procedure Widget_Entered (Tips_Query  : access Gtk_Tips_Query_Record'Class;
                             Params      : Gtk.Arguments.Gtk_Args;
                             Toggle      : Gtk_Toggle_Button)
   is
      --  Widget    : Gtk_Widget := Gtk_Widget (To_Object (Params, 1));
      Tip_Text    : constant String := To_String (Params, 2);
      --  Tip_Private : String := To_String (Params, 3);

   begin
      if Get_Active (Toggle) then
         if Tip_Text'Length /= 0 then
            Set_Text (Tips_Query, "There is a Tip!");
         else
            Set_Text (Tips_Query, "There is no Tip!");
         end if;
         --  Don't let GtkTipsQuery reset it's label
         Emit_Stop_By_Name (Tips_Query, "widget_entered");
      end if;
   end Widget_Entered;

   ---------------------
   -- Widget_Selected --
   ---------------------

   function Widget_Selected (Tips_Query  : access Gtk_Tips_Query_Record'Class;
                             Params      : Gtk.Arguments.Gtk_Args)
                            return Gint
   is
      Widget    : constant Gtk_Widget := Gtk_Widget (To_Object (Params, 1));
      --  Tip_Text    : String := To_String (Params, 2);
      Tip_Private : constant String := To_String (Params, 3);
      pragma Warnings (Off, Tips_Query);
   begin
      if Is_Created (Widget.all) then
         Ada.Text_IO.Put ("Help ");
         if Tip_Private'Length = 0 then
            Ada.Text_IO.Put ("None");
         else
            Ada.Text_IO.Put (Tip_Private);
         end if;
         Ada.Text_IO.Put_Line (" requested for "
                               & Type_Name (Get_Type (Widget)));
      end if;
      return 0;
   end Widget_Selected;

   -----------------
   -- Start_Query --
   -----------------

   procedure Start_Query (Tips_Query : access Gtk_Tips_Query_Record'Class) is
   begin
      Gtk.Tips_Query.Start_Query (Tips_Query);
   end Start_Query;

   -----------------
   -- Get_Data_Cb --
   -----------------

   procedure Get_Data_Cb (Button : access Gtk_Button_Record'Class) is
      Data : constant Gtk.Tooltips.Tooltips_Data :=
        Gtk.Tooltips.Get_Data (Button);
   begin
      Ada.Text_IO.Put_Line ("Result of call to Gtk.Tooltips.Get_Data:");
      Ada.Text_IO.Put_Line ("   Text=" & Data.Text);
      Ada.Text_IO.Put_Line ("   Text_Private=" & Data.Text_Private);
   end Get_Data_Cb;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1,
        Box2,
        Box3     : Gtk_Box;
      Button     : Gtk_Button;
      Tooltips   : Gtk_Tooltips;
      Toggle     : Gtk_Toggle_Button;
      Tips_Query : Gtk_Tips_Query;
      Frame2     : Gtk_Frame;
      Vbox       : Gtk_Box;

   begin
      Set_Label (Frame, "Tooltips");
      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Add (Frame, Vbox);

      Gtk_New (Tooltips);
      Tooltips_Data.Set (Vbox, Tooltips, "tooltips");

      Gtk_New_Vbox (Box1, False, 0);
      Pack_Start (Vbox, Box1, Expand => False, Fill => False);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, True, True, 0);

      Gtk_New (Button, "Button1");
      Show (Button);
      Pack_Start (Box2, Button, False, False, 0);
      Set_Tip (Tooltips, Button, "This is button 1",
               "ContextHelp/buttons/1");

      Gtk_New (Button, "(Print result of Get_Data)");
      Pack_Start (Box2, Button, False, False, 0);
      Set_Tip (Tooltips, Button,
               "This is button 2. This is also a really long tool tip which"
               &" probably won't fit on a single line and will therefore "
               & "need to be wrapped. Hopefully the wrapping will work "
               & "correctly.", "ContextHelp/buttons/2");
      Button_Handler.Connect
        (Button, "clicked",
         Button_Handler.To_Marshaller (Get_Data_Cb'Access));

      Gtk_New (Toggle, "Override TipsQuery Label");
      Pack_Start (Box2, Toggle, False, False, 0);
      Set_Tip (Tooltips, Toggle, "Toggle TipsQuery view.", "Hi!");

      Gtk_New_Vbox (Box3, False, 5);
      Set_Border_Width (Box3, 5);

      Gtk_New (Tips_Query);

      Gtk_New (Button, "[?]");
      Pack_Start (Box3, Button, False, False, 0);
      Query_Cb.Object_Connect
        (Button, "clicked",
         Query_Cb.To_Marshaller (Start_Query'Access),
         Slot_Object => Tips_Query);
      Set_Tip (Tooltips, Button, "Start the Tooltips Inspector",
               "ContextHelp/buttons/?");

      Pack_Start (Box3, Tips_Query, False, False, 0);
      Set_Caller (Tips_Query, Button);
      Entered_Cb.Connect (Tips_Query, "widget_entered",
                          Widget_Entered'Access, Toggle);
      Selected_Cb.Connect (Tips_Query, "widget_selected",
                           Widget_Selected'Access);

      Gtk_New (Frame2, "Tooltips Inspector");
      Set_Border_Width (Frame2, 0);
      Pack_Start (Box2, Frame2, True, True, 10);

      Add (Frame2, Box3);

      Show_All (Frame);
   end Run;

end Create_Tooltips;
