-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                  Copyright (C) 2010-2013, AdaCore                 --
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

with Ada.Text_IO;         use Ada.Text_IO;

with Glib;                use Glib;
with Glib.Main;           use Glib.Main;

with Gdk.Pixbuf;          use Gdk.Pixbuf;

with Gtk;                 use Gtk;
with Gtk.Alignment;       use Gtk.Alignment;
with Gtk.Assistant;       use Gtk.Assistant;
with Gtk.Bin;             use Gtk.Bin;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Label;           use Gtk.Label;
with Gtk.Progress_Bar;    use Gtk.Progress_Bar;
with Gtk.Radio_Button;    use Gtk.Radio_Button;
with Gtk.Stock;           use Gtk.Stock;
with Gtk.Toggle_Button;   use Gtk.Toggle_Button;
with Gtk.Widget;          use Gtk.Widget;

with Common;              use Common;

package body Create_Assistant is

   -----------------
   -- Local Types --
   -----------------

   type Page_Data_Record is record
      Assistant : Gtk_Assistant;
      Page      : Gtk_Vbox;
   end record;
   type Page_Data is access Page_Data_Record;
   --  User data for Check_Button_Page_Data_Cb callback package.

   type Assistant_Type is
     (Simple, Generous, Nonlinear, Looping, Full_Featured);
   All_Assistants : array (Assistant_Type) of Gtk_Assistant :=
     (others => null);
   --  For every assistant type, we toggle assistants on and off on
   --  each main window button click.  In order to do this we need
   --  to keep track of all assistants that we bring up.

   -------------------------------------
   -- Callback Package Instantiations --
   -------------------------------------

   package Time_Cb is new Glib.Main.Generic_Sources (Gtk_Assistant);
   package Assistant_Cb is new Handlers.Callback (Gtk_Assistant_Record);
   package Check_Button_Widget_Cb is new Handlers.User_Callback
     (Gtk_Check_Button_Record, Gtk_Widget);
   package Check_Button_Page_Data_Cb is new Handlers.User_Callback
     (Gtk_Check_Button_Record, Page_Data);
   package Radio_Button_Char_Cb is new Handlers.User_Callback
     (Gtk_Radio_Button_Record, Character);
   package Forward_Page_Functions is new Generic_Assistant_Functions
     (Gtk_Assistant);
   use Forward_Page_Functions;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "a @bGtk_Assistant@B is a kind of @bGtk_Window@B that is used"
        & " to guide users through multi-step operations, and appears as a"
        & " dialog with multiple stages.  An application might use this type"
        & " of functionality to create wizards.";
   end Help;

   -----------------------
   -- Complete_Callback --
   -----------------------

   procedure Complete_Callback
     (Check : access Gtk_Check_Button_Record'Class;
      Data  : Page_Data)
   is
      Complete : Boolean;
   begin
      Complete := Get_Active (Check);
      Set_Page_Complete (Data.Assistant, Data.Page, Complete);
   end Complete_Callback;

   ------------------------------
   -- Add_Completion_Test_Page --
   ------------------------------

   function Add_Completion_Test_Page
     (Assistant : Gtk_Assistant;
      Text      : String;
      Visible   : Boolean;
      Complete  : Boolean)
     return Gtk_Vbox
   is
      Page   : Gtk_Vbox;
      Check  : Gtk_Check_Button;
      Label1 : Gtk_Label;
      Data   : constant Page_Data := new Page_Data_Record;
      Tmp    : Gint;
      pragma Warnings (Off, Tmp);
   begin
      Gtk_New_Vbox (Page);
      Gtk_New (Check, "Complete");
      Gtk_New (Label1, Text);

      Add (Page, Label1);
      Add (Page, Check);

      Set_Active (Check, Complete);

      Data.Assistant := Assistant;
      Data.Page := Page;
      Check_Button_Page_Data_Cb.Connect
        (Check, "toggled", Complete_Callback'Access, Data);

      if Visible then
         Show_All (Page);
      end if;

      Tmp := Append_Page (Assistant, Page);
      Set_Page_Title (Assistant, Page, Text);
      Set_Page_Complete (Assistant, Page, Complete);

      return Page;
   end Add_Completion_Test_Page;

   ---------------------
   -- Cancel_Callback --
   ---------------------

   procedure Cancel_Callback (Assistant : access Gtk_Assistant_Record'Class) is
   begin
      Put_Line ("cancel");
      Hide (Assistant);
   end Cancel_Callback;

   --------------------
   -- Close_Callback --
   --------------------

   procedure Close_Callback (Assistant : access Gtk_Assistant_Record'Class) is
   begin
      Put_Line ("close");
      Hide (Assistant);
   end Close_Callback;

   --------------------
   -- Apply_Callback --
   --------------------

   procedure Apply_Callback (Assistant : access Gtk_Assistant_Record'Class) is
      pragma Unreferenced (Assistant);
   begin
      Put_Line ("apply");
   end Apply_Callback;

   ----------------------
   -- Progress_Timeout --
   ----------------------

   function Progress_Timeout (Assistant : Gtk_Assistant) return Boolean is
      Page_Num : constant Gint := Get_Current_Page (Assistant);
      Page     : constant Gtk_Bin :=
                   Gtk_Bin (Get_Nth_Page (Assistant, Page_Num));
      Progress : constant Gtk_Progress_Bar :=
                   Gtk_Progress_Bar (Get_Child (Page));

      Value    : Gdouble;
   begin
      Value := Get_Fraction (Progress);
      Value := Value + 0.1;
      Set_Fraction (Progress, Value);

      if Value >= 1.0 then
         Set_Page_Complete (Assistant, Page, True);
         return False;
      end if;

      return True;
   end Progress_Timeout;

   ----------------------
   -- Prepare_Callback --
   ----------------------

   procedure Prepare_Callback
     (Assistant : access Gtk_Assistant_Record'Class)
   is
      Page : constant Gtk_Widget :=
        Get_Nth_Page (Assistant, Get_Current_Page (Assistant));
   begin
      if Page.all in Gtk_Label_Record'Class then
         Put_Line ("prepare: " & Get_Text (Gtk_Label (Page)));

      elsif Get_Page_Type (Assistant, Page) = Gtk_Assistant_Page_Progress then
         declare
            Progress : constant Gtk_Progress_Bar :=
                       Gtk_Progress_Bar (Get_Child (Gtk_Alignment (Page)));
            Timer    : G_Source_Id;
            pragma Warnings (Off, Timer);
         begin
            Set_Page_Complete (Assistant, Page, False);
            Set_Fraction (Progress, 0.0);
            Timer := Time_Cb.Timeout_Add
              (300, Progress_Timeout'Access, Gtk_Assistant (Assistant));
         end;

      else
         Put_Line ("prepare: " & Get_Current_Page (Assistant)'Img);
      end if;
   end Prepare_Callback;

   -----------------------------
   -- Create_Simple_Assistant --
   -----------------------------

   procedure Create_Simple_Assistant
     (Widget : access Gtk_Button_Record'Class)
   is
      pragma Warnings (Off, Widget);
      Assistant : Gtk_Assistant renames All_Assistants (Simple);
      Page      : Gtk_Label;
      Page_Num  : Gint;
      pragma Warnings (Off, Page_Num);
   begin
      if Assistant = null then
         Gtk_New (Assistant);
         Set_Default_Size (Assistant, 400, 300);

         Assistant_Cb.Connect (Assistant, "cancel",  Cancel_Callback'Access);
         Assistant_Cb.Connect (Assistant, "close",   Close_Callback'Access);
         Assistant_Cb.Connect (Assistant, "apply",   Apply_Callback'Access);
         Assistant_Cb.Connect (Assistant, "prepare", Prepare_Callback'Access);

         Gtk_New (Page, "Page 1");
         Show (Page);
         Page_Num := Append_Page (Assistant, Page);
         Set_Page_Title (Assistant, Page, "Page 1");
         Set_Page_Complete (Assistant, Page, True);

         Gtk_New (Page, "Page 2");
         Show (Page);
         Page_Num := Append_Page (Assistant, Page);
         Set_Page_Title (Assistant, Page, "Page 2");
         Set_Page_Type (Assistant, Page, Gtk_Assistant_Page_Confirm);
         Set_Page_Complete (Assistant, Page, True);
      end if;

      if not Visible_Is_Set (Assistant) then
         Show (Assistant);
      else
         Destroy (Assistant);
         Assistant := null;
      end if;
   end Create_Simple_Assistant;

   ----------------------
   -- Visible_Callback --
   ----------------------

   procedure Visible_Callback
     (Check : access Gtk_Check_Button_Record'Class;
      Page  : Gtk_Widget)
   is
      Visible : constant Boolean := Get_Active (Check);
   begin
      if Visible then
         Show (Page);
      else
         Hide (Page);
      end if;
   end Visible_Callback;

   -------------------------------
   -- Create_Generous_Assistant --
   -------------------------------

   procedure Create_Generous_Assistant
     (Widget : access Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Widget);
      Assistant : Gtk_Assistant renames All_Assistants (Generous);
   begin
      if Assistant = null then
         Gtk_New (Assistant);
         Set_Default_Size (Assistant, 400, 300);

         Assistant_Cb.Connect (Assistant, "cancel",  Cancel_Callback'Access);
         Assistant_Cb.Connect (Assistant, "close",   Close_Callback'Access);
         Assistant_Cb.Connect (Assistant, "apply",   Apply_Callback'Access);
         Assistant_Cb.Connect (Assistant, "prepare", Prepare_Callback'Access);

         declare
            Tmp                    : Gint;
            Label1                 : Gtk_Label;
            Content_Page,
            More_Content_Page,
            Even_More_Content_Page : Gtk_Vbox;
            Check                  : Gtk_Check_Button;
            Alignment1             : Gtk_Alignment;
            Progress_Bar1          : Gtk_Progress_Bar;
            pragma Warnings (Off, Tmp);
            pragma Warnings (Off, Even_More_Content_Page);
         begin
            Gtk_New (Label1, "Introduction");
            Show (Label1);
            Tmp := Append_Page (Assistant, Label1);
            Set_Page_Title (Assistant, Label1, "Introduction");
            Set_Page_Type (Assistant, Label1, Gtk_Assistant_Page_Intro);
            Set_Page_Complete (Assistant, Label1, True);

            Content_Page := Add_Completion_Test_Page
              (Assistant, "Content", True, False);
            More_Content_Page := Add_Completion_Test_Page
              (Assistant, "More Content", True, True);

            Gtk_New (Check, "Next page visible");
            Set_Active (Check, True);
            Check_Button_Widget_Cb.Connect
              (Check, "toggled",
               Check_Button_Widget_Cb.To_Marshaller (Visible_Callback'Access),
               Gtk_Widget (More_Content_Page));
            Show (Check);
            Add (Content_Page, Check);

            Even_More_Content_Page := Add_Completion_Test_Page
              (Assistant, "Even More Content", True, True);

            Gtk_New (Label1, "Confirmation");
            Show (Label1);
            Tmp := Append_Page (Assistant, Label1);
            Set_Page_Title (Assistant, Label1, "Confirmation");
            Set_Page_Type (Assistant, Label1, Gtk_Assistant_Page_Confirm);
            Set_Page_Complete (Assistant, Label1, True);

            Gtk_New (Alignment1, 0.5, 0.5, 0.9, 0.0);
            Gtk_New (Progress_Bar1);
            Add (Alignment1, Progress_Bar1);
            Show_All (Alignment1);
            Tmp := Append_Page (Assistant, Alignment1);
            Set_Page_Title (Assistant, Alignment1, "Progress");
            Set_Page_Type (Assistant, Alignment1, Gtk_Assistant_Page_Progress);

            Gtk_New (Label1, "Summary");
            Show (Label1);
            Tmp := Append_Page (Assistant, Label1);
            Set_Page_Title (Assistant, Label1, "Summary");
            Set_Page_Type (Assistant, Label1, Gtk_Assistant_Page_Summary);
            Set_Page_Complete (Assistant, Label1, True);
         end;

      end if;

      if not Visible_Is_Set (Assistant) then
         Show (Assistant);
      else
         Destroy (Assistant);
         Assistant := null;
      end if;
   end Create_Generous_Assistant;

   -------------------
   -- Select_Branch --
   -------------------

   Selected_Branch : Character := 'A';

   procedure Select_Branch
     (Button : access Gtk_Radio_Button_Record'Class;
      Branch : Character)
   is
      pragma Unreferenced (Button);
   begin
      Selected_Branch := Branch;
   end Select_Branch;

   --------------------------------------
   -- Nonlinear_Assistant_Forward_Page --
   --------------------------------------

   function Nonlinear_Assistant_Forward_Page
     (Current_Page : Gint;
      Assistant    : Gtk_Assistant)
      return Gint
   is
      pragma Unreferenced (Assistant);
   begin
      case Current_Page is
         when 0 =>
            case Selected_Branch is
               when 'A' => return 1;
               when 'B' => return 2;
               when others => return -1;
            end case;
         when 1 | 2 =>
            return 3;
         when others =>
            return -1;
      end case;
   end Nonlinear_Assistant_Forward_Page;

   --------------------------------
   -- Create_Nonlinear_Assistant --
   --------------------------------

   procedure Create_Nonlinear_Assistant
     (Widget : access Gtk_Button_Record'Class)
   is
      Assistant    : Gtk_Assistant renames All_Assistants (Nonlinear);
      Page         : Gtk_Vbox;
      Label        : Gtk_Label;
      Radio_Button : Gtk_Radio_Button;
      Tmp          : Gint;
      pragma Unreferenced (Widget);
      pragma Warnings (Off, Tmp);
   begin
      if Assistant = null then
         Gtk_New (Assistant);
         Set_Default_Size (Assistant, 400, 300);

         Assistant_Cb.Connect (Assistant, "cancel",  Cancel_Callback'Access);
         Assistant_Cb.Connect (Assistant, "close",   Close_Callback'Access);
         Assistant_Cb.Connect (Assistant, "apply",   Apply_Callback'Access);
         Assistant_Cb.Connect (Assistant, "prepare", Prepare_Callback'Access);

         Set_Forward_Page_Func
           (Assistant, Nonlinear_Assistant_Forward_Page'Access, null);

         Gtk_New_Vbox (Page, False, 6);

         Gtk_New (Radio_Button, Label => "branch A");
         Pack_Start (Page, Radio_Button, False, False, 0);
         Radio_Button_Char_Cb.Connect
           (Radio_Button, "toggled", Select_Branch'Access, 'A');
         Set_Active (Radio_Button, True);

         Gtk_New (Radio_Button, Radio_Button, "branch B");
         Pack_Start (Page, Radio_Button, False, False, 0);
         Radio_Button_Char_Cb.Connect
           (Radio_Button, "toggled", Select_Branch'Access, 'B');

         Show_All (Page);
         Tmp := Append_Page (Assistant, Page);
         Set_Page_Title (Assistant, Page, "Page 1");
         Set_Page_Complete (Assistant, Page, True);

         Gtk_New (Label, "Page 2A");
         Show (Label);
         Tmp := Append_Page (Assistant, Label);
         Set_Page_Title (Assistant, Label, "Page 2A");
         Set_Page_Complete (Assistant, Label, True);

         Gtk_New (Label, "Page 2B");
         Show (Label);
         Tmp := Append_Page (Assistant, Label);
         Set_Page_Title (Assistant, Label, "Page 2B");
         Set_Page_Complete (Assistant, Label, True);

         Gtk_New (Label, "Confirmation");
         Show (Label);
         Tmp := Append_Page (Assistant, Label);
         Set_Page_Title (Assistant, Label, "Confirmation");
         Set_Page_Type (Assistant, Label, Gtk_Assistant_Page_Confirm);
         Set_Page_Complete (Assistant, Label, True);
      end if;

      if not Visible_Is_Set (Assistant) then
         Show (Assistant);
      else
         Destroy (Assistant);
         Assistant := null;
      end if;
   end Create_Nonlinear_Assistant;

   ------------------------------------
   -- Looping_Assistant_Forward_Page --
   ------------------------------------

   function Looping_Assistant_Forward_Page
     (Current_Page : Gint;
      Assistant    : Gtk_Assistant)
      return Gint
   is
   begin
      case Current_Page is
         when 0 .. 2 =>
            return Current_Page + 1;
         when 3 =>
            declare
               Loop_Toggle_Button : constant Gtk_Toggle_Button :=
                 Gtk_Toggle_Button (Get_Nth_Page (Assistant, Current_Page));
               Should_Loop : constant Boolean :=
                 Get_Active (Loop_Toggle_Button);
            begin
               if Should_Loop then
                  return 0;
               else
                  return 4;
               end if;
            end;
         when others =>
            return -1;
      end case;
   end Looping_Assistant_Forward_Page;

   ------------------------------
   -- Create_Looping_Assistant --
   ------------------------------

   procedure Create_Looping_Assistant
     (Widget : access Gtk_Button_Record'Class)
   is
      Assistant    : Gtk_Assistant renames All_Assistants (Looping);
      Label        : Gtk_Label;
      Check_Button : Gtk_Check_Button;
      Tmp          : Gint;
      pragma Unreferenced (Widget);
      pragma Warnings (Off, Tmp);
   begin
      if Assistant = null then
         Gtk_New (Assistant);
         Set_Default_Size (Assistant, 400, 300);

         Assistant_Cb.Connect (Assistant, "cancel",  Cancel_Callback'Access);
         Assistant_Cb.Connect (Assistant, "close",   Close_Callback'Access);
         Assistant_Cb.Connect (Assistant, "apply",   Apply_Callback'Access);
         Assistant_Cb.Connect (Assistant, "prepare", Prepare_Callback'Access);

         Set_Forward_Page_Func
           (Assistant, Looping_Assistant_Forward_Page'Access, Assistant);

         Gtk_New (Label, "Introduction");
         Show (Label);
         Tmp := Append_Page (Assistant, Label);
         Set_Page_Title (Assistant, Label, "Introduction");
         Set_Page_Type (Assistant, Label, Gtk_Assistant_Page_Intro);
         Set_Page_Complete (Assistant, Label, True);

         Gtk_New (Label, "Content");
         Show (Label);
         Tmp := Append_Page (Assistant, Label);
         Set_Page_Title (Assistant, Label, "Content");
         Set_Page_Complete (Assistant, Label, True);

         Gtk_New (Label, "More content");
         Show (Label);
         Tmp := Append_Page (Assistant, Label);
         Set_Page_Title (Assistant, Label, "More content");
         Set_Page_Complete (Assistant, Label, True);

         Gtk_New (Check_Button, "Loop?");
         Show (Check_Button);
         Tmp := Append_Page (Assistant, Check_Button);
         Set_Page_Title (Assistant, Check_Button, "Loop?");
         Set_Page_Complete (Assistant, Check_Button, True);

         Gtk_New (Label, "Confirmation");
         Show (Label);
         Tmp := Append_Page (Assistant, Label);
         Set_Page_Title (Assistant, Label, "Confirmation");
         Set_Page_Type (Assistant, Label, Gtk_Assistant_Page_Confirm);
         Set_Page_Complete (Assistant, Label, True);
      end if;

      if not Visible_Is_Set (Assistant) then
         Show (Assistant);
      else
         Destroy (Assistant);
         Assistant := null;
      end if;
   end Create_Looping_Assistant;

   ------------------------------------
   -- Create_Full_Featured_Assistant --
   ------------------------------------

   procedure Create_Full_Featured_Assistant
     (Widget : access Gtk_Button_Record'Class)
   is
      Assistant : Gtk_Assistant renames All_Assistants (Full_Featured);
      Button    : Gtk_Button;
      Label     : Gtk_Label;
      Pixbuf    : Gdk_Pixbuf;
      Tmp       : Gint;
      pragma Unreferenced (Widget);
      pragma Warnings (Off, Tmp);
   begin
      if Assistant = null then
         Gtk_New (Assistant);
         Set_Default_Size (Assistant, 400, 300);

         Gtk_New_From_Stock (Button, Stock_Stop);
         Show (Button);
         Add_Action_Widget (Assistant, Button);

         Assistant_Cb.Connect (Assistant, "cancel",  Cancel_Callback'Access);
         Assistant_Cb.Connect (Assistant, "close",   Close_Callback'Access);
         Assistant_Cb.Connect (Assistant, "apply",   Apply_Callback'Access);
         Assistant_Cb.Connect (Assistant, "prepare", Prepare_Callback'Access);

         Gtk_New (Label, "Page 1");
         Show (Label);
         Tmp := Append_Page (Assistant, Label);
         Set_Page_Title (Assistant, Label, "Page 1");
         Set_Page_Complete (Assistant, Label, True);

         --  Set a side image
         Pixbuf := Render_Icon (Label, Stock_Dialog_Warning, Icon_Size_Dialog);
         Set_Page_Side_Image (Assistant, Label, Pixbuf);

         --  Set a header image
         Pixbuf := Render_Icon (Label, Stock_Dialog_Info, Icon_Size_Dialog);
         Set_Page_Header_Image (Assistant, Label, Pixbuf);

         Gtk_New (Label, "Invisible page");
         Tmp := Append_Page (Assistant, Label);

         Gtk_New (Label, "Page 3");
         Show (Label);
         Tmp := Append_Page (Assistant, Label);
         Set_Page_Title (Assistant, Label, "Page 3");
         Set_Page_Type (Assistant, Label, Gtk_Assistant_Page_Confirm);
         Set_Page_Complete (Assistant, Label, True);

         --  Set a header image
         Pixbuf := Render_Icon (Label, Stock_Dialog_Info, Icon_Size_Dialog);
         Set_Page_Header_Image (Assistant, Label, Pixbuf);
      end if;

      if not Visible_Is_Set (Assistant) then
         Show (Assistant);
      else
         Destroy (Assistant);
         Assistant := null;
      end if;
   end Create_Full_Featured_Assistant;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1 : Gtk_Box;

      ---------
      -- Add --
      ---------

      procedure Add
        (Description : String;
         Callback    : Button_Handler.Simple_Handler)
      is
         Button : Gtk_Button;
      begin
         Gtk_New (Button, Description);
         Button_Handler.Connect (Button, "clicked", Callback);
         Pack_Start
           (Box1, Button, Expand => False, Fill => False, Padding => 10);
      end Add;

   begin
      Set_Label (Frame, "Assistant");

      Gtk_New_Vbox (Box1);
      Add (Frame, Box1);

      Add ("Simple Assistant",        Create_Simple_Assistant'Access);
      Add ("Generous Assistant",      Create_Generous_Assistant'Access);
      Add ("Nonlinear Assistant",     Create_Nonlinear_Assistant'Access);
      Add ("Looping Assistant",       Create_Looping_Assistant'Access);
      Add ("Full Featured Assistant", Create_Full_Featured_Assistant'Access);

      Show_All (Frame);
   end Run;

end Create_Assistant;
