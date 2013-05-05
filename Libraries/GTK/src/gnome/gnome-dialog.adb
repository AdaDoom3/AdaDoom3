-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--               Copyright (C) 2000 Helix Code, Inc.                 --
--               Copyright (C) 2000-2006 AdaCore                     --
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

with System;

with Gtk;          use Gtk;

package body Gnome.Dialog is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Dialog  : out Gnome_Dialog;
      Title   : String;
      Buttons : Chars_Ptr_Array) is
   begin
      Dialog := new Gnome_Dialog_Record;
      Initialize (Dialog, Title, Buttons);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Dialog  : access Gnome_Dialog_Record'Class;
      Title   : String;
      Buttons : Chars_Ptr_Array)
   is
      function Internal
        (Title : String; Buttons : Chars_Ptr_Array) return System.Address;
      pragma Import (C, Internal, "gnome_dialog_newv");

   begin
      Set_Object (Dialog, Internal (Title & ASCII.NUL, Buttons + Null_Ptr));
   end Initialize;

   -------------------
   -- Append_Button --
   -------------------

   procedure Append_Button
     (Dialog : access Gnome_Dialog_Record;
      Name   : String)
   is
      procedure Internal (Dialog : System.Address; Name : String);
      pragma Import (C, Internal, "gnome_dialog_append_button");

   begin
      Internal (Get_Object (Dialog), Name & ASCII.NUL);
   end Append_Button;

   procedure Append_Button
     (Dialog : access Gnome_Dialog_Record;
      Name   : String;
      Pixmap : String)
   is
      procedure Internal
        (Dialog : System.Address;
         Name   : String;
         Pixmap : String);
      pragma Import (C, Internal, "gnome_dialog_append_button_with_pixmap");

   begin
      Internal (Get_Object (Dialog), Name & ASCII.NUL, Pixmap & ASCII.NUL);
   end Append_Button;

   --------------------
   -- Append_Buttons --
   --------------------

   procedure Append_Buttons
     (Dialog  : access Gnome_Dialog_Record;
      Names   : Chars_Ptr_Array;
      Pixmaps : Chars_Ptr_Array)
   is
      procedure Internal
        (Dialog  : System.Address;
         Names   : Chars_Ptr_Array;
         Pixmaps : Chars_Ptr_Array);
      pragma Import (C, Internal, "gnome_dialog_append_buttons_with_pixmaps");
   begin
      Internal (Get_Object (Dialog),
                Names + Null_Ptr,
                Pixmaps + Null_Ptr);
   end Append_Buttons;

   procedure Append_Buttons
     (Dialog  : access Gnome_Dialog_Record;
      Buttons : Chars_Ptr_Array)
   is
      procedure Internal
        (Dialog  : System.Address;
         Buttons : Chars_Ptr_Array);
      pragma Import (C, Internal, "gnome_dialog_append_buttonsv");
   begin
      Internal (Get_Object (Dialog), Buttons + Null_Ptr);
   end Append_Buttons;

   -----------
   -- Close --
   -----------

   procedure Close (Dialog : access Gnome_Dialog_Record) is
      procedure Internal (Dialog : System.Address);
      pragma Import (C, Internal, "gnome_dialog_close");

   begin
      Internal (Get_Object (Dialog));
   end Close;

   -----------------
   -- Close_Hides --
   -----------------

   procedure Close_Hides
     (Dialog    : access Gnome_Dialog_Record;
      Just_Hide : Boolean)
   is
      procedure Internal
        (Dialog    : System.Address;
         Just_Hide : Gint);
      pragma Import (C, Internal, "gnome_dialog_close_hides");

   begin
      Internal (Get_Object (Dialog), Boolean'Pos (Just_Hide));
   end Close_Hides;

   ---------------------
   -- Editable_Enters --
   ---------------------

   procedure Editable_Enters
     (Dialog   : access Gnome_Dialog_Record;
      Editable : access Gtk.Editable.Gtk_Editable_Record'Class)
   is
      procedure Internal
        (Dialog    : System.Address;
         Editable  : System.Address);
      pragma Import (C, Internal, "gnome_dialog_editable_enters");

   begin
      Internal (Get_Object (Dialog), Get_Object (Editable));
   end Editable_Enters;

   ----------------
   -- Grab_Focus --
   ----------------

   procedure Grab_Focus (Dialog : access Gnome_Dialog_Record; Button : Gint) is
      procedure Internal
        (Dialog : System.Address;
         Button : Gint);
      pragma Import (C, Internal, "gnome_dialog_grab_focus");
   begin
      Internal (Get_Object (Dialog), Button);
   end Grab_Focus;

   ---------
   -- Run --
   ---------

   function Run (Dialog : access Gnome_Dialog_Record) return Gint is
      function Internal (Dialog : System.Address) return Gint;
      pragma Import (C, Internal, "gnome_dialog_run");

   begin
      return Internal (Get_Object (Dialog));
   end Run;

   -------------------
   -- Run_And_Close --
   -------------------

   function Run_And_Close (Dialog : access Gnome_Dialog_Record) return Gint is
      function Internal (Dialog : System.Address) return Gint;
      pragma Import (C, Internal, "gnome_dialog_run_and_close");

   begin
      return Internal (Get_Object (Dialog));
   end Run_And_Close;

   ---------------------
   -- Set_Accelerator --
   ---------------------

   procedure Set_Accelerator
     (Dialog     : access Gnome_Dialog_Record;
      Button     : Gint;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
        (Dialog     : System.Address;
         Button     : Gint;
         Accel_Key  : Gint;
         Accel_Mods : Gint);
      pragma Import (C, Internal, "gnome_dialog_set_accelerator");

   begin
      Internal
        (Get_Object (Dialog), Button,
         Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
         Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods));
   end Set_Accelerator;

   ---------------
   -- Set_Close --
   ---------------

   procedure Set_Close
     (Dialog       : access Gnome_Dialog_Record;
      Click_Closes : Boolean)
   is
      procedure Internal
        (Dialog       : System.Address;
         Click_Closes : Gint);
      pragma Import (C, Internal, "gnome_dialog_set_close");

   begin
      Internal (Get_Object (Dialog), Boolean'Pos (Click_Closes));
   end Set_Close;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (Dialog : access Gnome_Dialog_Record;
      Button : Gint)
   is
      procedure Internal
        (Dialog : System.Address;
         Button : Gint);
      pragma Import (C, Internal, "gnome_dialog_set_default");

   begin
      Internal (Get_Object (Dialog), Button);
   end Set_Default;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (Dialog : access Gnome_Dialog_Record;
      Parent : access Gtk.Window.Gtk_Window_Record'Class)
   is
      procedure Internal
        (Dialog : System.Address;
         Parent : System.Address);
      pragma Import (C, Internal, "gnome_dialog_set_parent");

   begin
      Internal (Get_Object (Dialog), Get_Object (Parent));
   end Set_Parent;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
     (Dialog  : access Gnome_Dialog_Record;
      Button  : Gint;
      Setting : Boolean)
   is
      procedure Internal
        (Dialog  : System.Address;
         Button  : Gint;
         Setting : Gint);
      pragma Import (C, Internal, "gnome_dialog_set_sensitive");

   begin
      Internal (Get_Object (Dialog), Button, Boolean'Pos (Setting));
   end Set_Sensitive;

end Gnome.Dialog;
