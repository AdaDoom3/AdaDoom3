-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2002                          --
--                         ACT-Europe                                --
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

with Glib; use Glib;
with Gtk;
with Gtk.Button;
with Gtk.Widget;

package Gnome.Font_Picker is

   type Gnome_Font_Picker_Record is new
     Gtk.Button.Gtk_Button_Record with private;
   type Gnome_Font_Picker is access all Gnome_Font_Picker_Record'Class;

   type Gnome_Font_Picker_Mode is
     (Mode_Pixmap,
      Mode_Font_Info,
      Mode_User_Widget,
      Mode_Unknown);

   procedure Gnome_New (Widget : out Gnome_Font_Picker);

   procedure Initialize (Widget : access Gnome_Font_Picker_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Fi_Set_Show_Size
     (Gfp       : access Gnome_Font_Picker_Record;
      Show_Size : Boolean);

   procedure Fi_Set_Use_Font_In_Label
     (Gfp               : access Gnome_Font_Picker_Record;
      Use_Font_In_Label : Boolean;
      Size              : Gint);

   function Get_Font_Name
     (Gfp : access Gnome_Font_Picker_Record) return String;

   function Get_Preview_Text
     (Gfp : access Gnome_Font_Picker_Record) return String;

   function Set_Font_Name
     (Gfp      : access Gnome_Font_Picker_Record;
      Fontname : String) return Boolean;

   procedure Set_Mode
     (Gfp  : access Gnome_Font_Picker_Record;
      Mode : Gnome_Font_Picker_Mode);

   procedure Set_Preview_Text
     (Gfp  : access Gnome_Font_Picker_Record;
      Text : String);

   procedure Set_Title
     (Gfp   : access Gnome_Font_Picker_Record;
      Title : String);

   procedure Uw_Set_Widget
     (Gfp    : access Gnome_Font_Picker_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "font_set"
   --    procedure Handler (Widget : access Gnome_Font_Picker_Record'Class;
   --       Font_Name : String);
   --
   --  </signals>

private
   type Gnome_Font_Picker_Record is new
     Gtk.Button.Gtk_Button_Record with null record;

   pragma Import (C, Get_Type, "gnome_font_picker_get_type");
end Gnome.Font_Picker;
