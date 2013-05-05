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

with Gtkada.Types; use Gtkada.Types;
with Gtk;
with Gtk.Widget;

package Gnome.Pixmap is

   type Gnome_Pixmap_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gnome_Pixmap is access all Gnome_Pixmap_Record'Class;

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer);

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Gnome_New
     (Widget  : out Gnome_Pixmap;
      Gpixmap : access Gnome_Pixmap_Record);

   procedure Initialize
     (Widget  : access Gnome_Pixmap_Record'Class;
      Gpixmap : access Gnome_Pixmap_Record);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Xpm_Data : Chars_Ptr_Array);

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Xpm_Data : Chars_Ptr_Array);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Filename : String);

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Filename : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Filename : String;
      Width    : Integer;
      Height   : Integer);

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Filename : String;
      Width    : Integer;
      Height   : Integer);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Load_File
     (Gpixmap  : access Gnome_Pixmap_Record;
      Filename : String);

   procedure Load_File_At_Size
     (Gpixmap  : access Gnome_Pixmap_Record;
      Filename : String;
      Width    : Integer;
      Height   : Integer);

   procedure Load_Xpm_D
     (Gpixmap  : access Gnome_Pixmap_Record;
      Xpm_Data : Chars_Ptr_Array);

   procedure Load_Xpm_D_At_Size
     (Gpixmap  : access Gnome_Pixmap_Record;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Pixmap_Record is new
     Gtk.Widget.Gtk_Widget_Record with null record;

   pragma Import (C, Get_Type, "gnome_pixmap_get_type");
end Gnome.Pixmap;
