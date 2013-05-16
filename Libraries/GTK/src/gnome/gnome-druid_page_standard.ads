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

with Gdk.Color, Gdk.Pixbuf;
--  with Gdk.ImlibImage;
with Gnome.Druid_Page;
with Gtk;

package Gnome.Druid_Page_Standard is

   type Gnome_Druid_Page_Standard_Record is new
     Gnome.Druid_Page.Gnome_Druid_Page_Record with private;
   type Gnome_Druid_Page_Standard is
     access all Gnome_Druid_Page_Standard_Record'Class;

   --  procedure Gnome_New
   --    (Widget: out Gnome_Druid_Page_Standard;
   --     Title : String;
   --     Logo  : Gdk.ImlibImage.Gdk_ImlibImage);

   --  procedure Initialize
   --    (Widget: access Gnome_Druid_Page_Standard_Record'Class;
   --     Title : String;
   --     Logo  : Gdk.ImlibImage.Gdk_ImlibImage);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gnome_New (Widget : out Gnome_Druid_Page_Standard);

   procedure Initialize
     (Widget : access Gnome_Druid_Page_Standard_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Set_Title
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Title               : String);

   procedure Set_Logo
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Logo                : Gdk.Pixbuf.Gdk_Pixbuf);

   procedure Set_Top_Watermark
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Top_Watermark       : Gdk.Pixbuf.Gdk_Pixbuf);

   procedure Set_Title_Foreground
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Color               : Gdk.Color.Gdk_Color);

   procedure Set_Background
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Color               : Gdk.Color.Gdk_Color);

   procedure Set_Logo_Background
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Color               : Gdk.Color.Gdk_Color);

   procedure Set_Contents_Background
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Color               : Gdk.Color.Gdk_Color);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Druid_Page_Standard_Record is new
     Gnome.Druid_Page.Gnome_Druid_Page_Record with null record;

   pragma Import (C, Get_Type, "gnome_druid_page_standard_get_type");
end Gnome.Druid_Page_Standard;
