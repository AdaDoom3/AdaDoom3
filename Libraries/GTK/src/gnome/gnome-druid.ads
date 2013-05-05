-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
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

with Gtk;
with Gtk.Container;
with Gnome.Druid_Page;

package Gnome.Druid is

   type Gnome_Druid_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Gnome_Druid is access all Gnome_Druid_Record'Class;

   procedure Gnome_New (Widget : out Gnome_Druid);

   procedure Initialize (Widget : access Gnome_Druid_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Append_Page
     (Druid : access Gnome_Druid_Record;
      Page  : access Gnome.Druid_Page.Gnome_Druid_Page_Record);

   procedure Insert_Page
     (Druid     : access Gnome_Druid_Record;
      Back_Page : access Gnome.Druid_Page.Gnome_Druid_Page_Record;
      Page      : access Gnome.Druid_Page.Gnome_Druid_Page_Record);

   procedure Prepend_Page
     (Druid : access Gnome_Druid_Record;
      Page  : access Gnome.Druid_Page.Gnome_Druid_Page_Record);

   procedure Set_Buttons_Sensitive
     (Druid            : access Gnome_Druid_Record;
      Back_Sensitive   : Boolean;
      Next_Sensitive   : Boolean;
      Cancel_Sensitive : Boolean);

   procedure Set_Page
     (Druid : access Gnome_Druid_Record;
      Page  : access Gnome.Druid_Page.Gnome_Druid_Page_Record);

   procedure Set_Show_Finish
     (Druid       : access Gnome_Druid_Record;
      Show_Finish : Boolean);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "cancel"
   --    procedure Handler (Widget : access Gnome_Druid_Record'Class);
   --
   --  </signals>

private
   type Gnome_Druid_Record is new
     Gtk.Container.Gtk_Container_Record with null record;

   pragma Import (C, Get_Type, "gnome_druid_get_type");
end Gnome.Druid;
