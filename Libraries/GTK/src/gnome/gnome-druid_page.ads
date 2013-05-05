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
with Gtk.Bin;

package Gnome.Druid_Page is

   type Gnome_Druid_Page_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Gnome_Druid_Page is access all Gnome_Druid_Page_Record'Class;

   function Back (Druid_Page : access Gnome_Druid_Page_Record)
                  return Boolean;

   function Cancel (Druid_Page : access Gnome_Druid_Page_Record)
                    return Boolean;

   procedure Finish (Druid_Page : access Gnome_Druid_Page_Record);

   function Next (Druid_Page : access Gnome_Druid_Page_Record)
                  return Boolean;

   procedure Prepare (Druid_Page : access Gnome_Druid_Page_Record);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "next"
   --    function Handler (Widget : access Gnome_Druid_Page_Record'Class;
   --       Druid : access Gtk.Widget.Gtk_Widget_Record'Class)
   --       return Gboolean;
   --
   --  - "prepare"
   --    procedure Handler (Widget : access Gnome_Druid_Page_Record'Class;
   --       Druid : access Gtk.Widget.Gtk_Widget_Record'Class);
   --
   --  - "back"
   --    function Handler (Widget : access Gnome_Druid_Page_Record'Class;
   --       Druid : access Gtk.Widget.Gtk_Widget_Record'Class)
   --       return Gboolean;
   --
   --  - "finish"
   --    procedure Handler (Widget : access Gnome_Druid_Page_Record'Class;
   --       Druid : access Gtk.Widget.Gtk_Widget_Record'Class);
   --
   --  - "cancel"
   --    function Handler (Widget : access Gnome_Druid_Page_Record'Class;
   --       Druid : access Gtk.Widget.Gtk_Widget_Record'Class)
   --       return Gboolean;
   --
   --  </signals>

private
   type Gnome_Druid_Page_Record is new Gtk.Bin.Gtk_Bin_Record with null record;

end Gnome.Druid_Page;
