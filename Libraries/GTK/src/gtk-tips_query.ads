-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2007 AdaCore                    --
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

--  <c_version>2.8.17</c_version>
--  <group>Obsolescent widgets</group>
--  <doc_ignore>

with Gtk.Label;
with Gtk.Widget;

package Gtk.Tips_Query is
   pragma Obsolescent;

   type Gtk_Tips_Query_Record is new Gtk.Label.Gtk_Label_Record with private;
   type Gtk_Tips_Query is access all Gtk_Tips_Query_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Tips_Query);

   procedure Initialize (Widget : access Gtk_Tips_Query_Record'Class);

   procedure Set_Caller
     (Tips_Query : access Gtk_Tips_Query_Record;
      Caller     : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Set_Labels
     (Tips_Query     : access Gtk_Tips_Query_Record;
      Label_Inactive : UTF8_String;
      Label_No_Tip   : UTF8_String);

   procedure Start_Query (Tips_Query : access Gtk_Tips_Query_Record);

   procedure Stop_Query (Tips_Query : access Gtk_Tips_Query_Record);

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "widget_selected"
   --  - "start_query"
   --  - "stop_query"
   --  - "widget_entered"
   --
   --  </signals>

   Signal_Widget_Selected : constant Glib.Signal_Name := "widget_selected";
   Signal_Start_Query     : constant Glib.Signal_Name := "start_query";
   Signal_Stop_Query      : constant Glib.Signal_Name := "stop_query";
   Signal_Widget_Entered  : constant Glib.Signal_Name := "widget_entered";

private
   type Gtk_Tips_Query_Record is new Gtk.Label.Gtk_Label_Record
     with null record;
end Gtk.Tips_Query;

--  This subprogram was never implemented, and the whole package is now
--  obsolescent
--  No binding: gtk_tips_query_get_type

--  </doc_ignore>
