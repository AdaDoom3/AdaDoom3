-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2010-2013, AdaCore                   --
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

--  <description>
--  A Gtk_Recent_Filter can be used to restrict the files being shown in a
--  Gtk_Recent_Chooser. Files can be filtered based on their name (with
--  Add_Pattern), on their mime type (with Add_Mime_Type), on the application
--  that has registered them (with Add_Application), or by a custom filter
--  function (with Add_Custom).
--
--  Filtering by mime type handles aliasing and subclassing of mime types;
--  e.g. a filter for text/plain also matches a file with mime type
--  application/rtf, since application/rtf is a subclass of text/plain.
--  Note that Gtk_Recent_Filter allows wildcards for the subtype of a mime
--  type, so you can e.g. filter for image/*.
--
--  Normally, filters are used by adding them to a Gtk_Recent_Chooser, see
--  Gtk.Recent_Chooser.Add_Filter, but it is also possible to manually use a
--  filter on a file with Filter.
--  </description>
--  <c_version>2.16.6</c_version>

with Glib.GSlist;
with Glib.Object;

package Gtk.Recent_Filter is

   type Gtk_Recent_Filter_Record is
     new Glib.Object.GObject_Record with private;
   type Gtk_Recent_Filter is access all Gtk_Recent_Filter_Record'Class;

   function Get_Type return GType;

   function Convert (Widget : Gtk_Recent_Filter) return System.Address;
   function Convert (Widget : System.Address) return Gtk_Recent_Filter;
   package Gtk_Recent_Filter_List is
     new Glib.GSlist.Generic_SList (Gtk_Recent_Filter);
   --  Instantiation of a singly-linked list of Gtk_Recent_Filter's.

   procedure Gtk_New (Widget : out Gtk_Recent_Filter);
   procedure Initialize (Widget : access Gtk_Recent_Filter_Record'Class);
   --  Creates a new Gtk_Recent_Filter with no rules added to it.
   --  Such filter does not accept any recently used resources, so is not
   --  particularly useful until you add rules with Add_Pattern, Add_Mime_Type,
   --  Add_Application, Add_Age.  To create a filter that accepts any recently
   --  used resource, use:
   --
   --     declare
   --        Filter : Gtk_Recent_Filter;
   --     begin
   --        Gtk_New (Filter);
   --        Add_Pattern (Filter, "*");
   --     end;

   procedure Add_Age
     (Filter : access Gtk_Recent_Filter_Record;
      Days   : Gint);
   --  Adds a rule that allows resources based on their age - that is, the
   --  number of days elapsed since they were last modified.

   procedure Add_Application
     (Filter      : access Gtk_Recent_Filter_Record;
      Application : UTF8_String);
   --  Adds a rule that allows resources based on the name of the application
   --  that has registered them.

   procedure Add_Group
     (Filter : access Gtk_Recent_Filter_Record;
      Group  : UTF8_String);
   --  Adds a rule that allows resources based on the name of the group
   --  to which they belong

   procedure Add_Mime_Type
     (Filter    : access Gtk_Recent_Filter_Record;
      Mime_Type : UTF8_String);
   --  Adds a rule that allows resources based on their registered MIME type.

   procedure Add_Pattern
     (Filter  : access Gtk_Recent_Filter_Record;
      Pattern : UTF8_String);
   --  Adds a rule that allows resources based on a pattern matching their
   --  display name.

   procedure Add_Pixbuf_Formats (Filter : access Gtk_Recent_Filter_Record);
   --  Adds a rule allowing image files in the formats supported
   --  by Gdk_Pixbuf.

   function Get_Name
     (Filter : access Gtk_Recent_Filter_Record) return UTF8_String;
   procedure Set_Name
     (Filter : access Gtk_Recent_Filter_Record;
      Name   : UTF8_String);
   --  Gets/Sets the human-readable name of the filter; this is the string
   --  that will be displayed in the recently used resources selector
   --  user interface if there is a selectable list of filters.

private

   type Gtk_Recent_Filter_Record is
     new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_recent_filter_get_type");

end Gtk.Recent_Filter;
