-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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
--  A Gtk_File_Filter can be used to restrict the files being shown in a
--  Gtk_File_Chooser. Files can be filtered based on their name (with
--  Add_Pattern), on their mime type (with Add_Mime_Type), or by a custom
--  filter function (with Add_Custom).
--
--  Filtering by mime types handles aliasing and subclassing of mime types;
--  e.g. a filter for text/plain also matches a file with mime type
--  application/rtf, since application/rtf is a subclass of text/plain. Note
--  that Gtk_File_Filter allows wildcards for the subtype of a mime type, so
--  you can e.g. filter for image/*.
--
--  Normally, filters are used by adding them to a Gtk_File_Chooser, see
--  Add_Filter, but it is also possible to manually use a filter on a file with
--  Filter.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Selectors</group>

with Glib;
with Glib.Object;
with System;

package Gtk.File_Filter is

   type Gtk_File_Filter_Record is
     new Glib.Object.GObject_Record with private;
   type Gtk_File_Filter is access all Gtk_File_Filter_Record'Class;

   type File_Filter_Info is new Glib.C_Proxy;
   --  An opaque structure that contains information about a file

   function Get_Filename     (Info : File_Filter_Info) return String;
   function Get_Uri          (Info : File_Filter_Info) return String;
   function Get_Display_Name (Info : File_Filter_Info) return String;
   function Get_Mime_Type    (Info : File_Filter_Info) return String;
   --  Return the various information known about the file. The empty string is
   --  returned when the associated information is unknown. Display_Name is the
   --  string used to display the file in a file_chooser.

   type File_Filter_Flags is mod 2 ** 8;
   Filter_Filename     : constant File_Filter_Flags := 2 ** 0;
   Filter_Uri          : constant File_Filter_Flags := 2 ** 1;
   Filter_Display_Name : constant File_Filter_Flags := 2 ** 2;
   Filter_Mime_Type    : constant File_Filter_Flags := 2 ** 3;
   --  These flags indicate what parts of a Gtk_File_Filter_Info struct are
   --  filled or need to be filled.

   type File_Filter_Func is access function
     (Info : File_Filter_Info) return Gboolean;
   pragma Convention (C, File_Filter_Func);
   --  Function used by custom filters

   function Get_Type return GType;
   --  Return the internal value associated with a Gtk_File_Filter

   procedure Gtk_New    (Filter : out Gtk_File_Filter);
   procedure Initialize (Filter : access Gtk_File_Filter_Record'Class);
   --  Creates a new Gtk_File_Filter with no rules added to it. Such a filter
   --  doesn't accept any files, so is not particularly useful until you add
   --  rules with Add_Mime_Type, Add_Pattern, or Add_Custom. To create a filter
   --  that accepts any file, use:
   --      Gtk_New (Filter);
   --      Add_Pattern (Filter, "*");

   procedure Set_Name
     (Filter : access Gtk_File_Filter_Record; Name : String);
   function Get_Name (Filter : access Gtk_File_Filter_Record) return String;
   --  Sets the human-readable name of the filter; this is the string
   --  that will be displayed in the file selector user interface if
   --  there is a selectable list of filters.

   procedure Add_Mime_Type
     (Filter    : access Gtk_File_Filter_Record;
      Mime_Type : String);
   --  Adds a rule allowing a given mime type to Filter.
   --  In particular, if you want to show directories only and not files, you
   --  could use "x-directory/normal" as the Mime type

   procedure Add_Pattern
     (Filter  : access Gtk_File_Filter_Record;
      Pattern : String);
   --  Adds a rule allowing a shell style glob to a filter.

   procedure Add_Pixbuf_Formats
     (Filter : access Gtk_File_Filter_Record);
   --  Adds a rule allowing image files in the formats supported
   --  by Gdk_Pixbuf.

   procedure Add_Custom
     (Filter : access Gtk_File_Filter_Record;
      Needed : File_Filter_Flags;
      Func   : File_Filter_Func;
      Data   : System.Address := System.Null_Address;
      Notify : G_Destroy_Notify_Address := null);
   --  Adds rule to a filter that allows files based on a custom callback
   --  function. The bitfield Needed which is passed in provides information
   --  about what sorts of information that the filter function needs;
   --  this allows GTK+ to avoid retrieving expensive information when
   --  it isn't needed by the filter.
   --  Notify is called when Data is no longer needed and should be freed.

private
   type Gtk_File_Filter_Record is
     new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_file_filter_get_type");
end Gtk.File_Filter;

--  No binding: gtk_file_filter_filter
--  No binding: gtk_file_filter_get_needed
