-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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
--  Gtk_Buildable allows objects to extend and customize their deserialization
--  from Gtk_Builder UI descriptions. The interface includes methods for
--  setting names and properties of objects, parsing custom tags and
--  constructing child objects.
--
--  The Gtk_Buildable interface is implemented by all widgets and many of the
--  non-widget objects that are provided by GTK+. The main user of this
--  interface is Gtk_Builder. There should be very little need for applications
--  to call any gtk_buildable_... functions.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Glib.Types;  use Glib.Types;
with Glib.Values; use Glib.Values;
with Gtk.Builder; use Gtk.Builder;

package Gtk.Buildable is

   type Gtk_Buildable is new Glib.Types.GType_Interface;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_buildable_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Child
      (Self     : Gtk_Buildable;
       Builder  : access Gtk.Builder.Gtk_Builder_Record'Class;
       Child    : access Glib.Object.GObject_Record'Class;
       The_Type : UTF8_String);
   --  Adds a child to Buildable. Type is an optional string describing how
   --  the child should be added.
   --  Since: gtk+ 2.12
   --  "builder": a Gtk.Builder.Gtk_Builder
   --  "child": child to add
   --  "type": kind of child or null

   function Construct_Child
      (Self    : Gtk_Buildable;
       Builder : access Gtk.Builder.Gtk_Builder_Record'Class;
       Name    : UTF8_String) return Glib.Object.GObject;
   --  Constructs a child of Buildable with the name Name.
   --  Gtk.Builder.Gtk_Builder calls this function if a "constructor" has been
   --  specified in the UI definition.
   --  Since: gtk+ 2.12
   --  "builder": Gtk.Builder.Gtk_Builder used to construct this object
   --  "name": name of child to construct

   procedure Custom_Finished
      (Self    : Gtk_Buildable;
       Builder : access Gtk.Builder.Gtk_Builder_Record'Class;
       Child   : access Glib.Object.GObject_Record'Class;
       Tagname : UTF8_String;
       Data    : System.Address);
   --  This is similar to Gtk.Buildable.Parser_Finished but is called once for
   --  each custom tag handled by the Buildable.
   --  Since: gtk+ 2.12
   --  "builder": a Gtk.Builder.Gtk_Builder
   --  "child": child object or null for non-child tags
   --  "tagname": the name of the tag
   --  "data": user data created in custom_tag_start

   procedure Custom_Tag_End
      (Self    : Gtk_Buildable;
       Builder : access Gtk.Builder.Gtk_Builder_Record'Class;
       Child   : access Glib.Object.GObject_Record'Class;
       Tagname : UTF8_String;
       Data    : System.Address);
   --  This is called at the end of each custom element handled by the
   --  buildable.
   --  Since: gtk+ 2.12
   --  "builder": Gtk.Builder.Gtk_Builder used to construct this object
   --  "child": child object or null for non-child tags
   --  "tagname": name of tag
   --  "data": user data that will be passed in to parser functions

   function Get_Internal_Child
      (Self      : Gtk_Buildable;
       Builder   : access Gtk.Builder.Gtk_Builder_Record'Class;
       Childname : UTF8_String) return Glib.Object.GObject;
   --  Get the internal child called Childname of the Buildable object.
   --  Since: gtk+ 2.12
   --  "builder": a Gtk.Builder.Gtk_Builder
   --  "childname": name of child

   function Get_Name (Self : Gtk_Buildable) return UTF8_String;
   procedure Set_Name (Self : Gtk_Buildable; Name : UTF8_String);
   --  Sets the name of the Buildable object.
   --  Since: gtk+ 2.12
   --  "name": name to set

   procedure Parser_Finished
      (Self    : Gtk_Buildable;
       Builder : access Gtk.Builder.Gtk_Builder_Record'Class);
   --  Called when the builder finishes the parsing of a <link
   --  linkend="BUILDER-UI">GtkBuilder UI definition</link>. Note that this
   --  will be called once for each time Gtk.Builder.Add_From_File or
   --  Gtk.Builder.Add_From_String is called on a builder.
   --  Since: gtk+ 2.12
   --  "builder": a Gtk.Builder.Gtk_Builder

   procedure Set_Buildable_Property
      (Self    : Gtk_Buildable;
       Builder : access Gtk.Builder.Gtk_Builder_Record'Class;
       Name    : UTF8_String;
       Value   : out Glib.Values.GValue);
   --  Sets the property name Name to Value on the Buildable object.
   --  Since: gtk+ 2.12
   --  "builder": a Gtk.Builder.Gtk_Builder
   --  "name": name of property
   --  "value": value of property

end Gtk.Buildable;
