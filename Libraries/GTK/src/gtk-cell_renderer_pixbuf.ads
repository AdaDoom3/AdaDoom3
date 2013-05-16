-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2013, AdaCore                   --
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
--  A Gtk_Cell_Renderer_Pixbuf can be used to render an image in a cell. It
--  allows to render either a given Gdk_Pixbuf (set via the pixbuf property) or
--  a stock icon (set via the stock-id property).
--
--  To support the tree view, Gtk_Cell_Renderer_Pixbuf also supports rendering
--  two alternative pixbufs, when the is-expander property is TRUE. If the
--  is-expanded property is TRUE and the pixbuf-expander-open property is set
--  to a pixbuf, it renders that pixbuf, if the is-expanded property is FALSE
--  and the pixbuf-expander-closed property is set to a pixbuf, it renders that
--  one.
--  </description>
--  <c_version>2.14</c_version>
--  <group>Trees and Lists</group>

with Glib.Properties;
with Gtk.Cell_Renderer;

package Gtk.Cell_Renderer_Pixbuf is

   type Gtk_Cell_Renderer_Pixbuf_Record is
     new Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record with private;
   type Gtk_Cell_Renderer_Pixbuf is
     access all Gtk_Cell_Renderer_Pixbuf_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Cell_Renderer_Pixbuf);
   procedure Initialize
     (Widget : access Gtk_Cell_Renderer_Pixbuf_Record'Class);
   --  Creates or initializes a new renderer

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Follow_State_Property
   --  Type:  Boolean
   --  Descr: Whether the rendered pixbuf should be
   --
   --  Name:  Gicon_Property
   --  Type:  Object
   --  Descr: The GIcon being displayed
   --
   --  Name:  Icon_Name_Property
   --  Type:  String
   --  Descr: The name of the icon from the icon theme
   --
   --  Name:  Pixbuf_Property
   --  Type:  Object
   --  Descr: The pixbuf to render
   --
   --  Name:  Pixbuf_Expander_Closed_Property
   --  Type:  Object
   --  Descr: Pixbuf for closed expander
   --
   --  Name:  Pixbuf_Expander_Open_Property
   --  Type:  Object
   --  Descr: Pixbuf for open expander
   --
   --  Name:  Stock_Detail_Property
   --  Type:  String
   --  Descr: Render detail to pass to the theme engine
   --
   --  Name:  Stock_Id_Property
   --  Type:  String
   --  Descr: The stock ID of the stock icon to render
   --
   --  Name:  Stock_Size_Property
   --  Type:  Uint
   --  Descr: The GtkIconSize value that specifies the size of the rendered
   --         icon
   --
   --  </properties>

   Follow_State_Property           : constant Glib.Properties.Property_Boolean;
   Gicon_Property                  : constant Glib.Properties.Property_Object;
   Icon_Name_Property              : constant Glib.Properties.Property_String;
   Pixbuf_Property                 : constant Glib.Properties.Property_Object;
   Pixbuf_Expander_Closed_Property : constant Glib.Properties.Property_Object;
   Pixbuf_Expander_Open_Property   : constant Glib.Properties.Property_Object;
   Stock_Detail_Property           : constant Glib.Properties.Property_String;
   Stock_Id_Property               : constant Glib.Properties.Property_String;
   Stock_Size_Property             : constant Glib.Properties.Property_Uint;

private
   type Gtk_Cell_Renderer_Pixbuf_Record is
     new Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record with null record;

   Follow_State_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("follow-state");
   Gicon_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("gicon");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Pixbuf_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixbuf");
   Pixbuf_Expander_Closed_Property : constant Glib.Properties.Property_Object
     := Glib.Properties.Build ("pixbuf-expander-closed");
   Pixbuf_Expander_Open_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixbuf-expander-open");
   Stock_Detail_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock-detail");
   Stock_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock-id");
   Stock_Size_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("stock-size");

   pragma Import (C, Get_Type, "gtk_cell_renderer_pixbuf_get_type");
end Gtk.Cell_Renderer_Pixbuf;
