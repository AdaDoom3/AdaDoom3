-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                  Copyright (C) 2010-2013, AdaCore                 --
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
--  Gtk.Builder - Build an interface from an XML UI definition as produced by
--  the Glade-3 GUI builder.
--
--  Note that GtkAda provides a higher-level API for using the GUI builder,
--  in Gtkada.Builder.
--
--  A Gtk_Builder is an auxiliary object that reads textual descriptions of a
--  user interface and instantiates the described objects. To pass a
--  description to a Gtk_Builder, call Add_From_File or Add_From_String.
--  These subprograms can be called multiple times; the builder merges the
--  content of all descriptions.
--
--  A Gtk_Builder holds a reference to all objects that it has constructed and
--  drops these references when it is finalized. This finalization can cause
--  the destruction of non-widget objects or widgets which are not contained
--  in a toplevel window. For toplevel windows constructed by a builder, it is
--  the responsibility of the user to call Gtk.Widget.Destroy to get rid of
--  them and all the widgets they contain.
--
--  The subprograms Get_Object and Get_Widget can be used to access the widgets
--  in the interface by the names assigned to them inside the UI description.
--  Toplevel windows returned by this subprogram will stay around until the
--  user explicitly destroys them with Gtk.Widget.Destroy.
--  Other widgets will either be part of a larger hierarchy constructed by
--  the builder (in which case you should not have to worry about their
--  lifecycle), or without a parent, in which case they have to be added
--  to some container to make use of them. Non-widget objects need to be
--  reffed with Glib.Object.Ref to keep them beyond the lifespan of the
--  builder.
--
--  The subprogram Connect_Signals_Full can be used to connect handlers to the
--  named signals in the description.
--  </description>
--  <group>GUI Builder</group>
--  <c_version>2.16.6</c_version>

with System;
with Interfaces.C.Strings;

with Glib;
with Glib.Error;
with Glib.Object;
with Glib.Properties;

with Gtk.Widget;

package Gtk.Builder is

   type Gtk_Builder_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Builder is access all Gtk_Builder_Record'Class;

   function Get_Type return GType;

   procedure Gtk_New (Builder : out Gtk_Builder);
   procedure Initialize (Builder : access Gtk_Builder_Record'Class);
   --  Creates a new Gtk_Builder object.

   function Error_Quark return GQuark;

   function Add_From_File
     (Builder  : access Gtk_Builder_Record;
      Filename : String)
      return Glib.Error.GError;
   --  Parses a file containing a Gtk_Builder UI definition and merges it with
   --  the current contents of builder.
   --  Returns: A GError if an error occured, otherwise null.

   function Add_From_String
     (Builder : access Gtk_Builder_Record;
      Buffer  : String;
      Length  : Gsize)
      return Glib.Error.GError;
   --  Parses a string containing a Gtk_Builder UI definition and merges it
   --  with the current contents of Builder.
   --  Returns: A GError if an error occured, otherwise null.

   function Get_Object
     (Builder     : access Gtk_Builder_Record;
      Object_Name : String)
      return Glib.Object.GObject;
   --  Gets the object named Object_Name. Note that this function does not
   --  increment the reference count of the returned object.  Returns null
   --  if it could not be found in the object tree.
   --  See also Get_Widget below.

   function Get_Widget
     (Builder : access Gtk_Builder_Record;
      Name    : String) return Gtk.Widget.Gtk_Widget;
   --  Utility function to retrieve a widget created by Builder.
   --  Returns null if no widget was found with the given name.

   ------------------------
   -- Connecting signals --
   ------------------------

   --  The following is a low-level binding to Gtk+.
   --
   --  You should not need to use this directly. Instead, use a
   --  Gtkada.Builder.Gtkada_Builder and use the procedures Register_Handler
   --  to connect your callbacks to signals defined in the GUI builder.

   type Gtk_Builder_Connect_Func is access procedure
     (Builder        : System.Address;
      Object         : System.Address;
      Signal_Name    : Interfaces.C.Strings.chars_ptr;
      Handler_Name   : Interfaces.C.Strings.chars_ptr;
      Connect_Object : System.Address;
      Flags          : Glib.G_Connect_Flags;
      User_Data      : System.Address);
   pragma Convention (C, Gtk_Builder_Connect_Func);
   --  This is the signature of a subprogram used to connect signals. It is
   --  used by the Connect_Signals and Connect_Signals_Full methods.
   --
   --  Parameters:
   --     Builder:        The address of a Gtk_Builder
   --     Object:         The object to connect a signal to
   --     Signal_Name:    The name of the signal
   --     Handler_Name:   The name of the handler
   --     Connect_Object: The internal address of a GObject
   --     Flags:          G_Connect_Flags to use
   --     User_Data:      user data

   procedure Connect_Signals_Full
     (Builder         : access Gtk_Builder_Record;
      Signal_Function : Gtk_Builder_Connect_Func;
      User_Data       : System.Address);
   --  This function can be thought of the interpreted language binding
   --  version of Connect_Signals, except that it does not require GModule
   --  to function correctly.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  Name:  Translation_Domain_Property
   --  Type:  String
   --  Descr: The translation domain used by gettext
   --
   --  </properties>

   Translation_Domain_Property : constant Glib.Properties.Property_String;

private

   type Gtk_Builder_Record is new Glib.Object.GObject_Record with null record;

   Translation_Domain_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("translation-domain");

   pragma Import (C, Error_Quark, "gtk_builder_error_quark");
   pragma Import (C, Get_Type, "gtk_builder_get_type");

end Gtk.Builder;
