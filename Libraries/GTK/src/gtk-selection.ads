-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2000-2013, AdaCore                  --
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
--  This package implements support for the selection mechanism (ie a way to
--  get a currently active selection anywhere on your Xserver or on your
--  Windows machine).
--
--  This also acts as the low-level support for drag-and-drop, as described
--  in Gtk.Dnd.
--
--  A lot of subprograms in this package work on Gdk_Atom types, instead of
--  strings. Converting from one to the other can easily be done through
--  calls to the subprograms in Gdk.Property (Atom_Intern and Atom_Name).
--  The reason we use Gdk_Atom is for efficiency, since comparing two integers
--  is of course faster than comparing two strings.
--
--  The selection mechanism is the primary mechanism by which applications
--  can transfer data to each other on a given system. Even though both
--  applications must be visible on the same screen, this does not mean that
--  they can access the same files or ressources, since they might in fact
--  be running on different machines. You should always keep this in mind
--  when setting the data to be transfered.

--  A selection is a essentially a named clipboard, identified by a string
--  interned as a Gdk_Atom. By claiming ownership of a selection, an
--  application indicates that it will be responsible for supplying its
--  contents.

--  The contents of a selection can be represented in a number of formats,
--  called targets. Each target is identified by an atom. A list of all
--  possible targets supported by the selection owner can be retrieved by
--  requesting the special target TARGETS. When a selection is retrieved, the
--  data is accompanied by a type (an atom), and a format (an integer,
--  representing the number of bits per item).
--
--  See also http://standards.freedesktop.org/clipboards-spec/ for
--  more information on the way selection works on X-Window systems.
--
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Inter-Process communication</group>
--  <testgtk>create_selection.adb</testgtk>

with Gdk.Types;
with Gdk.Pixbuf;
with Gtk.Widget;
with Gtkada.Types;
with GNAT.Strings;

package Gtk.Selection is

   type Selection_Data is new Gdk.C_Proxy;
   --  Contents of a selection or a drag-and-drop operation.
   --  This structure can only be created internally by GtkAda. However, you
   --  need to be able to access it to get the selection.
   --   - Selection and Target identify the request.
   --   - Type specifies the type of the return.
   --   - if Length is negative, the Data should be ignored. Otherwise, it
   --     contains the data itself.
   --   - Time gives the timestamp at which the data was sent.

   ---------------
   -- Selection --
   ---------------

   subtype Gdk_Selection is Gdk.Types.Gdk_Atom;
   --  These are predefined atom values for several common selections.
   --  You are of course free to create new ones, but most of the time you
   --  should simply use Selection_Primary unless you foresee the need for
   --  multiple simultaneous selections.
   --  To access the clipboard on windows machines, you might need to create
   --  a new selection with Gdk.Property.Atom_Intern ("CLIPBOARD");

   Selection_Primary   : constant Gdk_Selection;
   Selection_Secondary : constant Gdk_Selection;

   --------------------
   -- Selection_Type --
   --------------------

   subtype Gdk_Selection_Type is Gdk.Types.Gdk_Atom;
   --  Predefined atom values for selection types.
   --  Although the preferred way in GtkAda to indicate the type of a selection
   --  is to use mime types, these values are provided for compatibility with
   --  older X11 applications.

   Selection_Type_Atom     : constant Gdk_Selection_Type;
   --  A Gdk_Atom (format=32 bits)

   Selection_Type_Bitmap   : constant Gdk_Selection_Type;
   --  A Gdk_Bitmap Id (format=32 bits)

   Selection_Type_Colormap : constant Gdk_Selection_Type;
   --  A colormap Id (format=32 bits)

   Selection_Type_Drawable : constant Gdk_Selection_Type;
   --  A drawable Id (format=32 bits), ie the result of Gdk.Window.Get_Window.

   Selection_Type_Integer  : constant Gdk_Selection_Type;
   --  An integer (format=32 bits)

   Selection_Type_Pixmap   : constant Gdk_Selection_Type;
   --  A Gdk_Pixmap ID (format=32 bits)

   Selection_Type_Window   : constant Gdk_Selection_Type;
   --  A Gdk_Window ID (format=32 bits)

   Selection_Type_String   : constant Gdk_Selection_Type;
   --  A string encoded in Iso-latin1 format (format=8 bits per character)

   ----------------
   -- Gdk_Target --
   ----------------

   subtype Gdk_Target is Gdk.Types.Gdk_Atom;
   --  Predefined atom values which are used to describe possible targets for
   --  a selection. Other atoms can be used, and the recommended practice for
   --  GtkAda is to to use mime types for this purpose. However, supporting
   --  these types may be useful for compatibility with older programs.

   Target_Bitmap   : constant Gdk_Target;
   Target_Colormap : constant Gdk_Target;
   Target_Drawable : constant Gdk_Target;
   Target_Pixmap   : constant Gdk_Target;
   Target_String   : constant Gdk_Target;

   ------------------
   -- Target_Flags --
   ------------------

   type Target_Flags is new Integer;
   --  Used to specify constraints on an entry

   Target_No_Constraint : constant Target_Flags;
   --  No constraint is specified.

   Target_Same_App      : constant Target_Flags;
   --  If this is set, the target will only be selected for drags within a
   --  single application.

   Target_Same_Widget   : constant Target_Flags;
   --  If this is set, the target will only be selected for drags within a
   --  single widget.

   ------------------
   -- Target_Entry --
   ------------------

   type Target_Entry is record
      Target : Gtkada.Types.Chars_Ptr;
      Flags  : Target_Flags;
      Info   : Guint;
   end record;
   --  A single type of data that can be supplied or received during a
   --  drag-and-drop or a selection.
   --
   --  Target is a string that represents the drag type. This can be any
   --  string if you want to implement drag-and-drop inside your application.
   --  However, if you want to communicate with other external application,
   --  you should use MIME types, ie "text/plain", "text/uri-list", ...
   --  See the RFCs 2045, 2046, 2047, 2048, 2049 for more information on
   --  MIME types.
   --
   --  For more information, see
   --  ftp://ftp.isi.edu/in-notes/iana/assignments/media-types/
   --
   --  Another set of supported names are the ones associated with the X
   --  Inter-Client Communications Conventions Manual (ICCCM).
   --  Here some of the default names and their meaning. See the ICCCM manual
   --  online for a complete list (for instance at
   --  http://www.tronche.com/gui/x/icccm/).
   --
   --   - "TIMESTAMP"   (type Integer)  Timestamp used to acquire the selection
   --   - "TEXT"        (type Text)     Text in owner's encoding
   --   - "STRING"      (type String)   Iso Latin1 text
   --   - "PIXMAP"      (type Drawable) Pixmap Id
   --   - "BITMAP"      (type Bitmap)   Bitmap Id
   --   - "FOREGROUND"  (type Pixel)    Pixel Value
   --   - "BACKGROUND"  (type Pixel)    Pixel Value
   --
   --  Info is an application-assigned integer (i.e. that you choose), that
   --  will be passed as a signal parameter for all the dnd-related signals,
   --  like "selection_get". This saves a lot of expensive string compares,
   --  and in fact replaced Target everywhere in your application expect in
   --  Source_Set and Dest_Set.

   type Target_Entry_Array is array (Natural range <>) of Target_Entry;

   Any_Target_Entry : Target_Entry_Array (1 .. 0);
   --  To be used for drop sites that accept any kind of data.

   -----------------
   -- Target_List --
   -----------------

   type Target_List is new Gdk.C_Proxy;
   --  A list of targets.
   --  You can only manipulate this list through the functions below.

   function Target_List_New (Targets : Target_Entry_Array) return Target_List;
   --  Create a new list of target, starting from an array.

   procedure Target_List_Ref (List : Target_List);
   --  Increment the reference count for the list.
   --  You should almost never have to use this function, this is done
   --  transparently by GtkAda.

   procedure Target_List_Unref (List : Target_List);
   --  Decrement the reference count for the list.
   --  You should almost never have to use this function, since everything is
   --  done transparently by GtkAda.
   --  As usual, the list is freed when the reference count reaches 0.

   procedure Target_List_Add
     (List   : Target_List;
      Target : Gdk.Types.Gdk_Atom;
      Flags  : Guint;
      Info   : Guint);
   --  Add a new target to the list.
   --  You can for instance use the result of Get_Targets (Drag_Context) for
   --  the value of Target.

   procedure Target_List_Add_Table
     (List    : Target_List;
      Targets : Target_Entry_Array);
   --  Add a new set of targets to the list.

   procedure Target_List_Add_Text_Targets
     (List      : Target_List;
      Info      : Guint);
   --  Appends the text targets supported internally by gtk+ to List.
   --  All targets are added with the same info.
   --  Info will be passed back to the application.

   procedure Target_List_Add_URI_Targets
     (List      : Target_List;
      Info      : Guint);
   --  Appends the URI targets supported internally by gtk+ to List.
   --  All targets are added with the same info.

   procedure Target_List_Add_Image_Targets
     (List      : Target_List;
      Info      : Guint;
      Writable  : Boolean);
   --  Appends the image targets supported internally by gtk+ to List.
   --  All targets are added with the same info.
   --  If Writable is True, then only those targets for which gtk+ knows how to
   --  convert a Gdk_Pixbuf into the format are added.

   procedure Target_List_Remove
     (List   : Target_List;
      Target : Gdk.Types.Gdk_Atom);
   --  Remove a specific target from the list.

   procedure Target_List_Find
     (List   : Target_List;
      Target : Gdk.Types.Gdk_Atom;
      Info   : out Guint;
      Found  : out Boolean);
   --  Search for a specific target in the list.
   --  If the target was found, Found is set to True and Info contains the
   --  integer that was associated with the target when it was created.

   --------------------
   -- Selection_Data --
   --------------------

   function Selection_Get_Type return Glib.GType;
   --  Return the internal type used for a selection

   function Get_Selection (Selection : Selection_Data) return Gdk_Selection;
   --  Return the selection used (primary, clipboard, ...)

   function Get_Target (Selection : Selection_Data) return Gdk.Types.Gdk_Atom;
   --  Return the target of the selection (ie a MIME string that identifies
   --  the selection).

   function Get_Type (Selection : Selection_Data) return Gdk.Types.Gdk_Atom;
   --  Return the type of the selection, as defined in Gdk_Selection_Type,
   --  ie for compatibility with older X11 applications.

   function Get_Format (Selection : Selection_Data) return Gint;
   --  Return the format of the data.
   --  The semantics depends on the type of data. For instance, for strings
   --  this is the number of bits per character.

   function Get_Data (Selection : Selection_Data) return System.Address;
   --  Return the data of the selection.
   --  This should be ignored if Get_Length returns a value < 0.

   function Get_Data_As_String (Selection : Selection_Data) return String;
   --  Return the data as a string.
   --  This is only a convenience function, since it simply creates a string
   --  from the return of Get_Data.

   function Get_Length (Selection : Selection_Data) return Gint;
   --  Return the length of the data.

   ----------------------------------
   -- Setting and getting contents --
   ----------------------------------

   function Set_Pixbuf
     (Selection : Selection_Data;
      Pixbuf    : Gdk.Pixbuf.Gdk_Pixbuf) return Boolean;
   --  Sets the contents of the selection from a pixbuf
   --  The pixbuf is converted to the form determined by
   --  Get_Target (Selection_Data).
   --  Returns True if the selection was successfully set.

   function Get_Pixbuf
     (Selection : Selection_Data) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Gets the contents of the selection data as a pixbuf.
   --  Return value: if the selection data contained a recognized
   --  image type and it could be converted to a pixbuf, a
   --  newly allocated pixbuf is returned, otherwise null.
   --  If the result is non-null it must be freed with Unref.

   function Targets_Include_Image
     (Selection : Selection_Data; Writable : Boolean := True) return Boolean;
   --  Given a Selection object holding a list of targets, determines if any of
   --  the targets in these targets can be used to provide a Gdk.Pixbuf.
   --  Writable: whether to accept only targets for which gtk+ knows how to
   --  convert a pixbuf into the format.
   --  Returns True if Selection holds a list of targets and a suitable
   --  target for images is included

   function Set_Text
     (Selection : Selection_Data;
      Str       : UTF8_String) return Boolean;
   --  Sets the contents of the selection from a UTF-8 encoded string.
   --  The string is converted to the form determined by
   --  Get_Target (Selection_Data).

   function Get_Text (Selection : Selection_Data) return UTF8_String;
   --  Gets the contents of the selection data as a UTF-8 string.
   --  Return value: if the selection data contained a recognized
   --  text type and it could be converted to UTF-8, the string is returned.

   function Targets_Include_Text (Selection : Selection_Data) return Boolean;
   --  Given a Selection object holding a list of targets, determines if any of
   --  the targets can be used to provide text.

   function Set_Uris
     (Selection : Selection_Data;
      URIs      : GNAT.Strings.String_List)
      return Boolean;
   --  Sets the contents of the selection from a list of URIs.
   --  The string is converted to the form determined by
   --  Get_Target (Selection).
   --  Return True if the selection was successfully set.

   function Get_Uris
     (Selection : Selection_Data)
      return GNAT.Strings.String_List;
   --  Gets the contents of the selection data as array of URIs.
   --  The returned value must be freed by the caller.

   function Get_Targets
     (Selection : Selection_Data) return Gdk.Types.Gdk_Atom_Array;
   --  Gets the contents of Selection_Data as an array of targets.
   --  This can be used to interpret the results of getting
   --  the standard TARGETS target that is always supplied for
   --  any selection.
   --  This is different from Get_Target, which indicate the current format
   --  that the selection contains. Get_Targets only applies when Get_Target
   --  is "TARGETS".

   procedure Selection_Data_Set
     (Selection : Selection_Data;
      The_Type  : Gdk.Types.Gdk_Atom;
      Format    : Gint;
      Data      : System.Address;
      Length    : Gint);
   --  General form of Selection_Data_Set.
   --  Any data can be transmitted. Length is the number of bytes in Data.

   pragma Import (C, Selection_Data_Set, "gtk_selection_data_set");

   procedure Selection_Data_Set
     (Selection : Selection_Data;
      The_Type  : Gdk.Types.Gdk_Atom;
      Format    : Gint;
      Data      : String);
   --  Set the data for a selection (special case for strings)
   --  This function is generally called when a drag-and-drop operation
   --  ask the source widget for the data to be transmitted. In that case,
   --  a Selection_Data was already transmitted and is given as a handler
   --  parameter for the signal "drag_data_get". The_Type can simply be
   --  extracted from the Selection_Data.

   function Selection_Data_Copy
     (Selection : Selection_Data) return Selection_Data;
   --  Make a copy of a selection data.

   procedure Selection_Data_Free (Selection : Selection_Data);
   --  Free a Selection_Data structure returned from Selection_Data_Copy.

   --------------------------------
   -- Manipulating the selection --
   --------------------------------

   function Owner_Set
     (Widget    : Gtk.Widget.Gtk_Widget;
      Selection : Gdk_Selection := Selection_Primary;
      Time      : Guint32 := 0) return Boolean;
   --  Claim ownership of a given selection for a particular widget,
   --  or, if widget is null, release ownership of the selection.
   --
   --  Once a Widget has claimed selection, it is responsible for delivering
   --  the data whenever it is needed.
   --
   --  Time is the timestamp for claiming the selection (default is the current
   --  time).
   --  This function returns True if the operation succeeded.

   procedure Add_Target
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection;
      Target    : Gdk.Types.Gdk_Atom;
      Info      : Guint);
   --  Add specified target to the list of supported targets for a given
   --  widget and selection.
   --  Info is an integer which will be passed back to the application instead
   --  of a string when the target is used.

   procedure Add_Targets
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection;
      Targets   : Target_Entry_Array);
   --  Add a set of targets to the list of supported targets for a given widget
   --  and selection.

   procedure Clear_Targets
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection);
   --  Clear the list of supported targets for a given widget and selection.

   function Convert
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection := Selection_Primary;
      Target    : Gdk.Types.Gdk_Atom;
      Time      : Guint32 := 0) return Boolean;
   --  Request the contents of a selection.
   --  When received, a "selection_received" signal will be generated, and the
   --  widget needs to have a handler for it.
   --
   --  Target is the form of information desired, for instance an intern
   --  Gdk_Atom whose name is "text/plain", or one of the Gdk_Target values.
   --
   --  This function returns True if the request succeeded, False if the
   --  request could not be processed, for instance if there was already a
   --  request in process for this widget or this target is not known by the
   --  owner of the selection.
   --
   --  Widget is the widget which acts as a requestor.

   procedure Remove_All (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Remove all handlers and unsets ownership of all selections for a widget.
   --  Called when widget is being destroyed. This function will not generally
   --  be called by applications.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for the class
   --  Gtk.Widget.Gtk_Widget to support drag-and-drop.
   --  Please note that no default marshaller is provided in GtkAda for these
   --  handlers, and that you will have to use the general form of callbacks
   --  instead, getting the value of the parameters directly from the
   --  Gtk_Args structure.
   --
   --  - "selection_get"  (source side)
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class;
   --                       Data   : Selection_Data;
   --                       Info   : Guint;
   --                       Time   : Guint);
   --
   --    This signal is sent to the owner of a selection whenever some other
   --    widget wants to get data from that selection. The type of the data
   --    is indicated in Info, and is the third field that was set in the
   --    Target_Entrys for that specific widget and selection.
   --
   --    The handler should modify the Data in the selection.
   --
   --  - "selection_received"  (client side)
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class;
   --                       Data   : Selection_Data;
   --                       Time   : Guint);
   --
   --    This signal is sent to the receiver end of a selection, when the data
   --    has been sent by the owner. The receiver should call Convert, which
   --    will emit the signal selection_get to ask for the contents of the
   --    selection, and then selection_received will be emitted to warn the
   --    receiver.
   --
   --    Note: you can not connect this signal to a widget that does not have
   --    an associated Gdk_Window (i.e the flag Gtk.Widget.No_Window must not
   --    be set for this widget), since it needs to be able to receive
   --    Property_Notify events from the server. It will not work with a
   --    Gtk_Label for instance.
   --
   --  </signals>

private

   pragma Import (C, Target_List_Ref, "gtk_target_list_ref");
   pragma Import (C, Target_List_Unref, "gtk_target_list_unref");
   pragma Import (C, Target_List_Add, "gtk_target_list_add");
   pragma Import (C, Target_List_Remove, "gtk_target_list_remove");

   pragma Import (C, Get_Selection, "gtk_selection_data_get_selection");
   pragma Import (C, Get_Target,    "gtk_selection_data_get_target");
   pragma Import (C, Get_Type,      "gtk_selection_data_get_data_type");
   pragma Import (C, Get_Format,    "gtk_selection_data_get_format");
   pragma Import (C, Get_Data,      "gtk_selection_data_get_data");
   pragma Import (C, Get_Length,    "gtk_selection_data_get_length");

   pragma Import (C, Selection_Data_Copy, "gtk_selection_data_copy");
   pragma Import (C, Selection_Data_Free, "gtk_selection_data_free");
   pragma Import (C, Selection_Get_Type, "gtk_selection_data_get_type");
   pragma Import (C, Target_List_Add_Text_Targets,
                  "gtk_target_list_add_text_targets");
   pragma Import (C, Target_List_Add_URI_Targets,
                  "gtk_target_list_add_uri_targets");

   function Make_Atom (Num : Gulong) return Gdk.Types.Gdk_Atom;
   pragma Import (C, Make_Atom, "ada_make_atom");

   Selection_Primary   : constant Gdk_Selection := Make_Atom (1);
   Selection_Secondary : constant Gdk_Selection := Make_Atom (2);

   Selection_Type_Atom     : constant Gdk_Selection_Type := Make_Atom (4);
   Selection_Type_Bitmap   : constant Gdk_Selection_Type := Make_Atom (5);
   Selection_Type_Colormap : constant Gdk_Selection_Type := Make_Atom (7);
   Selection_Type_Drawable : constant Gdk_Selection_Type := Make_Atom (17);
   Selection_Type_Integer  : constant Gdk_Selection_Type := Make_Atom (19);
   Selection_Type_Pixmap   : constant Gdk_Selection_Type := Make_Atom (20);
   Selection_Type_Window   : constant Gdk_Selection_Type := Make_Atom (33);
   Selection_Type_String   : constant Gdk_Selection_Type := Make_Atom (31);

   Target_No_Constraint : constant Target_Flags := 0;
   Target_Same_App      : constant Target_Flags := 1;
   Target_Same_Widget   : constant Target_Flags := 2;

   Target_Bitmap   : constant Gdk_Target := Make_Atom (5);
   Target_Colormap : constant Gdk_Target := Make_Atom (7);
   Target_Drawable : constant Gdk_Target := Make_Atom (17);
   Target_Pixmap   : constant Gdk_Target := Make_Atom (20);
   Target_String   : constant Gdk_Target := Make_Atom (31);
end Gtk.Selection;

--  This function is indicated as obsolescent by gtk+ developers:
--  No binding: gtk_selection_clear

--  No binding: gtk_selection_owner_set_for_display
