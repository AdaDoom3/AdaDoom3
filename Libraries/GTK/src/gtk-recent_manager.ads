-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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
--  Gtk_Recent_Manager provides a facility for adding, removing and looking up
--  recently used files. Each recently used file is identified by its URI, and
--  has meta-data associated to it, like the names and command lines of the
--  applications that have registered it, the number of time each application
--  has registered the same file, the mime type of the file and whether the
--  file should be displayed only by the applications that have registered it.
--
--  The Gtk_Recent_Manager acts like a database of all the recently used files.
--  You can create new Gtk_Recent_Manager objects, but it is more efficient to
--  use the standard recent manager for the Gdk_Screen so that information
--  about recently used files is shared with other people using them. In case
--  the default screen is being used, adding a new recently used file is
--  as simple as:
--
--     declare
--        Manager : constant Gtk_Recent_Manager := Get_Default;
--     begin
--        Add_Item (Manager, File_URI);
--     end;
--
--  While looking up a recently used file is as simple as using:
--
--     declare
--        Manager : constant Gtk_Recent_Manager := Get_Default;
--        Info    : Gtk_Recent_Info;
--        Error   : Glib.Error.GError;
--     begin
--        Lookup_Item (Info, Manager, File_URI, Error);
--        if Error /= null then
--           --  Use the info object
--           Unref (Info);
--        else
--           Put_Line
--             ("Could not find the file: " & Glib.Error.Get_Message (Error));
--           Glib.Error.Error_Free (Error);
--        end if;
--     end;
--
--  Recently used files are supported since GTK+ 2.10.
--  </description>
--  <c_version>2.16.6</c_version>

with GNAT.Strings;

with Glib.Error;
with Glib.Glist;
with Glib.Properties;
with Glib.Object;
with Gdk.Pixbuf;
with Gdk.Screen;

package Gtk.Recent_Manager is

   type Gtk_Recent_Manager_Record is
     new Glib.Object.GObject_Record with private;
   type Gtk_Recent_Manager is access all Gtk_Recent_Manager_Record'Class;

   subtype time_t is Long_Integer;
   --  Type to interface with C's time_t type.  To convert this to/from
   --  an Ada type, look at Ada.Calendar.Conversion_Operations and be
   --  sure to pay special attention to the ranges each type is capable
   --  of representing.

   -----------------
   -- Recent_Info --
   -----------------

   type Gtk_Recent_Info_Record is
     new Glib.Object.GObject_Record with private;
   type Gtk_Recent_Info is access all Gtk_Recent_Info_Record'Class;

   function Convert (Widget : Gtk_Recent_Info) return System.Address;
   function Convert (Widget : System.Address) return Gtk_Recent_Info;

   package Gtk_Recent_Info_List is
     new Glib.Glist.Generic_List (Gtk_Recent_Info);
   --  Instantiation of a list of Gtk_Recent_Info objects.

   function Get_Type_Recent_Info return GType;
   --  Return the internal value associated with a Gtk_Recent_Info widget.

   function Exists (Info : access Gtk_Recent_Info_Record) return Boolean;
   --  Checks whether the resource pointed to by Info still exists.  At
   --  the moment this check is done only on resources pointing to local
   --  files.

   function Get_Added (Info : access Gtk_Recent_Info_Record) return time_t;
   --  Gets the timestamp (seconds from system's Epoch) when the resource
   --  was added to the recently used resources list.
   --
   --  Return value: the number of seconds elapsed from system's Epoch when
   --  the resource was added to the list, or -1 on failure.

   function Get_Age (Info : access Gtk_Recent_Info_Record) return Gint;
   --  Gets the number of days elapsed since the last update of the resource
   --  pointed by Info.
   --
   --  Return value: a positive integer containing the number of days
   --  elapsed since the time this resource was last modified.

   type Application_Info_Record is record
      Result : Boolean;
      --  True if an application with App_Name has registered this resource
      --  inside the recently used list, or False otherwise.

      App_Exec : String_Ptr;
      --  Pointer to the string containing the command line.  Free using
      --  Glib.Free when no longer needed.

      Count : Guint;
      --  The number of times this item was registered.

      Time : time_t;
      --  The timestamp this item was last registered for this application.
   end record;
   --  Information returned by Get_Application_Info, below.

   function Get_Application_Info
     (Info     : access Gtk_Recent_Info_Record;
      App_Name : String)
      return Application_Info_Record;
   --  Gets the data regarding the application that has registered the resource
   --  pointed by Info.
   --
   --  If the command line contains any escape characters defined inside the
   --  storage specification, they will be expanded.

   function Get_Description
     (Info : access Gtk_Recent_Info_Record) return UTF8_String;
      --  Gets the (short) description of the resource.

   function Get_Display_Name
     (Info : access Gtk_Recent_Info_Record) return String;
   --  Gets the name of the resource.  If none has been defined, the
   --  basename of the resource is obtained.

   function Get_Icon
     (Info : access Gtk_Recent_Info_Record;
      Size : Gint)
      return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Retrieves the icon of size Size (in pixels) associated to the
   --  resource MIME type.  Returns a Gdk_Pixbuf containing the icon, or
   --  null. Use Glib.Object.Unref when finished using the icon.

   function Get_Mime_Type
     (Info : access Gtk_Recent_Info_Record) return UTF8_String;
   --  Gets the MIME type of the resource.

   function Get_Modified (Info : access Gtk_Recent_Info_Record) return time_t;
   --  Gets the timestamp (seconds from system's Epoch) when the resource
   --  was last modified. Returns -1 on failure.

   function Get_Private_Hint
     (Info : access Gtk_Recent_Info_Record) return Boolean;
   --  Gets the value of the "private" flag.  Resources in the recently used
   --  list that have this flag set to True should only be displayed by the
   --  applications that have registered them.

   function Get_Uri (Info : access Gtk_Recent_Info_Record) return UTF8_String;
   --  Gets the URI of the resource.

   function Get_Visited (Info : access Gtk_Recent_Info_Record) return time_t;
   --  Gets the timestamp (seconds from system's Epoch) when the resource
   --  was last visited. Returns -1 on failure.

   function Has_Application
     (Info     : access Gtk_Recent_Info_Record;
      App_Name : UTF8_String)
      return Boolean;
   --  Checks whether an application registered this resource using
   --  App_Name.

   function Has_Group
     (Info       : access Gtk_Recent_Info_Record;
      Group_Name : UTF8_String)
      return Boolean;
   --  Checks whether Group_Name appears inside the groups registered for
   --  the recently used item Info.

   function Is_Local (Info : access Gtk_Recent_Info_Record) return Boolean;
   --  Checks whether the resource is local or not by looking at the
   --  scheme of its URI.

   function Match
     (Info_A : access Gtk_Recent_Info_Record'Class;
      Info_B : access Gtk_Recent_Info_Record'Class)
      return Boolean;
   --  Checks whether two Gtk_Recent_Info structures point to the same
   --  resource.

   function Ref (Info : access Gtk_Recent_Info_Record) return Gtk_Recent_Info;
   --  Increases the reference count of Recent_Info by one.

   procedure Unref (Info : access Gtk_Recent_Info_Record);
   --  Decreases the reference count of Info by one.  If the reference
   --  count reaches zero, Info is deallocated, and the memory freed.

   --------------------
   -- Recent_Manager --
   --------------------

   procedure Gtk_New (Widget : out Gtk_Recent_Manager);
   procedure Initialize (Widget : access Gtk_Recent_Manager_Record'Class);
   --  Creates a new recent manager object.  Recent manager objects are used to
   --  handle the list of recently used resources.  A Gtk_Recent_Manager object
   --  monitors the recently used resources list, and emits the "changed"
   --  signal each time something inside the list changes.
   --
   --  Gtk_Recent_Manager objects are expensive: be sure to create them only
   --  when needed. You should use Gtk.Recent_Manager.Get_Default instead.

   function Get_Type return GType;
   --  Return the internal value associated with a Gtk_Recent_Manager widget.

   function Error_Quark return GQuark;

   function Add_Full
     (Manager      : access Gtk_Recent_Manager_Record;
      Uri          : UTF8_String;
      Display_Name : UTF8_String := "";
      Description  : UTF8_String := "";
      Mime_Type    : UTF8_String;
      App_Name     : UTF8_String;
      App_Exec     : UTF8_String;
      Groups       : GNAT.Strings.String_List;
      Is_Private   : Boolean)
      return Boolean;
   --  Manager      : the Gtk_Recent_Manager on which to operate
   --  Uri          : pointer to resource
   --  Display_Name : a UTF-8 encoded string, containing the name of the
   --                 recently used resource to be displayed, or "".
   --  Description  : a UTF-8 encoded string, containing a short description
   --                 of the resource, or "".
   --  Mime_Type    : the MIME type of the resource.
   --  App_Name     : the name of the application that is registering this
   --                 recently used resource.
   --  App_Exec     : command line used to launch this resource; may contain
   --                 the "%f" and "%u" escape characters which will be
   --                 expanded to the resource file path and URI, respectively,
   --                 when the command line is retrieved.
   --  Groups       : a vector of strings containing groups names.
   --  Is_Private   : whether this resource should be displayed only by the
   --                 applications that have registered it or not.
   --
   --  Adds a new resource, pointed by Uri, into the recently used
   --  resources list, using the metadata specified.
   --
   --  The passed URI will be used to identify this resource inside the
   --  list.
   --
   --  In order to register the new recently used resource, metadata about
   --  the resource must be passed as well as the URI.  This metadata must
   --  contain the MIME type of the resource pointed by the URI; the name of
   --  the application that is registering the item, and a command line to be
   --  used when launching the item.
   --
   --  Optionally, it is possible to specify a UTF-8 string to be used when
   --  viewing the item instead of the last component of the URI; a short
   --  description of the item; whether the item should be considered private -
   --  that is, should be displayed only by the applications that have
   --  registered it.
   --
   --  Returns True if the new item was successfully added to the recently
   --  used resources list, False otherwise.

   function Add_Item
     (Manager : access Gtk_Recent_Manager_Record;
      Uri     : UTF8_String)
      return Boolean;
   --  Adds a new resource, pointed by Uri, into the recently used
   --  resources list.
   --
   --  This function automatically retrieves some of the needed
   --  metadata and setting other metadata to common default values; it
   --  then feeds the data to Add_Full.
   --
   --  See Add_Full if you want to explicitly define the metadata for the
   --  resource pointed by Uri.
   --
   --  Returns True if the new item was successfully added to the recently
   --  used resources list

   function Get_Default return Gtk_Recent_Manager;
   --  Gets a unique instance of Gtk_Recent_Manager that you can share
   --  in your application without caring about memory management. The
   --  returned instance will be freed when you application terminates.
   --
   --  Do not ref or unref the returned manager.

   function Get_Items
     (Manager : access Gtk_Recent_Manager_Record)
      return Gtk_Recent_Info_List.Glist;
   --  Gets the list of recently used resources.  Use Unref on each item
   --  inside the list, and then free the list itself using
   --  Glib.Object.Object_Simple_List.Free.

   function Get_Limit (Manager : access Gtk_Recent_Manager_Record) return Gint;
   procedure Set_Limit
     (Manager : access Gtk_Recent_Manager_Record;
      Limit   : Gint);
   --  Maximum number of items that the Get_Items function should return.
   --  If Limit is set to -1, then return all the items.

   function Has_Item
     (Manager : access Gtk_Recent_Manager_Record;
      Uri     : UTF8_String)
      return Boolean;
   --  Checks whether there is a recently used resource registered
   --  with Uri inside the recent manager.

   function Lookup_Item
     (Manager : access Gtk_Recent_Manager_Record;
      Uri     : UTF8_String;
      Error   : Glib.Error.GError)
      return Gtk_Recent_Info;
   --  Searches for a URI inside the recently used resources list, and
   --  returns a structure containing information about the resource
   --  like its MIME type, or its display name.
   --
   --  Returns null if the URI was not registered in the recently used
   --  resources list.  Free with Unref.

   function Move_Item
     (Manager : access Gtk_Recent_Manager_Record;
      Uri     : UTF8_String;
      New_Uri : UTF8_String;
      Error   : Glib.Error.GError)
      return Boolean;
   --  Changes the location of a recently used resource from Uri to New_Uri.
   --  A New_Uri of "" will remove the item pointed to by Uri in the list.
   --
   --  Please note that this function will not affect the resource pointed
   --  by the URIs, but only the URI used in the recently used resources list.

   function Purge_Items
     (Manager : access Gtk_Recent_Manager_Record;
      Error   : Glib.Error.GError)
      return Gint;
   --  Purges every item from the recently used resources list.
   --
   --  Returns the number of items that have been removed from the
   --  recently used resources list.

   function Remove_Item
     (Manager : access Gtk_Recent_Manager_Record;
      Uri     : UTF8_String;
      Error   : Glib.Error.GError)
      return Boolean;
   --  Removes a resource pointed by Uri from the recently used resources
   --  list handled by a recent manager.

   -----------------
   -- Obsolescent --
   -----------------

   function Get_For_Screen
     (Screen : access Gdk.Screen.Gdk_Screen_Record) return Gtk_Recent_Manager;
   pragma Obsolescent;  --  Get_For_Screen
   --  Gets the recent manager object associated with Screen; if this
   --  function has not previously been called for the given screen,
   --  a new recent manager object will be created and associated with
   --  the screen. Recent manager objects are fairly expensive to create,
   --  so using this function is usually a better choice than calling
   --  New and setting the screen yourself; by using this function a single
   --  recent manager object will be shared between users.
   --
   --  Return value: A unique Gtk_Recent_Manager associated with the given
   --  screen. This recent manager is associated to the with the screen
   --  and can be used as long as the screen is open. Do not ref or
   --  unref it.
   --
   --  Deprecated: 2.12: This function has been deprecated and should
   --  not be used in newly written code. Calling this function is
   --  equivalent to calling Get_Default.

   procedure Set_Screen
     (Manager : access Gtk_Recent_Manager_Record;
      Screen  : access Gdk.Screen.Gdk_Screen_Record'Class);
   pragma Obsolescent;  --  Set_Screen
   --  Sets the screen for a recent manager; the screen is used to
   --  track the user's currently configured recently used documents
   --  storage.
   --
   --  Deprecated: 2.12: This function has been deprecated and should
   --  not be used in newly written code. Calling this function has
   --  no effect.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  Name:  Filename_Property
   --  Type:  String
   --  Descr: The full path to the file to be used to store and read the list
   --
   --  Name:  Limit_Property
   --  Type:  Int
   --  Descr: The maximum number of items to be returned by
   --         Gtk.Recent_Manager.Get_Items
   --
   --  Name:  Size_Property
   --  Type:  Int
   --  Descr: The size of the recently used resources list
   --
   --  </properties>

   Filename_Property : constant Glib.Properties.Property_String;
   Limit_Property    : constant Glib.Properties.Property_Int;
   Size_Property     : constant Glib.Properties.Property_Int;

private

   type Gtk_Recent_Manager_Record is
     new Glib.Object.GObject_Record with null record;

   type Gtk_Recent_Info_Record is
     new Glib.Object.GObject_Record with null record;

   Filename_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("filename");
   Limit_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("limit");
   Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");

   pragma Import (C, Error_Quark, "gtk_recent_manager_error_quark");
   pragma Import (C, Get_Type, "gtk_recent_manager_get_type");
   pragma Import (C, Get_Type_Recent_Info, "gtk_recent_info_get_type");

end Gtk.Recent_Manager;
