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
--  G_Key_File lets you parse, edit or create files containing groups of
--  key-value pairs, which we call key files for lack of a better name. Several
--  freedesktop.org specifications use key files now, e.g the Desktop Entry
--  Specification and the Icon Theme Specification.
--
--  The syntax of key files is described in detail in the Desktop Entry
--  Specification, here is a quick summary: Key files consists of groups of
--  key-value pairs, interspersed with comments.
--
--       # this is just an example
--       # there can be comments before the first group
--       [First Group]
--       Name=Key File Example\tthis value shows\nescaping
--       # localized strings are stored in multiple key-value pairs
--       Welcome=Hello
--       Welcome[de]=Hallo
--       Welcome[fr_FR]=Bonjour
--       Welcome[it]=Ciao
--       Welcome[be@latin]=Hello
--       [Another Group]
--       Numbers=2;20;-200;0
--       Booleans=true;false;true;true
--
--  Groups are started by a header line containing the group name enclosed in
--  '[' and ']', and ended implicitly by the start of the next group or the end
--  of the file. Each key-value pair must be contained in a group.
--
--  Key-value pairs generally have the form key=value, with the exception of
--  localized strings, which have the form key[locale]=value, with a locale
--  identifier of the form lang_COUNTRYMODIFIER where COUNTRY and MODIFIER are
--  optional. Space before and after the '=' character are ignored. Newline,
--  tab, carriage return and backslash characters in value are escaped as \n,
--  \t, \r, and \\, respectively. To preserve leading spaces in values, these
--  can also be escaped as \s.
--
--  Key files can store strings (possibly with localized variants), integers,
--  booleans and lists of these. Lists are separated by a separator character,
--  typically ';' or ','. To use the list separator character in a value in a
--  list, it has to be escaped by prefixing it with a backslash.
--
--  This syntax is obviously inspired by the .ini files commonly met on
--  Windows, but there are some important differences:
--
--  .ini files use the ';' character to begin comments, key files use the '#'
--  character.
--
--  Key files do not allow for ungrouped keys meaning only comments can
--  precede the first group.
--
--  Key files are always encoded in UTF-8.
--
--  Key and Group names are case-sensitive, for example a group called [GROUP]
--  is a different group from [group].
--
--  .ini files don't have a strongly typed boolean entry type, they only have
--  GetProfileInt. In G_Key_File only true and false (in lower case) are
--  allowed.
--
--  Note that in contrast to the Desktop Entry Specification, groups in key
--  files may contain the same key multiple times; the last entry wins. Key
--  files may also contain multiple groups with the same name; they are merged
--  together. Another difference is that keys and group names in key files are
--  not restricted to ASCII characters.
--  </description>
--  <c_version>2.16.6</c_version>

with GNAT.Strings;

with Glib.Error;

package Glib.Key_File is

   type G_Key_File is new Glib.C_Proxy;

   type G_Key_File_Error is
     (Error_Unknown_Encoding,
      Error_Parse,
      Error_Not_Found,
      Error_Key_Not_Found,
      Error_Group_Not_Found,
      Error_Invalid_Value);
   pragma Convention (C, G_Key_File_Error);

   type G_Key_File_Flags is (None, Keep_Comments, Keep_Translations);
   pragma Convention (C, G_Key_File_Flags);

   --  Constants for handling freedesktop.org Desktop files
   Desktop_Group                : constant String := "Desktop Entry";

   Desktop_Key_Type             : constant String := "Type";
   Desktop_Key_Version          : constant String := "Version";
   Desktop_Key_Name             : constant String := "Name";
   Desktop_Key_Generic_Name     : constant String := "GenericName";
   Desktop_Key_No_Display       : constant String := "NoDisplay";
   Desktop_Key_Comment          : constant String := "Comment";
   Desktop_Key_Icon             : constant String := "Icon";
   Desktop_Key_Hidden           : constant String := "Hidden";
   Desktop_Key_Only_Show_In     : constant String := "OnlyShowIn";
   Desktop_Key_Not_Show_In      : constant String := "NotShowIn";
   Desktop_Key_Try_Exec         : constant String := "TryExec";
   Desktop_Key_Exec             : constant String := "Exec";
   Desktop_Key_Path             : constant String := "Path";
   Desktop_Key_Terminal         : constant String := "Terminal";
   Desktop_Key_Mime_Type        : constant String := "MimeType";
   Desktop_Key_Categories       : constant String := "Categories";
   Desktop_Key_Startup_Notify   : constant String := "StartupNotify";
   Desktop_Key_Startup_Wm_Class : constant String := "StartupWMClass";
   Desktop_Key_Url              : constant String := "URL";

   Desktop_Type_Application     : constant String := "Application";
   Desktop_Type_Link            : constant String := "Link";
   Desktop_Type_Directory       : constant String := "Directory";

   function Error_Quark return GQuark;

   procedure Gtk_New (Key_File : out G_Key_File);
   --  Creates a new empty G_Key_File object. Use Load_From_File,
   --  Load_From_Data, Load_From_Dirs or Load_From_Data_Dirs to read an
   --  existing key file.

   procedure Free (Key_File : in out G_Key_File);
   --  Frees a G_Key_File.

   procedure Set_List_Separator
     (Key_File  : G_Key_File;
      Separator : Gchar);
   --  Sets the character which is used to separate
   --  values in lists. Typically ';' or ',' are used
   --  as separators. The default list separator is ';'.

   function To_Data (Key_File : G_Key_File) return String;
   --  Output Key_File as a String.

   ------------
   -- Groups --
   ------------

   function Get_Start_Group (Key_File : G_Key_File) return String;
   --  Returns the name of the start group of the file.

   function Get_Groups (Key_File : G_Key_File) return GNAT.Strings.String_List;
   --  Returns all groups in the key file loaded with Key_File.

   function Has_Group
     (Key_File   : G_Key_File;
      Group_Name : String)
      return Boolean;
   --  Looks whether the key file has the group Group_Name.

   function Remove_Group
     (Key_File   : G_Key_File;
      Group_Name : String;
      Error      : Glib.Error.GError := null)
      return Boolean;
   --  Removes the specified group, Group_Name, from the key file.
   --  Returns True if the group was removed, False otherwise.

   ----------
   -- Keys --
   ----------

   function Get_Keys
     (Key_File   : G_Key_File;
      Group_Name : String;
      Error      : Glib.Error.GError := null)
      return GNAT.Strings.String_List;
   --  Returns all keys for the group name Group_Name.  In the event that the
   --  Group_Name cannot be found, an empty list is returned and Error is set
   --  to Error_Group_Not_Found.

   function Has_Key
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Boolean;
   --  Looks whether the key file has the key Key in the group Group_Name.

   function Remove_Key
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Boolean;
   --  Removes Key in Group_Name from the key file.
   --  Returns True if the key was removed, False otherwise.

   --------------
   -- Comments --
   --------------

   function Get_Comment
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return String;
   function Set_Comment
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String := "";
      Comment    : String;
      Error      : Glib.Error.GError := null)
      return Boolean;
   function Remove_Comment
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String := "";
      Error      : Glib.Error.GError := null)
      return Boolean;
   --  Places a comment above Key from Group_Name.
   --  If Key is null then Comment will be removed/written above Group_Name.
   --  If both Key and Group_Name are null, then Comment will be
   --  removed/written above the first group in the file.
   --
   --  Returns whether the operation was successful.

   -------------
   -- Boolean --
   -------------

   function Get_Boolean
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Boolean;
   procedure Set_Boolean
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Value      : Boolean);
   --  Association of a boolean value with Key under Group_Name.
   --
   --  Set_Boolean creates a key if Key cannot be found.
   --
   --  Get_Boolean returns the value associated with Key under Group_Name as a
   --  boolean.  If Key cannot be found then False is returned and error is set
   --  to Error_Not_Found. Likewise, if the value associated with Key cannot be
   --  interpreted as a boolean then False is returned and error is set to
   --  Error_Invalid_Value.

   type Boolean_List is array (Integer range <>) of Boolean;

   function Get_Boolean_List
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Boolean_List;
   procedure Set_Boolean_List
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      List       : Boolean_List);
   --  Associates a list of boolean values with Key under Group_Name.
   --  If Key cannot be found then it is created.
   --  If Group_Name is "", the start_group is used.

   ----------------------
   -- Gdouble (Double) --
   ----------------------

   function Get_Double
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Gdouble;
   procedure Set_Double
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Value      : Gdouble);
   --  Association of a double value with Key under Group_Name.
   --
   --  Set_Double creates a key if Key cannot be found.
   --
   --  Get_Double returns the value associated with Key under Group_Name as a
   --  double. If Group_Name is "", the start_group is used.
   --
   --  If Key cannot be found then 0.0 is returned and error is set to
   --  Error_Key_Not_Found. Likewise, if the value associated
   --  with Key cannot be interpreted as a double then 0.0 is returned
   --  and error is set to Error_Invalid_Value.

   type Double_List is array (Integer range <>) of Gdouble;
   pragma Convention (C, Double_List);

   function Get_Double_List
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Double_List;
   procedure Set_Double_List
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      List       : Double_List);
   --  Associates a list of double values with Key under
   --  Group_Name.  If Key cannot be found then it is created.

   --------------------
   -- Gint (Integer) --
   --------------------

   function Get_Integer
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Gint;
   procedure Set_Integer
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Value      : Gint);
   --  Association of an integer value with Key under Group_Name.
   --
   --  Set_Integer creates a key if Key cannot be found.
   --
   --  If Get_Integer cannot find Key then 0 is returned and Error is set to
   --  Error_Key_Not_Found. Likewise, if the value associated
   --  with Key cannot be interpreted as an integer then 0 is returned
   --  and error is set to Error_Invalid_Value.

   type Integer_List is array (Integer range <>) of Gint;
   pragma Convention (C, Integer_List);

   function Get_Integer_List
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return Integer_List;
   procedure Set_Integer_List
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      List       : Integer_List);
   --  Associates a list of integer values with Key under Group_Name.
   --  If Key cannot be found then it is created.

   ------------
   -- String --
   ------------

   function Get_String
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return String;
   procedure Set_String
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      The_String : String);
   --  Associates a new string value with Key under Group_Name.
   --  If Key cannot be found then it is created.
   --  If Group_Name cannot be found then it is created.
   --  Unlike Set_Value, this function handles characters
   --  that need escaping, such as newlines.

   function Get_String_List
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return GNAT.Strings.String_List;
   procedure Set_String_List
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      List       : GNAT.Strings.String_List);
   --  Associates a list of string values for Key under Group_Name.
   --  If Key cannot be found then it is created.
   --  If Group_Name cannot be found then it is created.

   -----------
   -- Value --
   -----------

   function Get_Value
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Error      : Glib.Error.GError := null)
      return String;
   procedure Set_Value
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Value      : String);
   --  Associates a new value with Key under Group_Name.
   --
   --  If Key cannot be found then it is created. If Group_Name cannot
   --  be found then it is created. To set an UTF-8 string which may contain
   --  characters that need escaping (such as newlines or spaces), use
   --  Set_String.

   -------------
   -- Loading --
   -------------

   function Load_From_Data
     (Key_File : G_Key_File;
      Data     : String;
      Flags    : G_Key_File_Flags;
      Error    : Glib.Error.GError := null)
      return Boolean;
   --  Loads Data (which holds the contents of a key file) into an empty
   --  G_Key_File structure.  If the object cannot be created then Error
   --  is set to a G_Key_File_Error.
   --
   --  Returns True if a key file could be loaded, False otherwise

   function Load_From_Data_Dirs
     (Key_File : G_Key_File;
      File     : String;
      Flags    : G_Key_File_Flags;
      Error    : Glib.Error.GError := null)
      return Boolean;
   --  This function looks for a key file named File in the paths
   --  returned from g_get_user_data_dir() and g_get_system_data_dirs()
   --  and loads the file into Key_File.  If the file could not be loaded
   --  then Error is set to either a G_File_Error or G_Key_File_Error.
   --
   --  Returns True if a key file could be loaded, False otherwise

   function Load_From_Dirs
     (Key_File    : G_Key_File;
      File        : String;
      Search_Dirs : GNAT.Strings.String_List;
      Flags       : G_Key_File_Flags;
      Error       : Glib.Error.GError := null)
      return Boolean;
   --  This function looks for a key file named File in the paths specified
   --  in Search_Dirs and loads the file into Key_File.  If the file could not
   --  be loaded then an Error is set to either a G_File_Error or
   --  G_Key_File_Error.
   --
   --  Returns True if a key file could be loaded, False otherwise

   function Load_From_File
     (Key_File : G_Key_File;
      File     : String;
      Flags    : G_Key_File_Flags;
      Error    : Glib.Error.GError := null)
      return Boolean;
   --  Loads a key file into an empty G_Key_File structure.
   --  If the file could not be loaded then Error is set to
   --  either a G_File_Error or G_Key_File_Error.
   --
   --  Returns True if a key file could be loaded, False otherwise

   -------------
   -- Locales --
   -------------

   function Get_Locale_String
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Locale     : String)
      return String;
   procedure Set_Locale_String
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Locale     : String;
      The_String : String);
   --  Associates a string value for Key and Locale under Group_Name.
   --  If the translation for Key cannot be found then it is created.

   function Get_Locale_String_List
     (Key_File   : G_Key_File;
      Group_Name : String;
      Key        : String;
      Locale     : String;
      Error      : Glib.Error.GError := null)
      return GNAT.Strings.String_List;
   procedure Set_Locale_String_List
     (Key_File   : G_Key_File;
      Group_Name : String := "";
      Key        : String;
      Locale     : String;
      List       : GNAT.Strings.String_List);
   --  Associates a list of string values for Key and Locale under
   --  Group_Name.  If the translation for Key cannot be found then
   --  it is created.

private

   pragma Import (C, Error_Quark, "g_key_file_error_quark");
   pragma Import (C, Free, "g_key_file_free");
   pragma Import (C, Set_List_Separator, "g_key_file_set_list_separator");

end Glib.Key_File;
