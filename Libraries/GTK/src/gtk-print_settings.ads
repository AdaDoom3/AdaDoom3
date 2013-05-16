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
--  A Gtk_Print_Settings object represents the settings of a print dialog in a
--  system-independent way. The main use for this object is that once you've
--  printed you can get a settings object that represents the settings the user
--  chose, and the next time you print you can pass that object in so that the
--  user doesn't have to re-set all his settings.
--
--  It's also possible to enumerate the settings so that you can easily save
--  the settings for the next time your app runs, or even store them in a
--  document. The predefined keys try to use shared values as much as possible
--  so that moving such a document between systems still works.
--  </description>
--  <c_version>2.16.6</c_version>

with Glib.Error;
with Glib.Key_File;
with Glib.Object;
with Gtk.Enums;
with Gtk.Paper_Size;

package Gtk.Print_Settings is

   type Gtk_Print_Settings_Record is
      new Glib.Object.GObject_Record with private;
   type Gtk_Print_Settings is access all Gtk_Print_Settings_Record'Class;

   function Get_Type return GType;

   procedure Gtk_New (Widget : out Gtk_Print_Settings);
   procedure Initialize (Widget : access Gtk_Print_Settings_Record'Class);
   --  Creates a new Gtk_Print_Settings object.

   procedure Gtk_New_From_File
     (Widget    : out Gtk_Print_Settings;
      File_Name : String;
      Error     : Glib.Error.GError := null);
   procedure Initialize_From_File
     (Widget    : access Gtk_Print_Settings_Record'Class;
      File_Name : String;
      Error     : Glib.Error.GError := null);
   --  Reads the print settings from File_Name. Returns a new
   --  Gtk_Print_Settings object with the restored settings, or null if an
   --  error occurred.  See To_File.

   procedure Gtk_New_From_Key_File
     (Widget     : out Gtk_Print_Settings;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error      : Glib.Error.GError := null);
   procedure Initialize_From_Key_File
     (Widget     : access Gtk_Print_Settings_Record'Class;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error      : Glib.Error.GError := null);
   --  Reads the print settings from the group Group_Name in Key_File.
   --  Returns a new Gtk_Print_Settings object with the restored settings,
   --  or null if an error occurred.

   function Copy
     (Other : access Gtk_Print_Settings_Record)
      return Gtk_Print_Settings;
   --  Copies a Gtk_Print_Settings object.

   -----------------
   -- Basic Types --
   -----------------

   function Has_Key
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String)
      return Boolean;
   --  Returns True, if a value is associated with Key.

   function Get
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String)
      return String;
   procedure Set
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Value    : String := "");
   --  Manipulate string value associated with Key.

   procedure Unset
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String);
   --  Removes any value associated with Key.
   --  This has the same effect as setting the value to null.

   function Get_Bool
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String)
      return Boolean;
   procedure Set_Bool
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Value    : Boolean);
   --  Gets/Sets the boolean represented by the value
   --  that is associated with Key.

   function Get_Double
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Def      : Gdouble := 0.0)
      return Gdouble;
   procedure Set_Double
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Value    : Gdouble);
   --  Gets/Sets the floating point number represented by
   --  the value that is associated with Key, or Def
   --  if the value does not represent a floating point number.
   --
   --  Floating point numbers are parsed with g_ascii_strtod().

   function Get_Int
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Def      : Gint := 0)
      return Gint;
   procedure Set_Int
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Value    : Gint);
   --  Gets/Sets the value of Key. Get_Int interprets the value as
   --  an integer, or else returns the default value.

   function Get_Length
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Unit     : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   procedure Set_Length
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Value    : Gdouble;
      Unit     : Gtk.Enums.Gtk_Unit);
   --  Gets/Sets the value associated with Key, interpreted
   --  as a length. The returned value is converted to Units.

   --------------------
   -- Print Settings --
   --------------------

   function Get_Collate
     (Settings : access Gtk_Print_Settings_Record)
      return Boolean;
   procedure Set_Collate
     (Settings : access Gtk_Print_Settings_Record;
      Collate  : Boolean);
   --  Whether to collate the printed pages

   function Get_Default_Source
     (Settings : access Gtk_Print_Settings_Record)
      return String;
   procedure Set_Default_Source
     (Settings       : access Gtk_Print_Settings_Record;
      Default_Source : String);
   --  The default source

   function Get_Dither
     (Settings : access Gtk_Print_Settings_Record)
      return String;
   procedure Set_Dither
     (Settings : access Gtk_Print_Settings_Record;
      Dither   : String);
   --  Gets/Sets the dithering that is used

   function Get_Duplex
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Print_Duplex;
   procedure Set_Duplex
     (Settings : access Gtk_Print_Settings_Record;
      Duplex   : Gtk.Enums.Gtk_Print_Duplex);
   --  Whether to print the output in duplex.

   function Get_Finishings
     (Settings : access Gtk_Print_Settings_Record)
      return String;
   procedure Set_Finishings
     (Settings   : access Gtk_Print_Settings_Record;
      Finishings : String);
   --  The finishings

   function Get_Media_Type
     (Settings : access Gtk_Print_Settings_Record)
      return String;
   procedure Set_Media_Type
     (Settings   : access Gtk_Print_Settings_Record;
      Media_Type : String);
   --  Gets/Sets the media type.
   --  The set of media types is defined in PWG 5101.1-2002 PWG.

   function Get_N_Copies
     (Settings : access Gtk_Print_Settings_Record)
      return Gint;
   procedure Set_N_Copies
     (Settings   : access Gtk_Print_Settings_Record;
      Num_Copies : Gint);
   --  Gets/Sets the number of copies to print

   function Get_Number_Up
     (Settings : access Gtk_Print_Settings_Record)
      return Gint;
   procedure Set_Number_Up
     (Settings  : access Gtk_Print_Settings_Record;
      Number_Up : Gint);
   --  Gets/Sets the number of pages per sheet

   function Get_Number_Up_Layout
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Number_Up_Layout;
   procedure Set_Number_Up_Layout
     (Settings         : access Gtk_Print_Settings_Record;
      Number_Up_Layout : Gtk.Enums.Gtk_Number_Up_Layout);
   --  Gets/Sets layout of page in number-up mode

   function Get_Orientation
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Page_Orientation;
   procedure Set_Orientation
     (Settings    : access Gtk_Print_Settings_Record;
      Orientation : Gtk.Enums.Gtk_Page_Orientation);
   --  Get the orientation.

   function Get_Output_Bin
     (Settings : access Gtk_Print_Settings_Record)
      return String;
   procedure Set_Output_Bin
     (Settings   : access Gtk_Print_Settings_Record;
      Output_Bin : String);
   --  Gets/Sets the output bin.

   type Gtk_Page_Range_Record is record
      Range_Start : Gint;
      Range_End   : Gint;
   end record;
   pragma Convention (C, Gtk_Page_Range_Record);
   type Gtk_Page_Range_Array is
     array (Integer range <>) of Gtk_Page_Range_Record;
   pragma Convention (C, Gtk_Page_Range_Array);
   --  Page range specification(s).

   function Get_Page_Ranges
     (Settings   : access Gtk_Print_Settings_Record)
      return Gtk_Page_Range_Array;
   procedure Set_Page_Ranges
     (Settings    : access Gtk_Print_Settings_Record;
      Page_Ranges : access Gtk_Page_Range_Array);
   --  Gets/Sets an array of Gtk_Page_Range_Records.

   function Get_Page_Set
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Page_Set;
   procedure Set_Page_Set
     (Settings : access Gtk_Print_Settings_Record;
      Page_Set : Gtk.Enums.Gtk_Page_Set);
   --  Gets/Sets the set of pages to print

   function Get_Paper_Height
     (Settings : access Gtk_Print_Settings_Record;
      Unit     : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   function Get_Paper_Width
     (Settings : access Gtk_Print_Settings_Record;
      Unit     : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   procedure Set_Paper_Height
     (Settings : access Gtk_Print_Settings_Record;
      Height   : Gdouble;
      Unit     : Gtk.Enums.Gtk_Unit);
   procedure Set_Paper_Width
     (Settings : access Gtk_Print_Settings_Record;
      Width    : Gdouble;
      Unit     : Gtk.Enums.Gtk_Unit);
   --  Get/Set the paper height/width, in units of Unit

   function Get_Paper_Size
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Paper_Size.Gtk_Paper_Size;
   procedure Set_Paper_Size
     (Settings   : access Gtk_Print_Settings_Record;
      Paper_Size : Gtk.Paper_Size.Gtk_Paper_Size);
   --  Sets/Gets the paper size.

   function Get_Print_Pages
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Print_Pages;
   procedure Set_Print_Pages
     (Settings : access Gtk_Print_Settings_Record;
      Pages    : Gtk.Enums.Gtk_Print_Pages);
   --  Gets/Sets which pages to print

   function Get_Printer
     (Settings : access Gtk_Print_Settings_Record)
      return String;
   procedure Set_Printer
     (Settings : access Gtk_Print_Settings_Record;
      Printer  : String);
   --  Gets/Sets the printer name

   function Get_Printer_Lpi
     (Settings : access Gtk_Print_Settings_Record)
      return Gdouble;
   procedure Set_Printer_Lpi
     (Settings : access Gtk_Print_Settings_Record;
      Lpi      : Gdouble);
   --  Gets/Sets the resolution in lpi (lines per inch)

   function Get_Quality
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Print_Quality;
   procedure Set_Quality
     (Settings : access Gtk_Print_Settings_Record;
      Quality  : Gtk.Enums.Gtk_Print_Quality);
   --  Gets/Sets the print quality

   function Get_Resolution
     (Settings : access Gtk_Print_Settings_Record)
      return Gint;
   procedure Set_Resolution
     (Settings   : access Gtk_Print_Settings_Record;
      Resolution : Gint);
   --  Gets/Sets the resolution in dpi.

   function Get_Resolution_X
     (Settings : access Gtk_Print_Settings_Record)
      return Gint;
   function Get_Resolution_Y
     (Settings : access Gtk_Print_Settings_Record)
      return Gint;
   procedure Set_Resolution_XY
     (Settings     : access Gtk_Print_Settings_Record;
      Resolution_X : Gint;
      Resolution_Y : Gint);
   --  Gets/Sets the horizontal/vertical resolution, in dpi.

   function Get_Reverse
     (Settings : access Gtk_Print_Settings_Record)
      return Boolean;
   procedure Set_Reverse
     (Settings : access Gtk_Print_Settings_Record;
      Rev      : Boolean);
   --  Returns whether to reverse the order of the printed pages

   function Get_Scale
     (Settings : access Gtk_Print_Settings_Record)
      return Gdouble;
   procedure Set_Scale
     (Settings : access Gtk_Print_Settings_Record;
      Scale    : Gdouble);
   --  Returns the scale in percent

   function Get_Use_Color
     (Settings : access Gtk_Print_Settings_Record)
      return Boolean;
   procedure Set_Use_Color
     (Settings  : access Gtk_Print_Settings_Record;
      Use_Color : Boolean);
   --  Returns whether to use color

   -----------------------------------
   -- Saving and Restoring Settings --
   -----------------------------------

   function Load_File
     (Settings  : access Gtk_Print_Settings_Record;
      File_Name : String;
      Error     : Glib.Error.GError := null)
      return Boolean;
   --  Reads the print settings from File_Name.  See To_File.
   --  Returns True on success.

   function Load_Key_File
     (Settings   : access Gtk_Print_Settings_Record;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error      : Glib.Error.GError := null)
      return Boolean;
   --  Reads the print settings from the group Group_Name
   --  ("Print Settings" by default) in Key_File.
   --  Returns True on success

   function To_File
     (Settings  : access Gtk_Print_Settings_Record;
      File_Name : String;
      Error     : Glib.Error.GError := null)
      return Boolean;
   --  This function saves the print settings from Settings to File_Name.
   --  Returns True on success

   procedure To_Key_File
     (Settings   : access Gtk_Print_Settings_Record;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "");
   --  This function adds the print settings from Settings to Key_File.
   --  if Group_Name is not specified, it defaults to "Print Settings".

private

   type Gtk_Print_Settings_Record is
      new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_print_settings_get_type");

end Gtk.Print_Settings;
