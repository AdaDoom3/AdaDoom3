-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
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
--  In recent versions of gtk+, this package has been deprecated in favor of
--  Gtk.UIManager. However, the subprograms have not been marked as such in the
--  C files themselves, so are still available in GtkAda as well.
--  </description>
--  <c_version>2.8.17</c_version>
--  <testgtk>create_item_factory.adb</testgtk>

with Glib.Object;

with Gdk.Types;
with Gtk.Accel_Group;
with Gtk.Widget;
with Gtkada.Types;

package Gtk.Item_Factory is
   pragma Obsolescent;

   type Gtk_Item_Factory_Record is new Object.GObject_Record with private;
   type Gtk_Item_Factory is access all Gtk_Item_Factory_Record'Class;

   --  <doc_ignore>

   type Item_Type_Enum is
     (Title,
      --  Create a title item

      Item,
      --  Create a simple item

      Image_Item,
      --  Create an item holding an image

      Stock_Item,
      --  Create an item holding a stock image

      Check_Item,
      --  Create a check item

      Toggle_Item,
      --  Create a toggle item

      Radio_Item,
      --  Create a radio item

      Separator,
      --  Create a separator

      Tearoff,
      --  Create a tearoff item

      Branch,
      --  Create an item to hold sub items

      Last_Branch
      --  Create a right justified item to hold sub items
     );
   --  Identify the predefined item types used to create a
   --  Gtk_Item_Factory_Entry.

   type Gtk_Item_Factory_Entry is private;
   type Gtk_Item_Factory_Entry_Array is array (Gint range <>) of
     Gtk_Item_Factory_Entry;

   procedure Gtk_New
     (Ifactory       : out Gtk_Item_Factory;
      Container_Type : Gtk_Type;
      Path           : UTF8_String;
      Accel_Group    : Gtk.Accel_Group.Gtk_Accel_Group);
   procedure Initialize
     (Ifactory       : access Gtk_Item_Factory_Record'Class;
      Container_Type : Gtk_Type;
      Path           : UTF8_String := "";
      Accel_Group    : Gtk.Accel_Group.Gtk_Accel_Group);
   --  Creates or initializes a new item factory.
   --  Possible values of Container_Type are:
   --    - Gtk.Option_Menu.Get_Type
   --    - Gtk.Menu_Bar.Get_Type
   --    - Gtk.Menu.Get_Type

   function Get_Type return Gtk_Type;
   --  Return the internal value associated with a Gtk_Item_Factory

   procedure Add_Foreign
     (Accel_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Full_Path    : UTF8_String;
      Accel_Group  : Gtk.Accel_Group.Gtk_Accel_Group;
      Keyval       : Guint;
      Modifiers    : Gdk.Types.Gdk_Modifier_Type);

   function From_Widget
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk_Item_Factory;

   function Path_From_Widget
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return UTF8_String;

   function Get_Item
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : UTF8_String) return Gtk.Widget.Gtk_Widget;

   function Get_Widget
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : UTF8_String) return Gtk.Widget.Gtk_Widget;

   function Get_Widget_By_Action
     (Ifactory : access Gtk_Item_Factory_Record;
      Action   : Guint) return Gtk.Widget.Gtk_Widget;

   function Get_Item_By_Action
     (Ifactory : access Gtk_Item_Factory_Record;
      Action   : Guint) return Gtk.Widget.Gtk_Widget;

   procedure Delete_Item
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : UTF8_String);

   procedure Delete_Entry
     (Ifactory : access Gtk_Item_Factory_Record;
      Ientry   : Gtk_Item_Factory_Entry);

   procedure Delete_Entries
     (Ifactory  : access Gtk_Item_Factory_Record;
      Entries   : Gtk_Item_Factory_Entry_Array);

   procedure Popup
     (Ifactory     : access Gtk_Item_Factory_Record;
      X            : Guint;
      Y            : Guint;
      Mouse_Button : Guint;
      Time         : Guint32);

   generic
      type Data_Type (<>) is limited private;
   package Data_Item is

      type Data_Type_Access is access all Data_Type;

      type Gtk_Print_Func is access procedure
        (Func_Data : Data_Type_Access;
         Str       : String);  --  gchar* ???

      type Gtk_Translate_Func is access function
        (Path      : UTF8_String;  --  const gchar* ???
         Func_Data : Data_Type_Access) return Gtkada.Types.Chars_Ptr;
      pragma Convention (C, Gtk_Translate_Func);

      type Limited_Widget is limited private;
      Null_Widget : constant Limited_Widget;

      function To_Widget
        (Widget : Limited_Widget) return Gtk.Widget.Gtk_Widget;

      type Gtk_Item_Factory_Callback is access procedure
        (Callback_Data   : Data_Type_Access;
         Callback_Action : Guint;
         Widget          : Limited_Widget);

      function Gtk_New
        (Path            : UTF8_String;
         Accelerator     : String := "";
         Callback        : Gtk_Item_Factory_Callback := null;
         Item_Type       : Item_Type_Enum;
         Callback_Action : Guint := 0) return Gtk_Item_Factory_Entry;
      --  Create a Gtk_Item_Factory_Entry.
      --  It is up to you to call Free at an appropriate point to avoid memory
      --  leaks.

      function Gtk_New
        (Path            : UTF8_String;
         Accelerator     : String := "";
         Callback        : Gtk_Item_Factory_Callback := null;
         Item_Type       : String := "";
         Callback_Action : Guint := 0) return Gtk_Item_Factory_Entry;
      --  Create a Gtk_Item_Factory_Entry.
      --  It is up to you to call Free at an appropriate point to avoid memory
      --  leaks.

      function Gtk_New
        (Path            : UTF8_String;
         Accelerator     : String := "";
         Stock_Id        : String;
         Callback        : Gtk_Item_Factory_Callback := null;
         Callback_Action : Guint := 0) return Gtk_Item_Factory_Entry;
      --  Create a Gtk_Item_Factory_Entry from a stock item.
      --  It is up to you to call Free at an appropriate point to avoid memory
      --  leaks.

      function Gtk_New
        (Path            : UTF8_String;
         Accelerator     : String := "";
         Callback        : Gtk_Item_Factory_Callback := null;
         Pixbuf          : access Guchar_Array;
         Callback_Action : Guint := 0) return Gtk_Item_Factory_Entry;
      --  Create a Gtk_Item_Factory_Entry from an inline pixbuf image.
      --  It is up to you to call Free at an appropriate point to avoid memory
      --  leaks.

      procedure Free (Ientry : in out Gtk_Item_Factory_Entry);
      --  Free all the dynamic data associated with an item factory entry.

      procedure Free (Ientries : in out Gtk_Item_Factory_Entry_Array);
      --  Free all the dynamic data associated with each item factory entry.

      procedure Create_Item
        (Ifactory      : access Gtk_Item_Factory_Record'Class;
         Ientry        : Gtk_Item_Factory_Entry;
         Callback_Data : Data_Type_Access;
         Callback_Type : Guint);
      --  Callback_Type = 0 -> Gtk_Item_Factory_Callback
      --  Callback_Type = 1 -> Gtk_Item_Factory_Callback1

      procedure Create_Items
        (Ifactory      : access Gtk_Item_Factory_Record'Class;
         Entries       : Gtk_Item_Factory_Entry_Array;
         Callback_Data : Data_Type_Access);

      function Popup_Data
        (Ifactory : access Gtk_Item_Factory_Record'Class)
         return Data_Type_Access;

      function Popup_Data_From_Widget
        (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
         return Data_Type_Access;

      procedure Popup_With_Data
        (Ifactory     : access Gtk_Item_Factory_Record'Class;
         Popup_Data   : Data_Type_Access;
         Destroy      : System.Address; --  Gtk_Destroy_Notify ???
         X            : Guint;
         Y            : Guint;
         Mouse_Button : Guint;
         Time         : Guint32);

      procedure Set_Translate_Func
        (Ifactory : access Gtk_Item_Factory_Record'Class;
         Func     : Gtk_Translate_Func;
         Data     : Data_Type_Access;
         Notify   : System.Address);  --  Gtk_Destroy_Notify ???

   private
      type Limited_Widget is new System.Address;
      Null_Widget : constant Limited_Widget :=
        Limited_Widget (System.Null_Address);
   end Data_Item;

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private
   type Gtk_Item_Factory_Record is new Glib.Object.GObject_Record
     with null record;

   type Gtk_Item_Factory_Entry is record
      Path            : Gtkada.Types.Chars_Ptr;
      Accelerator     : Gtkada.Types.Chars_Ptr;
      Callback        : System.Address;
      Callback_Action : Guint;
      Item_Type       : Gtkada.Types.Chars_Ptr;

      --  Extra data for some item types:
      --  Image_Item -> pointer to inline pixbuf
      --  Stock_Item -> name of stock item

      Extra_Data      : Gtkada.Types.Chars_Ptr;
   end record;
   pragma Convention (C, Gtk_Item_Factory_Entry);

   pragma Import (C, Get_Type, "gtk_item_factory_get_type");

end Gtk.Item_Factory;

--  The following subprograms never had a binding, are now obsolescent:
--  No binding: gtk_item_factories_path_delete
--  No binding: gtk_item_factory_construct
--  No binding: gtk_item_factory_create_items_ac
--  No binding: gtk_item_factory_create_menu_entries
--  No binding: gtk_item_factory_from_path
