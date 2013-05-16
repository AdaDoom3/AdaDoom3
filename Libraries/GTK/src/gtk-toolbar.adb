-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
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

with System;
with Gtk.Tool_Item;  use Gtk.Tool_Item;

with Glib.Type_Conversion_Hooks;

package body Gtk.Toolbar is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Toolbar_Record);
   pragma Warnings (Off, Type_Conversion);

   use type Gtk.Widget.Gtk_Widget;

   --------------------
   -- Append_Element --
   --------------------

   function Append_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : Gtk.Widget.Gtk_Widget := null;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Toolbar              : System.Address;
         The_Type             : Gtk_Toolbar_Child_Type;
         Widget               : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_append_element");

      T    : aliased constant UTF8_String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant UTF8_String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant UTF8_String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;
      W    : System.Address;
      I    : System.Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      if Widget = null then
         W := System.Null_Address;
      else
         W := Get_Object (Widget);
      end if;

      if Icon = null then
         I := System.Null_Address;
      else
         I := Get_Object (Icon);
      end if;

      return Gtk.Widget.Convert
        (Internal
          (Get_Object (Toolbar),
           The_Type,
           W, TA, TTA, TPTA, I, System.Null_Address,
           System.Null_Address));
   end Append_Element;

   -----------------
   -- Append_Item --
   -----------------

   function Append_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Button.Gtk_Button
   is
      function Internal
        (Toolbar              : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_append_item");

      Stub : Gtk.Button.Gtk_Button_Record;
      T    : aliased constant UTF8_String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant UTF8_String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant UTF8_String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;
      I    : System.Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      if Icon = null then
         I := System.Null_Address;
      else
         I := Get_Object (Icon);
      end if;

      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Toolbar),
                                  TA, TTA, TPTA, I, System.Null_Address,
                                  System.Null_Address),
                        Stub));
   end Append_Item;

   ------------------
   -- Append_Space --
   ------------------

   procedure Append_Space (Toolbar : access Gtk_Toolbar_Record) is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_append_space");

   begin
      Internal (Get_Object (Toolbar));
   end Append_Space;

   -------------------
   -- Append_Widget --
   -------------------

   procedure Append_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "")
   is
      procedure Internal
        (Toolbar              : System.Address;
         Widget               : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_append_widget");

      TT   : aliased constant UTF8_String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant UTF8_String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      Internal (Get_Object (Toolbar), Get_Object (Widget), TTA, TPTA);
   end Append_Widget;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
     (Toolbar : access Gtk_Toolbar_Record) return Gtk_Orientation
   is
      function Internal (Toolbar : System.Address) return Gtk_Orientation;
      pragma Import (C, Internal, "gtk_toolbar_get_orientation");

   begin
      return Internal (Get_Object (Toolbar));
   end Get_Orientation;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Toolbar : access Gtk_Toolbar_Record) return Gtk_Toolbar_Style
   is
      function Internal (Toolbar : System.Address) return Gtk_Toolbar_Style;
      pragma Import (C, Internal, "gtk_toolbar_get_style");

   begin
      return Internal (Get_Object (Toolbar));
   end Get_Style;

   -------------------
   -- Get_Icon_Size --
   -------------------

   function Get_Icon_Size
     (Toolbar : access Gtk_Toolbar_Record) return Gtk_Icon_Size
   is
      function Internal (Toolbar : System.Address) return Gtk_Icon_Size;
      pragma Import (C, Internal, "gtk_toolbar_get_icon_size");

   begin
      return Internal (Get_Object (Toolbar));
   end Get_Icon_Size;

   ------------------
   -- Get_Tooltips --
   ------------------

   function Get_Tooltips
     (Toolbar : access Gtk_Toolbar_Record) return Boolean
   is
      function Internal (Toolbar : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_toolbar_get_tooltips");

   begin
      return Internal (Get_Object (Toolbar)) /= 0;
   end Get_Tooltips;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Toolbar) is
   begin
      Widget := new Gtk_Toolbar_Record;
      Gtk.Toolbar.Initialize (Widget);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget      : out Gtk_Toolbar;
      Orientation : Gtk_Orientation;
      Style       : Gtk_Toolbar_Style) is
   begin
      Widget := new Gtk_Toolbar_Record;
      pragma Warnings (Off);
      Initialize (Widget, Orientation, Style);
      pragma Warnings (On);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Toolbar_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget      : access Gtk_Toolbar_Record'Class;
      Orientation : Gtk_Orientation;
      Style       : Gtk_Toolbar_Style) is
   begin
      Gtk.Toolbar.Initialize (Widget);
      Set_Orientation (Widget, Orientation);
      Set_Style (Widget, Style);
   end Initialize;

   --------------------
   -- Insert_Element --
   --------------------

   function Insert_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null;
      Position             : Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Toolbar              : System.Address;
         The_Type             : Gtk_Toolbar_Child_Type;
         Widget               : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address;
         Position             : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_insert_element");

      T    : aliased constant UTF8_String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant UTF8_String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant UTF8_String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;
      I    : System.Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      if Icon = null then
         I := System.Null_Address;
      else
         I := Get_Object (Icon);
      end if;

      return Gtk.Widget.Convert
        (Internal
          (Get_Object (Toolbar),
           The_Type,
           Get_Object (Widget),
           TA, TTA, TPTA, I,
           System.Null_Address,
           System.Null_Address,
           Position));
   end Insert_Element;

   -----------------
   -- Insert_Item --
   -----------------

   function Insert_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null;
      Position             : Gint) return Gtk.Button.Gtk_Button
   is
      function Internal
        (Toolbar              : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address;
         Position             : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_insert_item");

      Stub : Gtk.Button.Gtk_Button_Record;
      T    : aliased constant UTF8_String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant UTF8_String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant UTF8_String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;
      I    : System.Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      if Icon = null then
         I := System.Null_Address;
      else
         I := Get_Object (Icon);
      end if;

      return Gtk.Button.Gtk_Button
        (Get_User_Data
          (Internal
            (Get_Object (Toolbar),
             TA, TTA, TPTA, I,
             System.Null_Address, System.Null_Address,
             Position),
           Stub));
   end Insert_Item;

   ------------------
   -- Insert_Space --
   ------------------

   procedure Insert_Space
     (Toolbar : access Gtk_Toolbar_Record; Position : Gint)
   is
      procedure Internal (Toolbar : System.Address; Position : Gint);
      pragma Import (C, Internal, "gtk_toolbar_insert_space");

   begin
      Internal (Get_Object (Toolbar), Position);
   end Insert_Space;

   ------------------
   -- Remove_Space --
   ------------------

   procedure Remove_Space
     (Toolbar : access Gtk_Toolbar_Record; Position : Gint)
   is
      procedure Internal (Toolbar : System.Address; Position : Gint);
      pragma Import (C, Internal, "gtk_toolbar_remove_space");

   begin
      Internal (Get_Object (Toolbar), Position);
   end Remove_Space;

   ------------------
   -- Insert_Stock --
   ------------------

   function Insert_Stock
     (Toolbar              : access Gtk_Toolbar_Record;
      Stock_Id             : String;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Position             : Gint := -1) return Gtk.Button.Gtk_Button
   is
      function Internal
        (Toolbar              : System.Address;
         Stock_Id             : String;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address;
         Position             : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_insert_stock");

      Stub : Gtk.Button.Gtk_Button_Record;
      TT   : aliased constant UTF8_String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant UTF8_String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Toolbar),
          Stock_Id & ASCII.NUL, TTA, TPTA,
          System.Null_Address, System.Null_Address, Position),
                        Stub));
   end Insert_Stock;

   -------------------
   -- Insert_Widget --
   -------------------

   procedure Insert_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Position             : Gint)
   is
      procedure Internal
        (Toolbar              : System.Address;
         Widget               : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Position             : Gint);
      pragma Import (C, Internal, "gtk_toolbar_insert_widget");

      TT   : aliased constant UTF8_String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant UTF8_String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      Internal
        (Get_Object (Toolbar), Get_Object (Widget), TTA, TPTA, Position);
   end Insert_Widget;

   ---------------------
   -- Prepend_Element --
   ---------------------

   function Prepend_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Toolbar              : System.Address;
         The_Type             : Gtk_Toolbar_Child_Type;
         Widget               : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_prepend_element");

      T    : aliased constant UTF8_String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant UTF8_String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant UTF8_String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;
      I    : System.Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      if Icon = null then
         I := System.Null_Address;
      else
         I := Get_Object (Icon);
      end if;

      return Gtk.Widget.Convert
        (Internal
          (Get_Object (Toolbar),
           The_Type,
           Get_Object (Widget),
           TA, TTA, TPTA, I,
           System.Null_Address, System.Null_Address));
   end Prepend_Element;

   ------------------
   -- Prepend_Item --
   ------------------

   function Prepend_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Button.Gtk_Button
   is
      function Internal
        (Toolbar              : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_prepend_item");

      Stub : Gtk.Button.Gtk_Button_Record;
      T    : aliased constant UTF8_String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant UTF8_String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant UTF8_String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;
      I    : System.Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      if Icon = null then
         I := System.Null_Address;
      else
         I := Get_Object (Icon);
      end if;

      return Gtk.Button.Gtk_Button
        (Get_User_Data
          (Internal
            (Get_Object (Toolbar),
             TA, TTA, TPTA, I,
             System.Null_Address, System.Null_Address),
           Stub));
   end Prepend_Item;

   -------------------
   -- Prepend_Space --
   -------------------

   procedure Prepend_Space (Toolbar : access Gtk_Toolbar_Record) is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_prepend_space");

   begin
      Internal (Get_Object (Toolbar));
   end Prepend_Space;

   --------------------
   -- Prepend_Widget --
   --------------------

   procedure Prepend_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "")
   is
      procedure Internal
        (Toolbar              : System.Address;
         Widget               : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_prepend_widget");

      TT   : aliased constant UTF8_String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant UTF8_String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      Internal (Get_Object (Toolbar), Get_Object (Widget), TTA, TPTA);
   end Prepend_Widget;

   -------------------
   -- Set_Icon_Size --
   -------------------

   procedure Set_Icon_Size
     (Toolbar   : access Gtk_Toolbar_Record;
      Icon_Size : Gtk_Icon_Size)
   is
      procedure Internal
        (Toolbar : System.Address; Icon_Size : Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_toolbar_set_icon_size");

   begin
      Internal (Get_Object (Toolbar), Icon_Size);
   end Set_Icon_Size;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Toolbar     : access Gtk_Toolbar_Record;
      Orientation : Gtk_Orientation)
   is
      procedure Internal
        (Toolbar     : System.Address;
         Orientation : Gtk_Orientation);
      pragma Import (C, Internal, "gtk_toolbar_set_orientation");

   begin
      Internal (Get_Object (Toolbar), Orientation);
   end Set_Orientation;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
     (Toolbar : access Gtk_Toolbar_Record;
      Style   : Gtk_Toolbar_Style)
   is
      procedure Internal (Toolbar : System.Address; Style : Gtk_Toolbar_Style);
      pragma Import (C, Internal, "gtk_toolbar_set_style");

   begin
      Internal (Get_Object (Toolbar), Style);
   end Set_Style;

   ------------------
   -- Set_Tooltips --
   ------------------

   procedure Set_Tooltips
     (Toolbar : access Gtk_Toolbar_Record; Enable : Boolean)
   is
      procedure Internal (Toolbar : System.Address; Enable : Gint);
      pragma Import (C, Internal, "gtk_toolbar_set_tooltips");

   begin
      Internal (Get_Object (Toolbar), Boolean'Pos (Enable));
   end Set_Tooltips;

   -----------------
   -- Unset_Style --
   -----------------

   procedure Unset_Style (Toolbar : access Gtk_Toolbar_Record)
   is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_unset_style");
   begin
      Internal (Get_Object (Toolbar));
   end Unset_Style;

   ---------------------
   -- Unset_Icon_Size --
   ---------------------

   procedure Unset_Icon_Size (Toolbar : access Gtk_Toolbar_Record)
   is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_unset_icon_size");
   begin
      Internal (Get_Object (Toolbar));
   end Unset_Icon_Size;

   --------------------
   -- Get_Drop_Index --
   --------------------

   function Get_Drop_Index
     (Toolbar : access Gtk_Toolbar_Record;
      X       : Gint;
      Y       : Gint)
      return Gint
   is
      function Internal
        (Toolbar : System.Address;
         X       : Gint;
         Y       : Gint)
        return Gint;
      pragma Import (C, Internal, "gtk_toolbar_get_drop_index");
   begin
      return Internal (Get_Object (Toolbar), X, Y);
   end Get_Drop_Index;

   -----------------
   -- Get_N_Items --
   -----------------

   function Get_N_Items
     (Toolbar : access Gtk_Toolbar_Record)
     return Gint
   is
      function Internal
        (Toolbar : System.Address)
        return Gint;
      pragma Import (C, Internal, "gtk_toolbar_get_n_items");
   begin
      return Internal (Get_Object (Toolbar));
   end Get_N_Items;

   ----------------------
   -- Get_Relief_Style --
   ----------------------

   function Get_Relief_Style
     (Toolbar : access Gtk_Toolbar_Record)
      return Gtk_Relief_Style
   is
      function Internal
        (Toolbar : System.Address)
         return Gtk_Relief_Style;
      pragma Import (C, Internal, "gtk_toolbar_get_relief_style");
   begin
      return Internal (Get_Object (Toolbar));
   end Get_Relief_Style;

   --------------------
   -- Get_Show_Arrow --
   --------------------

   function Get_Show_Arrow
     (Toolbar : access Gtk_Toolbar_Record)
      return Boolean
   is
      function Internal
        (Toolbar : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_toolbar_get_show_arrow");
   begin
      return Boolean'Val (Internal (Get_Object (Toolbar)));
   end Get_Show_Arrow;

   --------------------
   -- Set_Show_Arrow --
   --------------------

   procedure Set_Show_Arrow
     (Toolbar    : access Gtk_Toolbar_Record;
      Show_Arrow : Boolean := True)
   is
      procedure Internal
        (Toolbar    : System.Address;
         Show_Arrow : Gboolean);
      pragma Import (C, Internal, "gtk_toolbar_set_show_arrow");
   begin
      Internal (Get_Object (Toolbar), Boolean'Pos (Show_Arrow));
   end Set_Show_Arrow;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Toolbar : access Gtk_Toolbar_Record;
      Item    : access Gtk_Tool_Item_Record'Class;
      Pos     : Gint := -1)
   is
      procedure Internal
        (Toolbar : System.Address;
         Item    : System.Address;
         Pos     : Gint);
      pragma Import (C, Internal, "gtk_toolbar_insert");
   begin
      Internal (Get_Object (Toolbar), Get_Object (Item), Pos);
   end Insert;

   --------------------
   -- Get_Item_Index --
   --------------------

   function Get_Item_Index
     (Toolbar : access Gtk_Toolbar_Record;
      Item    : access Gtk_Tool_Item_Record'Class)
      return Gint
   is
      function Internal
        (Toolbar : System.Address;
         Item    : System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_toolbar_get_item_index");
   begin
      return Internal (Get_Object (Toolbar), Get_Object (Item));
   end Get_Item_Index;

   ------------------
   -- Get_Nth_Item --
   ------------------

   function Get_Nth_Item
     (Toolbar : access Gtk_Toolbar_Record;
      N       : Gint)
      return Gtk_Tool_Item
   is
      function Internal
        (Toolbar : System.Address;
         N       : Gint)
         return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_get_nth_item");
      Stub : Gtk_Tool_Item_Record;
   begin
      return Gtk_Tool_Item
        (Get_User_Data
          (Internal (Get_Object (Toolbar), N), Stub));
   end Get_Nth_Item;

   -----------------------------
   -- Set_Drop_Highlight_Item --
   -----------------------------

   procedure Set_Drop_Highlight_Item
     (Toolbar   : access Gtk_Toolbar_Record;
      Tool_Item : access Gtk_Tool_Item_Record'Class;
      Index     : Gint)
   is
      procedure Internal
        (Toolbar   : System.Address;
         Tool_Item : System.Address;
         Index     : Gint);
      pragma Import (C, Internal, "gtk_toolbar_set_drop_highlight_item");
   begin
      Internal (Get_Object (Toolbar), Get_Object (Tool_Item), Index);
   end Set_Drop_Highlight_Item;

end Gtk.Toolbar;
