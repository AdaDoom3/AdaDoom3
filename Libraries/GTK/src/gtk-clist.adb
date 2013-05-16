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

with Interfaces.C.Strings;
with System;
with Unchecked_Deallocation;

with Gtk.Enums;              use Gtk.Enums;
with Gtk.Style;              use Gtk.Style;

with Gtkada.Types;           use Gtkada.Types;

with Glib.Type_Conversion_Hooks;

package body Gtk.Clist is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Clist_Record);
   pragma Warnings (Off, Type_Conversion);

   ------------
   -- Append --
   ------------

   function Append
     (Clist : access Gtk_Clist_Record;
      Text  : in     Chars_Ptr_Array) return Gint
   is
      function Internal
        (Clist : in System.Address; Text : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_clist_append");

   begin
      return Internal (Get_Object (Clist), Text (Text'First)'Address);
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Clist : access Gtk_Clist_Record) is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_clear");
   begin
      Internal (Get_Object (Clist));
   end Clear;

   -------------------------
   -- Column_Title_Active --
   -------------------------

   procedure Column_Title_Active
     (Clist  : access Gtk_Clist_Record;
      Column : in Gint)
   is
      procedure Internal (Clist  : in System.Address; Column : in Gint);
      pragma Import (C, Internal, "gtk_clist_column_title_active");
   begin
      Internal (Get_Object (Clist), Column);
   end Column_Title_Active;

   --------------------------
   -- Column_Title_Passive --
   --------------------------

   procedure Column_Title_Passive
     (Clist : access Gtk_Clist_Record;
      Column : in Gint)
   is
      procedure Internal (Clist  : in System.Address; Column : in Gint);
      pragma Import (C, Internal, "gtk_clist_column_title_passive");
   begin
      Internal (Get_Object (Clist), Column);
   end Column_Title_Passive;

   --------------------------
   -- Column_Titles_Active --
   --------------------------

   procedure Column_Titles_Active (Clist : access Gtk_Clist_Record) is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_column_titles_active");
   begin
      Internal (Get_Object (Clist));
   end Column_Titles_Active;

   ------------------------
   -- Column_Titles_Hide --
   ------------------------

   procedure Column_Titles_Hide (Clist : access Gtk_Clist_Record) is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_column_titles_hide");
   begin
      Internal (Get_Object (Clist));
   end Column_Titles_Hide;

   ---------------------------
   -- Column_Titles_Passive --
   ---------------------------

   procedure Column_Titles_Passive (Clist : access Gtk_Clist_Record) is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_column_titles_passive");
   begin
      Internal (Get_Object (Clist));
   end Column_Titles_Passive;

   ------------------------
   -- Column_Titles_Show --
   ------------------------

   procedure Column_Titles_Show (Clist : access Gtk_Clist_Record) is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_column_titles_show");
   begin
      Internal (Get_Object (Clist));
   end Column_Titles_Show;

   ----------------------
   -- Columns_Autosize --
   ----------------------

   function Columns_Autosize (Clist  : access Gtk_Clist_Record) return Gint is
      function Internal (Clist  : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_clist_columns_autosize");
   begin
      return Internal (Get_Object (Clist));
   end Columns_Autosize;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Clist : access Gtk_Clist_Record) is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_freeze");
   begin
      Internal (Get_Object (Clist));
   end Freeze;

   --------------------
   -- Get_Cell_Style --
   --------------------

   function Get_Cell_Style
     (Clist  : access Gtk_Clist_Record;
      Row    : in     Gint;
      Column : in     Gint) return Gtk.Style.Gtk_Style
   is
      function Internal
        (Clist  : in System.Address;
         Row    : in Gint;
         Column : in Gint) return System.Address;
      pragma Import (C, Internal, "gtk_clist_get_cell_style");
      Stub : Gtk_Style_Record;

   begin
      return Gtk_Style
        (Get_User_Data (Internal (Get_Object (Clist), Row, Column), Stub));
   end Get_Cell_Style;

   -------------------
   -- Get_Cell_Type --
   -------------------

   function Get_Cell_Type
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint)
      return      Gtk_Cell_Type
   is
      function Internal
        (Clist  : in System.Address;
         Row    : in Gint;
         Column : in Gint)
         return      Gint;
      pragma Import (C, Internal, "gtk_clist_get_cell_type");
   begin
      return Gtk_Cell_Type'Val (Internal (Get_Object (Clist), Row, Column));
   end Get_Cell_Type;

   ----------------------
   -- Get_Clist_Window --
   ----------------------

   function Get_Clist_Window
     (Clist : access Gtk_Clist_Record) return Gdk.Window.Gdk_Window
   is
      function Internal
        (Clist : in System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "ada_clist_get_clist_window");

   begin
      return Internal (Get_Object (Clist));
   end Get_Clist_Window;

   -----------------------
   -- Get_Column_Button --
   -----------------------

   function Get_Column_Widget
     (Clist  : access Gtk_Clist_Record;
      Column : in Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Clist  : in System.Address;
         Column : in Gint) return System.Address;
      pragma Import (C, Internal, "gtk_clist_get_column_widget");
      Stub : Gtk.Widget.Gtk_Widget_Record;

   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Clist), Column), Stub));
   end Get_Column_Widget;

   ----------------------
   -- Get_Column_Title --
   ----------------------

   function Get_Column_Title
     (Clist  : access Gtk_Clist_Record; Column : in Gint)
      return UTF8_String
   is
      function Internal (Clist : System.Address; Column : Gint)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_clist_get_column_title");
   begin
      return Interfaces.C.Strings.Value
        (Internal (Get_Object (Clist), Column));
   end Get_Column_Title;

   ---------------------
   --  Get_Focus_Row  --
   ---------------------

   function Get_Focus_Row (Clist : access Gtk_Clist_Record) return Gint is
      function Internal (Clist : System.Address) return Gint;
      pragma Import (C, Internal, "ada_clist_get_focus_row");
   begin
      return Internal (Get_Object (Clist));
   end Get_Focus_Row;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
     (Clist  : access Gtk_Clist_Record) return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Clist  : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_clist_get_hadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Clist)), Stub));
   end Get_Hadjustment;

   ---------------------
   -- Get_Sort_Column --
   ---------------------

   function Get_Sort_Column (Clist : access Gtk_Clist_Record) return Gint is
      function Internal (Clist : System.Address) return Gint;
      pragma Import (C, Internal, "ada_gtk_clist_get_sort_column");
   begin
      return Internal (Get_Object (Clist));
   end Get_Sort_Column;

   -------------------
   -- Get_Sort_Type --
   -------------------

   function Get_Sort_Type
     (Clist : access Gtk_Clist_Record) return Gtk_Sort_Type
   is
      function Internal (Clist : System.Address) return Gint;
      pragma Import (C, Internal, "ada_gtk_clist_get_sort_type");
   begin
      return Gtk_Sort_Type'Val (Internal (Get_Object (Clist)));
   end Get_Sort_Type;

   ----------------
   -- Get_Pixmap --
   ----------------

   procedure Get_Pixmap
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint;
      Pixmap   : out Gdk.Pixmap.Gdk_Pixmap;
      Mask     : out Gdk.Bitmap.Gdk_Bitmap;
      Is_Valid : out Boolean)
   is
      function Internal
        (Clist  : in System.Address;
         Row    : in Gint;
         Column : in Gint;
         Pixmap : access Gdk.Pixmap.Gdk_Pixmap;
         Mask   : access Gdk.Bitmap.Gdk_Bitmap) return Gint;
      pragma Import (C, Internal, "gtk_clist_get_pixmap");

      Pix : aliased Gdk.Pixmap.Gdk_Pixmap;
      Msk : aliased Gdk.Bitmap.Gdk_Bitmap;

   begin
      Is_Valid :=
        Boolean'Val (Internal
          (Get_Object (Clist), Row, Column,
           Pix'Access, Msk'Access));
      Pixmap := Pix;
      Mask   := Msk;
   end Get_Pixmap;

   ----------------
   -- Get_Pixmap --
   ----------------

   procedure Get_Pixmap
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gtk_Clist_Row;
      Column   : in Gint;
      Pixmap   : out Gdk.Pixmap.Gdk_Pixmap;
      Mask     : out Gdk.Bitmap.Gdk_Bitmap;
      Is_Valid : out Boolean)
   is
      function Internal
        (Clist  : in System.Address;
         Row    : in Gtk_Clist_Row;
         Column : in Gint;
         Pixmap : access Gdk.Pixmap.Gdk_Pixmap;
         Mask   : access Gdk.Bitmap.Gdk_Bitmap) return Gint;
      pragma Import (C, Internal, "ada_gtk_clist_get_pixmap");

      Pix : aliased Gdk.Pixmap.Gdk_Pixmap;
      Msk : aliased Gdk.Pixmap.Gdk_Pixmap;

   begin
      Is_Valid :=
        Boolean'Val (Internal
          (Get_Object (Clist), Row, Column, Pix'Access, Msk'Access));
      Pixmap := Pix;
      Mask   := Msk;
   end Get_Pixmap;

   -----------------
   -- Get_Pixtext --
   -----------------

   procedure Get_Pixtext
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint;
      Spacing  : out Guint8;
      Pixmap   : out Gdk.Pixmap.Gdk_Pixmap;
      Mask     : out Gdk.Bitmap.Gdk_Bitmap;
      Is_Valid : out Boolean)
   is
      function Internal
        (Clist   : in System.Address;
         Row     : in Gint;
         Column  : in Gint;
         Text    : access Interfaces.C.Strings.chars_ptr;
         Spacing : access Guint8;
         Pixmap  : access Gdk.Pixmap.Gdk_Pixmap;
         Mask    : access Gdk.Bitmap.Gdk_Bitmap) return Gint;
      pragma Import (C, Internal, "gtk_clist_get_pixtext");

      S    : aliased Interfaces.C.Strings.chars_ptr;
      Spac : aliased Guint8;
      Pix  : aliased Gdk.Pixmap.Gdk_Pixmap;
      Msk  : aliased Gdk.Bitmap.Gdk_Bitmap;

   begin
      Is_Valid := Boolean'Val (Internal
        (Get_Object (Clist), Row, Column, S'Access,
         Spac'Access, Pix'Access, Msk'Access));
      Spacing := Spac;
      Pixmap  := Pix;
      Mask    := Msk;
   end Get_Pixtext;

   ------------------
   -- Get_Row_List --
   ------------------

   function Get_Row_List
     (Clist : access Gtk_Clist_Record) return Row_List.Glist
   is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_clist_get_row_list");
      List : Row_List.Glist;
   begin
      Row_List.Set_Object (List, Internal (Get_Object (Clist)));
      return List;
   end Get_Row_List;

   -------------------
   -- Get_Row_Style --
   -------------------

   function Get_Row_Style
     (Clist  : access Gtk_Clist_Record;
      Row    : in     Gint) return Gtk.Style.Gtk_Style
   is
      function Internal
        (Clist  : in System.Address;
         Row    : in Gint) return System.Address;
      pragma Import (C, Internal, "gtk_clist_get_row_style");
      Stub : Gtk_Style_Record;
   begin
      return Gtk_Style
        (Get_User_Data (Internal (Get_Object (Clist), Row), Stub));
   end Get_Row_Style;

   --------------------
   -- Get_Selectable --
   --------------------

   function Get_Selectable
     (Clist : access Gtk_Clist_Record; Row : Gint) return Boolean
   is
      function Internal (Clist : System.Address; Row : Gint) return Gint;
      pragma Import (C, Internal, "gtk_clist_get_selectable");
   begin
      return Boolean'Val (Internal (Get_Object (Clist), Row));
   end Get_Selectable;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Widget : access Gtk_Clist_Record) return Gint_List.Glist
   is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_clist_get_selection");
      List : Gint_List.Glist;
   begin
      Gint_List.Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_Selection;

   ------------------------
   -- Get_Selection_Info --
   ------------------------

   procedure Get_Selection_Info
     (Clist   : access Gtk_Clist_Record;
      X       : in Gint;
      Y       : in Gint;
      Row     : out Gint;
      Column  : out Gint;
      Is_Valid : out Boolean)
   is
      function Internal
        (Clist  : in System.Address;
         X      : in Gint;
         Y      : in Gint;
         Row    : access Gint;
         Column : access Gint) return Gint;
      pragma Import (C, Internal, "gtk_clist_get_selection_info");

      Row_Out, Column_Out : aliased Gint;

   begin
      Is_Valid := Boolean'Val (Internal
        (Get_Object (Clist), X, Y, Row_Out'Access, Column_Out'Access));
      Row    := Row_Out;
      Column := Column_Out;
   end Get_Selection_Info;

   ------------------------
   -- Get_Selection_Mode --
   ------------------------

   function Get_Selection_Mode
     (Clist : access Gtk_Clist_Record) return Gtk_Selection_Mode
   is
      function Internal (Clist : System.Address) return Gint;
      pragma Import (C, Internal, "ada_clist_get_selection_mode");
   begin
      return Gtk_Selection_Mode'Val (Internal (Get_Object (Clist)));
   end Get_Selection_Mode;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint) return UTF8_String
   is
      function Internal
        (Clist  : in System.Address;
         Row    : in Gint;
         Column : in Gint;
         Text   : access Interfaces.C.Strings.chars_ptr) return Gint;
      pragma Import (C, Internal, "gtk_clist_get_text");

      function Internal2
        (Clist   : in System.Address;
         Row     : in Gint;
         Column  : in Gint;
         Text    : access Interfaces.C.Strings.chars_ptr;
         Spacing : in System.Address;
         Pixmap  : in System.Address;
         Mask    : access System.Address) return Gint;
      pragma Import (C, Internal2, "gtk_clist_get_pixtext");

      Is_Valid : Boolean;
      S        : aliased Interfaces.C.Strings.chars_ptr;
      Mask     : aliased System.Address;

   begin
      if Get_Cell_Type (Clist, Row, Column) = Cell_Text then
         Is_Valid := Boolean'Val (Internal
           (Get_Object (Clist), Row, Column, S'Access));
      else
         Is_Valid := Boolean'Val (Internal2
           (Get_Object (Clist), Row, Column, S'Access,
            System.Null_Address, System.Null_Address, Mask'Access));
      end if;

      if Is_Valid then
         return Interfaces.C.Strings.Value (S);
      else
         return "";
      end if;
   end Get_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Clist    : access Gtk_Clist_Record;
      Row      : Gtk_Clist_Row;
      Column   : in Gint) return UTF8_String
   is
      function Internal
        (Clist  : in System.Address;
         Row    : in Gtk_Clist_Row;
         Column : in Gint;
         Text   : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_gtk_clist_get_text");

      S        : aliased Interfaces.C.Strings.chars_ptr;
      Is_Valid : Gint;

   begin
      Is_Valid := Internal (Get_Object (Clist), Row, Column, S'Address);

      if Is_Valid /= 0 then
         return Interfaces.C.Strings.Value (S);
      else
         return "";
      end if;
   end Get_Text;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment (Clist  : access Gtk_Clist_Record)
     return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Clist  : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_clist_get_vadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Clist)), Stub));
   end Get_Vadjustment;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget  : out Gtk_Clist; Columns : in Gint) is
   begin
      Widget := new Gtk_Clist_Record;
      Initialize (Widget, Columns);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget  : out Gtk_Clist;
      Columns : in  Gint;
      Titles  : in  Chars_Ptr_Array) is
   begin
      Widget := new Gtk_Clist_Record;
      Initialize (Widget, Columns, Titles);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Clist_Record'Class; Columns : in Gint)
   is
      function Internal (Columns : in Gint) return System.Address;
      pragma Import (C, Internal, "gtk_clist_new");
   begin
      Set_Object (Widget, Internal (Columns));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget  : access Gtk_Clist_Record'Class;
      Columns : in     Gint;
      Titles  : in     Chars_Ptr_Array)
   is
      function Internal
        (Columns : in Gint;
         Titles  : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_clist_new_with_titles");

   begin
      Set_Object (Widget, Internal (Columns, Titles (Titles'First)'Address));
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Clist : access Gtk_Clist_Record;
      Row   : in     Gint;
      Text  : in     Chars_Ptr_Array)
   is
      procedure Internal
        (Clist : in System.Address;
         Row   : in Gint;
         Text  : in System.Address);
      pragma Import (C, Internal, "gtk_clist_insert");

   begin
      Internal (Get_Object (Clist), Row, Text (Text'First)'Address);
   end Insert;

   ------------
   -- Moveto --
   ------------

   procedure Moveto
     (Clist     : access Gtk_Clist_Record;
      Row       : in Gint;
      Column    : in Gint;
      Row_Align : in Gfloat;
      Col_Align : in Gfloat)
   is
      procedure Internal
        (Clist     : in System.Address;
         Row       : in Gint;
         Column    : in Gint;
         Row_Align : in Gfloat;
         Col_Align : in Gfloat);
      pragma Import (C, Internal, "gtk_clist_moveto");

   begin
      Internal (Get_Object (Clist),
                Row,
                Column,
                Row_Align,
                Col_Align);
   end Moveto;

   ---------------------------
   -- Optimal_Column_Widget --
   ---------------------------

   function Optimal_Column_Width
     (Clist : access Gtk_Clist_Record; Column : Gint) return Gint is
      function Internal (Clist : System.Address; Column : Gint) return Gint;
      pragma Import (C, Internal, "gtk_clist_optimal_column_width");
   begin
      return Internal (Get_Object (Clist), Column);
   end Optimal_Column_Width;

   -------------
   -- Prepend --
   -------------

   function Prepend
     (Clist : access Gtk_Clist_Record;
      Text  : in     Chars_Ptr_Array) return Gint
   is
      function Internal
        (Clist : in System.Address; Text  : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_clist_prepend");

   begin
      return Internal (Get_Object (Clist), Text (Text'First)'Address);
   end Prepend;

   ------------
   -- Remove --
   ------------

   procedure Remove (Clist : access Gtk_Clist_Record; Row   : in Gint) is
      procedure Internal (Clist : in System.Address; Row   : in Gint);
      pragma Import (C, Internal, "gtk_clist_remove");
   begin
      Internal (Get_Object (Clist), Row);
   end Remove;

   --------------------
   -- Row_Is_Visible --
   --------------------

   function Row_Is_Visible
     (Clist  : access Gtk_Clist_Record; Row : in Gint) return Gtk_Visibility
   is
      function Internal
        (Clist  : in System.Address; Row    : in Gint) return Gint;
      pragma Import (C, Internal, "gtk_clist_row_is_visible");

   begin
      return Gtk_Visibility'Val (Internal (Get_Object (Clist), Row));
   end Row_Is_Visible;

   --------------
   -- Row_Move --
   --------------

   procedure Row_Move
     (Clist      : access Gtk_Clist_Record;
      Source_Row : in     Gint;
      Dest_Row   : in     Gint)
   is
      procedure Internal
        (Clist      : in System.Address;
         Source_Row : in Gint;
         Dest_Row   : in Gint);
      pragma Import (C, Internal, "gtk_clist_row_move");

   begin
      Internal (Get_Object (Clist), Source_Row, Dest_Row);
   end Row_Move;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Clist : access Gtk_Clist_Record) is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_select_all");
   begin
      Internal (Get_Object (Clist));
   end Select_All;

   -------------------
   -- Set_Auto_Sort --
   -------------------

   procedure Set_Auto_Sort (Clist     : access Gtk_Clist_Record;
                            Auto_Sort : Boolean)
   is
      procedure Internal (Clist     : System.Address;
                          Auto_Sort : Integer);
      pragma Import (C, Internal, "gtk_clist_set_auto_sort");
   begin
      Internal (Get_Object (Clist), Boolean'Pos (Auto_Sort));
   end Set_Auto_Sort;

   ----------------
   -- Select_Row --
   ----------------

   procedure Select_Row (Clist  : access Gtk_Clist_Record;
                         Row : in Gint;
                         Column : in Gint)
   is
      procedure Internal (Clist  : in System.Address;
                          Row    : in Gint;
                          Column : in Gint);
      pragma Import (C, Internal, "gtk_clist_select_row");
   begin
      Internal (Get_Object (Clist), Row, Column);
   end Select_Row;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Clist : access Gtk_Clist_Record;
      Row   : in Gint;
      Color : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Clist : in System.Address;
         Row   : in Gint;
         Color : in System.Address);
      pragma Import (C, Internal, "gtk_clist_set_background");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Clist), Row, Color_A);
   end Set_Background;

   ------------------------
   -- Set_Button_Actions --
   ------------------------

   procedure Set_Button_Actions
     (Clist         : access Gtk_Clist_Record;
      Button        : Guint;
      Button_Action : Gtk_Button_Action)
   is
      procedure Internal
        (Clist         : System.Address;
         Button        : Guint;
         Button_Action : Gtk_Button_Action);
      pragma Import (C, Internal, "gtk_clist_set_button_actions");

   begin
      Internal (Get_Object (Clist), Button, Button_Action);
   end Set_Button_Actions;

   --------------------
   -- Set_Cell_Style --
   --------------------

   procedure Set_Cell_Style (Clist  : access Gtk_Clist_Record;
                             Row    : in Gint;
                             Column : in Gint;
                             Style  : in Gtk_Style) is
      procedure Internal
        (Clist : System.Address; Row : Gint;
                                 Column : Gint; Style : System.Address);
      pragma Import (C, Internal, "gtk_clist_set_cell_style");
   begin
      Internal (Get_Object (Clist), Row, Column, Get_Object (Style));
   end Set_Cell_Style;

   ----------------------------
   -- Set_Column_Auto_Resize --
   ----------------------------

   procedure Set_Column_Auto_Resize
     (Clist       : access Gtk_Clist_Record;
      Column      : in Gint;
      Auto_Resize : in Boolean)
   is
      procedure Internal
        (Clist  : System.Address; Column : Gint; Auto_Resize : Gint);
      pragma Import (C, Internal, "gtk_clist_set_column_auto_resize");
   begin
      Internal (Get_Object (Clist), Column, Boolean'Pos (Auto_Resize));
   end Set_Column_Auto_Resize;

   --------------------------
   -- Set_Column_Min_Width --
   --------------------------

   procedure Set_Column_Min_Width
     (Clist : access Gtk_Clist_Record; Column : Gint; Min_Width : Gint) is
      procedure Internal (Clist : System.Address; Column : Gint; W : Gint);
      pragma Import (C, Internal, "gtk_clist_set_column_min_width");
   begin
      Internal (Get_Object (Clist), Column, Min_Width);
   end Set_Column_Min_Width;

   --------------------------
   -- Set_Column_Max_Width --
   --------------------------

   procedure Set_Column_Max_Width
     (Clist : access Gtk_Clist_Record; Column : Gint; Max_Width : Gint) is
      procedure Internal (Clist : System.Address; Column : Gint; W : Gint);
      pragma Import (C, Internal, "gtk_clist_set_column_max_width");
   begin
      Internal (Get_Object (Clist), Column, Max_Width);
   end Set_Column_Max_Width;

   ------------------------------
   -- Set_Column_Justification --
   ------------------------------

   procedure Set_Column_Justification
     (Clist         : access Gtk_Clist_Record;
      Column        : in Gint;
      Justification : in Gtk_Justification)
   is
      procedure Internal
        (Clist         : System.Address;
         Column        : Gint;
         Justification : Gtk_Justification);
      pragma Import (C, Internal, "gtk_clist_set_column_justification");

   begin
      Internal (Get_Object (Clist), Column, Justification);
   end Set_Column_Justification;

   ---------------------------
   -- Set_Column_Resizeable --
   ---------------------------

   procedure Set_Column_Resizeable
     (Clist    : access Gtk_Clist_Record;
      Column   : in Gint;
      Resizeable : in Boolean)
   is
      procedure Internal
        (Clist  : System.Address; Column : Gint; Resizeable : Gint);
      pragma Import (C, Internal, "gtk_clist_set_column_resizeable");
   begin
      Internal (Get_Object (Clist), Column, Boolean'Pos (Resizeable));
   end Set_Column_Resizeable;

   ----------------------
   -- Set_Column_Title --
   ----------------------

   procedure Set_Column_Title
     (Clist  : access Gtk_Clist_Record;
      Column : in Gint;
      Title  : in UTF8_String)
   is
      procedure Internal
        (Clist  : in System.Address;
         Column : in Gint;
         Title  : in UTF8_String);
      pragma Import (C, Internal, "gtk_clist_set_column_title");
   begin
      Internal (Get_Object (Clist),
                Column,
                Title & ASCII.NUL);
   end Set_Column_Title;

   ---------------------------
   -- Set_Column_Visibility --
   ---------------------------

   procedure Set_Column_Visibility
     (Clist   : access Gtk_Clist_Record;
      Column  : in Gint;
      Visible : in Boolean)
   is
      procedure Internal
        (Clist  : System.Address; Column : Gint; Visible : Gint);
      pragma Import (C, Internal, "gtk_clist_set_column_visibility");
   begin
      Internal (Get_Object (Clist), Column, Boolean'Pos (Visible));
   end Set_Column_Visibility;

   -----------------------
   -- Set_Column_Widget --
   -----------------------

   procedure Set_Column_Widget
     (Clist  : access Gtk_Clist_Record;
      Column : in     Gint;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Clist  : in System.Address;
         Column : in Gint;
         Widget : in System.Address);
      pragma Import (C, Internal, "gtk_clist_set_column_widget");
   begin
      Internal (Get_Object (Clist), Column, Get_Object (Widget));
   end Set_Column_Widget;

   ----------------------
   -- Set_Column_Width --
   ----------------------

   procedure Set_Column_Width
     (Clist  : access Gtk_Clist_Record;
      Column : in Gint;
      Width  : in Gint)
   is
      procedure Internal
        (Clist  : in System.Address;
         Column : in Gint;
         Width  : in Gint);
      pragma Import (C, Internal, "gtk_clist_set_column_width");
   begin
      Internal (Get_Object (Clist),
                Column,
                Width);
   end Set_Column_Width;

   ----------------------
   -- Ada_Compare_Func --
   ----------------------

   function Ada_Compare_Func (Clist : System.Address;
                              Row1  : System.Address;
                              Row2  : System.Address)
                             return Gint;
   --  Function called by Clists to sort the list. We can not pass directly
   --  the user function to C, since it requires converting the arguments
   --  to the appropriate Ada types first.

   function Ada_Compare_Func (Clist : System.Address;
                              Row1  : System.Address;
                              Row2  : System.Address)
                             return Gint
   is
      Stub : Gtk_Clist_Record;
      List : Gtk_Clist;
   begin
      List := Gtk_Clist (Get_User_Data (Clist, Stub));
      return List.Sort_Func (List, Convert (Row1), Convert (Row2));
   end Ada_Compare_Func;

   ----------------------
   -- Set_Compare_Func --
   ----------------------

   procedure Set_Compare_Func (Clist : access Gtk_Clist_Record;
                               Func  : Gtk_Clist_Compare_Func)
   is
      procedure Internal (Clist : System.Address;
                          Func  : System.Address);
      pragma Import (C, Internal, "gtk_clist_set_compare_func");
   begin
      if Func = null then
         Clist.Sort_Func := null;
         Internal (Get_Object (Clist), System.Null_Address);
      else
         Clist.Sort_Func := Func;
         Internal (Get_Object (Clist), Ada_Compare_Func'Address);
      end if;
   end Set_Compare_Func;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground
     (Clist : access Gtk_Clist_Record;
      Row   : in Gint;
      Color : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Clist : in System.Address;
         Row   : in Gint;
         Color : in System.Address);
      pragma Import (C, Internal, "gtk_clist_set_foreground");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;
      Internal (Get_Object (Clist), Row, Color_A);
   end Set_Foreground;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
     (Clist      : access Gtk_Clist_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal (Clist      : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_clist_set_hadjustment");
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Adjustment = null then
         Internal (Get_Object (Clist), System.Null_Address);
      else
         Internal (Get_Object (Clist), Get_Object (Adjustment));
      end if;
   end Set_Hadjustment;

   ----------------
   -- Set_Pixmap --
   ----------------

   procedure Set_Pixmap
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint;
      Pixmap : in Gdk.Pixmap.Gdk_Pixmap;
      Mask   : in Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
        (Clist  : in System.Address;
         Row    : in Gint;
         Column : in Gint;
         Pixmap : in Gdk.Pixmap.Gdk_Pixmap;
         Mask   : in Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_clist_set_pixmap");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column,
                Pixmap,
                Mask);
   end Set_Pixmap;

   -----------------
   -- Set_Pixtext --
   -----------------

   procedure Set_Pixtext
     (Clist   : access Gtk_Clist_Record;
      Row     : in Gint;
      Column  : in Gint;
      Text    : in UTF8_String;
      Spacing : in Guint8;
      Pixmap  : in Gdk.Pixmap.Gdk_Pixmap;
      Mask    : in Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
        (Clist   : in System.Address;
         Row     : in Gint;
         Column  : in Gint;
         Text    : in UTF8_String;
         Spacing : in Guint8;
         Pixmap  : in Gdk.Pixmap.Gdk_Pixmap;
         Mask    : in Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_clist_set_pixtext");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column,
                Text & ASCII.NUL,
                Spacing,
                Pixmap,
                Mask);
   end Set_Pixtext;

   ---------------------
   -- Set_Reorderable --
   ---------------------

   procedure Set_Reorderable
     (Clist : access Gtk_Clist_Record; Reorderable : Boolean)
   is
      procedure Internal (Clist : System.Address; Reorderable : Gint);
      pragma Import (C, Internal, "gtk_clist_set_reorderable");
   begin
      Internal (Get_Object (Clist), Boolean'Pos (Reorderable));
   end Set_Reorderable;

   --------------------
   -- Set_Row_Height --
   --------------------

   procedure Set_Row_Height (Clist  : access Gtk_Clist_Record;
                             Height : in Gint)
   is
      procedure Internal (Clist  : in System.Address; Height : in Gint);
      pragma Import (C, Internal, "gtk_clist_set_row_height");
   begin
      Internal (Get_Object (Clist), Height);
   end Set_Row_Height;

   -------------------
   -- Set_Row_Style --
   -------------------

   procedure Set_Row_Style
     (Clist : access Gtk_Clist_Record; Row : Gint;
      Style : in Gtk_Style)
   is
      procedure Internal
        (Clist : System.Address; Row : Gint; Style : System.Address);
      pragma Import (C, Internal, "gtk_clist_set_row_style");
   begin
      Internal (Get_Object (Clist), Row, Get_Object (Style));
   end Set_Row_Style;

   --------------------
   -- Set_Selectable --
   --------------------

   procedure Set_Selectable
     (Clist : access Gtk_Clist_Record; Row : Gint; Selectable : Boolean) is
      procedure Internal
        (Clist : System.Address; Row : Gint; Selectable : Gint);
      pragma Import (C, Internal, "gtk_clist_set_selectable");
   begin
      Internal (Get_Object (Clist), Row, Boolean'Pos (Selectable));
   end Set_Selectable;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
     (Clist : access Gtk_Clist_Record;
      Mode  : in Gtk_Selection_Mode)
   is
      procedure Internal
        (Clist : System.Address;
         Mode  : Gtk_Selection_Mode);
      pragma Import (C, Internal, "gtk_clist_set_selection_mode");
   begin
      Internal (Get_Object (Clist), Mode);
   end Set_Selection_Mode;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
     (Clist    : access Gtk_Clist_Record;
      The_Type : Gtk_Shadow_Type)
   is
      procedure Internal
        (Clist    : System.Address;
         The_Type : Gtk_Shadow_Type);
      pragma Import (C, Internal, "gtk_clist_set_shadow_type");

   begin
      Internal (Get_Object (Clist), The_Type);
   end Set_Shadow_Type;

   ---------------
   -- Set_Shift --
   ---------------

   procedure Set_Shift
     (Clist      : access Gtk_Clist_Record;
      Row        : in Gint;
      Column     : in Gint;
      Vertical   : in Gint;
      Horizontal : in Gint)
   is
      procedure Internal
        (Clist      : in System.Address;
         Row        : in Gint;
         Column     : in Gint;
         Vertical   : in Gint;
         Horizontal : in Gint);
      pragma Import (C, Internal, "gtk_clist_set_shift");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column,
                Vertical,
                Horizontal);
   end Set_Shift;

   ---------------------
   -- Set_Show_Titles --
   ---------------------

   procedure Set_Show_Titles
     (Clist : access Gtk_Clist_Record; Show : Boolean) is
   begin
      if Show then
         Column_Titles_Show (Clist);
      end if;
   end Set_Show_Titles;

   ---------------------
   -- Set_Sort_Column --
   ---------------------

   procedure Set_Sort_Column (Clist  : access Gtk_Clist_Record;
                              Column : Gint)
   is
      procedure Internal (Clist : System.Address;
                          Column : Gint);
      pragma Import (C, Internal, "gtk_clist_set_sort_column");
   begin
      Internal (Get_Object (Clist), Column);
   end Set_Sort_Column;

   -------------------
   -- Set_Sort_Type --
   -------------------

   procedure Set_Sort_Type (Clist     : access Gtk_Clist_Record;
                            Sort_Type : Gtk_Sort_Type)
   is
      procedure Internal
        (Clist     : System.Address;
         Sort_Type : Gtk_Sort_Type);
      pragma Import (C, Internal, "gtk_clist_set_sort_type");

   begin
      Internal (Get_Object (Clist), Sort_Type);
   end Set_Sort_Type;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint;
      Text   : in UTF8_String)
   is
      procedure Internal
        (Clist  : in System.Address;
         Row    : in Gint;
         Column : in Gint;
         Text   : in UTF8_String);
      pragma Import (C, Internal, "gtk_clist_set_text");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column,
                Text & ASCII.NUL);
   end Set_Text;

   ------------------------
   -- Set_Use_Drag_Icons --
   ------------------------

   procedure Set_Use_Drag_Icons
     (Clist : access Gtk_Clist_Record; Use_Icons : Boolean)
   is
      procedure Internal (Clist : System.Address; Use_Icons : Guint);
      pragma Import (C, Internal, "gtk_clist_set_use_drag_icons");
   begin
      Internal (Get_Object (Clist), Boolean'Pos (Use_Icons));
   end Set_Use_Drag_Icons;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Clist      : access Gtk_Clist_Record;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal (Clist      : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_clist_set_vadjustment");
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Adjustment = null then
         Internal (Get_Object (Clist), System.Null_Address);
      else
         Internal (Get_Object (Clist), Get_Object (Adjustment));
      end if;
   end Set_Vadjustment;

   ----------
   -- Sort --
   ----------

   procedure Sort (Clist : access Gtk_Clist_Record) is
      procedure Internal (Clist : System.Address);
      pragma Import (C, Internal, "gtk_clist_sort");
   begin
      Internal (Get_Object (Clist));
   end Sort;

   ---------------
   -- Swap_Rows --
   ---------------

   procedure Swap_Rows (Clist : access Gtk_Clist_Record;
                        Row1  : in     Gint;
                        Row2  : in     Gint)
   is
      procedure Internal (Clist : in System.Address;
                          Row1  : in Gint;
                          Row2  : in Gint);
      pragma Import (C, Internal, "gtk_clist_swap_rows");
   begin
      Internal (Get_Object (Clist), Row1, Row2);
   end Swap_Rows;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Clist : access Gtk_Clist_Record)
   is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_thaw");
   begin
      Internal (Get_Object (Clist));
   end Thaw;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All (Clist : access Gtk_Clist_Record) is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_unselect_all");
   begin
      Internal (Get_Object (Clist));
   end Unselect_All;

   ------------------
   -- Unselect_Row --
   ------------------

   procedure Unselect_Row
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint)
   is
      procedure Internal
        (Clist  : in System.Address;
         Row    : in Gint;
         Column : in Gint);
      pragma Import (C, Internal, "gtk_clist_unselect_row");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column);
   end Unselect_Row;

   --------------------
   -- Undo_Selection --
   --------------------

   procedure Undo_Selection (Clist  : access Gtk_Clist_Record) is
      procedure Internal (Clist : System.Address);
      pragma Import (C, Internal, "gtk_clist_undo_selection");
   begin
      Internal (Get_Object (Clist));
   end Undo_Selection;

   -----------------------
   -- Set_Cell_Contents --
   -----------------------

   procedure Set_Cell_Contents
     (Clist     : access Gtk_Clist_Record;
      Row       : Gtk_Clist_Row;
      Column    : Gint;
      Cell_Type : Gtk_Cell_Type;
      Text      : UTF8_String;
      Spacing   : Guint8;
      Pixmap    : Gdk.Pixmap.Gdk_Pixmap;
      Mask      : Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal (Clist     : System.Address;
                          Row       : Gtk_Clist_Row;
                          Column    : Gint;
                          Cell_Type : Gtk_Cell_Type;
                          Text      : System.Address;
                          Spacing   : Guint8;
                          Pixmap    : Gdk.Pixmap.Gdk_Pixmap;
                          Mask      : Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "ada_gtk_clist_set_cell_contents");
      T : System.Address := System.Null_Address;
   begin
      if Text /= "" then
         T := Text'Address;
      end if;
      Internal (Get_Object (Clist), Row, Column,
                Cell_Type, T, Spacing, Pixmap, Mask);
   end Set_Cell_Contents;

   --------------
   -- Get_Rows --
   --------------

   function Get_Rows (Clist : access Gtk_Clist_Record) return Gint is
      function Internal (Clist : System.Address) return Gint;
      pragma Import (C, Internal, "ada_clist_get_rows");
   begin
      return Internal (Get_Object (Clist));
   end Get_Rows;

   -----------------
   -- Get_Columns --
   -----------------

   function Get_Columns (Clist : access Gtk_Clist_Record) return Gint is
      function Internal (Clist : System.Address) return Gint;
      pragma Import (C, Internal, "ada_clist_get_columns");
   begin
      return Internal (Get_Object (Clist));
   end Get_Columns;

   --------------
   -- Row_Data --
   --------------

   package body Row_Data is

      type Data_Access is access all Data_Type;
      type Cb_Record is
         record
            Ptr : Data_Access;
         end record;
      type Cb_Record_Access is access all Cb_Record;

      function Convert is new
        Unchecked_Conversion (System.Address, Cb_Record_Access);

      ----------
      -- Free --
      ----------

      procedure Free_Data (Data : System.Address) is
         procedure Internal is new
           Unchecked_Deallocation (Cb_Record, Cb_Record_Access);
         procedure Internal2 is new
           Unchecked_Deallocation (Data_Type, Data_Access);
         D : Cb_Record_Access := Convert (Data);

      begin
         Internal2 (D.Ptr);
         Internal (D);
      end Free_Data;

      ---------
      -- Get --
      ---------

      function Get (Object : access Gtk_Clist_Record'Class;
                    Row    : in     Gint)
                    return Data_Type
      is
         function Internal (Object : in System.Address;
                            Row    : in Gint)
                            return System.Address;
         pragma Import (C, Internal, "gtk_clist_get_row_data");
         D : constant Cb_Record_Access
           := Convert (Internal (Get_Object (Object), Row));
      begin
         return D.Ptr.all;
      end Get;

      ---------
      -- Get --
      ---------

      function Get (Object : access Gtk_Clist_Record'Class;
                    Row    : in     Gtk_Clist_Row)
                    return Data_Type
      is
         function Internal (Object : in System.Address;
                            Row    : in Gtk_Clist_Row)
                            return System.Address;
         pragma Import (C, Internal, "ada_gtk_clist_get_row_data");
         D : constant Cb_Record_Access :=
           Convert (Internal (Get_Object (Object), Row));

      begin
         return D.Ptr.all;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (Object : access Gtk_Clist_Record'Class;
                     Row    : in     Gint;
                     Data   : in     Data_Type)
      is
         function Convert is new Unchecked_Conversion (Cb_Record_Access,
                                                       System.Address);
         procedure Internal (Object  : in System.Address;
                             Row     : in Gint;
                             Data    : in System.Address;
                             Destroy : in System.Address);
         pragma Import (C, Internal, "gtk_clist_set_row_data_full");
         D : constant Cb_Record_Access :=
           new Cb_Record'(Ptr => new Data_Type'(Data));
      begin
         Internal (Get_Object (Object),
                   Row,
                   Convert (D),
                   Free_Data'Address);
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set (Object : access Gtk_Clist_Record'Class;
                     Row    : in     Gtk_Clist_Row;
                     Data   : in     Data_Type)
      is
         function Convert is new Unchecked_Conversion (Cb_Record_Access,
                                                       System.Address);
         procedure Internal (Object  : in System.Address;
                             Row     : in Gtk_Clist_Row;
                             Data    : in System.Address;
                             Destroy : in System.Address);
         pragma Import (C, Internal, "ada_gtk_clist_set_row_data_full");
         D : constant Cb_Record_Access :=
           new Cb_Record'(Ptr => new Data_Type'(Data));

      begin
         Internal (Get_Object (Object), Row, Convert (D), Free_Data'Address);
      end Set;
   end Row_Data;

end Gtk.Clist;
