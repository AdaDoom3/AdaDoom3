-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2002-2005 AdaCore                    --
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

with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Gdk.Rectangle;        use Gdk.Rectangle;
with Pango.Attributes;     use Pango.Attributes;
with Pango.Context;        use Pango.Context;
with Pango.Font;           use Pango.Font;
with Pango.Tabs;           use Pango.Tabs;
with System;               use System;
with Gtkada.Types;         use Gtkada.Types;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Pango.Layout is

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size
     (Layout : access Pango_Layout_Record;
      Width  : out Gint;
      Height : out Gint)
   is
      procedure Internal
        (Layout : System.Address; Width, Height : out Gint);
      pragma Import (C, Internal, "pango_layout_get_size");
   begin
      Internal (Get_Object (Layout), Width, Height);
   end Get_Size;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   procedure Get_Pixel_Size
     (Layout : access Pango_Layout_Record;
      Width  : out Gint;
      Height : out Gint)
   is
      procedure Internal
        (Layout : System.Address; Width, Height : out Gint);
      pragma Import (C, Internal, "pango_layout_get_pixel_size");
   begin
      Internal (Get_Object (Layout), Width, Height);
   end Get_Pixel_Size;

   -----------------
   -- Get_Extents --
   -----------------

   procedure Get_Extents
     (Layout       : access Pango_Layout_Record;
      Ink_Rect     : out Gdk.Rectangle.Gdk_Rectangle;
      Logical_Rect : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal (Layout : System.Address;
                          Ink_Rect, Logical_Rect : out Gdk_Rectangle);
      pragma Import (C, Internal, "pango_layout_get_extents");
   begin
      Internal (Get_Object (Layout), Ink_Rect, Logical_Rect);
   end Get_Extents;

   -----------------------
   -- Get_Pixel_Extents --
   -----------------------

   procedure Get_Pixel_Extents
     (Layout       : access Pango_Layout_Record;
      Ink_Rect     : out Gdk.Rectangle.Gdk_Rectangle;
      Logical_Rect : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal (Layout : System.Address;
                          Ink_Rect, Logical_Rect : out Gdk_Rectangle);
      pragma Import (C, Internal, "pango_layout_get_pixel_extents");
   begin
      Internal (Get_Object (Layout), Ink_Rect, Logical_Rect);
   end Get_Pixel_Extents;

   --------------------------
   -- Set_Font_Description --
   --------------------------

   procedure Set_Font_Description
     (Layout : access Pango_Layout_Record;
      Font   : Pango.Font.Pango_Font_Description)
   is
      procedure Internal
        (Layout : System.Address; Font : Pango_Font_Description);
      pragma Import (C, Internal, "pango_layout_set_font_description");
   begin
      Internal (Get_Object (Layout), Font);
   end Set_Font_Description;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Layout : out Pango_Layout;
      Context : access Pango.Context.Pango_Context_Record'Class)
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_layout_new");
   begin
      Layout := new Pango_Layout_Record;
      Set_Object (Layout, Internal (Get_Object (Context)));
   end Gdk_New;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context (Layout : access Pango_Layout_Record)
      return Pango.Context.Pango_Context
   is
      function Internal (Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_layout_get_context");
      Stub : Pango_Context_Record;
   begin
      return Pango_Context
        (Get_User_Data (Internal (Get_Object (Layout)), Stub));
   end Get_Context;

   ---------------------
   -- Context_Changed --
   ---------------------

   procedure Context_Changed (Layout : access Pango_Layout_Record) is
      procedure Internal (Layout : System.Address);
      pragma Import (C, Internal, "pango_layout_context_changed");
   begin
      Internal (Get_Object (Layout));
   end Context_Changed;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Layout : access Pango_Layout_Record; Text : String) is
      procedure Internal
        (Layout : System.Address; Text : String; Length : Gint);
      pragma Import (C, Internal, "pango_layout_set_text");
   begin
      Internal (Get_Object (Layout), Text, Text'Length);
   end Set_Text;

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup
     (Layout : access Pango_Layout_Record;
      Markup : Glib.UTF8_String)
   is
      procedure Internal
        (Layout : System.Address; Text : Glib.UTF8_String; Length : Gint);
      pragma Import (C, Internal, "pango_layout_set_markup");
   begin
      Internal (Get_Object (Layout), Markup, Markup'Length);
   end Set_Markup;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Layout : access Pango_Layout_Record)
      return Gtkada.Types.Chars_Ptr
   is
      function Internal (Layout : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "pango_layout_get_text");
   begin
      return Internal (Get_Object (Layout));
   end Get_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Layout : access Pango_Layout_Record) return String is
      C : constant Gtkada.Types.Chars_Ptr := Get_Text (Layout);
   begin
      if C = Gtkada.Types.Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Text;

   --------------------
   -- Get_Line_Count --
   --------------------

   function Get_Line_Count (Layout : access Pango_Layout_Record)
      return Glib.Gint
   is
      function Internal (Layout : System.Address) return Gint;
      pragma Import (C, Internal, "pango_layout_get_line_count");
   begin
      return Internal (Get_Object (Layout));
   end Get_Line_Count;

   -----------------
   -- Set_Justify --
   -----------------

   procedure Set_Justify
     (Layout : access Pango_Layout_Record; Justify : Boolean)
   is
      procedure Internal (Layout : System.Address; Justify : Integer);
      pragma Import (C, Internal, "pango_layout_set_justify");
   begin
      Internal (Get_Object (Layout), Boolean'Pos (Justify));
   end Set_Justify;

   -----------------
   -- Get_Justify --
   -----------------

   function Get_Justify (Layout : access Pango_Layout_Record) return Boolean is
      function Internal (Layout : System.Address) return Integer;
      pragma Import (C, Internal, "pango_layout_get_justify");
   begin
      return Boolean'Val (Internal (Get_Object (Layout)));
   end Get_Justify;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
     (Layout    : access Pango_Layout_Record'Class;
      Alignment : Pango_Alignment)
   is
      procedure Internal (Layout : System.Address; Align : Pango_Alignment);
      pragma Import (C, Internal, "pango_layout_set_alignment");
   begin
      Internal (Get_Object (Layout), Alignment);
   end Set_Alignment;

   -------------------
   -- Get_Alignment --
   -------------------

   function Get_Alignment (Layout : access Pango_Layout_Record)
      return Pango_Alignment
   is
      function Internal (Layout : System.Address) return Pango_Alignment;
      pragma Import (C, Internal, "pango_layout_get_alignment");
   begin
      return Internal (Get_Object (Layout));
   end Get_Alignment;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width
     (Layout : access Pango_Layout_Record; Width : Glib.Gint)
   is
      procedure Internal (Layout : System.Address; Width : Gint);
      pragma Import (C, Internal, "pango_layout_set_width");
   begin
      Internal (Get_Object (Layout), Width);
   end Set_Width;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Layout : access Pango_Layout_Record) return Glib.Gint is
      function Internal (Layout : System.Address) return Gint;
      pragma Import (C, Internal, "pango_layout_get_width");
   begin
      return Internal (Get_Object (Layout));
   end Get_Width;

   --------------
   -- Set_Wrap --
   --------------

   procedure Set_Wrap
     (Layout : access Pango_Layout_Record; Mode : Pango_Wrap_Mode)
   is
      procedure Internal (Layout : System.Address; Mode : Pango_Wrap_Mode);
      pragma Import (C, Internal, "pango_layout_set_wrap");
   begin
      Internal (Get_Object (Layout), Mode);
   end Set_Wrap;

   --------------
   -- Get_Wrap --
   --------------

   function Get_Wrap (Layout : access Pango_Layout_Record)
      return Pango_Wrap_Mode
   is
      function Internal (Layout : System.Address) return Pango_Wrap_Mode;
      pragma Import (C, Internal, "pango_layout_get_wrap");
   begin
      return Internal (Get_Object (Layout));
   end Get_Wrap;

   -----------------
   -- XY_To_Index --
   -----------------

   procedure XY_To_Index
     (Layout           : access Pango_Layout_Record;
      X_Pango, Y_Pango : Glib.Gint;
      Byte_Index       : out Integer;
      Trailing         : out Integer;
      Exact            : out Boolean)
   is
      function Internal
        (Layout   : System.Address;
         X, Y     : Gint;
         Index    : access Integer;
         Trailing : access Integer) return Integer;
      pragma Import (C, Internal, "pango_layout_xy_to_index");

      B, T : aliased Integer;
      E    : Integer;
   begin
      E := Internal (Get_Object (Layout), X_Pango, Y_Pango,
                     B'Unchecked_Access, T'Unchecked_Access);
      Byte_Index := B;
      Trailing := T;
      Exact := E = 1;
   end XY_To_Index;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Layout : access Pango_Layout_Record;
      Line   : Natural) return Pango_Layout_Line
   is
      function Internal (L : System.Address; Line : Integer)
         return Pango_Layout_Line;
      pragma Import (C, Internal, "pango_layout_get_line");
   begin
      return Internal (Get_Object (Layout), Line);
   end Get_Line;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
     (Layout : access Pango_Layout_Record;
      Attributes : Pango.Attributes.Pango_Attr_List)
   is
      procedure Internal (Layout : System.Address; Attr : Pango_Attr_List);
      pragma Import (C, Internal, "pango_layout_set_attributes");
   begin
      Internal (Get_Object (Layout), Attributes);
   end Set_Attributes;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes (Layout : access Pango_Layout_Record)
      return Pango.Attributes.Pango_Attr_List
   is
      function Internal (Layout : System.Address) return Pango_Attr_List;
      pragma Import (C, Internal, "pango_layout_get_attributes");
   begin
      return Internal (Get_Object (Layout));
   end Get_Attributes;

   --------------
   -- Set_Tabs --
   --------------

   procedure Set_Tabs
     (Layout : access Pango_Layout_Record;
      Tabs   : Pango.Tabs.Pango_Tab_Array)
   is
      procedure Internal (Layout : System.Address; Tabs : Pango_Tab_Array);
      pragma Import (C, Internal, "pango_layout_set_tabs");
   begin
      Internal (Get_Object (Layout), Tabs);
   end Set_Tabs;

   --------------
   -- Get_Tabs --
   --------------

   function Get_Tabs
     (Layout : access Pango_Layout_Record) return Pango.Tabs.Pango_Tab_Array
   is
      function Internal (Layout : System.Address) return Pango_Tab_Array;
      pragma Import (C, Internal, "pango_layout_get_tabs");
   begin
      return Internal (Get_Object (Layout));
   end Get_Tabs;

end Pango.Layout;
