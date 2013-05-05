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

package body Gtk.Tooltip is

   ----------------
   -- Set_Custom --
   ----------------

   procedure Set_Custom
     (Tooltip       : access Gtk_Tooltip_Record;
      Custom_Widget : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Tooltip, Custom_Widget : System.Address);
      pragma Import (C, Internal, "gtk_tooltip_set_custom");
   begin
      Internal (Get_Object (Tooltip), Get_Object (Custom_Widget));
   end Set_Custom;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
     (Tooltip : access Gtk_Tooltip_Record;
      Pixbuf  : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal (Tooltip, Pixbuf : System.Address);
      pragma Import (C, Internal, "gtk_tooltip_set_icon");
   begin
      Internal (Get_Object (Tooltip), Get_Object (Pixbuf));
   end Set_Icon;

   -----------------------------
   -- Set_Icon_From_Icon_Name --
   -----------------------------

   procedure Set_Icon_From_Icon_Name
     (Tooltip   : access Gtk_Tooltip_Record;
      Icon_Name : String;
      Size      : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
        (Tooltip   : System.Address;
         Icon_Name : System.Address;
         Size      : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_tooltip_set_icon_from_icon_name");

      Tmp : constant String := Icon_Name & ASCII.NUL;
   begin
      if Icon_Name = "" then
         Internal (Get_Object (Tooltip), System.Null_Address, Size);
      else
         Internal (Get_Object (Tooltip), Tmp'Address, Size);
      end if;
   end Set_Icon_From_Icon_Name;

   -------------------------
   -- Set_Icon_From_Stock --
   -------------------------

   procedure Set_Icon_From_Stock
     (Tooltip  : access Gtk_Tooltip_Record;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
        (Tooltip  : System.Address;
         Stock_Id : System.Address;
         Size     : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_tooltip_set_icon_from_stock");

      Tmp : constant String := Stock_Id & ASCII.NUL;
   begin
      if Stock_Id = "" then
         Internal (Get_Object (Tooltip), System.Null_Address, Size);
      else
         Internal (Get_Object (Tooltip), Tmp'Address, Size);
      end if;
   end Set_Icon_From_Stock;

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup
     (Tooltip : access Gtk_Tooltip_Record;
      Markup  : UTF8_String)
   is
      procedure Internal (Tooltip, Markup : System.Address);
      pragma Import (C, Internal, "gtk_tooltip_set_markup");

      Tmp : constant UTF8_String := Markup & ASCII.NUL;
   begin
      if Markup = "" then
         Internal (Get_Object (Tooltip), System.Null_Address);
      else
         Internal (Get_Object (Tooltip), Tmp'Address);
      end if;
   end Set_Markup;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Tooltip : access Gtk_Tooltip_Record;
      Text    : UTF8_String)
   is
      procedure Internal (Tooltip, Text : System.Address);
      pragma Import (C, Internal, "gtk_tooltip_set_text");

      Tmp : constant UTF8_String := Text & ASCII.NUL;
   begin
      if Text = "" then
         Internal (Get_Object (Tooltip), System.Null_Address);
      else
         Internal (Get_Object (Tooltip), Tmp'Address);
      end if;
   end Set_Text;

   ------------------
   -- Set_Tip_Area --
   ------------------

   procedure Set_Tip_Area
     (Tooltip : access Gtk_Tooltip_Record;
      Rect    : Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Tooltip : System.Address;
         Rect    : Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tooltip_set_tip_area");
   begin
      Internal (Get_Object (Tooltip), Rect);
   end Set_Tip_Area;

   ---------------------------
   -- Trigger_Tooltip_Query --
   ---------------------------

   procedure Trigger_Tooltip_Query
     (Display : access Gdk.Display.Gdk_Display_Record)
   is
      procedure Internal (Display : System.Address);
      pragma Import (C, Internal, "gtk_tooltip_trigger_tooltip_query");
   begin
      Internal (Get_Object (Display));
   end Trigger_Tooltip_Query;

end Gtk.Tooltip;
