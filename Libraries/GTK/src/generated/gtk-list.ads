
-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
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
--
--
--  </description>
--  <group>Obsolescent widgets</group>
--  <testgtk>create_list.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.List is

   pragma Obsolescent;

   type Gtk_List_Record is new Gtk_Container_Record with null record;
   type Gtk_List is access all Gtk_List_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (List : out Gtk_List);
   procedure Initialize (List : access Gtk_List_Record'Class);

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_list_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append_Items
      (List  : access Gtk_List_Record;
       Items : Gtk.Widget.Widget_List.Glist);

   function Child_Position
      (List  : access Gtk_List_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint;

   procedure Clear_Items
      (List    : access Gtk_List_Record;
       Start   : Gint;
       The_End : Gint);
   --  Remove some items from the list. If The_End is negative, it means the
   --  end of the list. The first item in the list has an index of 0

   procedure End_Drag_Selection (List : access Gtk_List_Record);

   procedure End_Selection (List : access Gtk_List_Record);

   procedure Extend_Selection
      (List                 : access Gtk_List_Record;
       Scroll_Type          : Gtk.Enums.Gtk_Scroll_Type;
       Position             : Gfloat;
       Auto_Start_Selection : Boolean);

   procedure Insert_Items
      (List     : access Gtk_List_Record;
       Items    : Gtk.Widget.Widget_List.Glist;
       Position : Gint);

   procedure Prepend_Items
      (List  : access Gtk_List_Record;
       Items : Gtk.Widget.Widget_List.Glist);

   procedure Remove_Items
      (List  : access Gtk_List_Record;
       Items : Gtk.Widget.Widget_List.Glist);

   procedure Remove_Items_No_Unref
      (List  : access Gtk_List_Record;
       Items : Gtk.Widget.Widget_List.Glist);

   procedure Scroll_Horizontal
      (List        : access Gtk_List_Record;
       Scroll_Type : Gtk.Enums.Gtk_Scroll_Type;
       Position    : Gfloat);

   procedure Scroll_Vertical
      (List        : access Gtk_List_Record;
       Scroll_Type : Gtk.Enums.Gtk_Scroll_Type;
       Position    : Gfloat);

   procedure Select_All (List : access Gtk_List_Record);

   procedure Select_Child
      (List  : access Gtk_List_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Select_Item (List : access Gtk_List_Record; Item : Gint);

   procedure Set_Selection_Mode
      (List : access Gtk_List_Record;
       Mode : Gtk.Enums.Gtk_Selection_Mode);

   procedure Start_Selection (List : access Gtk_List_Record);

   procedure Toggle_Add_Mode (List : access Gtk_List_Record);

   procedure Toggle_Focus_Row (List : access Gtk_List_Record);

   procedure Toggle_Row
      (List : access Gtk_List_Record;
       Item : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Undo_Selection (List : access Gtk_List_Record);

   procedure Unselect_All (List : access Gtk_List_Record);

   procedure Unselect_Child
      (List  : access Gtk_List_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Unselect_Item (List : access Gtk_List_Record; Item : Gint);

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Selection (Widget : access Gtk.List.Gtk_List_Record)
   return Widget_List.Glist;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_List_Record, Gtk_List);
   function "+"
     (Widget : access Gtk_List_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_List
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Selection_Mode_Property
   --  Type: Gtk.Enums.Gtk_Selection_Mode
   --  Flags: read-write

   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "select-child"
   --     procedure Handler
   --       (Self   : access Gtk_List_Record'Class;
   --        Object : Gtk.Widget.Gtk_Widget);
   --
   --  "selection-changed"
   --     procedure Handler (Self : access Gtk_List_Record'Class);
   --
   --  "unselect-child"
   --     procedure Handler
   --       (Self   : access Gtk_List_Record'Class;
   --        Object : Gtk.Widget.Gtk_Widget);

   Signal_Select_Child : constant Glib.Signal_Name := "select-child";
   Signal_Selection_Changed : constant Glib.Signal_Name := "selection-changed";
   Signal_Unselect_Child : constant Glib.Signal_Name := "unselect-child";

private
   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode :=
     Gtk.Enums.Build ("selection-mode");
end Gtk.List;
