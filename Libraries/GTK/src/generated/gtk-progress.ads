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
--  This package is deprecated. It now only acts as an abstract base class for
--  other widgets.
--
--  </description>
--  <group>Obsolescent widgets</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Progress is

   pragma Obsolescent;

   type Gtk_Progress_Record is new Gtk_Widget_Record with null record;
   type Gtk_Progress is access all Gtk_Progress_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_progress_get_type");

   -------------
   -- Methods --
   -------------

   procedure Configure
      (Progress : access Gtk_Progress_Record;
       Value    : Gdouble;
       Min      : Gdouble;
       Max      : Gdouble);

   function Get_Current_Percentage
      (Progress : access Gtk_Progress_Record) return Gdouble;

   function Get_Current_Text
      (Progress : access Gtk_Progress_Record) return UTF8_String;

   function Get_Percentage_From_Value
      (Progress : access Gtk_Progress_Record;
       Value    : Gdouble) return Gdouble;

   function Get_Text_From_Value
      (Progress : access Gtk_Progress_Record;
       Value    : Gdouble) return UTF8_String;

   function Get_Value (Progress : access Gtk_Progress_Record) return Gdouble;
   procedure Set_Value
      (Progress : access Gtk_Progress_Record;
       Value    : Gdouble);

   procedure Set_Activity_Mode
      (Progress      : access Gtk_Progress_Record;
       Activity_Mode : Boolean);

   procedure Set_Adjustment
      (Progress   : access Gtk_Progress_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   procedure Set_Format_String
      (Progress : access Gtk_Progress_Record;
       Format   : UTF8_String);

   procedure Set_Percentage
      (Progress   : access Gtk_Progress_Record;
       Percentage : Gdouble);

   procedure Set_Show_Text
      (Progress  : access Gtk_Progress_Record;
       Show_Text : Boolean);

   procedure Set_Text_Alignment
      (Progress : access Gtk_Progress_Record;
       X_Align  : Gfloat;
       Y_Align  : Gfloat);

   ------------
   -- Fields --
   ------------

   function Get_Adjustment
      (Progress : access Gtk_Progress_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   function Get_Activity_Mode
      (Progress : access Gtk_Progress_Record) return Guint;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Progress_Record, Gtk_Progress);
   function "+"
     (Widget : access Gtk_Progress_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Progress
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Activity_Mode_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Show_Text_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Text_Xalign_Property
   --  Type: Gfloat
   --  Flags: read-write
   --
   --  Name: Text_Yalign_Property
   --  Type: Gfloat
   --  Flags: read-write

   Activity_Mode_Property : constant Glib.Properties.Property_Boolean;
   Show_Text_Property : constant Glib.Properties.Property_Boolean;
   Text_Xalign_Property : constant Glib.Properties.Property_Float;
   Text_Yalign_Property : constant Glib.Properties.Property_Float;

private
   Activity_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activity-mode");
   Show_Text_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-text");
   Text_Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("text-xalign");
   Text_Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("text-yalign");
end Gtk.Progress;
