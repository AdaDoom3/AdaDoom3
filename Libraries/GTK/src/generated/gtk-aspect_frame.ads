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
--  A Gtk_Aspect_Frame is the same type of widget as a frame, but it
--  constrains its child to a specific aspect ratio between its width and its
--  height.
--
--  This ratio can either be given explicitly by the user, or chosen from the
--  widget's initial size request (might be different from the one if was
--  actually given).
--
--  </description>
--  <group>Layout Containers</group>
--  <testgtk>create_frame.adb.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Frame;       use Gtk.Frame;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Aspect_Frame is

   type Gtk_Aspect_Frame_Record is new Gtk_Frame_Record with null record;
   type Gtk_Aspect_Frame is access all Gtk_Aspect_Frame_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Aspect_Frame : out Gtk_Aspect_Frame;
       Label        : UTF8_String := "";
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean);
   procedure Initialize
      (Aspect_Frame : access Gtk_Aspect_Frame_Record'Class;
       Label        : UTF8_String := "";
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean);
   --  Create a new Gtk.Aspect_Frame.Gtk_Aspect_Frame.
   --  "label": Label text.
   --  "xalign": Horizontal alignment of the child within the allocation of
   --  the Gtk.Aspect_Frame.Gtk_Aspect_Frame. This ranges from 0.0 (left
   --  aligned) to 1.0 (right aligned)
   --  "yalign": Vertical alignment of the child within the allocation of the
   --  Gtk.Aspect_Frame.Gtk_Aspect_Frame. This ranges from 0.0 (left aligned)
   --  to 1.0 (right aligned)
   --  "ratio": The desired aspect ratio.
   --  "obey_child": If True, Ratio is ignored, and the aspect ratio is taken
   --  from the requistion of the child.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_aspect_frame_get_type");

   -------------
   -- Methods --
   -------------

   procedure Set
      (Aspect_Frame : access Gtk_Aspect_Frame_Record;
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean);
   --  Set parameters for an existing Gtk.Aspect_Frame.Gtk_Aspect_Frame.
   --  "xalign": Horizontal alignment of the child within the allocation of
   --  the Gtk.Aspect_Frame.Gtk_Aspect_Frame. This ranges from 0.0 (left
   --  aligned) to 1.0 (right aligned)
   --  "yalign": Vertical alignment of the child within the allocation of the
   --  Gtk.Aspect_Frame.Gtk_Aspect_Frame. This ranges from 0.0 (left aligned)
   --  to 1.0 (right aligned)
   --  "ratio": The desired aspect ratio.
   --  "obey_child": If True, Ratio is ignored, and the aspect ratio is taken
   --  from the requistion of the child.

   ------------
   -- Fields --
   ------------

   function Get_Xalign
      (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat;

   function Get_Yalign
      (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat;

   function Get_Ratio
      (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Aspect_Frame_Record, Gtk_Aspect_Frame);
   function "+"
     (Widget : access Gtk_Aspect_Frame_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Aspect_Frame
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Obey_Child_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Ratio_Property
   --  Type: Gfloat
   --  Flags: read-write
   --
   --  Name: Xalign_Property
   --  Type: Gfloat
   --  Flags: read-write
   --
   --  Name: Yalign_Property
   --  Type: Gfloat
   --  Flags: read-write

   Obey_Child_Property : constant Glib.Properties.Property_Boolean;
   Ratio_Property : constant Glib.Properties.Property_Float;
   Xalign_Property : constant Glib.Properties.Property_Float;
   Yalign_Property : constant Glib.Properties.Property_Float;

private
   Obey_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("obey-child");
   Ratio_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("ratio");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
end Gtk.Aspect_Frame;
