-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  This widget represents a widget that can be dragged by the user to change
--  the visible area of another widget. It is typically only used through a
--  Gtk.Scrolled_Window, although you might need, from time to time, to use it
--  directly if the widget you want to scroll isn't entirely suitable for a
--  scrolled window.
--  For instance, if you are creating your own drawing area, unlimited in size,
--  you do not want to create a Gtk_Drawing_Area 100_000 pixels large, since
--  that would use too much memory. Instead, you create one with just the
--  size of the visible area on the screen, then connect it with a scrollbar so
--  that when the user moves the scrollbar, you change what should be displayed
--  in the drawing area.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Scrolling</group>

with Glib.Properties;
with Gtk.GRange;
with Gtk.Adjustment;

package Gtk.Scrollbar is

   type Gtk_Scrollbar_Record is new Gtk.GRange.Gtk_Range_Record with private;
   subtype Gtk_Hscrollbar_Record is Gtk_Scrollbar_Record;
   subtype Gtk_Vscrollbar_Record is Gtk_Scrollbar_Record;

   type Gtk_Scrollbar is access all Gtk_Scrollbar_Record'Class;
   subtype Gtk_Hscrollbar is Gtk_Scrollbar;
   subtype Gtk_Vscrollbar is Gtk_Scrollbar;

   procedure Gtk_New_Hscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   procedure Initialize_Hscrollbar
     (Widget     : access Gtk_Scrollbar_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Creates or initializes a new horizontal scrollbar

   procedure Gtk_New_Vscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   procedure Initialize_Vscrollbar
     (Widget     : access Gtk_Scrollbar_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Creates or initializes a new vertical scrollbar

   function Get_Type return Gtk.Gtk_Type;
   function Hscrollbar_Get_Type return Gtk.Gtk_Type;
   function Vscrollbar_Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Scrollbar.

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Fixed_Slider_Length_Property
   --  Type:  Boolean
   --  Descr: Don't change slider size, just lock it to the minimum length
   --
   --  Name:  Has_Backward_Stepper_Property
   --  Type:  Boolean
   --  Descr: Display the standard backward arrow button
   --
   --  Name:  Has_Forward_Stepper_Property
   --  Type:  Boolean
   --  Descr: Display the standard forward arrow button
   --
   --  Name:  Has_Secondary_Backward_Stepper_Property
   --  Type:  Boolean
   --  Descr: Display a second backward arrow button on the opposite end of the
   --         scrollbar
   --
   --  Name:  Has_Secondary_Forward_Stepper_Property
   --  Type:  Boolean
   --  Descr: Display a secondary forward arrow button on the opposite end of
   --         the scrollbar
   --
   --  Name:  Min_Slider_Length_Property
   --  Type:  Int
   --  Descr: Minimum length of scrollbar slider
   --  </style_properties>

   Fixed_Slider_Length_Property  : constant Glib.Properties.Property_Boolean;
   Has_Backward_Stepper_Property : constant Glib.Properties.Property_Boolean;
   Has_Forward_Stepper_Property  : constant Glib.Properties.Property_Boolean;
   Has_Secondary_Backward_Stepper_Property : constant
     Glib.Properties.Property_Boolean;
   Has_Secondary_Forward_Stepper_Property : constant
     Glib.Properties.Property_Boolean;
   Min_Slider_Length_Property    : constant Glib.Properties.Property_Int;

private
   type Gtk_Scrollbar_Record is new Gtk.GRange.Gtk_Range_Record
     with null record;

   Fixed_Slider_Length_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("fixed-slider-length");
   Has_Backward_Stepper_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-backward-stepper");
   Has_Forward_Stepper_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-forward-stepper");
   Has_Secondary_Backward_Stepper_Property : constant
     Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-secondary-backward-stepper");
   Has_Secondary_Forward_Stepper_Property : constant
     Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-secondary-forward-stepper");
   Min_Slider_Length_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("min-slider-length");

   pragma Import (C, Get_Type, "gtk_scrollbar_get_type");
   pragma Import (C, Hscrollbar_Get_Type, "gtk_hscrollbar_get_type");
   pragma Import (C, Vscrollbar_Get_Type, "gtk_vscrollbar_get_type");
end Gtk.Scrollbar;
