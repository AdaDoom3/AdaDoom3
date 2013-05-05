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
--  Gtk.Tooltip belongs to the new tooltips API that was introduced in
--  GTK+ 2.12 and which deprecates the old Gtk.Tooltips API.
--
--  Basic tooltips can be realized simply by using Set_Tooltip_Text or
--  Set_Tooltip_Markup without any explicit tooltip object.
--
--  When you need a tooltip with a little more fancy contents, like adding
--  an image, or you want the tooltip to have different contents per
--  Gtk_Tree_View row or cell, you will have to do a little more work:
--
--  Set the "has-tooltip" property to True, this will make GTK+ monitor the
--  widget for motion and related events which are needed to determine when
--  and where to show a tooltip.
--
--  Connect to the "query-tooltip" signal. This signal will be emitted when a
--  tooltip is supposed to be shown. One of the arguments passed to the signal
--  handler is a Gtk_Tooltip object. This is the object that we are about to
--  display as a tooltip, and can be manipulated in your callback using
--  functions like Set_Icon.  There are functions for setting the tooltip's
--  markup, setting an image from a stock icon, or even putting in a custom
--  widget.
--
--  Return True from your query-tooltip handler. This causes the tooltip to
--  be shown. If you return False, it will not be shown.
--
--  In the probably rare case where you want to have even more control over
--  the tooltip that is about to be shown, you can set your own Gtk_Window
--  which will be used as tooltip window. This works as follows:
--
--  Set "has-tooltip" and connect to "query-tooltip" as before.
--
--  Use Gtk.Widget.Set_Tooltip_Window to set a Gtk_Window created by you as
--  tooltip window.
--
--  In the ::query-tooltip callback you can access your window using
--  Gtk.Widget.Get_Tooltip_Window and manipulate as you wish. The semantics
--  of the return value are exactly as before, return True to show the window,
--  False to not show it.
--  </description>
--  <c_version>2.16.6</c_version>

with Glib;
with Glib.Object;
with Gdk.Display;
with Gdk.Pixbuf;
with Gdk.Rectangle;
with Gtk.Enums;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Tooltip is

   type Gtk_Tooltip_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Tooltip is access all Gtk_Tooltip_Record'Class;

   function Get_Type return GType;

   procedure Set_Custom
     (Tooltip       : access Gtk_Tooltip_Record;
      Custom_Widget : access Gtk_Widget_Record'Class);
   --  Replaces the widget packed into the tooltip with Custom_widget.
   --  By default a box with a Gtk_Image and Gtk_Label is embedded in
   --  the tooltip, which can be configured using Set_Markup and Set_Icon.

   procedure Set_Icon
     (Tooltip : access Gtk_Tooltip_Record;
      Pixbuf  : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Sets the icon of the tooltip (which is in front of the text) to be
   --  Pixbuf.  If Pixbuf is null, the image will be hidden.

   procedure Set_Icon_From_Icon_Name
     (Tooltip   : access Gtk_Tooltip_Record;
      Icon_Name : String;
      Size      : Gtk.Enums.Gtk_Icon_Size);
   --  Sets the icon of the tooltip (which is in front of the text) to be
   --  the icon indicated by Icon_Name with the size indicated by Size.
   --  If Icon_Name is "", the image will be hidden.

   procedure Set_Icon_From_Stock
     (Tooltip  : access Gtk_Tooltip_Record;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size);
   --  Sets the icon of the tooltip (which is in front of the text) to be
   --  the stock item indicated by Stock_Id with the size indicated by Size.
   --  If Stock_Id is "", the image will be hidden.

   procedure Set_Markup
     (Tooltip : access Gtk_Tooltip_Record;
      Markup  : UTF8_String);
   --  Sets the text of the tooltip to be Markup, which is marked up with
   --  the Pango text markup language.  If Markup is "", the label will be
   --  hidden.

   procedure Set_Text
     (Tooltip : access Gtk_Tooltip_Record;
      Text    : UTF8_String);
   --  Sets the text of the tooltip to be Text. If Text is "", the label
   --  will be hidden. See also Set_Markup.

   procedure Set_Tip_Area
     (Tooltip : access Gtk_Tooltip_Record;
      Rect    : Gdk.Rectangle.Gdk_Rectangle);
   --  Sets the area of the widget, where the contents of this tooltip apply,
   --  to be Rect (in widget coordinates).  This is especially useful for
   --  properly setting tooltips on Gtk_Tree_View rows and cells,
   --  Gtk_Icon_Views, etc.
   --
   --  For setting tooltips on Gtk_Tree_View, please refer to the convenience
   --  functions for this: Gtk.Tree_View.Set_Tooltip_Row and
   --  Gtk.Tree_View.Set_Tooltip_Cell.

   procedure Trigger_Tooltip_Query
     (Display : access Gdk.Display.Gdk_Display_Record);
   --  Triggers a new tooltip query on Display, in order to update the current
   --  visible tooltip, or to show/hide the current tooltip.  This function is
   --  useful to call when, for example, the state of the widget changed by a
   --  key press.

private
   type Gtk_Tooltip_Record is new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_tooltip_get_type");
end Gtk.Tooltip;
