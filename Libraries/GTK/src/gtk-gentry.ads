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
--  A Gtk_Entry is a single line text editing widget.
--  The text is automatically scrolled if it is longer than can be displayed
--  on the screen, so that the cursor position is visible at all times.
--
--  See Gtk_Text_View for a multiple-line text editing widget.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Numeric/Text Data Entry</group>
--  <testgtk>create_entry.adb</testgtk>
--  <screenshot>gtk-gentry</screenshot>

with Glib.G_Icon;
with Glib.Properties;
with Gdk.Dnd;
with Gdk.Pixbuf;
with Gtk.Adjustment;
with Gtk.Editable;
with Gtk.Entry_Completion;  use Gtk.Entry_Completion;
with Gtk.Image;
with Gtk.Selection;
with Gtk.Style;
with Pango.Layout;

package Gtk.GEntry is

   type Gtk_Entry_Icon_Position is
     (Gtk_Entry_Icon_Primary, Gtk_Entry_Icon_Secondary);
   pragma Convention (C, Gtk_Entry_Icon_Position);

   type Gtk_Entry_Record is new Gtk.Editable.Gtk_Editable_Record with private;
   --  Gtk_Entry is actually a child of Gtk_Widget, and implements the
   --  Gtk_Editable interface, but GtkAda does not support yet interfaces,
   --  so use direct inheritance for now ???

   type Gtk_Entry is access all Gtk_Entry_Record'Class;
   subtype Gtk_GEntry is Gtk_Entry;

   procedure Gtk_New (Widget : out Gtk_Entry);
   --  Create a new entry with no maximum length for the text

   procedure Initialize (Widget : access Gtk_Entry_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Entry.

   function Get_Text_Length (The_Entry : access Gtk_Entry_Record)
      return Guint16;
   --  Retrieves the current length of the text in The_Entry.

   procedure Set_Visibility
     (The_Entry : access Gtk_Entry_Record; Visible : Boolean);
   function Get_Visibility
     (The_Entry : access Gtk_Entry_Record) return Boolean;
   --  Set the visibility of the characters in the entry.
   --  If Visible is set to False, the characters will be replaced with
   --  the invisible character ('*' by default) in the display, and when the
   --  text is copied elsewhere.

   procedure Set_Invisible_Char
     (The_Entry : access Gtk_Entry_Record; Char : Gunichar);
   function Get_Invisible_Char
     (The_Entry : access Gtk_Entry_Record) return Gunichar;
   --  Gets/Sets the character to use in place of the actual text when
   --  Set_Visibility has been called to set text visibility to False.
   --  i.e. this is the character used in "password mode" to
   --  show the user how many characters have been typed. By default, GTK+
   --  picks the best invisible char available in the current font. If you
   --  set the invisible char to 0, then the user will get no feedback
   --  at all; there will be no text on the screen as they type.

   procedure Unset_Invisible_Char (The_Entry : access Gtk_Entry_Record);
   --  Unsets the invisible char previously set with Set_Invisible_Char,
   --  so that the default invisible char is used again.

   procedure Set_Has_Frame
     (The_Entry : access Gtk_Entry_Record; Setting : Boolean := True);
   function Get_Has_Frame
     (The_Entry : access Gtk_Entry_Record) return Boolean;
   --  Set whether the entry has a beveled frame around it.

   procedure Set_Max_Length
     (The_Entry : access Gtk_Entry_Record; Max : Gint);
   function Get_Max_Length (The_Entry : access Gtk_Entry_Record) return Gint;
   --  Set the maximum length for the text.
   --  The current text is truncated if needed.

   procedure Set_Activates_Default
     (The_Entry : access Gtk_Entry_Record; Setting : Boolean);
   function Get_Activates_Default
     (The_Entry : access Gtk_Entry_Record) return Boolean;
   --  If Setting is True, pressing Enter in the Entry will activate the
   --  default widget for the window containing the entry. This usually means
   --  that the dialog box containing the entry will be closed, since the
   --  default widget is usually one of the dialog buttons.
   --
   --  (For experts: if Setting is True, the entry calls
   --  Gtk.Window.Activate_Default on the window containing the entry, in
   --  the default handler for the "activate" signal.)

   procedure Set_Width_Chars
     (The_Entry : access Gtk_Entry_Record'Class; Width : Gint);
   function Get_Width_Chars
     (The_Entry : access Gtk_Entry_Record'Class) return Gint;
   --  Number of characters to leave space for in the entry, on the screen.
   --  This is the number of visible characters, not the maximal number of
   --  characters the entry can contain

   procedure Set_Text
     (The_Entry : access Gtk_Entry_Record; Text : UTF8_String);
   function Get_Text (The_Entry : access Gtk_Entry_Record) return UTF8_String;
   --  Modify the text in the entry.
   --  The text is cut at the maximum length that was set when the entry was
   --  created.
   --  The text replaces the current contents.

   procedure Set_Alignment (Ent  : access Gtk_Entry_Record; Xalign : Gfloat);
   function Get_Alignment   (Ent : access Gtk_Entry_Record) return Gfloat;
   --  Sets the alignment for the contents of the entry. This controls
   --  the horizontal positioning of the contents when the displayed
   --  text is shorter than the width of the entry.

   procedure Set_Completion
     (Ent        : access Gtk_Entry_Record;
      Completion : access Gtk_Entry_Completion_Record'Class);
   function Get_Completion
     (Ent : access Gtk_Entry_Record)
      return Gtk_Entry_Completion;
   --  Sets Completion to be the auxiliary completion object to use with Ent.
   --  All further configuration of the completion mechanism is done on
   --  Completion using the Gtk.Entry_Completion API.

   function Text_Index_To_Layout_Index
     (Ent        : access Gtk_Entry_Record;
      Text_Index : Gint)
      return Gint;
   --  Converts from a position in the entry's layout (returned by Get_Layout)
   --  to a position in the entry contents (returned by Get_Text).
   --  Returns the byte index into the entry layout text

   function Layout_Index_To_Text_Index
     (Ent           : access Gtk_Entry_Record;
      Layout_Index : Gint)
      return Gint;
   --  Converts from a position in the entry contents (returned
   --  by Get_Text) to a position in the
   --  entry's layout (returned by Get_Layout,
   --  with text retrieved via pango.layout.Get_Text).
   --  Return the byte index into the entry contents

   procedure Get_Layout_Offsets
     (The_Entry : access Gtk_Entry_Record;
      X         : out Gint;
      Y         : out Gint);
   --  Obtain the position of the Pango_Layout used to render text
   --  in the entry, in widget coordinates. Useful if you want to line
   --  up the text in an entry with some other text, e.g. when using the
   --  entry to implement editable cells in a sheet widget.
   --
   --  Also useful to convert mouse events into coordinates inside the
   --  Pango_Layout, e.g. to take some action if some part of the entry text
   --  is clicked.
   --
   --  Note that as the user scrolls around in the entry the offsets will
   --  change; you'll need to connect to the "notify::scroll_offset"
   --  signal to track this. Remember when using the Pango_Layout
   --  functions you need to convert to and from pixels using
   --  Pango_Pixels or Pango_Scale.

   function Get_Layout (The_Entry : access Gtk_Entry_Record)
      return Pango.Layout.Pango_Layout;
   --  Return the widget that manages all the layout of text (left-to-right,
   --  right-to-left, fonts,...). Changing the font used for the entry should
   --  be done by changing the font using for this layout. Note that you should
   --  also change the font in the Pango_Context returned by Get_Pango_Context,
   --  or the next keypress event in the entry will restore the default initial
   --  font.
   --
   --  The layout is useful to e.g. convert text positions to pixel positions,
   --  in combination with Get_Layout_Offsets.  The returned layout is owned by
   --  the entry so need not be freed by the caller.

   function Get_Current_Icon_Drag_Source (The_Entry : access Gtk_Entry_Record)
      return Gint;
   --  Returns the index of the icon which is the source of the current
   --  DND operation, or -1.
   --
   --  This function is meant to be used in a #GtkWidget::drag-data-get
   --  callback.

   procedure Set_Icon_Drag_Source
     (The_Entry   : access Gtk_Entry_Record;
      Icon_Pos    : Gtk_Entry_Icon_Position;
      Target_List : Gtk.Selection.Target_List;
      Actions     : Gdk.Dnd.Drag_Action);
   --  Sets up the icon at the given position so that GTK+ will start a drag
   --  operation when the user clicks and drags the icon.
   --
   --  To handle the drag operation, you need to connect to the usual
   --  #GtkWidget::drag-data-get (or possibly #GtkWidget::drag-data-delete)
   --  signal, and use Get_Current_Icon_Drag_Source in your signal handler
   --  to find out if the drag was started from an icon.
   --
   --  By default, GTK+ uses the icon as the drag icon. You can use the
   --  #GtkWidget::drag-begin signal to set a different icon. Note that you
   --  have to use g_signal_connect_after() to ensure that your signal handler
   --  gets executed after the default handler.

   function Get_Cursor_Hadjustment (The_Entry : access Gtk_Entry_Record)
      return Gtk.Adjustment.Gtk_Adjustment;
   procedure Set_Cursor_Hadjustment
     (The_Entry  : access Gtk_Entry_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Hooks up an adjustment to the cursor position in an entry, so that when
   --  the cursor is moved, the adjustment is scrolled to show that position.
   --  See Gtk.Scrolled_Window.Get_Hadjustment for a typical way of obtaining
   --  the adjustment.
   --
   --  The adjustment has to be in pixel units and in the same coordinate
   --  system as the entry.
   --
   --  Get_Cursor_Hadjustment returns the horizontal cursor adjustment, or
   --  null if none has been set.

   function Get_Icon_Activatable
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return Boolean;
   procedure Set_Icon_Activatable
     (The_Entry   : access Gtk_Entry_Record;
      Icon_Pos    : Gtk_Entry_Icon_Position;
      Activatable : Boolean);
   --  Get/Sets whether the icon is activatable.

   function Get_Icon_At_Pos
     (The_Entry : access Gtk_Entry_Record;
      X         : Gint;
      Y         : Gint)
      return Gint;
   --  Finds the icon at the given position and return its index.
   --  If (X, Y) doesn't lie inside an icon, -1 is returned.
   --  This function is intended for use in a GtkWidget "query-tooltip"
   --  signal handler.

   function Get_Icon_Gicon
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return Glib.G_Icon.G_Icon;
   procedure Set_Icon_From_Gicon
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Icon      : Glib.G_Icon.G_Icon);
   --  Sets the icon shown in the entry at the specified position
   --  from the current icon theme.
   --  If the icon isn't known, a "broken image" icon will be displayed
   --  instead.
   --
   --  If Icon is null, no icon will be shown in the specified position.

   function Get_Icon_Name
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return UTF8_String;
   --  Retrieves the icon name used for the icon, or "" if there is
   --  no icon or if the icon was set by some other method (e.g., by
   --  pixbuf, stock or gicon).

   procedure Set_Icon_From_Icon_Name
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Icon_Name : UTF8_String);
   --  Sets the icon shown in the entry at the specified position
   --  from the current icon theme.
   --
   --  If the icon name isn't known, a "broken image" icon will be displayed
   --  instead.
   --
   --  If Icon_Name is "", no icon will be shown in the specified position.

   function Get_Icon_Pixbuf
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Retrieves the image used for the icon.
   --
   --  Unlike the other methods of setting and getting icon data, this
   --  method will work regardless of whether the icon was set using a
   --  Gdk_Pixbuf, a G_Icon, a stock item, or an icon name.
   --
   --  Returns: A Gdk_Pixbuf, or null if no icon is set for this position.

   procedure Set_Icon_From_Pixbuf
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Pixbuf    : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Sets the icon shown in the specified position using a pixbuf.
   --  If Pixbuf is null, no icon will be shown in the specified position.

   function Get_Icon_Stock
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return UTF8_String;
   --  Retrieves the stock id used for the icon, or "" if there is
   --  no icon or if the icon was set by some other method (e.g., by
   --  pixbuf, icon name or gicon).
   --
   --  Returns a stock id, or "" if no icon is set or if the icon
   --  wasn't set from a stock id

   procedure Set_Icon_From_Stock
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Stock_Id  : UTF8_String);
   --  Sets the icon shown in the entry at the specified position from
   --  a stock image.
   --
   --  If Stock_Id is "", no icon will be shown in the specified position.

   function Get_Icon_Sensitive
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return Boolean;
   procedure Set_Icon_Sensitive
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Sensitive : Boolean);
   --  Gets/Sets the sensitivity for the specified icon.

   function Get_Icon_Storage_Type
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return Gtk.Image.Gtk_Image_Type;
   --  Gets the type of representation being used by the icon
   --  to store image data. If the icon has no image data,
   --  the return value will be Gtk.Image.Image_Empty.

   function Get_Icon_Tooltip_Markup
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return UTF8_String;
   procedure Set_Icon_Tooltip_Markup
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Tooltip   : UTF8_String);
   --  Gets/Sets Tooltip as the contents of the tooltip for the icon at
   --  the specified position. Tooltip is assumed to be marked up with
   --  the Pango text markup language.
   --
   --  Use "" for Tooltip to remove an existing tooltip.
   --
   --  See also Gtk.Widget.Set_Tooltip_Markup and Set_Icon_Tooltip_Text

   function Get_Icon_Tooltip_Text
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return UTF8_String;
   procedure Set_Icon_Tooltip_Text
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Tooltip   : UTF8_String);
   --  Gets/Sets Tooltip as the contents of the tooltip for the icon
   --  at the specified position.
   --
   --  Use "" for Tooltip to remove an existing tooltip.
   --
   --  See also Gtk.Widget.Set_Tooltip_Text and Set_Icon_Tooltip_Markup

   function Get_Inner_Border (The_Entry : access Gtk_Entry_Record)
      return Gtk.Style.Gtk_Border;
   procedure Set_Inner_Border
     (The_Entry : access Gtk_Entry_Record;
      Border    : Gtk.Style.Gtk_Border);
   --  Gets/Sets The_Entry's inner-border property.  null signifies that
   --  the property is (should be) cleared.  The inner-border is the area
   --  around the entry's text, but inside its frame.
   --
   --  If set, this property overrides the inner-border style property.
   --  Overriding the style-provided border is useful when you want to do
   --  in-place editing of some text in a canvas or list widget, where
   --  pixel-exact positioning of the entry is important.

   function Get_Overwrite_Mode (The_Entry : access Gtk_Entry_Record)
      return Boolean;
   procedure Set_Overwrite_Mode
     (The_Entry : access Gtk_Entry_Record;
      Overwrite : Boolean);
   --  Gets/Sets whether text is overwritten when typing in the Gtk_Entry.

   function Get_Progress_Fraction (The_Entry : access Gtk_Entry_Record)
      return Gdouble;
   procedure Set_Progress_Fraction
     (The_Entry : access Gtk_Entry_Record;
      Fraction  : Gdouble);
   --  Causes the entry's progress indicator to "fill in" the given
   --  fraction of the bar. The fraction should be between 0.0 and 1.0,
   --  inclusive.

   function Get_Progress_Pulse_Step (The_Entry : access Gtk_Entry_Record)
      return Gdouble;
   procedure Set_Progress_Pulse_Step
     (The_Entry : access Gtk_Entry_Record;
      Fraction  : Gdouble);
   --  Gets/Sets the fraction of total entry width to move the progress
   --  bouncing block for each call to Progress_Pulse.

   procedure Progress_Pulse (The_Entry : access Gtk_Entry_Record);
   --  Indicates that some progress is made, but you don't know how much.
   --  Causes the entry's progress indicator to enter "activity mode,"
   --  where a block bounces back and forth. Each call to
   --  Progress_Pulse causes the block to move by a little bit
   --  (the amount of movement per pulse is determined by
   --  Set_Progress_Pulse_Step).

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   procedure Gtk_New (Widget : out Gtk_Entry; Max : Gint);
   pragma Obsolescent;  --  New_With_Max_Length
   --  Create a new entry with a maximum length for the text.
   --  The text can never be longer than Max characters.

   procedure Initialize
     (Widget : access Gtk_Entry_Record'Class; Max : Gint);
   pragma Obsolescent;
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Append_Text
     (The_Entry : access Gtk_Entry_Record; Text : UTF8_String);
   pragma Obsolescent ("See Gtk.Editable.Insert_Text");  --  Append_Text
   --  Append a new string at the end of the existing one.

   procedure Prepend_Text
     (The_Entry : access Gtk_Entry_Record; Text : UTF8_String);
   pragma Obsolescent ("See Gtk.Editable.Insert_Text");  --  Prepend_Text
   --  Insert some text at the beginning of the entry.

   procedure Set_Editable
     (The_Entry : access Gtk_Entry_Record; Editable : Boolean);
   pragma Obsolescent;  --  Set_Editable

   function Get_Chars (The_Entry : access Gtk_Entry_Record) return UTF8_String
                       renames Get_Text;
   --  pragma Obsolescent;
   --  Convenience function provided for compatibility with GtkAda 1.2

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Activates_Default_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether to activate the default widget (such as the default
   --         button in a dialog) when Enter is pressed.)
   --
   --  Name:  Caps_Lock_Warning_Property
   --  Type:  Boolean
   --  Descr: Whether password entries will show a warning when Caps Lock is on
   --
   --  Name:  Cursor_Position_Property
   --  Type:  Int
   --  Descr: The current position of the insertion cursor in chars
   --
   --  Name:  Editable_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the entry contents can be edited
   --  See also:  Set_Editable
   --
   --  Name:  Has_Frame_Property
   --  Type:  Boolean
   --  Descr: FALSE removes outside bevel from entry
   --
   --  Name:  Im_Module_Property
   --  Type:  String
   --  Descr: Which IM module should be used
   --
   --  Name:  Inner_Border_Property
   --  Type:  Boxed
   --  Descr: Border between text and frame. Overrides the inner-border
   --         style property
   --
   --  Name:  Invisible_Char_Property
   --  Type:  Gunichar
   --  Flags: read-write
   --  Descr: The character to use when masking entry contents
   --         (in "password mode")
   --
   --  Name:  Invisible_Char_Set_Property
   --  Type:  Boolean
   --  Descr: Whether the invisible char has been set
   --
   --  Name:  Max_Length_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: Maximum number of characters for this entry
   --  See also:  Set_Max_Length
   --
   --  Name:  Overwrite_Mode_Property
   --  Type:  Boolean
   --  Descr: Whether new text overwrites existing text
   --
   --  Name:  Primary_Icon_Activatable_Property
   --  Type:  Boolean
   --  Descr: Whether the primary icon is activatable
   --
   --  Name:  Primary_Icon_Gicon_Property
   --  Type:  Object
   --  Descr: GIcon for primary icon
   --
   --  Name:  Primary_Icon_Name_Property
   --  Type:  String
   --  Descr: Icon name for primary icon
   --
   --  Name:  Primary_Icon_Pixbuf_Property
   --  Type:  Object
   --  Descr: Primary pixbuf for the entry
   --
   --  Name:  Primary_Icon_Sensitive_Property
   --  Type:  Boolean
   --  Descr: Whether the primary icon is sensitive
   --
   --  Name:  Primary_Icon_Stock_Property
   --  Type:  String
   --  Descr: Stock ID for primary icon
   --
   --  Name:  Primary_Icon_Storage_Type_Property
   --  Type:  Enum
   --  Descr: The representation being used for primary icon
   --
   --  Name:  Primary_Icon_Tooltip_Markup_Property
   --  Type:  String
   --  Descr: The contents of the tooltip on the primary icon
   --
   --  Name:  Primary_Icon_Tooltip_Text_Property
   --  Type:  String
   --  Descr: The contents of the tooltip on the primary icon
   --
   --  Name:  Progress_Fraction_Property
   --  Type:  Double
   --  Descr: The current fraction of the task that's been completed
   --
   --  Name:  Progress_Pulse_Step_Property
   --  Type:  Double
   --  Descr: The fraction of total entry width to move the progress
   --         bouncing block for each call to Progress_Pulse
   --
   --  Name:  Scroll_Offset_Property
   --  Type:  Gint
   --  Flags: read only
   --  Descr: Number of pixels of the entry scrolled off the screen to the
   --         left
   --
   --  Name:  Secondary_Icon_Activatable_Property
   --  Type:  Boolean
   --  Descr: Whether the secondary icon is activatable
   --
   --  Name:  Secondary_Icon_Gicon_Property
   --  Type:  Object
   --  Descr: GIcon for secondary icon
   --
   --  Name:  Secondary_Icon_Name_Property
   --  Type:  String
   --  Descr: Icon name for secondary icon
   --
   --  Name:  Secondary_Icon_Pixbuf_Property
   --  Type:  Object
   --  Descr: Secondary pixbuf for the entry
   --
   --  Name:  Secondary_Icon_Sensitive_Property
   --  Type:  Boolean
   --  Descr: Whether the secondary icon is sensitive
   --
   --  Name:  Secondary_Icon_Stock_Property
   --  Type:  String
   --  Descr: Stock ID for secondary icon
   --
   --  Name:  Secondary_Icon_Storage_Type_Property
   --  Type:  Enum
   --  Descr: The representation being used for secondary icon
   --
   --  Name:  Secondary_Icon_Tooltip_Markup_Property
   --  Type:  String
   --  Descr: The contents of the tooltip on the secondary icon
   --
   --  Name:  Secondary_Icon_Tooltip_Text_Property
   --  Type:  String
   --  Descr: The contents of the tooltip on the secondary icon
   --
   --  Name:  Selection_Bound_Property
   --  Type:  Int
   --  Descr: The position of the opposite end of the selection from the cursor
   --         in chars
   --
   --  Name:  Shadow_Type_Property
   --  Type:  Enum
   --  Descr: Which kind of shadow to draw around the entry when has-frame
   --         is set
   --
   --  Name:  Text_Length_Property
   --  Type:  Uint
   --  Descr: Length of the text currently in the entry
   --
   --  Name:  Text_Position_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: The current position of the insertion point
   --
   --  Name:  Text_Property
   --  Type:  String
   --  Descr: The contents of the entry
   --
   --  Name:  Truncate_Multiline_Property
   --  Type:  Boolean
   --  Descr: Whether to truncate multiline pastes to one line.
   --
   --  Name:  Visibility_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: FALSE displays the "invisible char" instead of the actual
   --         text (password mode)
   --  See also:  Set_Visibility
   --
   --  Name:  Width_Chars_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: Number of characters to leave space for in the entry.
   --  See also: Set_Width_Chars
   --
   --  Name:  Xalign_Property
   --  Type:  Float
   --  Descr: The horizontal alignment, from 0 (left) to 1 (right). Reversed
   --         for RTL layouts.
   --
   --  </properties>

   Activates_Default_Property : constant Glib.Properties.Property_Boolean;
   Caps_Lock_Warning_Property : constant Glib.Properties.Property_Boolean;
   Cursor_Position_Property   : constant Glib.Properties.Property_Int;
   Editable_Property          : constant Glib.Properties.Property_Boolean;
   Has_Frame_Property         : constant Glib.Properties.Property_Boolean;
   Im_Module_Property         : constant Glib.Properties.Property_String;
   Inner_Border_Property      : constant Glib.Properties.Property_Boxed;
   Invisible_Char_Property    : constant Glib.Properties.Property_Unichar;
   Invisible_Char_Set_Property :
                                constant Glib.Properties.Property_Boolean;
   Max_Length_Property        : constant Glib.Properties.Property_Int;
   Overwrite_Mode_Property    : constant Glib.Properties.Property_Boolean;
   Primary_Icon_Activatable_Property :
                                constant Glib.Properties.Property_Boolean;
   Primary_Icon_Gicon_Property :
                                constant Glib.Properties.Property_Object;
   Primary_Icon_Name_Property : constant Glib.Properties.Property_String;
   Primary_Icon_Pixbuf_Property :
                                constant Glib.Properties.Property_Object;
   Primary_Icon_Sensitive_Property :
                                constant Glib.Properties.Property_Boolean;
   Primary_Icon_Stock_Property :
                                constant Glib.Properties.Property_String;
   Primary_Icon_Storage_Type_Property :
                                constant Glib.Properties.Property_Enum;
   Primary_Icon_Tooltip_Markup_Property :
                                constant Glib.Properties.Property_String;
   Primary_Icon_Tooltip_Text_Property :
                                constant Glib.Properties.Property_String;
   Progress_Fraction_Property : constant Glib.Properties.Property_Double;
   Progress_Pulse_Step_Property :
                                constant Glib.Properties.Property_Double;
   Scroll_Offset_Property     : constant Glib.Properties.Property_Int;
   Secondary_Icon_Activatable_Property :
                                constant Glib.Properties.Property_Boolean;
   Secondary_Icon_Gicon_Property :
                                constant Glib.Properties.Property_Object;
   Secondary_Icon_Name_Property :
                                constant Glib.Properties.Property_String;
   Secondary_Icon_Pixbuf_Property :
                                constant Glib.Properties.Property_Object;
   Secondary_Icon_Sensitive_Property :
                                constant Glib.Properties.Property_Boolean;
   Secondary_Icon_Stock_Property :
                                constant Glib.Properties.Property_String;
   Secondary_Icon_Storage_Type_Property :
                                constant Glib.Properties.Property_Enum;
   Secondary_Icon_Tooltip_Markup_Property :
                                constant Glib.Properties.Property_String;
   Secondary_Icon_Tooltip_Text_Property :
                                constant Glib.Properties.Property_String;
   Selection_Bound_Property   : constant Glib.Properties.Property_Int;
   Shadow_Type_Property       : constant Glib.Properties.Property_Enum;
   Text_Length_Property       : constant Glib.Properties.Property_Uint;
   Text_Position_Property     : constant Glib.Properties.Property_Int;
   Text_Property              : constant Glib.Properties.Property_String;
   Truncate_Multiline_Property :
                                constant Glib.Properties.Property_Boolean;
   Visibility_Property        : constant Glib.Properties.Property_Boolean;
   Width_Chars_Property       : constant Glib.Properties.Property_Int;
   Xalign_Property            : constant Glib.Properties.Property_Float;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Icon_Prelight_Property
   --  Type:  Boolean
   --  Descr: Whether activatable icons should prelight when hovered
   --
   --  Name:  Progress_Border_Property
   --  Type:  Boxed
   --  Descr: Border around the progress bar
   --
   --  Name:  State_Hint_Property
   --  Type:  Boolean
   --  Descr: Whether to pass a proper state when drawing shadow or background
   --
   --  </style_properties>

   Icon_Prelight_Property   : constant Glib.Properties.Property_Boolean;
   Progress_Border_Property : constant Glib.Properties.Property_Boxed;
   State_Hint_Property      : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "activate"
   --    procedure Handler (Ent : access Gtk_Entry_Record'Class);
   --    Called when the entry is activated, for instance when the user
   --    presses <enter> while in it
   --
   --  - "populate_popup"
   --    procedure Handler
   --       (Ent  : access Gtk_Entry_Record'Class;
   --        Menu : access Gtk_Menu_Record'Class);
   --    ???
   --
   --  - "move_cursor"
   --    procedure Handler
   --       (Ent              : access Gtk_Entry_Record'Class;
   --        Step             : Gtk_Movement_Step;
   --        Amount           : Gint;
   --        Extend_Selection : Boolean);
   --    You should emit this signal to request that the cursor be moved. This
   --    is mostly used when connected to a keybinding, as is done by default
   --    for the arrow keys for instance.
   --
   --  - "insert_at_cursor"
   --    procedure Handler
   --       (Ent  : access Gtk_Entry_Record'Class;
   --        Text : String);
   --    You should emit this signal to request that some text be inserted at
   --    the current cursor location. This is mostly used from key bindings.
   --
   --  - "delete_from_cursor"
   --    procedure Handler
   --       (Ent              : access Gtk_Entry_Record'Class;
   --        Step             : Gtk_Movement_Step;
   --        Amount           : Gint);
   --    You should emit this signal to request that some text be delete from
   --    the cursor position.
   --
   --  - "cut_clipboard"
   --    procedure Handler (Ent : access Gtk_Entry_Record'Class);
   --    You should emit this signal to request that the current selection be
   --    deleted and copied into the clipboard. This is mostly used from key
   --    bindings.
   --
   --  - "copy_clipboard"
   --    procedure Handler (Ent : access Gtk_Entry_Record'Class);
   --    You should emit this signal to request that the current selection be
   --    copied into the clipboard. This is mostly used from key
   --    bindings.
   --
   --  - "paste_clipboard"
   --    procedure Handler (Ent : access Gtk_Entry_Record'Class);
   --    You should emit this signal to request that the clipboard be inserted
   --    at the current cursor location. This is mostly used from key bindings.
   --
   --  - "toggle_overwrite"
   --    procedure Handler (Ent : access Gtk_Entry_Record'Class);
   --    You should emit this signal to request that the insertion mode be
   --    changed. This is mostly used from a key binding, as is done by default
   --    for the Insert key.
   --  </signals>

   Signal_Activate           : constant Glib.Signal_Name :=
                                 "activate";
   Signal_Backspace          : constant Glib.Signal_Name :=
                                 "backspace";
   Signal_Copy_Clipboard     : constant Glib.Signal_Name :=
                                 "copy_clipboard";
   Signal_Cut_Clipboard      : constant Glib.Signal_Name :=
                                 "cut_clipboard";
   Signal_Delete_From_Cursor : constant Glib.Signal_Name :=
                                 "delete_from_cursor";
   Signal_Insert_At_Cursor   : constant Glib.Signal_Name :=
                                 "insert_at_cursor";
   Signal_Move_Cursor        : constant Glib.Signal_Name :=
                                 "move_cursor";
   Signal_Paste_Clipboard    : constant Glib.Signal_Name :=
                                 "paste_clipboard";
   Signal_Populate_Popup     : constant Glib.Signal_Name :=
                                 "populate_popup";
   Signal_Toggle_Overwrite   : constant Glib.Signal_Name :=
                                 "toggle_overwrite";

private
   type Gtk_Entry_Record is new
     Gtk.Editable.Gtk_Editable_Record with null record;

   --  properties
   Activates_Default_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activates_default");
   Caps_Lock_Warning_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("caps-lock-warning");
   Cursor_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("cursor-position");
   Editable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Has_Frame_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-frame");
   Im_Module_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("im-module");
   Inner_Border_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("inner-border");
   Invisible_Char_Property : constant Glib.Properties.Property_Unichar :=
     Glib.Properties.Build ("invisible_char");
   Invisible_Char_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible-char-set");
   Max_Length_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max_length");
   Overwrite_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("overwrite-mode");
   Primary_Icon_Activatable_Property :
     constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("primary-icon-activatable");
   Primary_Icon_Gicon_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("primary-icon-gicon");
   Primary_Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("primary-icon-name");
   Primary_Icon_Pixbuf_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("primary-icon-pixbuf");
   Primary_Icon_Sensitive_Property :
     constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("primary-icon-sensitive");
   Primary_Icon_Stock_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("primary-icon-stock");
   Primary_Icon_Storage_Type_Property :
     constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("primary-icon-storage-type");
   Primary_Icon_Tooltip_Markup_Property :
     constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("primary-icon-tooltip-markup");
   Primary_Icon_Tooltip_Text_Property :
     constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("primary-icon-tooltip-text");
   Progress_Fraction_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("progress-fraction");
   Progress_Pulse_Step_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("progress-pulse-step");
   Scroll_Offset_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scroll_offset");
   Secondary_Icon_Activatable_Property :
     constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("secondary-icon-activatable");
   Secondary_Icon_Gicon_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("secondary-icon-gicon");
   Secondary_Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-icon-name");
   Secondary_Icon_Pixbuf_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("secondary-icon-pixbuf");
   Secondary_Icon_Sensitive_Property :
     constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("secondary-icon-sensitive");
   Secondary_Icon_Stock_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-icon-stock");
   Secondary_Icon_Storage_Type_Property :
     constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("secondary-icon-storage-type");
   Secondary_Icon_Tooltip_Markup_Property :
     constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-icon-tooltip-markup");
   Secondary_Icon_Tooltip_Text_Property :
     constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-icon-tooltip-text");
   Selection_Bound_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("selection-bound");
   Shadow_Type_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("shadow-type");
   Text_Length_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("text-length");
   Text_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("text_position");
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Truncate_Multiline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("truncate-multiline");
   Visibility_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visibility");
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width_chars");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");

   --  style properties
   Icon_Prelight_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("icon-prelight");
   Progress_Border_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("progress-border");
   State_Hint_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("state-hint");

   pragma Import (C, Get_Type, "gtk_entry_get_type");
end Gtk.GEntry;

--  This subprogram was never bound, and is now obsolescent:
--  No binding: gtk_entry_select_region
--  No binding: gtk_entry_set_position
