-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2007 AdaCore                    --
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
--  This widget displays a view of a Gtk_Text_Buffer. Multiple views can be
--  set on a given buffer.
--  </description>
--  <c_version>2.8.17</c_version>
--  <screenshot>gtk-text_view.png</screenshot>
--  <see>Gtk.Text_Buffer</see>
--  <see>Gtk.Text_Tag</see>
--  <see>Gtk.Text_Attributes</see>
--  <testgtk>create_text_view.adb</testgtk>
--  <group>Multiline Text Editor</group>

with Glib.Properties;
with Gdk.Rectangle;
with Gdk.Window;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Text_Attributes;
with Gtk.Text_Buffer;
with Gtk.Text_Child;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Widget;
with Pango.Tabs;

package Gtk.Text_View is

   type Gtk_Text_View_Record is
     new Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Text_View is access all Gtk_Text_View_Record'Class;

   procedure Gtk_New
     (Widget : out Gtk_Text_View;
      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer := null);
   procedure Initialize
     (Widget : access Gtk_Text_View_Record'Class;
      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer);
   --  Creates or initializes a new Gtk_Text_View.
   --  If Buffer is null, an empty default buffer will be created for you. Get
   --  the buffer with Get_Buffer.
   --  Otherwise, create a new text view widget displaying Buffer.
   --  One buffer can be shared among many widgets.
   --  The text view adds its own reference count to the buffer; it does not
   --  take over an existing reference.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with this widget.

   procedure Set_Buffer
     (Text_View : access Gtk_Text_View_Record;
      Buffer    : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class);
   --  Set Buffer as the buffer being displayed by Text_View.
   --  The previous buffer displayed by the text view is unreferenced, and a
   --  reference is added to Buffer. If you owned a reference to Buffer before
   --  passing it to this function, you must remove that reference yourself;
   --  Gtk_Text_View will not "adopt" it.

   function Get_Buffer
     (Text_View : access Gtk_Text_View_Record)
      return Gtk.Text_Buffer.Gtk_Text_Buffer;
   --  Return the Gtk_Text_Buffer being displayed by this text view.
   --  The reference count on the buffer is not incremented; the caller of this
   --  function won't own a new reference.

   function Scroll_To_Iter
     (Text_View     : access Gtk_Text_View_Record;
      Iter          : Gtk.Text_Iter.Gtk_Text_Iter;
      Within_Margin : Gdouble;
      Use_Align     : Boolean;
      Xalign        : Gdouble;
      Yalign        : Gdouble) return Boolean;
   --  Scroll Text_View so that Iter is on the screen in the position
   --  indicated by Xalign and Yalign. An alignment of 0.0 indicates left or
   --  top, 1.0 indicates right or bottom, 0.5 means center. If Use_Align is
   --  False, the text scrolls the minimal distance to get the mark onscreen,
   --  possibly not scrolling at all. The effective screen for purposes of this
   --  function is reduced by a margin of size Within_Margin.
   --  Note: This function uses the currently-computed height of the lines in
   --  the text buffer. Note that line heights are computed in an idle handler;
   --  so this function may not have the desired effect if it's called before
   --  the height computations. To avoid oddness, consider using
   --  Scroll_To_Mark which saves a point to be scrolled to after line
   --  validation.

   procedure Scroll_To_Mark
     (Text_View     : access Gtk_Text_View_Record;
      Mark          : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
      Within_Margin : Gdouble := 0.0;
      Use_Align     : Boolean := False;
      Xalign        : Gdouble := 0.0;
      Yalign        : Gdouble := 0.0);
   --  Scroll Text_View so that Mark is on the screen in the position indicated
   --  by Xalign and Yalign. An alignment of 0.0 indicates left or top, 1.0
   --  indicates right or bottom, 0.5 means center. If Use_Align is False, the
   --  text scrolls the minimal distance to get the mark onscreen, possibly not
   --  scrolling at all. The effective screen for purposes of this function is
   --  reduced by a margin of size Within_Margin.

   procedure Scroll_Mark_Onscreen
     (Text_View : access Gtk_Text_View_Record;
      Mark      : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --  Same as the above with the default values

   function Move_Mark_Onscreen
     (Text_View : access Gtk_Text_View_Record;
      Mark      : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
      return Boolean;
   --  Move a mark within the buffer so that it's located within the
   --  currently-visible text area.
   --  Return value: True if the mark moved (wasn't already onscreen).

   function Place_Cursor_Onscreen
     (Text_View : access Gtk_Text_View_Record) return Boolean;
   --  Move the cursor to the currently visible region of the buffer, if it
   --  isn't there already.
   --  Return value: True if the cursor had to be moved.

   procedure Get_Visible_Rect
     (Text_View    : access Gtk_Text_View_Record;
      Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fill Visible_Rect with the currently-visible region of the buffer, in
   --  buffer coordinates. Convert to window coordinates with
   --  Buffer_To_Window_Coords.

   procedure Get_Iter_Location
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      Location  : out Gdk.Rectangle.Gdk_Rectangle);
   --  Get a rectangle which roughly contains the character at iter.
   --  The rectangle position is in buffer coordinates; use
   --  Buffer_To_Window_Coords to convert these coordinates to coordinates for
   --  one of the windows in the text view.

   procedure Get_Iter_At_Location
     (Text_View : access Gtk_Text_View_Record;
      Iter      : out Gtk.Text_Iter.Gtk_Text_Iter;
      X         : Gint;
      Y         : Gint);
   --  Retrieve the iterator at buffer coordinates X and Y. Buffer coordinates
   --  are coordinates for the entire buffer, not just the currently-displayed
   --  portion. If you have coordinates from an event, you have to convert
   --  those to buffer coordinates with Window_To_Buffer_Coords.

   procedure Get_Iter_At_Position
     (Text_View : access Gtk_Text_View_Record;
      Iter      : out Gtk.Text_Iter.Gtk_Text_Iter;
      Trailing  : out Gint;
      X         : Gint;
      Y         : Gint);
   --  Retrieves the iterator pointing to the character at buffer coordinates X
   --  and Y. Buffer coordinates are coordinates for the entire buffer, not
   --  just the currently-displayed portion. If you have coordinates from an
   --  event, you have to convert those to buffer coordinates with
   --  Window_To_Buffer_Coords.
   --  Note that this is different from Get_Iter_At_Location(),
   --  which returns cursor locations, i.e. positions between characters)
   --  Trailing is set to indicate where in the grapheme the user clicked. It
   --  will be either 0, or the number of characters in the grapheme. 0
   --  represents the trailing edge of the grapheme.

   procedure Get_Line_Yrange
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      Y         : out Gint;
      Height    : out Gint);
   --  Get the Y coordinate of the top of the line containing Iter,
   --  and the Height of the line. The coordinate is a buffer coordinate;
   --  convert to window coordinates with Buffer_To_Window_Coords.

   procedure Get_Line_At_Y
     (Text_View   : access Gtk_Text_View_Record;
      Target_Iter : out Gtk.Text_Iter.Gtk_Text_Iter;
      Y           : Gint;
      Line_Top    : out Gint);
   --  Get the Gtk_Text_Iter at the start of the line containing the
   --  coordinate Y. Y is in buffer coordinates, convert from window
   --  coordinates with Window_To_Buffer_Coords.
   --  Line_Top will be filled with the coordinate of the top edge of the line.

   procedure Buffer_To_Window_Coords
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Window_X  : out Gint;
      Window_Y  : out Gint);
   --  Convert coordinate (Buffer_X, Buffer_Y) to coordinates for the window
   --  Win, and store the result in (Window_X, Window_Y).

   procedure Window_To_Buffer_Coords
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type;
      Window_X  : Gint;
      Window_Y  : Gint;
      Buffer_X  : out Gint;
      Buffer_Y  : out Gint);
   --  Convert coordinates on the window identified by Win to buffer
   --  coordinates, storing the result in (Buffer_X, Buffer_Y).

   function Get_Window
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type) return Gdk.Window.Gdk_Window;
   --  Retrieve the Gdk_Window corresponding to an area of the text view;
   --  possible windows include the overall widget window, child windows on the
   --  left, right, top, bottom, and the window that displays the text buffer.
   --  Windows are null and nonexistent if their width or height is 0, and are
   --  nonexistent before the widget has been realized.

   function Get_Window_Type
     (Text_View : access Gtk_Text_View_Record;
      Window    : Gdk.Window.Gdk_Window) return Gtk.Enums.Gtk_Text_Window_Type;
   --  Usually used to find out which window an event corresponds to.
   --  If you connect to an event signal on Text_View, this function should be
   --  called on Get_Window (Event) to see which window it was.

   procedure Set_Border_Window_Size
     (Text_View : access Gtk_Text_View_Record;
      The_Type  : Gtk.Enums.Gtk_Text_Window_Type;
      Size      : Gint);
   function Get_Border_Window_Size
     (Text_View : access Gtk_Text_View_Record;
      The_Type  : Gtk.Enums.Gtk_Text_Window_Type) return Gint;
   --  Set the width of Text_Window_Left or Text_Window_Right,
   --  or the height of Text_Window_Top or Text_Window_Bottom.
   --  Automatically destroy the corresponding window if the size is set to 0,
   --  and create the window if the size is set to non-zero.

   --  <doc_ignore>
   procedure Set_Disable_Scroll_On_Focus
     (Text_View : access Gtk_Text_View_Record;
      Set       : Boolean);
   function Get_Disable_Scroll_On_Focus
     (Text_View : access Gtk_Text_View_Record) return Boolean;
   --  Set whether the Text_View should scroll to the cursor when it gets the
   --  focus. (This is the default behaviour)
   --  This procedure has no effect for gtk+ 2.2.2 or later.
   --  </doc_ignore>

   ---------------
   -- Iterators --
   ---------------
   --  You can manipulate iterators either through the buffer directly (thus
   --  independently of any display properties), or through the property (if
   --  you need to reference to what the user is actually seeing on the screen)

   procedure Forward_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);
   procedure Forward_Display_Line_End
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);
   --  Moves the given Iter forward by one display (wrapped) line.  A
   --  display line is different from a paragraph. Paragraphs are
   --  separated by newlines or other paragraph separator characters.
   --  Display lines are created by line-wrapping a paragraph.  If
   --  wrapping is turned off, display lines and paragraphs will be the
   --  same. Display lines are divided differently for each view, since
   --  they depend on the view's width; paragraphs are the same in all
   --  views, since they depend on the contents of the Gtk_Text_Buffer.
   --  Returns True if Iter was moved and is not on the end iterator.

   procedure Backward_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);
   procedure Backward_Display_Line_Start
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);
   --  Moves the given Iter backward by one display (wrapped) line. A display
   --  line is different from a paragraph. Paragraphs are separated by newlines
   --  or other paragraph separator characters. Display lines are created by
   --  line-wrapping a paragraph. If wrapping is turned off, display lines and
   --  paragraphs will be the same. Display lines are divided differently for
   --  each view, since they depend on the view's width; paragraphs are the
   --  same in all views, since they depend on the contents of the
   --  Gtk_Text_Buffer.
   --  Returns True if Iter was moved and is not on the end iterator

   function Starts_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Determines whether Iter is at the start of a display line. See
   --  Forward_Display_Line for an explanation of display lines vs. paragraphs.
   --  Returns true if Iter begins a wrapped line.

   procedure Move_Visually
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count     : Gint;
      Result    : out Boolean);
   --  Move the iterator a given number of characters visually, treating it as
   --  the strong cursor position. If Count is positive, then the new strong
   --  cursor position will be Count positions to the right of the old cursor
   --  position. If Count is negative then the new strong cursor position will
   --  be Count positions to the left of the old cursor position.
   --
   --  In the presence of bidirection text, the correspondence between logical
   --  and visual order will depend on the direction of the current run, and
   --  there may be jumps when the cursor is moved off of the end of a run.
   --
   --  Returns True if Iter moved and is not on the end iterator

   ----------------------
   -- Children widgets --
   ----------------------
   --  Any widget can be put in a text_view, for instance to provide an
   --  interactive area.

   procedure Add_Child_In_Window
     (Text_View    : access Gtk_Text_View_Record;
      Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Which_Window : Gtk.Enums.Gtk_Text_Window_Type;
      Xpos         : Gint;
      Ypos         : Gint);
   --  Adds a child at fixed coordinates in one of the text widget's windows.
   --  The window must have nonzero size (see Set_Border_Window_Size). Note
   --  that the child coordinates are given relative to the Gdk_Window in
   --  question, and that these coordinates have no sane relationship to
   --  scrolling. When placing a child in GTK_TEXT_WINDOW_WIDGET, scrolling is
   --  irrelevant, the child floats above all scrollable areas. But when
   --  placing a child in one of the scrollable windows (border windows or text
   --  window), you'll need to compute the child's correct position in buffer
   --  coordinates any time scrolling occurs or buffer changes occur, and then
   --  call Move_Child() to update the child's position. Unfortunately there's
   --  no good way to detect that scrolling has occurred, using the current
   --  API; a possible hack would be to update all child positions when the
   --  scroll adjustments change or the text buffer changes.

   procedure Add_Child_At_Anchor
     (Text_View : access Gtk_Text_View_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Anchor    : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class);
   --  Adds a child widget in the text buffer, at the given Anchor.

   procedure Move_Child
     (Text_View : access Gtk_Text_View_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Xpos      : Gint;
      Ypos      : Gint);
   --  Updates the position of a child, as for Add_Child_In_Window.
   --  Child must already have been added to the text_view.

   ----------------
   -- Attributes --
   ----------------

   function Get_Default_Attributes
     (Text_View : access Gtk_Text_View_Record)
      return Gtk.Text_Attributes.Gtk_Text_Attributes;
   --  Obtains a copy of the default text attributes. These are the attributes
   --  used for text unless a tag overrides them. You'd typically pass the
   --  default attributes in to Gtk.Text_Iter.Get_Attributes in order to get
   --  the attributes in effect at a given text position.
   --  The returned value is a copy and should be freed by the caller.

   procedure Set_Cursor_Visible
     (Text_View : access Gtk_Text_View_Record;
      Setting   : Boolean := True);
   function Get_Cursor_Visible
     (Text_View : access Gtk_Text_View_Record) return Boolean;
   --  Toggle whether the insertion point is displayed.
   --  A buffer with no editable text probably shouldn't have a visible cursor,
   --  so you may want to turn the cursor off.

   procedure Set_Wrap_Mode
     (Text_View : access Gtk_Text_View_Record;
      Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode);
   function Get_Wrap_Mode
     (Text_View : access Gtk_Text_View_Record) return Gtk.Enums.Gtk_Wrap_Mode;
   --  Set the line wrapping for the view.

   procedure Set_Editable
     (Text_View : access Gtk_Text_View_Record;
      Setting   : Boolean := True);
   function Get_Editable
     (Text_View : access Gtk_Text_View_Record) return Boolean;
   --  Set the default editability of the Gtk_Text_View.
   --  You can override this default setting with tags in the buffer, using the
   --  "editable" attribute of tags.

   procedure Set_Pixels_Above_Lines
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Above_Lines : Gint);
   function Get_Pixels_Above_Lines
     (Text_View : access Gtk_Text_View_Record) return Gint;
   --  Sets the default number of blank pixels above paragraphs in Text_View.
   --  Tags in the buffer for Text_View may override the defaults.

   procedure Set_Pixels_Below_Lines
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Below_Lines : Gint);
   function Get_Pixels_Below_Lines
     (Text_View : access Gtk_Text_View_Record) return Gint;
   --  Sets the default number of pixels of blank space
   --  to put below paragraphs in Text_View. May be overridden
   --  by tags applied to Text_View's buffer.

   procedure Set_Pixels_Inside_Wrap
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Inside_Wrap : Gint);
   function Get_Pixels_Inside_Wrap
     (Text_View : access Gtk_Text_View_Record) return Gint;
   --  Sets the default number of pixels of blank space to leave between
   --  display/wrapped lines within a paragraph. May be overridden by
   --  tags in Text_View's buffer.

   procedure Set_Justification
     (Text_View     : access Gtk_Text_View_Record;
      Justification : Gtk.Enums.Gtk_Justification);
   function Get_Justification
     (Text_View : access Gtk_Text_View_Record)
      return Gtk.Enums.Gtk_Justification;
   --  Sets the default justification of text in Text_View.
   --  Tags in the view's buffer may override the default.

   procedure Set_Left_Margin
     (Text_View   : access Gtk_Text_View_Record;
      Left_Margin : Gint);
   function Get_Left_Margin
     (Text_View : access Gtk_Text_View_Record) return Gint;
   --  Sets the default left margin for text in Text_View.
   --  Tags in the buffer may override the default.

   procedure Set_Right_Margin
     (Text_View    : access Gtk_Text_View_Record;
      Right_Margin : Gint);
   function Get_Right_Margin
     (Text_View : access Gtk_Text_View_Record) return Gint;
   --  Sets the default right margin for text in the text view.
   --  Tags in the buffer may override the default.

   procedure Set_Indent
     (Text_View : access Gtk_Text_View_Record; Indent : Gint);
   function Get_Indent (Text_View : access Gtk_Text_View_Record) return Gint;
   --  Sets the default indentation for paragraphs in Text_View.
   --  Tags in the buffer may override the default.

   procedure Set_Tabs
     (Text_View : access Gtk_Text_View_Record;
      Tabs      : Pango.Tabs.Pango_Tab_Array);
   function Get_Tabs
     (Text_View : access Gtk_Text_View_Record)
      return Pango.Tabs.Pango_Tab_Array;
   --  Sets the default tab stops for paragraphs in Text_View. Tags in the
   --  buffer may override the default
   --  The returned array will be Null_Tab_Array if "standard" (8-space) tabs
   --  are used. Free the return value Pango.Tabs.Free

   procedure Set_Overwrite
     (Text_View : access Gtk_Text_View_Record; Overwrite : Boolean);
   function Get_Overwrite
     (Text_View : access Gtk_Text_View_Record) return Boolean;
   --  Changes the Text_View overwrite mode.

   procedure Set_Accepts_Tab
     (Text_View   : access Gtk_Text_View_Record;  Accepts_Tab : Boolean);
   function Get_Accepts_Tab
     (Text_View : access Gtk_Text_View_Record) return Boolean;
   --  Sets the behavior of the text widget when the Tab key is pressed. If
   --  Accepts_Tab is True a tab character is inserted, otherwise the keyboard
   --  focus is moved to the next widget in the focus chain.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Accepts_Tab_Property
   --  Type:  Boolean
   --  Descr: Whether Tab will result in a tab character being entered
   --
   --  Name:  Buffer_Property
   --  Type:  Object
   --  Descr: The buffer which is displayed
   --
   --  Name:  Cursor_Visible_Property
   --  Type:  Boolean
   --  Descr: If the insertion cursor is shown
   --
   --  Name:  Editable_Property
   --  Type:  Boolean
   --  Descr: Whether the text can be modified by the user
   --
   --  Name:  Indent_Property
   --  Type:  Int
   --  Descr: Amount to indent the paragraph, in pixels
   --
   --  Name:  Justification_Property
   --  Type:  Enum
   --  Descr: Left, right, or center justification
   --
   --  Name:  Left_Margin_Property
   --  Type:  Int
   --  Descr: Width of the left margin in pixels
   --
   --  Name:  Overwrite_Property
   --  Type:  Boolean
   --  Descr: Whether entered text overwrites existing contents
   --
   --  Name:  Pixels_Above_Lines_Property
   --  Type:  Int
   --  Descr: Pixels of blank space above paragraphs
   --
   --  Name:  Pixels_Below_Lines_Property
   --  Type:  Int
   --  Descr: Pixels of blank space below paragraphs
   --
   --  Name:  Pixels_Inside_Wrap_Property
   --  Type:  Int
   --  Descr: Pixels of blank space between wrapped lines in a paragraph
   --
   --  Name:  Right_Margin_Property
   --  Type:  Int
   --  Descr: Width of the right margin in pixels
   --
   --  Name:  Tabs_Property
   --  Type:  Boxed
   --  Descr: Custom tabs for this text
   --
   --  Name:  Wrap_Mode_Property
   --  Type:  Enum
   --  Descr: Whether to wrap lines never, at word boundaries, or at character
   --         boundaries
   --
   --  </properties>

   Accepts_Tab_Property        : constant Glib.Properties.Property_Boolean;
   Buffer_Property             : constant Glib.Properties.Property_Object;
   Cursor_Visible_Property     : constant Glib.Properties.Property_Boolean;
   Editable_Property           : constant Glib.Properties.Property_Boolean;
   Indent_Property             : constant Glib.Properties.Property_Int;
   Justification_Property      : constant Gtk.Enums.Property_Gtk_Justification;
   Left_Margin_Property        : constant Glib.Properties.Property_Int;
   Overwrite_Property          : constant Glib.Properties.Property_Boolean;
   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int;
   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int;
   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int;
   Right_Margin_Property       : constant Glib.Properties.Property_Int;
   --  Tabs_Property           : constant Glib.Properties.Property_Boxed;
   Wrap_Mode_Property          : constant Gtk.Enums.Property_Gtk_Wrap_Mode;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Error_Underline_Color_Property
   --  Type:  Boxed
   --  Descr: Color with which to draw error-indication underlines
   --  </style_properties>

   --  Error_Underline_Color_Property : constant
   --    Glib.Properties.Property_Boxed;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "set_scroll_adjustments"
   --    procedure Handler
   --      (Widget      : access Gtk_Text_View_Record'Class;
   --       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
   --       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --
   --  - "populate_popup"
   --    procedure Handler
   --      (Widget : access Gtk_Text_View_Record'Class;
   --       Menu   : access Gtk.Menu.Gtk_Menu_Record'Class);
   --
   --  - "move_cursor"
   --    procedure Handler
   --      (Widget           : access Gtk_Text_View_Record'Class;
   --       Step             : Gtk_Movement_Step;
   --       Count            : Gint;
   --       Extend_Selection : Boolean);
   --
   --  - "set_anchor"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "insert_at_cursor"
   --    procedure Handler
   --      (Widget : access Gtk_Text_View_Record'Class; Str : UTF8_String);
   --
   --  - "delete_from_cursor"
   --    procedure Handler
   --      (Widget   : access Gtk_Text_View_Record'Class;
   --       The_Type : Gtk_Delete_Type;
   --       Count    : Gint);
   --
   --  - "cut_clipboard"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "copy_clipboard"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "paste_clipboard"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "toggle_overwrite"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  </signals>

   Signal_Backspace              : constant Glib.Signal_Name :=
                                     "backspace";
   Signal_Copy_Clipboard         : constant Glib.Signal_Name :=
                                     "copy_clipboard";
   Signal_Cut_Clipboard          : constant Glib.Signal_Name :=
                                     "cut_clipboard";
   Signal_Delete_From_Cursor     : constant Glib.Signal_Name :=
                                     "delete_from_cursor";
   Signal_Insert_At_Cursor       : constant Glib.Signal_Name :=
                                     "insert_at_cursor";
   Signal_Move_Cursor            : constant Glib.Signal_Name :=
                                     "move_cursor";
   Signal_Move_Focus             : constant Glib.Signal_Name :=
                                     "move_focus";
   Signal_Move_Viewport          : constant Glib.Signal_Name :=
                                     "move_viewport";
   Signal_Page_Horizontally      : constant Glib.Signal_Name :=
                                     "page_horizontally";
   Signal_Paste_Clipboard        : constant Glib.Signal_Name :=
                                     "paste_clipboard";
   Signal_Populate_Popup         : constant Glib.Signal_Name :=
                                     "populate_popup";
   Signal_Select_All             : constant Glib.Signal_Name :=
                                     "select_all";
   Signal_Set_Anchor             : constant Glib.Signal_Name :=
                                     "set_anchor";
   Signal_Set_Scroll_Adjustments : constant Glib.Signal_Name :=
                                     "set_scroll_adjustments";
   Signal_Toggle_Overwrite       : constant Glib.Signal_Name :=
                                     "toggle_overwrite";

private
   type Gtk_Text_View_Record is new Gtk.Container.Gtk_Container_Record with
     null record;

   Accepts_Tab_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("accepts-tab");
   Buffer_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("buffer");
   Cursor_Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("cursor-visible");
   Editable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Indent_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("indent");
   Justification_Property : constant Gtk.Enums.Property_Gtk_Justification :=
     Gtk.Enums.Build ("justification");
   Left_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("left-margin");
   Overwrite_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("overwrite");
   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-above-lines");
   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-below-lines");
   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-inside-wrap");
   Right_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("right-margin");
--     Tabs_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("tabs");
   Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Wrap_Mode :=
     Gtk.Enums.Build ("wrap-mode");

--     Error_Underline_Color_Property : constant Glib.Properties.Property_Boxed
--       := Glib.Properties.Build ("error-underline-color");

   pragma Import (C, Get_Type, "gtk_text_view_get_type");
end Gtk.Text_View;

--  No binding: gtk_text_view_new

--  <example>
--  --  The following example creates an empty text view, and puts it in a
--  --  scrolling area so that if more text is added, scrollbars are created
--  --  automatically.
--
--  declare
--     View     : Gtk_Text_View;
--     Buffer   : Gtk_Text_Buffer;
--     Scrolled : Gtk_Scrolled_Window;
--  begin
--     Gtk_New (Scrolled);
--     Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
--     Gtk_New (Buffer);
--     Gtk_New (View, Buffer);
--     Add (Scrolled, View);
--  end;
--  </example>
