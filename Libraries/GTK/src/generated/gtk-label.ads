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
--  A Gtk_Label is a light widget associated with some text you want to
--  display on the screen. You can change the text dynamically if needed.
--
--  The text can be on multiple lines if you separate each line with the
--  ASCII.LF character. However, this is not the recommended way to display
--  long texts (see the Gtk_Text widget instead).
--
--  == Mnemonics ==
--
--  Labels may contain mnemonics. Mnemonics are underlined characters in the
--  label, used for keyboard navigation. Mnemonics are created by providing
--  string with an underscore before the mnemonic character, such as "_File",
--  to the functions gtk_new_with_mnemonic or set_text_with_mnemonic().
--
--  Mnemonics automatically activate any activatable widget the label is
--  inside, such as a Gtk_Button; if the label is not inside the mnemonic's
--  target widget, you have to tell the label about the target using
--  set_mnemonic_widget(). For instance: declare Button : Gtk_Button; Label :
--  Gtk_Label; begin Gtk_New (Button); Gtk_New_With_Mnemonic (Label, "_File");
--  Add (Button, Label); end; However, there exists a convenience function in
--  Gtk.Button to create such a button already.
--
--  == Markup ==
--
--  To make it easy to format text in a label (changing colors, fonts, etc.),
--  label text can be provided in a simple markup format. Here's how to create
--  a label with a small font: Gtk_New (Label, "<small>hello</small>");
--
--  The markup must be valid, and <>& characters must be escaped with &lt;
--  &gt; and &amp;
--
--  Markup strings are just a convenient way to set the Pango_Attr_List on
--  label; Set_Attributes() may be a simpler way to set attributes in some
--  cases. Be careful though; Pango_Attr_List tends to cause
--  internationalization problems, unless you're applying attributes to the
--  entire string (i.e. unless you set the range of each attribute to [0,
--  G_MAXINT)). The reason is that specifying the start_index and end_index for
--  a Pango_Attribute requires knowledge of the exact string being displayed,
--  so translations will cause problems.
--
--  == Selectable labels ==
--
--  Labels can be made selectable with Set_Selectable. Selectable labels allow
--  the user to copy the label contents to the clipboard. Only should be made
--  selectable.
--
--  </description>
--  <screenshot>gtk-label</screenshot>
--  <group>Display widgets</group>
--  <testgtk>create_label.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Properties;  use Glib.Properties;
with Glib.Types;       use Glib.Types;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Misc;         use Gtk.Misc;
with Gtk.Widget;       use Gtk.Widget;
with Pango.Attributes; use Pango.Attributes;
with Pango.Layout;     use Pango.Layout;

package Gtk.Label is

   type Gtk_Label_Record is new Gtk_Misc_Record with null record;
   type Gtk_Label is access all Gtk_Label_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Label : out Gtk_Label; Str : UTF8_String := "");
   procedure Initialize
      (Label : access Gtk_Label_Record'Class;
       Str   : UTF8_String := "");
   --  Creates a new label with the given text inside it. You can pass null to
   --  get an empty label widget.
   --  "str": The text of the label

   procedure Gtk_New_With_Mnemonic
      (Label : out Gtk_Label;
       Str   : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Label : access Gtk_Label_Record'Class;
       Str   : UTF8_String);
   --  Creates a new Gtk.Label.Gtk_Label, containing the text in Str. If
   --  characters in Str are preceded by an underscore, they are underlined. If
   --  you need a literal underscore character in a label, use '__' (two
   --  underscores). The first underlined character represents a keyboard
   --  accelerator called a mnemonic. The mnemonic key can be used to activate
   --  another widget, chosen automatically, or explicitly using
   --  Gtk.Label.Set_Mnemonic_Widget. If Gtk.Label.Set_Mnemonic_Widget is not
   --  called, then the first activatable ancestor of the Gtk.Label.Gtk_Label
   --  will be chosen as the mnemonic widget. For instance, if the label is
   --  inside a button or menu item, the button or menu item will automatically
   --  become the mnemonic widget and be activated by the mnemonic.
   --  "str": The text of the label, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_label_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Angle (Label : access Gtk_Label_Record) return Gdouble;
   procedure Set_Angle (Label : access Gtk_Label_Record; Angle : Gdouble);
   --  Sets the angle of rotation for the label. An angle of 90 reads from
   --  from bottom to top, an angle of 270, from top to bottom. The angle
   --  setting for the label is ignored if the label is selectable, wrapped, or
   --  ellipsized.
   --  Since: gtk+ 2.6
   --  "angle": the angle that the baseline of the label makes with the
   --  horizontal, in degrees, measured counterclockwise

   function Get_Attributes
      (Label : access Gtk_Label_Record)
       return Pango.Attributes.Pango_Attr_List;
   procedure Set_Attributes
      (Label : access Gtk_Label_Record;
       Attrs : out Pango.Attributes.Pango_Attr_List);
   --  Sets a Pango.Attributes.Pango_Attr_List; the attributes in the list are
   --  applied to the label text.
   --  Note: The attributes set with this function will be applied and merged
   --  with any other attributes previously effected by way of the
   --  Gtk.Label.Gtk_Label:use-underline or Gtk.Label.Gtk_Label:use-markup
   --  properties. While it is not recommended to mix markup strings with
   --  manually set attributes, if you must; know that the attributes will be
   --  applied to the label after the markup string is parsed.
   --  "attrs": a Pango.Attributes.Pango_Attr_List

   function Get_Current_Uri
      (Label : access Gtk_Label_Record) return UTF8_String;
   --  Returns the URI for the currently active link in the label. The active
   --  link is the one under the mouse pointer or, in a selectable label, the
   --  link in which the text cursor is currently positioned. This function is
   --  intended for use in a Gtk.Label.Gtk_Label::activate-link handler or for
   --  use in a Gtk.Widget.Gtk_Widget::query-tooltip handler. not be freed or
   --  modified.
   --  Since: gtk+ 2.18

   function Get_Ellipsize
      (Label : access Gtk_Label_Record)
       return Pango.Layout.Pango_Ellipsize_Mode;
   procedure Set_Ellipsize
      (Label : access Gtk_Label_Record;
       Mode  : Pango.Layout.Pango_Ellipsize_Mode);
   --  if there is not enough space to render the entire string.
   --  Since: gtk+ 2.6
   --  "mode": a Pango.Layout.Pango_Ellipsize_Mode

   function Get_Justify
      (Label : access Gtk_Label_Record) return Gtk.Enums.Gtk_Justification;
   procedure Set_Justify
      (Label : access Gtk_Label_Record;
       Jtype : Gtk.Enums.Gtk_Justification);
   --  Sets the alignment of the lines in the text of the label relative to
   --  each other. %GTK_JUSTIFY_LEFT is the default value when the widget is
   --  first created with Gtk.Label.Gtk_New. If you instead want to set the
   --  alignment of the label as a whole, use Gtk.Misc.Set_Alignment instead.
   --  Gtk.Label.Set_Justify has no effect on labels containing only a single
   --  line.
   --  "jtype": a Gtk.Enums.Gtk_Justification

   function Get_Label (Label : access Gtk_Label_Record) return UTF8_String;
   procedure Set_Label (Label : access Gtk_Label_Record; Str : UTF8_String);
   --  Sets the text of the label. The label is interpreted as including
   --  embedded underlines and/or Pango markup depending on the values of the
   --  Gtk.Label.Gtk_Label:use-underline" and Gtk.Label.Gtk_Label:use-markup
   --  properties.
   --  "str": the new text to set for the label

   function Get_Layout
      (Label : access Gtk_Label_Record) return Pango.Layout.Pango_Layout;
   --  Gets the Pango.Layout.Pango_Layout used to display the label. The
   --  layout is useful to e.g. convert text positions to pixel positions, in
   --  combination with Gtk.Label.Get_Layout_Offsets. The returned layout is
   --  owned by the label so need not be freed by the caller.

   procedure Get_Layout_Offsets
      (Label : access Gtk_Label_Record;
       X     : out Gint;
       Y     : out Gint);
   --  Obtains the coordinates where the label will draw the
   --  Pango.Layout.Pango_Layout representing the text in the label; useful to
   --  convert mouse events into coordinates inside the
   --  Pango.Layout.Pango_Layout, e.g. to take some action if some part of the
   --  label is clicked. Of course you will need to create a
   --  Gtk.Event_Box.Gtk_Event_Box to receive the events, and pack the label
   --  inside it, since labels are a GTK_NO_WINDOW widget. Remember when using
   --  the Pango.Layout.Pango_Layout functions you need to convert to and from
   --  pixels using PANGO_PIXELS or PANGO_SCALE.
   --  "x": location to store X offset of layout, or null
   --  "y": location to store Y offset of layout, or null

   function Get_Line_Wrap (Label : access Gtk_Label_Record) return Boolean;
   procedure Set_Line_Wrap (Label : access Gtk_Label_Record; Wrap : Boolean);
   --  Toggles line wrapping within the Gtk.Label.Gtk_Label widget. True makes
   --  it break lines if text exceeds the widget's size. False lets the text
   --  get cut off by the edge of the widget if it exceeds the widget size.
   --  Note that setting line wrapping to True does not make the label wrap at
   --  its parent container's width, because GTK+ widgets conceptually can't
   --  make their requisition depend on the parent container's size. For a
   --  label that wraps at a specific position, set the label's width using
   --  Gtk.Widget.Set_Size_Request.
   --  "wrap": the setting

   function Get_Line_Wrap_Mode
      (Label : access Gtk_Label_Record) return Pango.Layout.Pango_Wrap_Mode;
   procedure Set_Line_Wrap_Mode
      (Label     : access Gtk_Label_Record;
       Wrap_Mode : Pango.Layout.Pango_Wrap_Mode);
   --  If line wrapping is on (see Gtk.Label.Set_Line_Wrap) this controls how
   --  the line wrapping is done. The default is %PANGO_WRAP_WORD which means
   --  wrap on word boundaries.
   --  Since: gtk+ 2.10
   --  "wrap_mode": the line wrapping mode

   function Get_Max_Width_Chars
      (Label : access Gtk_Label_Record) return Gint;
   procedure Set_Max_Width_Chars
      (Label   : access Gtk_Label_Record;
       N_Chars : Gint);
   --  Sets the desired maximum width in characters of Label to N_Chars.
   --  Since: gtk+ 2.6
   --  "n_chars": the new desired maximum width, in characters.

   function Get_Mnemonic_Keyval
      (Label : access Gtk_Label_Record) return Guint;
   --  If the label has been set so that it has an mnemonic key this function
   --  returns the keyval used for the mnemonic accelerator. If there is no
   --  mnemonic set up it returns GDK_VoidSymbol.

   function Get_Mnemonic_Widget
      (Label : access Gtk_Label_Record) return Gtk.Widget.Gtk_Widget;
   procedure Set_Mnemonic_Widget
      (Label  : access Gtk_Label_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  If the label has been set so that it has an mnemonic key (using i.e.
   --  Gtk.Label.Set_Markup_With_Mnemonic, Gtk.Label.Set_Text_With_Mnemonic,
   --  Gtk.Label.Gtk_New_With_Mnemonic or the "use_underline" property) the
   --  label can be associated with a widget that is the target of the
   --  mnemonic. When the label is inside a widget (like a
   --  Gtk.Button.Gtk_Button or a Gtk.Notebook.Gtk_Notebook tab) it is
   --  automatically associated with the correct widget, but sometimes (i.e.
   --  when the target is a Gtk.GEntry.Gtk_Entry next to the label) you need to
   --  set it explicitly using this function. The target widget will be
   --  accelerated by emitting the GtkWidget::mnemonic-activate signal on it.
   --  The default handler for this signal will activate the widget if there
   --  are no mnemonic collisions and toggle focus between the colliding
   --  widgets otherwise.
   --  "widget": the target Gtk.Widget.Gtk_Widget

   function Get_Selectable (Label : access Gtk_Label_Record) return Boolean;
   procedure Set_Selectable
      (Label   : access Gtk_Label_Record;
       Setting : Boolean);
   --  Selectable labels allow the user to select text from the label, for
   --  copy-and-paste.
   --  "setting": True to allow selecting text in the label

   procedure Get_Selection_Bounds
      (Label         : access Gtk_Label_Record;
       Start         : out Gint;
       The_End       : out Gint;
       Has_Selection : out Boolean);
   --  Gets the selected range of characters in the label, returning True if
   --  there's a selection.
   --  "start": return location for start of selection, as a character offset
   --  "end": return location for end of selection, as a character offset

   function Get_Single_Line_Mode
      (Label : access Gtk_Label_Record) return Boolean;
   procedure Set_Single_Line_Mode
      (Label            : access Gtk_Label_Record;
       Single_Line_Mode : Boolean);
   --  Sets whether the label is in single line mode.
   --  Since: gtk+ 2.6
   --  "single_line_mode": True if the label should be in single line mode

   function Get_Text (Label : access Gtk_Label_Record) return UTF8_String;
   procedure Set_Text (Label : access Gtk_Label_Record; Str : UTF8_String);
   --  Sets the text within the Gtk.Label.Gtk_Label widget. It overwrites any
   --  text that was there before. This will also clear any previously set
   --  mnemonic accelerators.
   --  "str": The text you want to set

   function Get_Track_Visited_Links
      (Label : access Gtk_Label_Record) return Boolean;
   procedure Set_Track_Visited_Links
      (Label       : access Gtk_Label_Record;
       Track_Links : Boolean);
   --  Sets whether the label should keep track of clicked links (and use a
   --  different color for them).
   --  Since: gtk+ 2.18
   --  "track_links": True to track visited links

   function Get_Use_Markup (Label : access Gtk_Label_Record) return Boolean;
   procedure Set_Use_Markup
      (Label   : access Gtk_Label_Record;
       Setting : Boolean);
   --  Sets whether the text of the label contains markup in <link
   --  linkend="PangoMarkupFormat">Pango's text markup language</link>. See
   --  Gtk.Label.Set_Markup.
   --  "setting": True if the label's text should be parsed for markup.

   function Get_Use_Underline
      (Label : access Gtk_Label_Record) return Boolean;
   procedure Set_Use_Underline
      (Label   : access Gtk_Label_Record;
       Setting : Boolean);
   --  If true, an underline in the text indicates the next character should
   --  be used for the mnemonic accelerator key.
   --  "setting": True if underlines in the text indicate mnemonics

   function Get_Width_Chars (Label : access Gtk_Label_Record) return Gint;
   procedure Set_Width_Chars
      (Label   : access Gtk_Label_Record;
       N_Chars : Gint);
   --  Sets the desired width in characters of Label to N_Chars.
   --  Since: gtk+ 2.6
   --  "n_chars": the new desired width, in characters.

   function Parse_Uline
      (Label  : access Gtk_Label_Record;
       String : UTF8_String) return Guint;

   procedure Select_Region
      (Label        : access Gtk_Label_Record;
       Start_Offset : Gint := -1;
       End_Offset   : Gint := -1);
   --  Selects a range of characters in the label, if the label is selectable.
   --  See Gtk.Label.Set_Selectable. If the label is not selectable, this
   --  function has no effect. If Start_Offset or
   --  "start_offset": start offset (in characters not bytes)
   --  "end_offset": end offset (in characters not bytes)

   procedure Set_Markup (Label : access Gtk_Label_Record; Str : UTF8_String);
   --  Parses Str which is marked up with the <link
   --  linkend="PangoMarkupFormat">Pango text markup language</link>, setting
   --  the label's text and attribute list based on the parse results. If the
   --  Str is external data, you may need to escape it with
   --  g_markup_escape_text or g_markup_printf_escaped<!-- -->: |[ char
   --  *markup; markup = g_markup_printf_escaped ("&lt;span
   --  style=\"italic\"&gt;&percnt;s&lt;/span&gt;", str); gtk_label_set_markup
   --  (GTK_LABEL (label), markup); g_free (markup); ]|
   --  "str": a markup string (see <link linkend="PangoMarkupFormat">Pango
   --  markup format</link>)

   procedure Set_Markup_With_Mnemonic
      (Label : access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Parses Str which is marked up with the <link
   --  linkend="PangoMarkupFormat">Pango text markup language</link>, setting
   --  the label's text and attribute list based on the parse results. If
   --  characters in Str are preceded by an underscore, they are underlined
   --  indicating that they represent a keyboard accelerator called a mnemonic.
   --  The mnemonic key can be used to activate another widget, chosen
   --  automatically, or explicitly using Gtk.Label.Set_Mnemonic_Widget.
   --  "str": a markup string (see <link linkend="PangoMarkupFormat">Pango
   --  markup format</link>)

   procedure Set_Pattern
      (Label   : access Gtk_Label_Record;
       Pattern : UTF8_String);
   --  Change the underlines pattern.
   --  Pattern is a simple string made of underscore and space characters,
   --  matching the ones in the string. GtkAda will underline every letter that
   --  matches an underscore.
   --  An empty string disables the underlines.
   --  example: If the text is FooBarBaz and the Pattern is "___ ___" then
   --  both "Foo" and "Baz" will be underlined, but not "Bar".

   procedure Set_Text_With_Mnemonic
      (Label : access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Sets the label's text from the string Str. If characters in Str are
   --  preceded by an underscore, they are underlined indicating that they
   --  represent a keyboard accelerator called a mnemonic. The mnemonic key can
   --  be used to activate another widget, chosen automatically, or explicitly
   --  using Gtk.Label.Set_Mnemonic_Widget.
   --  "str": a string

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Label_Record, Gtk_Label);
   function "+"
     (Widget : access Gtk_Label_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Label
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Angle_Property
   --  Type: Gdouble
   --  Flags: read-write
   --  The angle that the baseline of the label makes with the horizontal, in
   --  degrees, measured counterclockwise. An angle of 90 reads from from
   --  bottom to top, an angle of 270, from top to bottom. Ignored if the label
   --  is selectable, wrapped, or ellipsized.
   --
   --  Name: Cursor_Position_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Justify_Property
   --  Type: Gtk.Enums.Gtk_Justification
   --  Flags: read-write
   --
   --  Name: Label_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --
   --  Name: Max_Width_Chars_Property
   --  Type: Gint
   --  Flags: read-write
   --  The desired maximum width of the label, in characters. If this property
   --  is set to -1, the width will be calculated automatically, otherwise the
   --  label will request space for no more than the requested number of
   --  characters. If the Gtk.Label.Gtk_Label:width-chars property is set to a
   --  positive value, then the "max-width-chars" property is ignored.
   --
   --  Name: Mnemonic_Keyval_Property
   --  Type: Guint
   --  Flags: read-write
   --
   --  Name: Mnemonic_Widget_Property
   --  Type: Gtk.Widget.Gtk_Widget
   --  Flags: read-write
   --
   --  Name: Pattern_Property
   --  Type: UTF8_String
   --  Flags: write
   --
   --  Name: Selectable_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Selection_Bound_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Single_Line_Mode_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether the label is in single line mode. In single line mode, the
   --  height of the label does not depend on the actual text, it is always set
   --  to ascent + descent of the font. This can be an advantage in situations
   --  where resizing the label because of text changes would be distracting,
   --  e.g. in a statusbar.
   --
   --  Name: Track_Visited_Links_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Set this property to True to make the label track which links have been
   --  clicked. It will then apply the ::visited-link-color color, instead of
   --  ::link-color.
   --
   --  Name: Use_Markup_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Use_Underline_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Width_Chars_Property
   --  Type: Gint
   --  Flags: read-write
   --  The desired width of the label, in characters. If this property is set
   --  to -1, the width will be calculated automatically, otherwise the label
   --  will request either 3 characters or the property value, whichever is
   --  greater. If the "width-chars" property is set to a positive value, then
   --  the Gtk.Label.Gtk_Label:max-width-chars property is ignored.
   --
   --  Name: Wrap_Property
   --  Type: Boolean
   --  Flags: read-write

   Angle_Property : constant Glib.Properties.Property_Double;
   Cursor_Position_Property : constant Glib.Properties.Property_Int;
   Justify_Property : constant Gtk.Enums.Property_Gtk_Justification;
   Label_Property : constant Glib.Properties.Property_String;
   Max_Width_Chars_Property : constant Glib.Properties.Property_Int;
   Mnemonic_Keyval_Property : constant Glib.Properties.Property_Uint;
   Mnemonic_Widget_Property : constant Glib.Properties.Property_Object;
   Pattern_Property : constant Glib.Properties.Property_String;
   Selectable_Property : constant Glib.Properties.Property_Boolean;
   Selection_Bound_Property : constant Glib.Properties.Property_Int;
   Single_Line_Mode_Property : constant Glib.Properties.Property_Boolean;
   Track_Visited_Links_Property : constant Glib.Properties.Property_Boolean;
   Use_Markup_Property : constant Glib.Properties.Property_Boolean;
   Use_Underline_Property : constant Glib.Properties.Property_Boolean;
   Width_Chars_Property : constant Glib.Properties.Property_Int;
   Wrap_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "activate-current-link"
   --     procedure Handler (Self : access Gtk_Label_Record'Class);
   --  A <link linkend="keybinding-signals">keybinding signal</link> which
   --  gets emitted when the user activates a link in the label. Applications
   --  may also emit the signal with g_signal_emit_by_name if they need to
   --  control activation of URIs programmatically. The default bindings for
   --  this signal are all forms of the Enter key.
   --
   --  "activate-link"
   --     function Handler
   --       (Self : access Gtk_Label_Record'Class;
   --        Uri  : UTF8_String) return Boolean;
   --    --  "uri": the URI that is activated
   --  The signal which gets emitted to activate a URI. Applications may
   --  connect to it to override the default behaviour, which is to call
   --  gtk_show_uri().
   --  Returns True if the link has been activated
   --
   --  "copy-clipboard"
   --     procedure Handler (Self : access Gtk_Label_Record'Class);
   --  The ::copy-clipboard signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to copy the selection to the clipboard. The default binding for this
   --  signal is Ctrl-c.
   --
   --  "move-cursor"
   --     procedure Handler
   --       (Self             : access Gtk_Label_Record'Class;
   --        Step             : MovementStep;
   --        Count            : Gint;
   --        Extend_Selection : Boolean);
   --    --  "step": the granularity of the move, as a GtkMovementStep
   --    --  "count": the number of Step units to move
   --    --  "extend_selection": True if the move should extend the selection
   --  The ::move-cursor signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  when the user initiates a cursor movement. If the cursor is not visible
   --  in Entry, this signal causes the viewport to be moved instead.
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control the cursor
   --  programmatically. The default bindings for this signal come in two
   --  variants, the variant with the Shift modifier extends the selection, the
   --  variant without the Shift modifer does not. There are too many key
   --  combinations to list them all here. <itemizedlist> <listitem>Arrow keys
   --  move by individual characters/lines</listitem> <listitem>Ctrl-arrow key
   --  combinations move by words/paragraphs</listitem> <listitem>Home/End keys
   --  move to the ends of the buffer</listitem> </itemizedlist>
   --
   --  "populate-popup"
   --     procedure Handler
   --       (Self : access Gtk_Label_Record'Class;
   --        Menu : Gtk.Menu.Gtk_Menu);
   --    --  "menu": the menu that is being populated
   --  The ::populate-popup signal gets emitted before showing the context
   --  menu of the label. Note that only selectable labels have context menus.
   --  If you need to add items to the context menu, connect to this signal and
   --  append your menuitems to the Menu.

   Signal_Activate_Current_Link : constant Glib.Signal_Name := "activate-current-link";
   Signal_Activate_Link : constant Glib.Signal_Name := "activate-link";
   Signal_Copy_Clipboard : constant Glib.Signal_Name := "copy-clipboard";
   Signal_Move_Cursor : constant Glib.Signal_Name := "move-cursor";
   Signal_Populate_Popup : constant Glib.Signal_Name := "populate-popup";

private
   Angle_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("angle");
   Cursor_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("cursor-position");
   Justify_Property : constant Gtk.Enums.Property_Gtk_Justification :=
     Gtk.Enums.Build ("justify");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Max_Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-width-chars");
   Mnemonic_Keyval_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("mnemonic-keyval");
   Mnemonic_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("mnemonic-widget");
   Pattern_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("pattern");
   Selectable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("selectable");
   Selection_Bound_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("selection-bound");
   Single_Line_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("single-line-mode");
   Track_Visited_Links_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("track-visited-links");
   Use_Markup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-markup");
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-chars");
   Wrap_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap");
end Gtk.Label;
