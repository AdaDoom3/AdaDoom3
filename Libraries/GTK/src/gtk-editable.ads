-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2007 AdaCore                    --
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
--  This widget is an abstract widget designed to support the common
--  functionalities of all widgets for editing text. It provides general
--  services to manipulate an editable widget, a large number of action
--  signals used for key bindings, and several signals that an
--  application can connect to to modify the behavior of a widget.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Numeric/Text Data Entry</group>

with Gtk.Widget;

package Gtk.Editable is

   type Gtk_Editable_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Editable is access all Gtk_Editable_Record'Class;
   --  Gtk_Editable is now an interface, not an object per se.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Editable.

   procedure Select_Region
     (Editable : access Gtk_Editable_Record;
      Start    : Gint;
      The_End  : Gint := -1);
   --  Select the region of text from Start to The_End.
   --  The characters that are selected are those characters at positions
   --  from Start up to, but not including The_End. If The_End_Pos is
   --  negative, then the characters selected will be those characters
   --  from Start to the end of the text.

   procedure Get_Selection_Bounds
     (Widget    : access Gtk_Editable_Record;
      Success   : out Boolean;
      Start_Pos : out Guint;
      End_Pos   : out Guint);
   --  Return the position of the start and end of the current selection.
   --  If success is false, Start_Pos and End_Pos are not modified.

   procedure Insert_Text
     (Editable : access Gtk_Editable_Record;
      New_Text : UTF8_String;
      Position : in out Gint);
   --  Insert the given string at the given position.
   --  Position is set to the new cursor position. If Position is -1, the
   --  text is appended at the end.

   procedure Delete_Text
     (Editable  : access Gtk_Editable_Record;
      Start_Pos : Gint := 0;
      End_Pos   : Gint := -1);
   --  Delete the characters from Start_Pos to End_Pos.
   --  If End_Pos is negative, the characters are deleted from Start_Pos to the
   --  end of the text.

   function Get_Chars
     (Editable  : access Gtk_Editable_Record;
      Start_Pos : Gint := 0;
      End_Pos   : Gint := -1) return UTF8_String;
   --  Get the text from Start_Pos to End_Pos.
   --  If End_Pos is negative, the text from Start_Pos to the end is returned.

   procedure Cut_Clipboard (Editable : access Gtk_Editable_Record);
   --  Copy the characters in the current selection to the clipboard.
   --  The selection is then deleted.

   procedure Copy_Clipboard (Editable : access Gtk_Editable_Record);
   --  Copy the characters in the current selection to the clipboard.

   procedure Paste_Clipboard (Editable : access Gtk_Editable_Record);
   --  The contents of the clipboard is pasted into the given widget at
   --  the current cursor position.

   procedure Delete_Selection (Editable : access Gtk_Editable_Record);
   --  Disclaim and delete the current selection.

   procedure Set_Position
     (Editable : access Gtk_Editable_Record;
      Position : Gint);
   function Get_Position (Editable : access Gtk_Editable_Record) return Gint;
   --  Change the position of the cursor in the entry.
   --  The cursor is displayed before the character with the given
   --  index in the widget (the first character has index 0). The
   --  value must be less than or equal to the number of characters in the
   --  widget. A value of -1 indicates that the position
   --  should be set after the last character in the entry.
   --  Note that this position is in characters, not in bytes.

   procedure Set_Editable
     (Widget   : access Gtk_Editable_Record;
      Editable : Boolean := True);
   function Get_Editable
     (Editable : access Gtk_Editable_Record) return Boolean;
   --  Set the editable status of the entry.
   --  If Editable is False, the user can not modify the contents of the entry.
   --  This does not affect the user of the insertion functions above.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   ---------------
   --  Signals  --
   ---------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "insert_text"
   --    procedure Handler (Widget   : access Gtk_Editable_Record'Class;
   --                       Text     : in UTF8_String;
   --                       Length   : in Gint;
   --                       Position : in Gint_Access);
   --
   --    Emitted when some text is inserted inside the widget by the
   --    user. The default handler inserts the text into the widget.
   --    By connecting a handler to this signal, and then by stopping
   --    the signal with Gtk.Handlers.Emit_Stop_By_Name, it is possible
   --    to modify the inserted text, or even prevent it from being
   --    inserted.
   --    Position.all should be modified by the callback, and indicates
   --    the new position of the cursor after the text has been inserted.
   --
   --  - "delete_text"
   --    procedure Handler (Widget    : access Gtk_Editable_Record'Class;
   --                       Start_Pos : in Gint;
   --                       End_Pos   : in Gint);
   --
   --    Emitted when some text is deleted by the user. As for the
   --    "insert-text" handler, it is possible to override the default
   --    behavior by connecting a handler to this signal, and then
   --    stopping the signal.
   --
   --  - "changed"
   --    procedure Handler (Widget : access Gtk_Editable_Record'Class);
   --
   --    Called when the contents of Widget has changed
   --  </signals>

   Signal_Changed     : constant Glib.Signal_Name := "changed";
   Signal_Delete_Text : constant Glib.Signal_Name := "delete_text";
   Signal_Insert_Text : constant Glib.Signal_Name := "insert_text";

private
   type Gtk_Editable_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_editable_get_type");
end Gtk.Editable;
