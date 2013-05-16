-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-1999 E. Briot, J. Brobecker and A. Charlet   --
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
--  <group>Obsolescent widgets</group>

with Gtk.Widget;

package Gtk.Old_Editable is
   pragma Obsolescent;

   type Gtk_Old_Editable_Record is new
     Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Old_Editable is access all Gtk_Old_Editable_Record'Class;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Old_Editable.

   procedure Changed (Editable : access Gtk_Old_Editable_Record);
   --  Cause the "changed" signal to be emitted.

   procedure Claim_Selection
     (Editable : access Gtk_Old_Editable_Record;
      Claim    : in Boolean := True;
      Time     : in Guint32);
   --  If Claim is set to True, claim the ownership of the primary X selection.
   --  Otherwise, release it. "Time" should be set to the
   --  time of the last-change time for the specified selection. It is
   --  discarded if it is earlier than the current last-change time, or
   --  later than the current X server time.

   procedure Copy_Clipboard
     (Editable : access Gtk_Old_Editable_Record;
      Time     : in Guint32);
   --  Copy the characters in the current selection to the clipboard.

   procedure Cut_Clipboard
     (Editable : access Gtk_Old_Editable_Record;
      Time     : Guint32);
   --  Copy the characters in the current selection to the clipboard.
   --  The selection is then deleted.

   procedure Delete_Selection (Editable : access Gtk_Old_Editable_Record);
   --  Disclaim and delete the current selection.

   procedure Delete_Text
     (Editable  : access Gtk_Old_Editable_Record;
      Start_Pos : Gint := 0;
      End_Pos   : Gint := -1);
   --  Delete the characters from Start_Pos to End_Pos.
   --  If End_Pos is negative, the characters are deleted from Start_Pos to the
   --  end of the text.

   function Get_Chars
     (Editable  : access Gtk_Old_Editable_Record;
      Start_Pos : Gint := 0;
      End_Pos   : Gint := -1) return UTF8_String;
   --  Get the text from Start_Pos to End_Pos.
   --  If End_Pos is negative, the text from Start_Pos to the end is returned.

   function Get_Clipboard_Text
     (Widget : access Gtk_Old_Editable_Record) return UTF8_String;
   --  Return the last text copied from the clipboard.

   function Get_Editable
     (Widget : access Gtk_Old_Editable_Record) return Boolean;
   --  Return True if the widget is editable by the user.

   procedure Set_Editable
     (Widget   : access Gtk_Old_Editable_Record;
      Editable : Boolean := True);
   --  Set the editable status of the entry.
   --  If Editable is False, the user can not modify the contents of the entry.
   --  This does not affect the user of the insertion functions above.

   function Get_Has_Selection
     (Widget : access Gtk_Old_Editable_Record) return Boolean;
   --  Return True if the selection is owned by the widget.

   function Get_Selection_End_Pos
     (Widget : access Gtk_Old_Editable_Record) return Guint;
   --  Return the position of the end of the current selection.

   function Get_Selection_Start_Pos
     (Widget : access Gtk_Old_Editable_Record) return Guint;
   --  Return the position of the beginning of the current selection.

   procedure Insert_Text
     (Editable : access Gtk_Old_Editable_Record;
      New_Text : UTF8_String;
      Position : in out Gint);
   --  Insert the given string at the given position.
   --  Position is set to the new cursor position.

   procedure Paste_Clipboard
     (Editable : access Gtk_Old_Editable_Record;
      Time     : Guint32);
   --  The contents of the clipboard is pasted into the given widget at
   --  the current cursor position.

   procedure Select_Region
     (Editable : access Gtk_Old_Editable_Record;
      Start    : Gint;
      The_End  : Gint := -1);
   --  Select the region of text from Start to The_End.
   --  The characters that are selected are those characters at positions
   --  from Start up to, but not including The_End. If The_End_Pos is
   --  negative, then the characters selected will be those characters
   --  from Start to the end of the text.

   procedure Set_Position
     (Editable : access Gtk_Old_Editable_Record;
      Position : Gint);
   --  Change the position of the cursor in the entry.
   --  The cursor is displayed before the character with the given
   --  index in the widget (the first character has index 0). The
   --  value must be less than or equal to the number of characters in the
   --  widget. A value of -1 indicates that the position
   --  should be set after the last character in the entry.
   --  Note that this position is in characters, not in bytes.

   function Get_Position
     (Editable : access Gtk_Old_Editable_Record) return Gint;
   --  Return the position of the cursor.

   ---------------
   --  Signals  --
   ---------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class);
   --
   --    emitted when the user has changed the text of the widget.
   --
   --  - "insert_text"
   --    procedure Handler (Widget   : access Gtk_Old_Editable_Record'Class;
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
   --    procedure Handler (Widget    : access Gtk_Old_Editable_Record'Class;
   --                       Start_Pos : in Gint;
   --                       End_Pos   : in Gint);
   --
   --    Emitted when some text is deleted by the user. As for the
   --    "insert-text" handler, it is possible to override the default
   --    behavior by connecting a handler to this signal, and then
   --    stopping the signal.
   --
   --  - "activate"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class);
   --
   --    Emitted when the user has activated the widget in some fashion.
   --
   --  - "set-editable"
   --    procedure Handler (Widget     : access Gtk_Old_Editable_Record'Class;
   --                       Is_Editable: in Boolean);
   --
   --    Emitting this signal is equivalent to calling Set_Old_Editable.
   --
   --  - "move_cursor"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class;
   --                       X, Y   : in Gint);
   --
   --    Emitting this signal will move the cursor position for X
   --    characters horizontally, and Y characters vertically.
   --
   --  - "move_word"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class;
   --                       N      : in Gint);
   --
   --    Emitting this signal will move the cursor by N words (N can be
   --    negative).
   --
   --  - "move_page"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class;
   --                       X, Y   : in Gint);
   --
   --    Emitting this signal will move the cursor for X pages
   --    horizontally, and Y pages vertically.
   --
   --  - "move_to_row"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class;
   --                       Row    : in Gint);
   --
   --    Emitting this signal will move the cursor to the given row.
   --
   --  - "move_to_column"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class;
   --                       Column : in Gint);
   --
   --    Emitting this signal will move the cursor to the given column.
   --
   --  - "kill_char"
   --    procedure Handler (Widget    : access Gtk_Old_Editable_Record'Class;
   --                       Direction : in Gint);
   --
   --    Emitting this signal deletes a single character. If Direction
   --    is positive, delete forward, else delete backward.
   --
   --  - "kill_word"
   --    procedure Handler (Widget    : access Gtk_Old_Editable_Record'Class;
   --                       Direction : in Gint);
   --
   --    Emitting this signal deletes a single word. If Direction is
   --    positive, delete forward, otherwise delete backward.
   --
   --  - "kill_line"
   --    procedure Handler (Widget    : access Gtk_Old_Editable_Record'Class;
   --                       Direction : in Gint);
   --
   --    Emitting this signal deletes a single line. If Direction is
   --    positive, delete forward, otherwise delete backward.
   --
   --  - "cut_clipboard"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class);
   --
   --    Emitting this signal will cut the current selection to the
   --    clipboard.
   --
   --  - "copy_clipboard"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class);
   --
   --    Emitting this signal will copy the current selection to the
   --    clipboard.
   --
   --  - "paste_clipboard"
   --    procedure Handler (Widget : access Gtk_Old_Editable_Record'Class);
   --
   --    Emitting this signal will paste the clipboard into the text
   --    of the widget at the current cursor position.
   --
   --  </signals>

   Signal_Activate        : constant Glib.Signal_Name := "activate";
   Signal_Copy_Clipboard  : constant Glib.Signal_Name := "copy_clipboard";
   Signal_Cut_Clipboard   : constant Glib.Signal_Name := "cut_clipboard";
   Signal_Kill_Char       : constant Glib.Signal_Name := "kill_char";
   Signal_Kill_Line       : constant Glib.Signal_Name := "kill_line";
   Signal_Kill_Word       : constant Glib.Signal_Name := "kill_word";
   Signal_Move_Cursor     : constant Glib.Signal_Name := "move_cursor";
   Signal_Move_Page       : constant Glib.Signal_Name := "move_page";
   Signal_Move_To_Column  : constant Glib.Signal_Name := "move_to_column";
   Signal_Move_To_Row     : constant Glib.Signal_Name := "move_to_row";
   Signal_Move_Word       : constant Glib.Signal_Name := "move_word";
   Signal_Paste_Clipboard : constant Glib.Signal_Name := "paste_clipboard";
   Signal_Set_Editable    : constant Glib.Signal_Name := "set-editable";

private
   type Gtk_Old_Editable_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_old_editable_get_type");
end Gtk.Old_Editable;
