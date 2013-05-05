-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--               Copyright (C) 2000-2002 ACT-Europe                  --
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
--  These functions provide an applications programmer with default
--  icons for toolbars, menu pixmaps, etc. One such `icon' should have
--  at least three pixmaps to reflect it's state. There is a `regular'
--  pixmap, a `disabled' pixmap and a `focused' pixmap. You can get
--  either each of these pixmaps by calling Gnome.Stock.Pixmap or you
--  can get a widget by calling Gnome.Stock.Pixmap_Widget. This widget
--  is a container which shows the pixmap, that is
--  reflecting the current state of the widget. If for example you
--  Gtk.Container.Add this widget to a button, which is currently not
--  sensitive, the widget will just show the `disabled' pixmap. If the
--  state of the button changes to sensitive, the widget will change to
--  the `regular' pixmap. The `focused' pixmap will be shown, when the
--  mouse pointer enters the widget.
--
--  We now have stock buttons too. To use them, just replace any
--  Gtk.Button.Gtk_New with Gnome.Stock.Button (Button_...).
--  This function returns a Gtk_Button with a gettexted default text and an
--  icon.
--  </description>

with Gtk;
with Gnome.Pixmap;

package Gnome.Stock is

   type Gnome_Stock_Record is new
     Gnome.Pixmap.Gnome_Pixmap_Record with private;
   type Gnome_Stock is access all Gnome_Stock_Record'Class;

   --  The names of `well known' icons. I define these strings mainly to
   --  prevent errors due to typos.

   Pixmap_New         : constant String := "New";
   Pixmap_Open        : constant String := "Open";
   Pixmap_Close       : constant String := "Close";
   Pixmap_Revert      : constant String := "Revert";
   Pixmap_Save        : constant String := "Save";
   Pixmap_Save_As     : constant String := "Save As";
   Pixmap_Cut         : constant String := "Cut";
   Pixmap_Copy        : constant String := "Copy";
   Pixmap_Paste       : constant String := "Paste";
   Pixmap_Clear       : constant String := "Clear";
   Pixmap_Properties  : constant String := "Properties";
   Pixmap_Preferences : constant String := "Preferences";
   Pixmap_Help        : constant String := "Help";
   Pixmap_Scores      : constant String := "Scores";
   Pixmap_Print       : constant String := "Print";
   Pixmap_Search      : constant String := "Search";
   Pixmap_Srchrpl     : constant String := "Search/Replace";
   Pixmap_Back        : constant String := "Back";
   Pixmap_Forward     : constant String := "Forward";
   Pixmap_First       : constant String := "First";
   Pixmap_Last        : constant String := "Last";
   Pixmap_Home        : constant String := "Home";
   Pixmap_Stop        : constant String := "Stop";
   Pixmap_Refresh     : constant String := "Refresh";
   Pixmap_Undo        : constant String := "Undo";
   Pixmap_Redo        : constant String := "Redo";
   Pixmap_Timer       : constant String := "Timer";
   Pixmap_Timer_Stop  : constant String := "Timer Stopped";
   Pixmap_Mail        : constant String := "Mail";
   Pixmap_Mail_Rcv    : constant String := "Receive Mail";
   Pixmap_Mail_Snd    : constant String := "Send Mail";
   Pixmap_Mail_Rpl    : constant String := "Reply to Mail";
   Pixmap_Mail_Fwd    : constant String := "Forward Mail";
   Pixmap_Mail_New    : constant String := "New Mail";
   Pixmap_Trash       : constant String := "Trash";
   Pixmap_Trash_Full  : constant String := "Trash Full";
   Pixmap_Undelete    : constant String := "Undelete";
   Pixmap_Spellcheck  : constant String := "Spellchecker";
   Pixmap_Mic         : constant String := "Microphone";
   Pixmap_Line_In     : constant String := "Line In";
   Pixmap_Cdrom       : constant String := "Cdrom";
   Pixmap_Volume      : constant String := "Volume";
   Pixmap_Midi        : constant String := "Midi";
   Pixmap_Book_Red    : constant String := "Book Red";
   Pixmap_Book_Green  : constant String := "Book Green";
   Pixmap_Book_Blue   : constant String := "Book Blue";
   Pixmap_Book_Yellow : constant String := "Book Yellow";
   Pixmap_Book_Open   : constant String := "Book Open";
   Pixmap_About       : constant String := "About";
   Pixmap_Quit        : constant String := "Quit";
   Pixmap_Multiple    : constant String := "Multiple";
   Pixmap_Not         : constant String := "Not";
   Pixmap_Convert     : constant String := "Convert";
   Pixmap_Jump_To     : constant String := "Jump To";
   Pixmap_Up          : constant String := "Up";
   Pixmap_Down        : constant String := "Down";
   Pixmap_Top         : constant String := "Top";
   Pixmap_Bottom      : constant String := "Bottom";
   Pixmap_Attach      : constant String := "Attach";
   Pixmap_Index       : constant String := "Index";
   Pixmap_Font        : constant String := "Font";
   Pixmap_Exec        : constant String := "Exec";

   Pixmap_Align_Left     : constant String := "Left";
   Pixmap_Align_Right    : constant String := "Right";
   Pixmap_Align_Center   : constant String := "Center";
   Pixmap_AlIgn_Justify  : constant String := "Justify";

   Pixmap_Text_Bold      : constant String := "Bold";
   Pixmap_Text_Italic    : constant String := "Italic";
   Pixmap_Text_Underline : constant String := "Underline";
   Pixmap_Text_Strikeout : constant String := "Strikeout";

   Pixmap_Text_Indent    : constant String := "Text Indent";
   Pixmap_Text_Unindent  : constant String := "Text Unindent";

   Pixmap_Exit           : constant String := Pixmap_Quit;

   Pixmap_Colorselector  : constant String := "Color Select";

   Pixmap_Add            : constant String := "Add";
   Pixmap_Remove         : constant String := "Remove";

   Pixmap_Table_Borders  : constant String := "Table Borders";
   Pixmap_Table_Fill     : constant String := "Table Fill";

   Pixmap_Text_Bulleted_List : constant String :=
     "Text Bulleted List";
   Pixmap_Text_Numbered_List : constant String :=
     "Text Numbered List";

   --  The basic pixmap version of an icon.

   Pixmap_Regular  : constant String := "regular";
   Pixmap_Disabled : constant String := "disabled";
   Pixmap_Focused  : constant String := "focused";

   --  Buttons

   Button_Ok     : constant String := "Button_Ok";
   Button_Cancel : constant String := "Button_Cancel";
   Button_Yes    : constant String := "Button_Yes";
   Button_No     : constant String := "Button_No";
   Button_Close  : constant String := "Button_Close";
   Button_Apply  : constant String := "Button_Apply";
   Button_Help   : constant String := "Button_Help";
   Button_Next   : constant String := "Button_Next";
   Button_Prev   : constant String := "Button_Prev";
   Button_Up     : constant String := "Button_Up";
   Button_Down   : constant String := "Button_Down";
   Button_Font   : constant String := "Button_Font";

   --  Menus

   Menu_Blank        : constant String := "Menu_";
   Menu_New          : constant String := "Menu_New";
   Menu_Save         : constant String := "Menu_Save";
   Menu_Save_As      : constant String := "Menu_Save As";
   Menu_Revert       : constant String := "Menu_Revert";
   Menu_Open         : constant String := "Menu_Open";
   Menu_Close        : constant String := "Menu_Close";
   Menu_Quit         : constant String := "Menu_Quit";
   Menu_Cut          : constant String := "Menu_Cut";
   Menu_Copy         : constant String := "Menu_Copy";
   Menu_Paste        : constant String := "Menu_Paste";
   Menu_Prop         : constant String := "Menu_Properties";
   Menu_Pref         : constant String := "Menu_Preferences";
   Menu_About        : constant String := "Menu_About";
   Menu_Scores       : constant String := "Menu_Scores";
   Menu_Undo         : constant String := "Menu_Undo";
   Menu_Redo         : constant String := "Menu_Redo";
   Menu_Print        : constant String := "Menu_Print";
   Menu_Search       : constant String := "Menu_Search";
   Menu_Srchrpl      : constant String := "Menu_Search/Replace";
   Menu_Back         : constant String := "Menu_Back";
   Menu_Forward      : constant String := "Menu_Forward";
   Menu_First        : constant String := "Menu_First";
   Menu_Last         : constant String := "Menu_Last";
   Menu_Home         : constant String := "Menu_Home";
   Menu_Stop         : constant String := "Menu_Stop";
   Menu_Refresh      : constant String := "Menu_Refresh";
   Menu_Mail         : constant String := "Menu_Mail";
   Menu_Mail_Rcv     : constant String := "Menu_Receive Mail";
   Menu_Mail_Snd     : constant String := "Menu_Send Mail";
   Menu_Mail_Rpl     : constant String := "Menu_Reply to Mail";
   Menu_Mail_Fwd     : constant String := "Menu_Forward Mail";
   Menu_Mail_New     : constant String := "Menu_New Mail";
   Menu_Trash        : constant String := "Menu_Trash";
   Menu_Trash_Full   : constant String := "Menu_Trash Full";
   Menu_Undelete     : constant String := "Menu_Undelete";
   Menu_Timer        : constant String := "Menu_Timer";
   Menu_Timer_Stop   : constant String := "Menu_Timer Stopped";
   Menu_Spellcheck   : constant String := "Menu_Spellchecker";
   Menu_Mic          : constant String := "Menu_Microphone";
   Menu_Line_In      : constant String := "Menu_Line In";
   Menu_Cdrom        : constant String := "Menu_Cdrom";
   Menu_Volume       : constant String := "Menu_Volume";
   Menu_Midi         : constant String := "Menu_Midi";
   Menu_Book_Red     : constant String := "Menu_Book Red";
   Menu_Book_Green   : constant String := "Menu_Book Green";
   Menu_Book_Blue    : constant String := "Menu_Book Blue";
   Menu_Book_Yellow  : constant String := "Menu_Book Yellow";
   Menu_Book_Open    : constant String := "Menu_Book Open";
   Menu_Convert      : constant String := "Menu_Convert";
   Menu_Jump_To      : constant String := "Menu_Jump To";
   Menu_Up           : constant String := "Menu_Up";
   Menu_Down         : constant String := "Menu_Down";
   Menu_Top          : constant String := "Menu_Top";
   Menu_Bottom       : constant String := "Menu_Bottom";
   Menu_Attach       : constant String := "Menu_Attach";
   Menu_Index        : constant String := "Menu_Index";
   Menu_Font         : constant String := "Menu_Font";
   Menu_Exec         : constant String := "Menu_Exec";

   Menu_Align_Left     : constant String := "Menu_Left";
   Menu_Align_Right    : constant String := "Menu_Right";
   Menu_Align_Center   : constant String := "Menu_Center";
   Menu_Align_Justify  : constant String := "Menu_Justify";

   Menu_Text_Bold      : constant String := "Menu_Bold";
   Menu_Text_Italic    : constant String := "Menu_Italic";
   Menu_Text_Underline : constant String := "Menu_Underline";
   Menu_Text_Strikeout : constant String := "Menu_Strikeout";

   Menu_Exit           : constant String := Menu_Quit;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Stock_Record is new
     Gnome.Pixmap.Gnome_Pixmap_Record with null record;

   pragma Import (C, Get_Type, "gnome_stock_get_type");
end Gnome.Stock;
