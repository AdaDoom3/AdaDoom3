-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000-2003                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                  Copyright (C) 2004-2013, AdaCore                 --
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

with Ada.Unchecked_Conversion;
with Common;            use Common;
with Gdk.Bitmap;        use Gdk.Bitmap;
with Gdk.Color;         use Gdk.Color;
with Gdk.Drawable;      use Gdk.Drawable;
with Gdk.Event;         use Gdk.Event;
with Gdk.GC;            use Gdk.GC;
with Gdk.Pixmap;        use Gdk.Pixmap;
with Gdk.Rectangle;     use Gdk.Rectangle;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Types;         use Gdk.Types;
with Gdk.Window;        use Gdk.Window;
with Glib;              use Glib;
with Gtk.Arguments;     use Gtk.Arguments;
with Gtk.Box;           use Gtk.Box;
with Gtk.Button;        use Gtk.Button;
with Gtk.Combo;         use Gtk.Combo;
with Gtk.Curve;         use Gtk.Curve;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Extra.Border_Combo;  use Gtk.Extra.Border_Combo;
with Gtk.Extra.Color_Combo;   use Gtk.Extra.Color_Combo;
with Gtk.Extra.Font_Combo;    use Gtk.Extra.Font_Combo;
with Gtk.Extra.Item_Entry;    use Gtk.Extra.Item_Entry;
with Gtk.Extra.Sheet;   use Gtk.Extra.Sheet;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Label;         use Gtk.Label;
with Gtk.Menu;          use Gtk.Menu;
with Gtk.Menu_Item;     use Gtk.Menu_Item;
with Gtk.Notebook;      use Gtk.Notebook;
with Gtk.Image;         use Gtk.Image;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Toolbar;       use Gtk.Toolbar;
with Gtk.Widget;        use Gtk.Widget;
with Pango.Font;        use Pango.Font;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Gtk.Type_Conversion;
pragma Warnings (Off, Gtk.Type_Conversion);
--  Required because we get the entry from a sheet.

with Unchecked_Conversion;
with System;
with Ada.Text_IO;       use Ada.Text_IO;

package body Create_Sheet is

   package Sheet_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Sheet_Record, Boolean);
   package Item_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Menu_Item_Record, String);

   package ICS renames Interfaces.C.Strings;
   function Ns (S : String) return Interfaces.C.Strings.chars_ptr
               renames Interfaces.C.Strings.New_String;
   function To_Range is new Unchecked_Conversion
     (System.Address, Gtk_Sheet_Range);

   type Gint_Access is access Gint;
   function To_Gint_Access is new Unchecked_Conversion
     (System.Address, Gint_Access);

   Sheets           : array (0 .. Gint'(2)) of Gtk_Sheet;
   Scrolled_Windows : array (0 .. Gint'(2)) of Gtk_Scrolled_Window;
   Notebook         : Gtk_Notebook;
   Left_Button      : Gtk_Toggle_Button;
   Center_Button    : Gtk_Toggle_Button;
   Right_Button     : Gtk_Toggle_Button;
   Curve            : Gtk_Curve;
   Bg_Pixmap        : Gtk_Image;
   Fg_Pixmap        : Gtk_Image;
   GEntry           : Gtk_Entry;
   Location         : Gtk_Label;
   Popup            : Gtk_Menu;

   Bullet_Xpm : constant ICS.chars_ptr_array :=
     (Ns ("16 16 26 1"),
      Ns ("       c #FFFFFFFFFFFF"),
      Ns (".      c #000000000000"),
      Ns ("X      c #0000E38D0000"),
      Ns ("o      c #0000EBAD0000"),
      Ns ("O      c #0000F7DE0000"),
      Ns ("+      c #0000FFFF0000"),
      Ns ("@      c #0000CF3C0000"),
      Ns ("#      c #0000D75C0000"),
      Ns ("$      c #0000B6DA0000"),
      Ns ("%      c #0000C30B0000"),
      Ns ("&      c #0000A2890000"),
      Ns ("*      c #00009A690000"),
      Ns ("=      c #0000AEBA0000"),
      Ns ("-      c #00008E380000"),
      Ns (";      c #000086170000"),
      Ns (":      c #000079E70000"),
      Ns (">      c #000071C60000"),
      Ns (",      c #000065950000"),
      Ns ("<      c #000059650000"),
      Ns ("1      c #000051440000"),
      Ns ("2      c #000045140000"),
      Ns ("3      c #00003CF30000"),
      Ns ("4      c #000030C20000"),
      Ns ("5      c #000028A20000"),
      Ns ("6      c #00001C710000"),
      Ns ("7      c #000014510000"),
      Ns ("     ......     "),
      Ns ("    .XooO++.    "),
      Ns ("  ..@@@#XoO+..  "),
      Ns (" .$$$$$%@#XO++. "),
      Ns (" .&&*&&=$%@XO+. "),
      Ns (".*-;;;-*&=%@XO+."),
      Ns (".;:>>>:;-&=%#o+."),
      Ns (".>,<<<,>:-&$@XO."),
      Ns (".<12321<>;*=%#o."),
      Ns (".1345431,:-&$@o."),
      Ns (".2467642<>;&$@X."),
      Ns (" .57.753<>;*$@. "),
      Ns (" .467642<>;&$@. "),
      Ns ("  ..5431,:-&..  "),
      Ns ("    .21<>;*.    "),
      Ns ("     ......     "));

   Center_Just : constant ICS.chars_ptr_array :=
     (Ns ("26 26 2 1"),
      Ns (".      c #None"),
      Ns ("X      c #000000000000"),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("        XXXXXXXXXXXX        "),
      Ns ("        XXXXXXXXXXXX        "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("        XXXXXXXXXXXX        "),
      Ns ("        XXXXXXXXXXXX        "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("        XXXXXXXXXXXX        "),
      Ns ("        XXXXXXXXXXXX        "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "));

   Font : constant ICS.chars_ptr_array :=
     (Ns ("26 26 3 1"),
      Ns ("       c #None"),
      Ns (".      c #000000000000"),
      Ns ("X      c #000000000000"),
      Ns ("                          "),
      Ns ("                          "),
      Ns ("                          "),
      Ns ("            .             "),
      Ns ("           ...            "),
      Ns ("           ...            "),
      Ns ("          .....           "),
      Ns ("          .....           "),
      Ns ("         .. ....          "),
      Ns ("         .. ....          "),
      Ns ("        ..   ....         "),
      Ns ("        .........         "),
      Ns ("       ...........        "),
      Ns ("       ..     ....        "),
      Ns ("      ..       ....       "),
      Ns ("      ..       ....       "),
      Ns ("    .....     .......     "),
      Ns ("                          "),
      Ns ("                          "),
      Ns ("                          "),
      Ns ("     XXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXX     "),
      Ns ("                          "),
      Ns ("                          "),
      Ns ("                          "));

   Left_Just : constant ICS.chars_ptr_array :=
     (Ns ("26 26 2 1"),
      Ns (".      c #None"),
      Ns ("X      c #000000000000"),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXX          "),
      Ns ("     XXXXXXXXXXXXX          "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXX          "),
      Ns ("     XXXXXXXXXXXXX          "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXX          "),
      Ns ("     XXXXXXXXXXXXX          "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "));

   Paint : constant ICS.chars_ptr_array :=
     (Ns ("26 26 6 1"),
      Ns (".      c #None"),
      Ns ("a      c #000000000000"),
      Ns ("e      c #929292929292"),
      Ns ("g      c #DBDBDBDBDBDB"),
      Ns ("h      c #FFFFFFFFFFFF"),
      Ns ("X      c #FFFFFFFFFFFF"),
      Ns (".........................."),
      Ns ("...........ee............."),
      Ns ("..........eeee............"),
      Ns (".........eeggee..........."),
      Ns (".........eegaee..........."),
      Ns (".........eeahee..........."),
      Ns (".........aahheeaa........."),
      Ns (".........ahhgeegaaa......."),
      Ns ("........ahhghaeggaaa......"),
      Ns (".......ahhghagaggeaaa....."),
      Ns ("......ahhghggaggeeaaae...."),
      Ns (".....ahhghgggggeeaaaae...."),
      Ns (".....ahghgggggeeaeaaae...."),
      Ns ("......ahgggggeeaeeaaae...."),
      Ns (".......ahgggeeaee.aaae...."),
      Ns ("........aggeeaee..aaee...."),
      Ns (".........aeeaee...aee....."),
      Ns ("..........aaee.....e......"),
      Ns ("...........ee............."),
      Ns (".........................."),
      Ns ("....XXXXXXXXXXXXXXXXXX...."),
      Ns ("....XXXXXXXXXXXXXXXXXX...."),
      Ns ("....XXXXXXXXXXXXXXXXXX...."),
      Ns ("....XXXXXXXXXXXXXXXXXX...."),
      Ns (".........................."));

   Right_Just : constant ICS.chars_ptr_array :=
     (Ns ("26 26 2 1"),
      Ns (".      c #None"),
      Ns ("X      c #000000000000"),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("          XXXXXXXXXXXXX     "),
      Ns ("          XXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("          XXXXXXXXXXXXX     "),
      Ns ("          XXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("     XXXXXXXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("          XXXXXXXXXXXXX     "),
      Ns ("          XXXXXXXXXXXXX     "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "),
      Ns ("                            "));

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Sheet@B is an improved version of a @bGtk_Table@B."
        & " Like a table, it can contain children associated with its"
        & " cells, but provides a much more efficient way to store text"
        & " since you don't need to create a widget for every cell."
        & ASCII.LF
        & "In this example, the table has 1000 lines."
        & ASCII.LF
        & "You can highlight a specific range in the table by using the"
        & " Clip and Unclip subprograms, bound to Ctrl-C and Ctrl-X in this"
        & " demo."
        & ASCII.LF
        & "This demo also integrates the following widgets:"
        & " @bGtk_Border_Combo@B, @bGtk_Color_Combo@B, @bGtk_Font_Combo@B"
        & " @bGtk_IEntry@B"
        & ASCII.LF
        & "Note also that only the table itself is a @bGtk_Sheet@B. For"
        & " instance, the entry at the top of the screen is not part of the"
        & " sheet, and is managed through callbacks (see the code).";
   end Help;

   ------------------------
   -- Do_Hide_Row_Titles --
   ------------------------

   procedure Do_Hide_Row_Titles (Button : access Gtk_Button_Record'Class) is
      pragma Warnings (Off, Button);
      Cur_Page : Gint;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Hide_Row_Titles (Sheets (Cur_Page));
   end Do_Hide_Row_Titles;

   ---------------------------
   -- Do_Hide_Column_Titles --
   ---------------------------

   procedure Do_Hide_Column_Titles (Button : access Gtk_Button_Record'Class) is
      pragma Warnings (Off, Button);
      Cur_Page : Gint;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Hide_Column_Titles (Sheets (Cur_Page));
   end Do_Hide_Column_Titles;

   ------------------------
   -- Do_Show_Row_Titles --
   ------------------------

   procedure Do_Show_Row_Titles (Button : access Gtk_Button_Record'Class) is
      pragma Warnings (Off, Button);
      Cur_Page : Gint;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Show_Row_Titles (Sheets (Cur_Page));
   end Do_Show_Row_Titles;

   ---------------------------
   -- Do_Show_Column_Titles --
   ---------------------------

   procedure Do_Show_Column_Titles (Button : access Gtk_Button_Record'Class) is
      pragma Warnings (Off, Button);
      Cur_Page : Gint;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Show_Column_Titles (Sheets (Cur_Page));
   end Do_Show_Column_Titles;

   ------------------
   -- Justify_Left --
   ------------------

   procedure Justify_Left (Button : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Button);
      Cur_Page : Gint;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Set_Active (Left_Button, True);
      Set_Active (Right_Button, False);
      Set_Active (Center_Button, False);
      Range_Set_Justification (Sheets (Cur_Page),
                               Get_Range (Sheets (Cur_Page)),
                               Justify_Left);
   end Justify_Left;

   --------------------
   -- Justify_Center --
   --------------------

   procedure Justify_Center (Button : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Button);
      Cur_Page : Gint;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Set_Active (Left_Button, False);
      Set_Active (Right_Button, False);
      Set_Active (Center_Button, True);
      Range_Set_Justification (Sheets (Cur_Page),
                               Get_Range (Sheets (Cur_Page)),
                               Justify_Center);
   end Justify_Center;

   -------------------
   -- Justify_Right --
   -------------------

   procedure Justify_Right (Button : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Button);
      Cur_Page : Gint;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Set_Active (Left_Button, False);
      Set_Active (Right_Button, True);
      Set_Active (Center_Button, False);
      Range_Set_Justification (Sheets (Cur_Page),
                               Get_Range (Sheets (Cur_Page)),
                               Justify_Right);
   end Justify_Right;

   ----------------
   -- Show_Entry --
   ----------------

   procedure Show_Entry  (Widget : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Widget);
      Cur_Page : Gint;
      Sheet_Entry : Gtk_Entry;
   begin
      if not Flag_Is_Set (Widget, Has_Focus) then
         return;
      end if;

      Cur_Page := Get_Current_Page (Notebook);
      Sheet_Entry := Gtk_Entry (Get_Entry (Sheets (Cur_Page)));
      Set_Text (GEntry, Get_Text (Sheet_Entry));
   end Show_Entry;

   -------------------------
   -- Activate_Sheet_Cell --
   -------------------------
   --  Called whenever a new cell has been selected (through the mouse
   --  or the keyboard). It updates the coordinates as well the entry
   --  that reflects the contents of the cell.

   function Activate_Sheet_Cell (Sheet  : access Gtk_Sheet_Record'Class;
                                 Params : Gtk.Arguments.Gtk_Args)
                                return Boolean
   is
      pragma Warnings (Off, Params);
      Row         : constant Gint := To_Gint (Params, 1);
      Column      : constant Gint := To_Gint (Params, 2);
      Arow        : Gint;
      Acol        : Gint;
   begin
      declare
         S : constant String := Get_Column_Title (Sheet, Column);
      begin
         if S = "" then
            Set_Text (Location, "Row:" & Gint'Image (Row)
                      & " Column:" & Gint'Image (Column));
         else
            Set_Text (Location, "Row:" & Gint'Image (Row) & " Column:" & S);
         end if;
      end;

      --  Set_Max_Length (GEntry, Get_Max_Length (Sheet_Entry));
      Set_Text (GEntry, Cell_Get_Text (Sheet, Row, Column));

      Get_Active_Cell (Sheet, Arow, Acol);
      --  Get_Attributes (Sheet, Row, Col, Attributes);
      --  Set_Editable (GEntry, Get_Is_Editable (Attributes));
--        case Get_Justification (Attributes) is
--       when Justify_Left   => Justify_Left (Sheet);
--       when Justify_Center => Justify_Center (Sheet);
--       when Justify_Right  => Justify_Right (Sheet);
--       when others         => Justify_Left (Sheet);
--        end case;

      return True;
   end Activate_Sheet_Cell;

   -----------------------
   -- Clipboard_Handler --
   -----------------------

   function Clipboard_Handler
     (Sheet : access Gtk_Sheet_Record'Class;
      Key   : Gdk_Event)
      return Boolean
   is
   begin
      if (Get_State (Key) and Control_Mask) /= 0 then
         if Get_Key_Val (Key) = GDK_C
           or else Get_Key_Val (Key) = GDK_LC_c
         then
            Clip_Range (Sheet, Get_Range (Sheet));
         elsif Get_Key_Val (Key) = GDK_X
           or else Get_Key_Val (Key) = GDK_LC_x
         then
            Unclip_Range (Sheet);
         end if;
      end if;
      return False;
   end Clipboard_Handler;

   --------------------
   -- Resize_Handler --
   --------------------

   procedure Resize_Handler (Widget : access Gtk_Widget_Record'Class;
                             Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Warnings (Off, Widget);
      Old_Range : constant Gtk_Sheet_Range :=
        To_Range (To_Address (Params, 1));
      New_Range : constant Gtk_Sheet_Range :=
        To_Range (To_Address (Params, 2));
   begin
      Put_Line ("Resize: Old selection: "
                & Gint'Image (Old_Range.Row0)
                & Gint'Image (Old_Range.Col0)
                & Gint'Image (Old_Range.Rowi)
                & Gint'Image (Old_Range.Coli));
      Put_Line ("New selection: "
                & Gint'Image (New_Range.Row0)
                & Gint'Image (New_Range.Col0)
                & Gint'Image (New_Range.Rowi)
                & Gint'Image (New_Range.Coli));
   end Resize_Handler;

   ------------------
   -- Move_Handler --
   ------------------

   procedure Move_Handler (Widget : access Gtk_Widget_Record'Class;
                           Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Warnings (Off, Widget);
      Old_Range : constant Gtk_Sheet_Range :=
        To_Range (To_Address (Params, 1));
      New_Range : constant Gtk_Sheet_Range :=
        To_Range (To_Address (Params, 2));
   begin
      Put_Line ("Move: Old selection: "
                & Gint'Image (Old_Range.Row0)
                & Gint'Image (Old_Range.Col0)
                & Gint'Image (Old_Range.Rowi)
                & Gint'Image (Old_Range.Coli));
      Put_Line ("New selection: "
                & Gint'Image (New_Range.Row0)
                & Gint'Image (New_Range.Col0)
                & Gint'Image (New_Range.Rowi)
                & Gint'Image (New_Range.Coli));
   end Move_Handler;

   --------------------
   -- Alarm_Traverse --
   --------------------

   function Alarm_Traverse (Widget : access Gtk_Sheet_Record'Class;
                            Params : Gtk.Arguments.Gtk_Args)
                           return Boolean
   is
      pragma Warnings (Off, Widget);
      Row : constant Gint := To_Gint (Params, 1);
      Col : constant Gint := To_Gint (Params, 2);
      New_Row : constant Gint_Access :=
        To_Gint_Access (To_Address (Params, 3));
      New_Col : constant Gint_Access :=
        To_Gint_Access (To_Address (Params, 4));
   begin
      Put_Line ("Traverse: from ("
                & Gint'Image (Row) & ","
                & Gint'Image (Col) & ") to ("
                & Gint'Image (New_Row.all) & ","
                & Gint'Image (New_Col.all) & ")");
      return True;
   end Alarm_Traverse;

   ----------------
   -- Show_Child --
   ----------------

   procedure Show_Child (Button : access Gtk_Button_Record'Class) is
      pragma Warnings (Off, Button);
   begin
      if not Flag_Is_Set (Curve, Mapped) then
         Attach (Sheets (0), Curve, 0, 4);
      end if;
   end Show_Child;

   ---------------------
   -- Popup_Activated --
   ---------------------
   --  Callback for all the items in the contextual menu of the second
   --  example.
   --  Data is the name of the item.
   --  We already know that the item is valid, since otherwise it would not
   --  have been activated in the contextual menu (see Build_Menu).

   procedure Popup_Activated (Item : access Gtk_Menu_Item_Record'Class;
                              Data : String)
   is
      pragma Warnings (Off, Item);
      Cur_Page : constant Gint := Get_Current_Page (Notebook);
      Sheet    : constant Gtk_Sheet := Sheets (Cur_Page);
   begin
      if Data = "Add Column   " then
         Add_Column (Sheet, 1);

      elsif Data = "Add Row      " then
         Add_Row (Sheet, 1);

      elsif Data = "Insert Row   " then
         Insert_Rows
           (Sheet,
            Guint (Get_Range (Sheet).Row0),
            Guint (Get_Range (Sheet).Rowi - Get_Range (Sheet).Row0 + 1));

      elsif Data = "Insert Column" then
         Insert_Columns
           (Sheet,
            Guint (Get_Range (Sheet).Col0),
            Guint (Get_Range (Sheet).Coli - Get_Range (Sheet).Col0 + 1));

      elsif Data = "Delete Row   " then
         if Get_State (Sheet) = Sheet_Row_Selected then
            Delete_Rows
              (Sheet,
               Guint (Get_Range (Sheet).Row0),
               Guint (Get_Range (Sheet).Rowi - Get_Range (Sheet).Row0 + 1));
         end if;

      elsif Data = "Delete Column" then
         if Get_State (Sheet) = Sheet_Column_Selected then
            Delete_Rows
              (Sheet,
               Guint (Get_Range (Sheet).Col0),
               Guint (Get_Range (Sheet).Coli - Get_Range (Sheet).Col0 + 1));
         end if;

      elsif Data = "Clear Cells  " then
         if Get_State (Sheet) /= Sheet_Normal then
            Range_Clear (Sheet, Get_Range (Sheet));
         end if;
      end if;

      Destroy (Popup);
   end Popup_Activated;

   ----------------
   -- Build_Menu --
   ----------------
   --  Builds a contextual menu.
   --  Some items are not activated if no line or row is selected.

   function Build_Menu (Sheet : access Gtk_Sheet_Record'Class)
                       return Gtk_Menu
   is
      Menu : Gtk_Menu;
      Item : Gtk_Menu_Item;
      Items : constant array (0 .. 6) of String (1 .. 13) :=
        ("Add Column   ",
         "Add Row      ",
         "Insert Row   ",
         "Insert Column",
         "Delete Row   ",
         "Delete Column",
         "Clear Cells  ");
   begin
      Gtk_New (Menu);

      for I in 0 .. 6 loop
         Gtk_New (Item, Items (I));

         Item_Handler.Connect
           (Item, "activate",
            Item_Handler.To_Marshaller (Popup_Activated'Access),
            Items (I));

         Set_Flags (Item, Sensitive + Can_Focus);
         case I is
            when 2 | 4 =>
               --  Can only insert or delete a row if there is a selected one
               if Get_State (Sheet) /= Sheet_Row_Selected then
                  Unset_Flags (Item, Sensitive + Can_Focus);
               end if;

            when 3 | 5 =>
               --  Can only insert or delete a col. if there is a selected one
               if Get_State (Sheet) /= Sheet_Column_Selected then
                  Unset_Flags (Item, Sensitive + Can_Focus);
               end if;

            when others =>
               null;
         end case;

         Show (Item);
         Append (Menu, Item);
      end loop;
      return Menu;
   end Build_Menu;

   --------------
   -- Do_Popup --
   --------------
   --  Popup a contextual menu in the sheet.
   --  The menu is rebuild every time, depending on the context.
   --  This function is used for the second example.

   function Do_Popup (Sheet : access Gtk_Sheet_Record'Class;
                      Event : Gdk_Event)
                     return Boolean
   is
      X, Y : Gint;
      Mods : Gdk_Modifier_Type;
      Win  : Gdk_Window;
   begin
      Get_Pointer (Get_Window (Sheet), X, Y, Mods, Win);

      if (Mods and Button3_Mask) /= 0 then

         --  Destroy the previous popup
         if Popup /= null then
            Unref (Popup);
         end if;

         --  Build the new one.
         Popup := Build_Menu (Sheet);
         Gtk.Menu.Popup (Menu          => Popup,
                         Button        => Get_Button (Event),
                         Activate_Time => Get_Time (Event));
      end if;
      return True;
   end Do_Popup;

   -----------------
   -- Format_Text --
   -----------------
   --  Returns a modified version of Text.
   --  This is used to alter the contents of cells in the second demo.

   function Format_Text (Sheet         : access Gtk_Sheet_Record'Class;
                         Text          : String)
                        return String
   is
      pragma Warnings (Off, Sheet);
      type Cell_Format is (Text_Format, Numeric_Format);
      Format     : Cell_Format := Numeric_Format;
   begin
      if Text /= "" then
         for Ipos in Text'Range loop
            case Text (Ipos) is
               when '.' | ' ' | ',' | '-' | '+'
                 | 'd' | 'D' | 'E' | 'e' | '1' | '2' | '3' | '4'
                 | '5' | '6' | '7' | '8' | '9' | '0' =>
                  null;

               when others =>
                  Format := Text_Format;
            end case;
         end loop;

         if Format = Text_Format then
            return Text;
         else
            return "Floating: " & Text;
         end if;
      end if;
      return Text;
   end Format_Text;

   -------------------
   -- Parse_Numbers --
   -------------------
   --  In the second demo, this callback is called every time the content of a
   --  cell is changed.
   --  This is used to automatically change the contents of the cell.

   procedure Parse_Numbers (Widget : access Gtk_Widget_Record'Class) is
      Sheet : constant Gtk_Sheet := Gtk_Sheet (Widget);
   begin
      declare
         Label : constant String := Format_Text
           (Sheet, Get_Text (Gtk_IEntry (Get_Entry (Sheet))));
         Row,
         Col   : Gint;
      begin
         Get_Active_Cell (Sheet, Row, Col);
         Set_Cell (Sheet, Row, Col, Justify_Left, Label);
      end;
   end Parse_Numbers;

   --------------------
   -- Build_Example2 --
   --------------------

   procedure Build_Example2 (Sheet : access Gtk_Sheet_Record'Class) is
      R : aliased Gtk_Sheet_Range_Record;
      R2 : constant Gtk_Sheet_Range := R'Unchecked_Access;
      Color : Gdk_Color;
      Tmp   : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Set_Autoscroll (Sheet, False);
      Set_Selection_Mode (Sheet, Selection_Single);

      R := (Row0 => 0, Rowi => 2,
            Col0 => 0, Coli => Gint (Get_Columns_Count (Sheet)));
      Range_Set_Editable (Sheet, R2, False);
      Color := Parse ("light gray");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Background (Sheet, R2, Color);
      Color := Parse ("blue");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Foreground (Sheet, R2, Color);

      R.Row0 := 1;
      Color := Parse ("red");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Foreground (Sheet, R2, Color);

      R.Row0 := 2;
      Color := Parse ("black");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Foreground (Sheet, R2, Color);

      --  The first three rows can not be edited
      Row_Set_Sensitivity (Sheet, 0, False);
      Row_Set_Sensitivity (Sheet, 1, False);
      Row_Set_Sensitivity (Sheet, 2, False);

      Set_Cell (Sheet, 0, 2, Justify_Center,
                "Click the right mouse button to display a popup");
      Set_Cell (Sheet, 1, 2, Justify_Center,
                "You can connect a parser to the 'set cell' signal");
      Set_Cell (Sheet, 2, 2, Justify_Center,
                "(Try typing numbers)");
      Tmp := Set_Active_Cell (Sheet, 3, 0);

      Sheet_Cb.Connect (Sheet, "button_press_event",
                        Sheet_Cb.To_Marshaller (Do_Popup'Access));
      Widget_Handler.Connect
        (Sheet, "set_cell",
         Widget_Handler.To_Marshaller (Parse_Numbers'Access));
   end Build_Example2;

   --------------------
   -- Build_Example3 --
   --------------------

   procedure Build_Example3 (Sheet : access Gtk_Sheet_Record'Class) is
      R  : aliased Gtk_Sheet_Range_Record;
      R2 : constant Gtk_Sheet_Range := R'Unchecked_Access;
      Color : Gdk_Color;
   begin
      R := (Row0 => 0, Rowi => 10, Col0 => 0, Coli => 6);
      Color := Parse ("orange");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Background (Sheet, R2, Color);

      Color := Parse ("Violet");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Foreground (Sheet, R2, Color);

      R.Row0 := 1;
      Color := Parse ("blue");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Background (Sheet, R2, Color);

      R.Coli := 0;
      Color := Parse ("dark green");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Background (Sheet, R2, Color);

      R.Row0 := 0;
      Color := Parse ("dark blue");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Border_Color (Sheet, R2, Color);
      Range_Set_Border (Sheet, R2, Right_Border, 4, Line_Double_Dash);

      R.Coli := 0;
      R.Col0 := 0;
      R.Rowi := 0;
      Color := Parse ("red");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Border
        (Sheet, R2, Right_Border + Bottom_Border, 4, Line_Solid);

      R.Rowi := 0;
      R.Col0 := 1;
      R.Coli := 6;
      Color := Parse ("dark blue");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Border_Color (Sheet, R2, Color);
      Range_Set_Border (Sheet, R2, Bottom_Border, 4, Line_Double_Dash);

      Set_Autoresize (Sheet, True);

      --  Change the type of entries
      Change_Entry (Sheet, Gtk.Combo.Get_Type);
   end Build_Example3;

   --------------------
   -- Build_Example1 --
   --------------------

   procedure Build_Example1 (Sheet : access Gtk_Sheet_Record'Class) is
      Font_Name1 : constant String := "Arial 36";
      Font_Name2 : constant String := "Arial 28";
      Colormap : constant Gdk.Color.Gdk_Colormap := Get_Default_Colormap;
      R : aliased Gtk_Sheet_Range_Record;
      R2 : constant Gtk_Sheet_Range := R'Unchecked_Access;
      Color : Gdk_Color;
      Pixmap : Gdk_Pixmap;
      Bullet : array (0 .. 5) of Gtk_Image;
      Mask   : Gdk_Bitmap;
      Show_Button : Gtk_Button;
   begin
      for I in 0 .. Gint (Get_Columns_Count (Sheet)) - 1 loop
         Column_Button_Add_Label (Sheet, I, "A" & Gint'Image (I));
         Set_Column_Title (Sheet, I, "A" & Gint'Image (I));
      end loop;

      Row_Button_Add_Label (Sheet, 0,
                            "This is " & ASCII.LF & "a multiline"
                            & ASCII.LF & "label");
      Row_Button_Justify (Sheet, 0, Justify_Right);

      R := (Row0 => 1, Rowi => 2, Col0 => 1, Coli => 3);
      Clip_Range (Sheet, R2);
      Range_Set_Font (Sheet, R2, From_String (Font_Name2));
      Color := Parse ("red");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Foreground (Sheet, R2, Color);

      Set_Cell (Sheet, 1, 2, Justify_Center, "Welcome to");

      R.Row0 := 2;
      Range_Set_Font (Sheet, R2, From_String (Font_Name1));
      Color := Parse ("blue");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Foreground (Sheet, R2, Color);

      Set_Cell (Sheet, 2, 2, Justify_Center, "GtkSheet");

      R := (Row0 => 3, Rowi => 3, Col0 => 0, Coli => 4);
      Color := Parse ("dark gray");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Background (Sheet, R2, Color);
      Color := Parse ("green");
      Alloc (Get_Colormap (Sheet), Color);
      Range_Set_Foreground (Sheet, R2, Color);

      Set_Cell (Sheet, 3, 2, Justify_Center, "a Matrix widget for GtkAda");

      Set_Cell (Sheet, 4, 1, Justify_Left,
                "GtkSheet is a matrix where you can allocate cells of text.");
      Set_Cell (Sheet, 5, 1, Justify_Left,
                "Cell contents can be edited interactively with an specially"
                & " designed entry");
      Set_Cell (Sheet, 6, 1, Justify_Left,
                "You can change colors, borders, and many other attributes");
      Set_Cell (Sheet, 7, 1, Justify_Left,
                "Drag & drop or resize the selection clicking the corner "
                & "or border");
      Set_Cell (Sheet, 8, 1, Justify_Left,
                "Clip the selection with Ctrl-C (unclip with Ctrl-X)");
      Set_Cell (Sheet, 9, 1, Justify_Left,
                "(The selection handler has not been implemented yet)");
      Set_Cell (Sheet, 10, 1, Justify_Left,
                "You can add buttons, charts, pixmaps, and other widgets");

      Sheet_Cb.Connect (Sheet, "key_press_event",
                        Sheet_Cb.To_Marshaller (Clipboard_Handler'Access));
      Widget_Handler.Connect (Sheet, "resize_range",
                              Resize_Handler'Access);
      Widget_Handler.Connect (Sheet, "move_range",
                              Move_Handler'Access);
      Sheet_Cb.Connect (Sheet, "traverse",
                        Alarm_Traverse'Access);

      Gtk_New (Curve);
      Set_Range (Curve, 0.0, 200.0, 0.0, 200.0);
      Show (Curve);

      Create_From_Xpm_D (Pixmap,
                         Null_Window,
                         Colormap,
                         Mask,
                         Null_Color,
                         Bullet_Xpm);
      for I in 0 .. 4 loop
         Gtk_New (Bullet (I), Pixmap, Mask);
         Show (Bullet (I));
         Attach (Sheet, Bullet (I), 4 + Gint (I), 0);
      end loop;

      Gtk_New (Bullet (5), Pixmap, Mask);
      Show (Bullet (5));
      Attach (Sheet, Bullet (5), 10, 0);

      Gtk_New (Show_Button, "Show me a plot");
      Show (Show_Button);
      Set_USize (Show_Button, 100, 60);
      Attach (Sheet, Show_Button, 12, 2);

      Button_Handler.Connect
        (Show_Button, "clicked",
         Button_Handler.To_Marshaller (Show_Child'Access));
   end Build_Example1;

   ---------------
   -- Change_Bg --
   ---------------

   procedure Change_Bg (Button : access Gtk_Widget_Record'Class;
                        Params : Gtk.Arguments.Gtk_Args)
   is
      Cur_Page : Gint;
      Current  : Gtk_Sheet;
      Color    : Gdk_Color;
      Color_Name : constant String := To_String (Params, 2);
      Tmp_Gc   : Gdk_GC;
      Pix      : Gdk_Pixmap;
      Mask     : Gdk_Bitmap;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Current  := Sheets (Cur_Page);

      --  Update the sheet
      Color := Parse (Color_Name);
      Alloc (Get_Colormap (Button), Color);
      Range_Set_Background (Current, Get_Range (Current), Color);

      --  Update the button of the combo box
      Gdk_New (Tmp_Gc, Get_Window (Button));
      Set_Foreground (Tmp_Gc, Color);
      Get (Bg_Pixmap, Pix, Mask);
      Draw_Rectangle (Pix, Tmp_Gc, True, 4, 20, 18, 4);
      Draw (Bg_Pixmap, Full_Area);

      Unref (Tmp_Gc);
   end Change_Bg;

   ---------------
   -- Change_Fg --
   ---------------

   procedure Change_Fg (Button : access Gtk_Widget_Record'Class;
                        Params : Gtk.Arguments.Gtk_Args)
   is
      type Gdk_Color_Access is access Gdk.Color.Gdk_Color;
      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Gdk_Color_Access);
      Cur_Page : Gint;
      Current  : Gtk_Sheet;
      Color    : Gdk_Color;
      Color_Access : constant System.Address := To_Address (Params, 2);
      Tmp_Gc   : Gdk_GC;
      Pix      : Gdk_Pixmap;
      Mask     : Gdk_Bitmap;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Current  := Sheets (Cur_Page);

      --  Update the sheet
      Color := Convert (Color_Access).all;
      Range_Set_Foreground (Current, Get_Range (Current), Color);

      --  Update the button of the combo box
      Gdk_New (Tmp_Gc, Get_Window (Button));
      Set_Foreground (Tmp_Gc, Color);
      Get (Fg_Pixmap, Pix, Mask);
      Draw_Rectangle (Pix, Tmp_Gc, True, 5, 20, 16, 4);
      Draw (Fg_Pixmap, Full_Area);
      Unref (Tmp_Gc);
   end Change_Fg;

   --------------
   -- New_Font --
   --------------

   procedure New_Font (Combo : access Gtk_Widget_Record'Class) is
      Cur_Page : Gint;
      Current  : Gtk_Sheet;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Current  := Sheets (Cur_Page);
      Range_Set_Font (Current, Get_Range (Current),
                      Get_Font_Description (Gtk_Font_Combo (Combo)));
   end New_Font;

   -------------------
   -- Change_Border --
   -------------------

   procedure Change_Border (Combo : access Gtk_Widget_Record'Class;
                            Border : Gint)
   is
      pragma Warnings (Off, Combo);
      Cur_Page    : Gint;
      Current     : Gtk_Sheet;
      Border_Mask : Gtk_Sheet_Border;
      R           : Gtk_Sheet_Range;
      Width       : constant Guint := 3;
      Auxcol      : Gint;
      Auxrange    : aliased Gtk_Sheet_Range_Record;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Current  := Sheets (Cur_Page);
      R := Get_Range (Current);
      Range_Set_Border (Current, R, No_Border, 0, Line_Solid);

      case Border is
         when 1 =>
            R.Rowi := R.Row0;
            Range_Set_Border (Current, R, Top_Border, Width, Line_Solid);
         when 2 =>
            R.Row0 := R.Rowi;
            Range_Set_Border (Current, R, Bottom_Border, Width, Line_Solid);
         when 3 =>
            R.Col0 := R.Coli;
            Range_Set_Border (Current, R, Right_Border, Width, Line_Solid);
         when 4 =>
            R.Coli := R.Col0;
            Range_Set_Border (Current, R, Left_Border, Width, Line_Solid);
         when 5 =>
            if R.Col0 = R.Coli then
               Range_Set_Border (Current, R,
                                 Left_Border + Right_Border, Width,
                                 Line_Solid);
            else
               Auxcol := R.Coli;
               R.Coli := R.Col0;
               Range_Set_Border (Current, R, Left_Border, Width, Line_Solid);
               R.Col0 := Auxcol;
               R.Coli := Auxcol;
               Range_Set_Border (Current, R, Right_Border, Width, Line_Solid);
            end if;
         when 6 =>
            if R.Row0 = R.Rowi then
               Range_Set_Border (Current, R,
                                 Top_Border + Bottom_Border,
                                 Width, Line_Solid);
            else
               Auxcol := R.Rowi;
               R.Rowi := R.Row0;
               Range_Set_Border (Current, R, Top_Border, Width, Line_Solid);
               R.Row0 := Auxcol;
               R.Rowi := Auxcol;
               Range_Set_Border (Current, R, Bottom_Border, Width, Line_Solid);
            end if;
         when 7 =>
            Range_Set_Border (Current, R,
                              Right_Border + Left_Border, Width, Line_Solid);
         when 8 =>
            Range_Set_Border (Current, R,
                              Top_Border + Bottom_Border, Width, Line_Solid);
         when 9 =>
            Range_Set_Border (Current, R, All_Borders, Width, Line_Solid);
            for I in R.Row0 .. R.Rowi loop
               for J in R.Col0 .. R.Coli loop
                  Border_Mask := All_Borders;
                  Auxrange := (Row0 => I,
                               Rowi => I,
                               Col0 => J,
                               Coli => J);
                  if I = R.Rowi then
                     Border_Mask := Border_Mask - Bottom_Border;
                  end if;
                  if I = R.Row0 then
                     Border_Mask := Border_Mask - Top_Border;
                  end if;
                  if J = R.Coli then
                     Border_Mask := Border_Mask - Right_Border;
                  end if;
                  if J = R.Col0 then
                     Border_Mask := Border_Mask - Left_Border;
                  end if;
                  if Border_Mask /= All_Borders then
                     Range_Set_Border (Current, Auxrange'Unchecked_Access,
                                       Border_Mask, Width, Line_Solid);
                  end if;
               end loop;
            end loop;
         when 10 =>
            for I in R.Row0 .. R.Rowi loop
               for J in R.Col0 .. R.Coli loop
                  Border_Mask := No_Border;
                  Auxrange := (Row0 => I, Rowi => I,
                               Col0 => J, Coli => J);
                  if I = R.Rowi then
                     Border_Mask := Border_Mask + Bottom_Border;
                  end if;
                  if I = R.Row0 then
                     Border_Mask := Border_Mask + Top_Border;
                  end if;
                  if J = R.Coli then
                     Border_Mask := Border_Mask + Right_Border;
                  end if;
                  if J = R.Col0 then
                     Border_Mask := Border_Mask + Left_Border;
                  end if;
                  if Border_Mask /= No_Border then
                     Range_Set_Border (Current, Auxrange'Unchecked_Access,
                                       Border_Mask, Width, Line_Solid);
                  end if;
               end loop;
            end loop;
         when 11 =>
            Range_Set_Border (Current, R, All_Borders, Width, Line_Solid);
         when others => null;
      end case;
   end Change_Border;

   ----------------------
   -- Show_Sheet_Entry --
   ----------------------
   --  Update the entry used in the cell itself with the content of the
   --  entry displayed at the top of the table.

   procedure Show_Sheet_Entry (Widget : access Gtk_Widget_Record'Class) is
      Cur_Page    : Gint;
      Current     : Gtk_Sheet;
      Sheet_Entry : Gtk_IEntry;
   begin

      --  To prevent an infinite loop with Show_Entry
      if not Flag_Is_Set (Widget, Has_Focus) then
         return;
      end if;

      Cur_Page := Get_Current_Page (Notebook);
      Current  := Sheets (Cur_Page);
      Sheet_Entry := Gtk_IEntry (Get_Entry (Current));
      Set_Text (Sheet_Entry, Get_Text (GEntry));
   end Show_Sheet_Entry;

   --------------------------
   -- Activate_Sheet_Entry --
   --------------------------

   procedure Activate_Sheet_Entry (Widget : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Widget);
      Cur_Page      : Gint;
      Current       : Gtk_Sheet;
      Sheet_Entry   : Gtk_IEntry;
      Row           : Gint;
      Col           : Gint;
   begin
      Cur_Page := Get_Current_Page (Notebook);
      Current  := Sheets (Cur_Page);
      Get_Active_Cell (Current, Row, Col);
      Sheet_Entry := Gtk_IEntry (Get_Entry (Current));
      Set_Cell (Current, Row, Col, Justify_Left, Get_Text (Sheet_Entry));
   end Activate_Sheet_Entry;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Main_Vbox          : Gtk_Box;
      Show_Hide_Box      : Gtk_Box;
      Status_Box         : Gtk_Box;
      Hide_Row_Titles    : Gtk_Button;
      Hide_Column_Titles : Gtk_Button;
      Show_Row_Titles    : Gtk_Button;
      Show_Column_Titles : Gtk_Button;
      Toolbar            : Gtk_Toolbar;
      Label              : Gtk_Label;
      Pixmap             : Gdk_Pixmap;
      Tpixmap            : Gtk_Image;
      Colormap           : constant Gdk_Colormap := Get_Default_Colormap;
      Mask               : Gdk_Bitmap;
      Font_Combo         : Gtk_Font_Combo;
      Border_Combo       : Gtk_Border_Combo;
      Fg_Color_Combo     : Gtk_Color_Combo;
      Bg_Color_Combo     : Gtk_Color_Combo;

   begin
      Gtk.Frame.Set_Label (Frame, "Gtk_Sheet");

      Gtk_New_Vbox (Main_Vbox, Homogeneous => False, Spacing => 1);
      Set_Border_Width (Main_Vbox, 0);
      Add (Frame, Main_Vbox);

      -------
      --  The buttons to hide or show the titles
      -------

      Gtk_New_Hbox (Show_Hide_Box, Homogeneous => False, Spacing => 1);
      Gtk_New (Hide_Row_Titles, "Hide Row Titles");
      Gtk_New (Hide_Column_Titles, "Hide Column Titles");
      Gtk_New (Show_Row_Titles, "Show Row Titles");
      Gtk_New (Show_Column_Titles, "Show Column Titles");
      Pack_Start (Show_Hide_Box, Hide_Row_Titles, True, True, 0);
      Pack_Start (Show_Hide_Box, Hide_Column_Titles, True, True, 0);
      Pack_Start (Show_Hide_Box, Show_Row_Titles, True, True, 0);
      Pack_Start (Show_Hide_Box, Show_Column_Titles, True, True, 0);
      Button_Handler.Connect
        (Hide_Row_Titles, "clicked",
         Button_Handler.To_Marshaller (Do_Hide_Row_Titles'Access));
      Button_Handler.Connect
        (Hide_Column_Titles, "clicked",
         Button_Handler.To_Marshaller (Do_Hide_Column_Titles'Access));
      Button_Handler.Connect
        (Show_Row_Titles, "clicked",
         Button_Handler.To_Marshaller (Do_Show_Row_Titles'Access));
      Button_Handler.Connect
        (Show_Column_Titles, "clicked",
         Button_Handler.To_Marshaller (Do_Show_Column_Titles'Access));

      Pack_Start (Main_Vbox, Show_Hide_Box, False, True, 0);

      -------
      --  The Toolbars
      -------

      Gtk_New (Toolbar);
      Set_Orientation (Toolbar, Orientation_Horizontal);
      Append_Space (Toolbar);

      Gtk_New (Font_Combo);
      Append_Widget (Toolbar, Font_Combo, "font", "font");
      Widget_Handler.Connect (Font_Combo, "changed",
                              Widget_Handler.To_Marshaller (New_Font'Access));

      Append_Space (Toolbar);

      -------
      --  The alignment buttons
      -------

      Gtk_New (Left_Button);
      Append_Widget (Toolbar, Left_Button, "justify left", "left");
      Widget_Handler.Connect
        (Left_Button, "released",
         Widget_Handler.To_Marshaller (Justify_Left'Access));

      Gtk_New (Center_Button);
      Append_Widget (Toolbar, Center_Button, "justify center", "center");
      Widget_Handler.Connect (Center_Button, "released",
                         Widget_Handler.To_Marshaller (Justify_Center'Access));

      Gtk_New (Right_Button);
      Append_Widget (Toolbar, Right_Button, "justify right", "right");
      Widget_Handler.Connect (Right_Button, "released",
                         Widget_Handler.To_Marshaller (Justify_Right'Access));

      Append_Space (Toolbar);

      Gtk_New (Border_Combo);
      Append_Widget (Toolbar, Border_Combo, "border", "border");
      Set_USize (Get_Button (Border_Combo), 32, 32);
      Widget_Handler.Connect
        (Border_Combo, "changed",
         Widget_Handler.To_Marshaller (Change_Border'Access));

      Append_Space (Toolbar);

      Gtk_New (Fg_Color_Combo);
      Append_Widget (Toolbar, Fg_Color_Combo, "font color", "font color");
      Widget_Handler.Connect (Fg_Color_Combo, "changed", Change_Fg'Access);

      Append_Space (Toolbar);

      Gtk_New (Bg_Color_Combo);
      Append_Widget (Toolbar, Bg_Color_Combo, "background color",
                     "background color");
      Widget_Handler.Connect (Bg_Color_Combo, "changed", Change_Bg'Access);

      Append_Space (Toolbar);

      Pack_Start (Main_Vbox, Toolbar, False, True, 0);

      -------
      --  The status box (which cell is selected)
      -------

      Gtk_New_Hbox (Status_Box, False, 1);
      Set_Border_Width (Status_Box, 0);
      Pack_Start (Main_Vbox, Status_Box, False, True, 0);

      Gtk_New (Location, "");
      Set_USize (Location, 160, 20);
      Pack_Start (Status_Box, Location, False, True, 0);

      -------
      --  The entry used to edit the cell
      -------

      Gtk_New (GEntry);
      Pack_Start (Status_Box, GEntry, True, True, 0);

      -------
      --  The notebook
      -------

      Gtk_New (Notebook);
      Set_Tab_Pos (Notebook, Pos_Bottom);
      Pack_Start (Main_Vbox, Notebook, True, True, 0);

      for I in 0 .. Gint'(2) loop
         Gtk_New (Sheets (I), 1000, 26, "Example " & Gint'Image (I));
         Gtk_New (Scrolled_Windows (I));
         Add (Scrolled_Windows (I), Sheets (I));
         Show (Scrolled_Windows (I));

         Gtk_New (Label, "Folder" & Gint'Image (I));
         Append_Page (Notebook, Scrolled_Windows (I), Label);

         Widget_Handler.Connect (Get_Entry (Sheets (I)), "changed",
                           Widget_Handler.To_Marshaller (Show_Entry'Access));
         Sheet_Cb.Connect (Sheets (I), "activate",
                           Activate_Sheet_Cell'Access);
      end loop;

      Widget_Handler.Connect
        (GEntry, "changed",
         Widget_Handler.To_Marshaller (Show_Sheet_Entry'Access));
      Widget_Handler.Connect
        (GEntry, "activate",
         Widget_Handler.To_Marshaller (Activate_Sheet_Entry'Access));

      Build_Example1 (Sheets (0));
      Build_Example2 (Sheets (1));
      Build_Example3 (Sheets (2));

      Widget_Handler.Connect
         (Get_Entry (Sheets (2)), "changed",
          Widget_Handler.To_Marshaller (Show_Entry'Access));

      -------
      --  The pixmaps
      -------

      Create_From_Xpm_D (Pixmap,
                         Gdk.Window.Null_Window,
                         Colormap,
                         Mask,
                         Gdk.Color.Null_Color,
                         Left_Just);
      Gtk_New (Tpixmap, Pixmap, Mask);
      Add (Left_Button, Tpixmap);

      Create_From_Xpm_D (Pixmap,
                         Gdk.Window.Null_Window,
                         Colormap,
                         Mask,
                         Gdk.Color.Null_Color,
                         Center_Just);
      Gtk_New (Tpixmap, Pixmap, Mask);
      Add (Center_Button, Tpixmap);

      Create_From_Xpm_D (Pixmap,
                         Gdk.Window.Null_Window,
                         Colormap,
                         Mask,
                         Gdk.Color.Null_Color,
                         Right_Just);
      Gtk_New (Tpixmap, Pixmap, Mask);
      Add (Right_Button, Tpixmap);

      Create_From_Xpm_D (Pixmap,
                         Gdk.Window.Null_Window,
                         Colormap,
                         Mask,
                         Gdk.Color.Null_Color,
                         Paint);
      Gtk_New (Bg_Pixmap, Pixmap, Mask);
      Add (Get_Button (Bg_Color_Combo), Bg_Pixmap);

      Create_From_Xpm_D (Pixmap,
                         Gdk.Window.Null_Window,
                         Colormap,
                         Mask,
                         Gdk.Color.Null_Color,
                         Font);
      Gtk_New (Fg_Pixmap, Pixmap, Mask);
      Add (Get_Button (Fg_Color_Combo), Fg_Pixmap);

      Show_All (Frame);
   end Run;

end Create_Sheet;
