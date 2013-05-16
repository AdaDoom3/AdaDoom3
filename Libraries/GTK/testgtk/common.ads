-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                     Copyright (C) 2003 ACT Europe                 --
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

with Glib;             use Glib;
with Gtk;              use Gtk;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Dialog;       use Gtk.Dialog;
with Gtk.Label;        use Gtk.Label;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Option_Menu;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;
with Gtkada.Types;     use Gtkada.Types;
with Interfaces.C.Strings;
with Gtk.Extra.Plot;

package Common is

   --  This package is created to avoid the instantiation of the
   --  generic packages for callbacks. This provides a much smaller
   --  executable

   --  It also contains services that are used in 2 or more examples
   --  of testgtk.

   package Widget_Handler is new Handlers.Callback (Gtk_Widget_Record);
   package User_Widget_Handler is new Handlers.User_Callback
     (Gtk_Widget_Record, Gtk_Widget);
   package Label_Handler  is new Handlers.Callback (Gtk_Label_Record);
   package Adj_Handler    is new Handlers.Callback (Gtk_Adjustment_Record);
   package Check_Handler  is new Handlers.Callback (Gtk_Check_Button_Record);
   package Button_Handler is new Handlers.Callback (Gtk_Button_Record);

   type Gtk_Window_Access is access all Gtk_Window;
   package Destroy_Handler is new Handlers.User_Callback
     (Gtk_Window_Record, Gtk_Window_Access);
   procedure Destroy_Window (Win : access Gtk.Window.Gtk_Window_Record'Class;
                             Ptr : Gtk_Window_Access);

   type Gtk_Dialog_Access is access all Gtk_Dialog;
   package Destroy_Dialog_Handler is new Handlers.User_Callback
     (Gtk_Dialog_Record, Gtk_Dialog_Access);
   procedure Destroy_Dialog (Win : access Gtk_Dialog_Record'Class;
                             Ptr : Gtk_Dialog_Access);

   procedure Build_Option_Menu
     (Omenu   : out Gtk.Option_Menu.Gtk_Option_Menu;
      Gr      : in out Widget_SList.GSlist;
      Items   : Chars_Ptr_Array;
      History : Gint;
      Cb      : Widget_Handler.Marshallers.Void_Marshaller.Handler);
   --  Builds an option menu with the given list of items.
   --  If 'History' is in Items'Range, then item number 'History'
   --  will be set to active.

   function Image_Of (I : Gint) return String;
   --  Returns the image of the given Gint. The leading spaces are
   --  stripped.

   procedure Set_Default_Plot_Attributes
     (Plot : access Gtk.Extra.Plot.Gtk_Plot_Record'Class);
   --  Set the default attributes for a plot (fonts,...)

   package ICS renames Interfaces.C.Strings;
   Book_Open_Xpm    : ICS.chars_ptr_array :=
     (ICS.New_String ("16 16 4 1"),
      ICS.New_String ("       c None s None"),
      ICS.New_String (".      c black"),
      ICS.New_String ("X      c #808080"),
      ICS.New_String ("o      c white"),
      ICS.New_String ("                "),
      ICS.New_String ("  ..            "),
      ICS.New_String (" .Xo.    ...    "),
      ICS.New_String (" .Xoo. ..oo.    "),
      ICS.New_String (" .Xooo.Xooo...  "),
      ICS.New_String (" .Xooo.oooo.X.  "),
      ICS.New_String (" .Xooo.Xooo.X.  "),
      ICS.New_String (" .Xooo.oooo.X.  "),
      ICS.New_String (" .Xooo.Xooo.X.  "),
      ICS.New_String (" .Xooo.oooo.X.  "),
      ICS.New_String ("  .Xoo.Xoo..X.  "),
      ICS.New_String ("   .Xo.o..ooX.  "),
      ICS.New_String ("    .X..XXXXX.  "),
      ICS.New_String ("    ..X.......  "),
      ICS.New_String ("     ..         "),
      ICS.New_String ("                "));
   Book_Closed_Xpm  : ICS.chars_ptr_array :=
     (ICS.New_String ("16 16 6 1"),
      ICS.New_String ("       c None s None"),
      ICS.New_String (".      c black"),
      ICS.New_String ("X      c red"),
      ICS.New_String ("o      c yellow"),
      ICS.New_String ("O      c #808080"),
      ICS.New_String ("#      c white"),
      ICS.New_String ("                "),
      ICS.New_String ("       ..       "),
      ICS.New_String ("     ..XX.      "),
      ICS.New_String ("   ..XXXXX.     "),
      ICS.New_String (" ..XXXXXXXX.    "),
      ICS.New_String (".ooXXXXXXXXX.   "),
      ICS.New_String ("..ooXXXXXXXXX.  "),
      ICS.New_String (".X.ooXXXXXXXXX. "),
      ICS.New_String (".XX.ooXXXXXX..  "),
      ICS.New_String (" .XX.ooXXX..#O  "),
      ICS.New_String ("  .XX.oo..##OO. "),
      ICS.New_String ("   .XX..##OO..  "),
      ICS.New_String ("    .X.#OO..    "),
      ICS.New_String ("     ..O..      "),
      ICS.New_String ("      ..        "),
      ICS.New_String ("                "));

   Mini_Page_Xpm  : ICS.chars_ptr_array :=
     (ICS.New_String ("16 16 4 1"),
      ICS.New_String ("       c None s None"),
      ICS.New_String (".      c black"),
      ICS.New_String ("X      c white"),
      ICS.New_String ("O      c #808080"),
      ICS.New_String ("                "),
      ICS.New_String ("   .......      "),
      ICS.New_String ("   .XXXXX..     "),
      ICS.New_String ("   .XoooX.X.    "),
      ICS.New_String ("   .XXXXX....   "),
      ICS.New_String ("   .XooooXoo.o  "),
      ICS.New_String ("   .XXXXXXXX.o  "),
      ICS.New_String ("   .XooooooX.o  "),
      ICS.New_String ("   .XXXXXXXX.o  "),
      ICS.New_String ("   .XooooooX.o  "),
      ICS.New_String ("   .XXXXXXXX.o  "),
      ICS.New_String ("   .XooooooX.o  "),
      ICS.New_String ("   .XXXXXXXX.o  "),
      ICS.New_String ("   ..........o  "),
      ICS.New_String ("    oooooooooo  "),
      ICS.New_String ("                "));

   Gtk_Mini_Xpm  : ICS.chars_ptr_array :=
     (ICS.New_String ("15 20 17 1"),
      ICS.New_String ("       c None"),
      ICS.New_String (".      c #14121F"),
      ICS.New_String ("+      c #278828"),
      ICS.New_String ("@      c #9B3334"),
      ICS.New_String ("#      c #284C72"),
      ICS.New_String ("$      c #24692A"),
      ICS.New_String ("%      c #69282E"),
      ICS.New_String ("&      c #37C539"),
      ICS.New_String ("*      c #1D2F4D"),
      ICS.New_String ("=      c #6D7076"),
      ICS.New_String ("-      c #7D8482"),
      ICS.New_String (";      c #E24A49"),
      ICS.New_String (">      c #515357"),
      ICS.New_String (",      c #9B9C9B"),
      ICS.New_String ("'      c #2FA232"),
      ICS.New_String (")      c #3CE23D"),
      ICS.New_String ("!      c #3B6CCB"),
      ICS.New_String ("               "),
      ICS.New_String ("      ***>     "),
      ICS.New_String ("    >.*!!!*    "),
      ICS.New_String ("   ***....#*=  "),
      ICS.New_String ("  *!*.!!!**!!# "),
      ICS.New_String (" .!!#*!#*!!!!# "),
      ICS.New_String (" @%#!.##.*!!$& "),
      ICS.New_String (" @;%*!*.#!#')) "),
      ICS.New_String (" @;;@%!!*$&)'' "),
      ICS.New_String (" @%.%@%$'&)$+' "),
      ICS.New_String (" @;...@$'*'*)+ "),
      ICS.New_String (" @;%..@$+*.')$ "),
      ICS.New_String (" @;%%;;$+..$)# "),
      ICS.New_String (" @;%%;@$$$'.$# "),
      ICS.New_String (" %;@@;;$$+))&* "),
      ICS.New_String ("  %;;;@+$&)&*  "),
      ICS.New_String ("   %;;@'))+>   "),
      ICS.New_String ("    %;@'&#     "),
      ICS.New_String ("     >%$$      "),
      ICS.New_String ("      >=       "));

end Common;
