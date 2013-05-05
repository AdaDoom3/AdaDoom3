-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                 Copyright (C) 2010-2013, AdaCore                  --
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

with Glib;  use Glib;

with Gtkada.Printing;        use Gtkada.Printing;
with Gtk.Print_Context;      use Gtk.Print_Context;

with Gtk.Widget;             use Gtk.Widget;

with Testcairo_Drawing;      use Testcairo_Drawing;

package Testcairo_Printing is

   type Testcairo_Print_Operation_Record is new Gtkada_Print_Operation_Record
   with record
      Test   : Test_Type;
      Win    : Gtk_Widget;
   end record;
   type Testcairo_Print_Operation is access all
     Testcairo_Print_Operation_Record'Class;

   procedure Draw_Page
     (Op          : access Testcairo_Print_Operation_Record;
      Context     : Gtk_Print_Context;
      Page_Number : Gint);
   --  Handler responsible for printing pages

end Testcairo_Printing;
