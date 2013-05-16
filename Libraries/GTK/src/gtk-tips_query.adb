-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2003 ACT-Europe                 --
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

with System;

package body Gtk.Tips_Query is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tips_Query) is
   begin
      Widget := new Gtk_Tips_Query_Record;
      Gtk.Tips_Query.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Tips_Query_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tips_query_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ----------------
   -- Set_Caller --
   ----------------

   procedure Set_Caller
      (Tips_Query : access Gtk_Tips_Query_Record;
       Caller     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Tips_Query : System.Address;
         Caller     : System.Address);
      pragma Import (C, Internal, "gtk_tips_query_set_caller");

   begin
      Internal (Get_Object (Tips_Query), Get_Object (Caller));
   end Set_Caller;

   ----------------
   -- Set_Labels --
   ----------------

   procedure Set_Labels
     (Tips_Query     : access Gtk_Tips_Query_Record;
      Label_Inactive : UTF8_String;
      Label_No_Tip   : UTF8_String)
   is
      procedure Internal
        (Tips_Query     : System.Address;
         Label_Inactive : UTF8_String;
         Label_No_Tip   : UTF8_String);
      pragma Import (C, Internal, "gtk_tips_query_set_labels");

   begin
      Internal (Get_Object (Tips_Query),
                Label_Inactive & ASCII.NUL,
                Label_No_Tip & ASCII.NUL);
   end Set_Labels;

   -----------------
   -- Start_Query --
   -----------------

   procedure Start_Query (Tips_Query : access Gtk_Tips_Query_Record) is
      procedure Internal (Tips_Query : System.Address);
      pragma Import (C, Internal, "gtk_tips_query_start_query");

   begin
      Internal (Get_Object (Tips_Query));
   end Start_Query;

   ----------------
   -- Stop_Query --
   ----------------

   procedure Stop_Query (Tips_Query : access Gtk_Tips_Query_Record) is
      procedure Internal (Tips_Query : System.Address);
      pragma Import (C, Internal, "gtk_tips_query_stop_query");

   begin
      Internal (Get_Object (Tips_Query));
   end Stop_Query;

end Gtk.Tips_Query;
