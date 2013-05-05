-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Glib; use Glib;
with Gtk; use Gtk;
with System;

package body Gnome.Druid is

   use Gnome.Druid_Page;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget : out Gnome_Druid) is
   begin
      Widget := new Gnome_Druid_Record;
      Gnome.Druid.Initialize (Widget);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gnome_Druid_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_druid_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -----------------
   -- Append_Page --
   -----------------

   procedure Append_Page
     (Druid : access Gnome_Druid_Record;
      Page  : access Gnome_Druid_Page_Record)
   is
      procedure Internal
        (Druid : System.Address;
         Page  : System.Address);
      pragma Import (C, Internal, "gnome_druid_append_page");
   begin
      Internal (Get_Object (Druid),
                Get_Object (Page));
   end Append_Page;

   -----------------
   -- Insert_Page --
   -----------------

   procedure Insert_Page
     (Druid     : access Gnome_Druid_Record;
      Back_Page : access Gnome_Druid_Page_Record;
      Page      : access Gnome_Druid_Page_Record)
   is
      procedure Internal
        (Druid     : System.Address;
         Back_Page : System.Address;
         Page      : System.Address);
      pragma Import (C, Internal, "gnome_druid_insert_page");
   begin
      Internal (Get_Object (Druid),
                Get_Object (Back_Page),
                Get_Object (Page));
   end Insert_Page;

   ------------------
   -- Prepend_Page --
   ------------------

   procedure Prepend_Page
     (Druid : access Gnome_Druid_Record;
      Page  : access Gnome_Druid_Page_Record)
   is
      procedure Internal
        (Druid : System.Address;
         Page  : System.Address);
      pragma Import (C, Internal, "gnome_druid_prepend_page");
   begin
      Internal (Get_Object (Druid),
                Get_Object (Page));
   end Prepend_Page;

   ---------------------------
   -- Set_Buttons_Sensitive --
   ---------------------------

   procedure Set_Buttons_Sensitive
     (Druid            : access Gnome_Druid_Record;
      Back_Sensitive   : Boolean;
      Next_Sensitive   : Boolean;
      Cancel_Sensitive : Boolean)
   is
      procedure Internal
        (Druid            : System.Address;
         Back_Sensitive   : Gint;
         Next_Sensitive   : Gint;
         Cancel_Sensitive : Gint);
      pragma Import (C, Internal, "gnome_druid_set_buttons_sensitive");
   begin
      Internal (Get_Object (Druid),
                Boolean'Pos (Back_Sensitive),
                Boolean'Pos (Next_Sensitive),
                Boolean'Pos (Cancel_Sensitive));
   end Set_Buttons_Sensitive;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page
     (Druid : access Gnome_Druid_Record;
      Page  : access Gnome_Druid_Page_Record)
   is
      procedure Internal
        (Druid : System.Address;
         Page  : System.Address);
      pragma Import (C, Internal, "gnome_druid_set_page");
   begin
      Internal (Get_Object (Druid),
                Get_Object (Page));
   end Set_Page;

   ---------------------
   -- Set_Show_Finish --
   ---------------------

   procedure Set_Show_Finish
     (Druid       : access Gnome_Druid_Record;
      Show_Finish : Boolean)
   is
      procedure Internal
        (Druid       : System.Address;
         Show_Finish : Gint);
      pragma Import (C, Internal, "gnome_druid_set_show_finish");
   begin
      Internal (Get_Object (Druid),
                Boolean'Pos (Show_Finish));
   end Set_Show_Finish;

end Gnome.Druid;
