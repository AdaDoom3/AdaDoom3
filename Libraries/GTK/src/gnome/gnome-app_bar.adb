-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2006                          --
--                           AdaCore                                 --
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

with Gtk; use Gtk;
with Gtk.Widget; use Gtk.Widget;
with Interfaces.C.Strings;
with System;

package body Gnome.App_Bar is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget        : out Gnome_App_Bar;
      Has_Progress  : Boolean;
      Has_Status    : Boolean;
      Interactivity : Gnome_Preferences_Type)
   is
   begin
      Widget := new Gnome_App_Bar_Record;
      Initialize (Widget, Has_Progress, Has_Status, Interactivity);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget        : access Gnome_App_Bar_Record'Class;
      Has_Progress  : Boolean;
      Has_Status    : Boolean;
      Interactivity : Gnome_Preferences_Type)
   is
      function Internal
        (Has_Progress  : Gint;
         Has_Status    : Gint;
         Interactivity : Gint) return System.Address;
      pragma Import (C, Internal, "gnome_appbar_new");
   begin
      Set_Object
        (Widget,
         Internal
           (Boolean'Pos (Has_Progress),
            Boolean'Pos (Has_Status),
            Gnome_Preferences_Type'Pos (Interactivity)));
   end Initialize;

   -------------------------
   -- Appbar_Clear_Prompt --
   -------------------------

   procedure Appbar_Clear_Prompt (Appbar : access Gnome_App_Bar_Record)
   is
      procedure Internal (Appbar : System.Address);
      pragma Import (C, Internal, "gnome_appbar_clear_prompt");
   begin
      Internal (Get_Object (Appbar));
   end Appbar_Clear_Prompt;

   ------------------------
   -- Appbar_Clear_Stack --
   ------------------------

   procedure Appbar_Clear_Stack (Appbar : access Gnome_App_Bar_Record)
   is
      procedure Internal (Appbar : System.Address);
      pragma Import (C, Internal, "gnome_appbar_clear_stack");
   begin
      Internal (Get_Object (Appbar));
   end Appbar_Clear_Stack;

   -------------------------
   -- Appbar_Get_Progress --
   -------------------------

   pragma Warnings (Off); --  Gtk.Progress is obsolescent
   function Appbar_Get_Progress
     (Appbar : access Gnome_App_Bar_Record) return Gtk.Progress.Gtk_Progress
   is
      function Internal (Appbar : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_appbar_get_progress");
   begin
      return Gtk.Progress.Gtk_Progress
        (Widget.Convert (Internal (Get_Object (Appbar))));
   end Appbar_Get_Progress;
   pragma Warnings (On);

   -------------------------
   -- Appbar_Get_Response --
   -------------------------

   function Appbar_Get_Response (Appbar : access Gnome_App_Bar_Record)
                                 return String
   is
      function Internal (Appbar : System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_appbar_get_response");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Appbar)));
   end Appbar_Get_Response;

   ----------------
   -- Appbar_Pop --
   ----------------

   procedure Appbar_Pop (Appbar : access Gnome_App_Bar_Record)
   is
      procedure Internal (Appbar : System.Address);
      pragma Import (C, Internal, "gnome_appbar_pop");
   begin
      Internal (Get_Object (Appbar));
   end Appbar_Pop;

   -----------------
   -- Appbar_Push --
   -----------------

   procedure Appbar_Push
     (Appbar : access Gnome_App_Bar_Record;
      Status : String)
   is
      procedure Internal
        (Appbar : System.Address;
         Status : String);
      pragma Import (C, Internal, "gnome_appbar_push");
   begin
      Internal (Get_Object (Appbar),
                Status & ASCII.NUL);
   end Appbar_Push;

   --------------------
   -- Appbar_Refresh --
   --------------------

   procedure Appbar_Refresh (Appbar : access Gnome_App_Bar_Record)
   is
      procedure Internal (Appbar : System.Address);
      pragma Import (C, Internal, "gnome_appbar_refresh");
   begin
      Internal (Get_Object (Appbar));
   end Appbar_Refresh;

   ------------------------
   -- Appbar_Set_Default --
   ------------------------

   procedure Appbar_Set_Default
     (Appbar         : access Gnome_App_Bar_Record;
      Default_Status : String)
   is
      procedure Internal
        (Appbar         : System.Address;
         Default_Status : String);
      pragma Import (C, Internal, "gnome_appbar_set_default");
   begin
      Internal (Get_Object (Appbar),
                Default_Status & ASCII.NUL);
   end Appbar_Set_Default;

   -------------------------
   -- Appbar_Set_Progress --
   -------------------------

   procedure Appbar_Set_Progress_Percentage
     (Appbar     : access Gnome_App_Bar_Record;
      Percentage : Gfloat)
   is
      procedure Internal
        (Appbar     : System.Address;
         Percentage : Gfloat);
      pragma Import (C, Internal, "gnome_appbar_set_progress_percentage");
   begin
      Internal (Get_Object (Appbar),
                Percentage);
   end Appbar_Set_Progress_Percentage;

   -----------------------
   -- Appbar_Set_Prompt --
   -----------------------

   procedure Appbar_Set_Prompt
     (Appbar : access Gnome_App_Bar_Record;
      Prompt : String;
      Modal  : Boolean)
   is
      procedure Internal
        (Appbar : System.Address;
         Prompt : String;
         Modal  : Gint);
      pragma Import (C, Internal, "gnome_appbar_set_prompt");
   begin
      Internal (Get_Object (Appbar),
                Prompt & ASCII.NUL,
                Boolean'Pos (Modal));
   end Appbar_Set_Prompt;

   -----------------------
   -- Appbar_Set_Status --
   -----------------------

   procedure Appbar_Set_Status
     (Appbar : access Gnome_App_Bar_Record;
      Status : String)
   is
      procedure Internal
        (Appbar : System.Address;
         Status : String);
      pragma Import (C, Internal, "gnome_appbar_set_status");
   begin
      Internal (Get_Object (Appbar),
                Status & ASCII.NUL);
   end Appbar_Set_Status;

end Gnome.App_Bar;
