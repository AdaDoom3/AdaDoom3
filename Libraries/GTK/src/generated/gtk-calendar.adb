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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Calendar is
   type Detail_Func_Data is record
      Callback   : Gtk_Calendar_Detail_Func;
      User_Data  : System.Address;
      On_Destroy : G_Destroy_Notify_Address;
   end record;
   type Detail_Func_Data_Access is access Detail_Func_Data;
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Detail_Func_Data_Access);
   procedure Free_Detail_Func_Data (Data : System.Address);
   pragma Convention (C, Free_Detail_Func_Data);
   function Detail_Callback
     (Calendar : System.Address;
      Year, Month, Day : Guint;
      User_Data : System.Address) return chars_ptr;
   pragma Convention (C, Detail_Callback);
   --  Support for GtkCalendarDetailFunc

   function Detail_Callback
     (Calendar : System.Address;
      Year, Month, Day : Guint;
      User_Data : System.Address) return Interfaces.C.Strings.chars_ptr
   is
      Stub : Gtk_Calendar_Record;
      Cal  : constant Gtk_Calendar :=
      Gtk_Calendar (Get_User_Data (Calendar, Stub));
      Data : constant Detail_Func_Data_Access := Convert (User_Data);
      Details : constant String :=
      Data.Callback (Cal, Year, Month, Day, Data.User_Data);
   begin
      if Details = "" then
         return Interfaces.C.Strings.Null_Ptr;
      else
         return New_String (Details);
      end if;
   end Detail_Callback;

   procedure Free_Detail_Func_Data (Data : System.Address) is
      use System;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Detail_Func_Data, Detail_Func_Data_Access);
      D : Detail_Func_Data_Access := Convert (Data);
   begin
      if D.On_Destroy /= null and then D.User_Data /= Null_Address then
         D.On_Destroy (D.User_Data);
      end if;
      Unchecked_Free (D);
   end Free_Detail_Func_Data;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Calendar_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Calendar : out Gtk_Calendar) is
   begin
      Calendar := new Gtk_Calendar_Record;
      Gtk.Calendar.Initialize (Calendar);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Calendar : access Gtk_Calendar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_calendar_new");
   begin
      Set_Object (Calendar, Internal);
   end Initialize;

   -----------------
   -- Clear_Marks --
   -----------------

   procedure Clear_Marks (Calendar : access Gtk_Calendar_Record) is
      procedure Internal (Calendar : System.Address);
      pragma Import (C, Internal, "gtk_calendar_clear_marks");
   begin
      Internal (Get_Object (Calendar));
   end Clear_Marks;

   ---------------------
   -- Display_Options --
   ---------------------

   procedure Display_Options
      (Calendar : access Gtk_Calendar_Record;
       Flags    : Gtk_Calendar_Display_Options)
   is
      procedure Internal
         (Calendar : System.Address;
          Flags    : Gtk_Calendar_Display_Options);
      pragma Import (C, Internal, "gtk_calendar_display_options");
   begin
      Internal (Get_Object (Calendar), Flags);
   end Display_Options;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Calendar : access Gtk_Calendar_Record) is
      procedure Internal (Calendar : System.Address);
      pragma Import (C, Internal, "gtk_calendar_freeze");
   begin
      Internal (Get_Object (Calendar));
   end Freeze;

   --------------
   -- Get_Date --
   --------------

   procedure Get_Date
      (Calendar : access Gtk_Calendar_Record;
       Year     : out Guint;
       Month    : out Guint;
       Day      : out Guint)
   is
      procedure Internal
         (Calendar : System.Address;
          Year     : out Guint;
          Month    : out Guint;
          Day      : out Guint);
      pragma Import (C, Internal, "gtk_calendar_get_date");
   begin
      Internal (Get_Object (Calendar), Year, Month, Day);
   end Get_Date;

   ----------------------------
   -- Get_Detail_Height_Rows --
   ----------------------------

   function Get_Detail_Height_Rows
      (Calendar : access Gtk_Calendar_Record) return Gint
   is
      function Internal (Calendar : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_calendar_get_detail_height_rows");
   begin
      return Internal (Get_Object (Calendar));
   end Get_Detail_Height_Rows;

   ----------------------------
   -- Get_Detail_Width_Chars --
   ----------------------------

   function Get_Detail_Width_Chars
      (Calendar : access Gtk_Calendar_Record) return Gint
   is
      function Internal (Calendar : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_calendar_get_detail_width_chars");
   begin
      return Internal (Get_Object (Calendar));
   end Get_Detail_Width_Chars;

   -------------------------
   -- Get_Display_Options --
   -------------------------

   function Get_Display_Options
      (Calendar : access Gtk_Calendar_Record)
       return Gtk_Calendar_Display_Options
   is
      function Internal
         (Calendar : System.Address) return Gtk_Calendar_Display_Options;
      pragma Import (C, Internal, "gtk_calendar_get_display_options");
   begin
      return Internal (Get_Object (Calendar));
   end Get_Display_Options;

   --------------
   -- Mark_Day --
   --------------

   function Mark_Day
      (Calendar : access Gtk_Calendar_Record;
       Day      : Guint) return Boolean
   is
      function Internal
         (Calendar : System.Address;
          Day      : Guint) return Integer;
      pragma Import (C, Internal, "gtk_calendar_mark_day");
   begin
      return Boolean'Val (Internal (Get_Object (Calendar), Day));
   end Mark_Day;

   ----------------
   -- Select_Day --
   ----------------

   procedure Select_Day (Calendar : access Gtk_Calendar_Record; Day : Guint) is
      procedure Internal (Calendar : System.Address; Day : Guint);
      pragma Import (C, Internal, "gtk_calendar_select_day");
   begin
      Internal (Get_Object (Calendar), Day);
   end Select_Day;

   ------------------
   -- Select_Month --
   ------------------

   function Select_Month
      (Calendar : access Gtk_Calendar_Record;
       Month    : Guint;
       Year     : Guint) return Boolean
   is
      function Internal
         (Calendar : System.Address;
          Month    : Guint;
          Year     : Guint) return Integer;
      pragma Import (C, Internal, "gtk_calendar_select_month");
   begin
      return Boolean'Val (Internal (Get_Object (Calendar), Month, Year));
   end Select_Month;

   procedure Set_Detail_Func
     (Calendar : access Gtk_Calendar_Record;
      Func     : Gtk_Calendar_Detail_Func;
      Data     : System.Address;
      Destroy  : G_Destroy_Notify_Address)
   is
      procedure Internal
        (Calendar : System.Address;
         Func     : System.Address;
         Data     : System.Address;
         Destroy  : G_Destroy_Notify_Address);
      pragma Import (C, Internal, "gtk_calendar_set_detail_func");
      D : constant Detail_Func_Data_Access :=
         new Detail_Func_Data'(Func, Data, Destroy);
   begin
      Internal (Get_Object (Calendar),
                Detail_Callback'Address, D.all'Address,
                Free_Detail_Func_Data'Access);
   end Set_Detail_Func;

   ----------------------------
   -- Set_Detail_Height_Rows --
   ----------------------------

   procedure Set_Detail_Height_Rows
      (Calendar : access Gtk_Calendar_Record;
       Rows     : Gint)
   is
      procedure Internal (Calendar : System.Address; Rows : Gint);
      pragma Import (C, Internal, "gtk_calendar_set_detail_height_rows");
   begin
      Internal (Get_Object (Calendar), Rows);
   end Set_Detail_Height_Rows;

   ----------------------------
   -- Set_Detail_Width_Chars --
   ----------------------------

   procedure Set_Detail_Width_Chars
      (Calendar : access Gtk_Calendar_Record;
       Chars    : Gint)
   is
      procedure Internal (Calendar : System.Address; Chars : Gint);
      pragma Import (C, Internal, "gtk_calendar_set_detail_width_chars");
   begin
      Internal (Get_Object (Calendar), Chars);
   end Set_Detail_Width_Chars;

   -------------------------
   -- Set_Display_Options --
   -------------------------

   procedure Set_Display_Options
      (Calendar : access Gtk_Calendar_Record;
       Flags    : Gtk_Calendar_Display_Options)
   is
      procedure Internal
         (Calendar : System.Address;
          Flags    : Gtk_Calendar_Display_Options);
      pragma Import (C, Internal, "gtk_calendar_set_display_options");
   begin
      Internal (Get_Object (Calendar), Flags);
   end Set_Display_Options;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Calendar : access Gtk_Calendar_Record) is
      procedure Internal (Calendar : System.Address);
      pragma Import (C, Internal, "gtk_calendar_thaw");
   begin
      Internal (Get_Object (Calendar));
   end Thaw;

   ----------------
   -- Unmark_Day --
   ----------------

   function Unmark_Day
      (Calendar : access Gtk_Calendar_Record;
       Day      : Guint) return Boolean
   is
      function Internal
         (Calendar : System.Address;
          Day      : Guint) return Integer;
      pragma Import (C, Internal, "gtk_calendar_unmark_day");
   begin
      return Boolean'Val (Internal (Get_Object (Calendar), Day));
   end Unmark_Day;

end Gtk.Calendar;
