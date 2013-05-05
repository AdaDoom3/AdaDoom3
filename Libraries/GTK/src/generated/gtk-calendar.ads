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

--  <description>
--  Gtk_Calendar is a widget that displays a calendar, one month at a time. It
--  can be created with Gtk_New.
--
--  The month and year currently displayed can be altered with Select_Month.
--  The exact day can be selected from the displayed month using Select_Day.
--
--  The way in which the calendar itself is displayed can be altered using
--  Display_Options.
--
--  The selected date can be retrieved from a Gtk_Calendar using Get_Date.
--
--  If performing many 'mark' operations, the calendar can be frozen to
--  prevent flicker, using Freeze, and 'thawed' again using Thaw.
--
--  </description>
--  <screenshot>gtk-calendar</screenshot>
--  <group>Selectors</group>
--  <testgtk>create_calendar.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Calendar is

   type Gtk_Calendar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Calendar is access all Gtk_Calendar_Record'Class;

   type Gtk_Calendar_Display_Options is mod 2 ** 8;

   Show_Heading : constant Gtk_Calendar_Display_Options := 2 ** 0;
   --  Specify that the month and year should be displayed.

   Show_Day_Names : constant Gtk_Calendar_Display_Options := 2 ** 1;
   --  Specify that three letter day descriptions should be present.

   No_Month_Change : constant Gtk_Calendar_Display_Options := 2 ** 2;
   --  Prevent the user from switching months with the calendar.

   Show_Week_Numbers : constant Gtk_Calendar_Display_Options := 2 ** 3;
   --  Display each week numbers of the current year, down the left side of
   --  the calendar.

   Week_Start_Monday : constant Gtk_Calendar_Display_Options := 2 ** 4;
   --  Start the calendar week on Monday, instead of the default Sunday.

   type Gtk_Calendar_Detail_Func is access function
     (Calendar  : access Gtk_Calendar_Record'Class;
      Year      : Guint;
      Month     : Guint;
      Day       : Guint;
      User_Data : System.Address) return String;
   --  Return the details for the given day, or the empty string when there
   --  are no details.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Calendar : out Gtk_Calendar);
   procedure Initialize (Calendar : access Gtk_Calendar_Record'Class);
   --  Creates a new calendar, with the current date being selected.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_calendar_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear_Marks (Calendar : access Gtk_Calendar_Record);
   --  Remove all visual markers.

   procedure Display_Options
      (Calendar : access Gtk_Calendar_Record;
       Flags    : Gtk_Calendar_Display_Options);
   pragma Obsolescent (Display_Options);
   function Get_Display_Options
      (Calendar : access Gtk_Calendar_Record)
       return Gtk_Calendar_Display_Options;
   procedure Set_Display_Options
      (Calendar : access Gtk_Calendar_Record;
       Flags    : Gtk_Calendar_Display_Options);
   --  Sets display options (whether to display the heading and the month
   --  headings).
   --  Since: gtk+ 2.4
   --  "flags": the display options to set

   procedure Freeze (Calendar : access Gtk_Calendar_Record);
   pragma Obsolescent (Freeze);
   --  Does nothing. Previously locked the display of the calendar until it
   --  was thawed with Gtk.Calendar.Thaw.
   --  Deprecated

   procedure Get_Date
      (Calendar : access Gtk_Calendar_Record;
       Year     : out Guint;
       Month    : out Guint;
       Day      : out Guint);
   --  Obtains the selected date from a Gtk.Calendar.Gtk_Calendar.
   --  "year": location to store the year as a decimal number (e.g. 2011), or
   --  null
   --  "month": location to store the month number (between 0 and 11), or null
   --  "day": location to store the day number (between 1 and 31), or null

   function Get_Detail_Height_Rows
      (Calendar : access Gtk_Calendar_Record) return Gint;
   procedure Set_Detail_Height_Rows
      (Calendar : access Gtk_Calendar_Record;
       Rows     : Gint);
   --  Updates the height of detail cells. See
   --  Gtk.Calendar.Gtk_Calendar:detail-height-rows.
   --  Since: gtk+ 2.14
   --  "rows": detail height in rows.

   function Get_Detail_Width_Chars
      (Calendar : access Gtk_Calendar_Record) return Gint;
   procedure Set_Detail_Width_Chars
      (Calendar : access Gtk_Calendar_Record;
       Chars    : Gint);
   --  Updates the width of detail cells. See
   --  Gtk.Calendar.Gtk_Calendar:detail-width-chars.
   --  Since: gtk+ 2.14
   --  "chars": detail width in characters.

   function Mark_Day
      (Calendar : access Gtk_Calendar_Record;
       Day      : Guint) return Boolean;
   --  Places a visual marker on a particular day. Note that this function
   --  always returns True, and you should ignore the return value. In GTK+ 3,
   --  this function will not return a value.
   --  "day": the day number to mark between 1 and 31.

   procedure Select_Day (Calendar : access Gtk_Calendar_Record; Day : Guint);
   --  Selects a day from the current month.
   --  "day": the day number between 1 and 31, or 0 to unselect the currently
   --  selected day.

   function Select_Month
      (Calendar : access Gtk_Calendar_Record;
       Month    : Guint;
       Year     : Guint) return Boolean;
   --  Shifts the calendar to a different month. Note that this function
   --  always returns True, and you should ignore the return value. In GTK+ 3,
   --  this function will not return a value.
   --  "month": a month number between 0 and 11.
   --  "year": the year the month is in.

   procedure Set_Detail_Func
      (Calendar : access Gtk_Calendar_Record;
       Func     : Gtk_Calendar_Detail_Func;
       Data     : System.Address;
       Destroy  : Glib.G_Destroy_Notify_Address);
   --  Installs a function which provides Pango markup with detail information
   --  for each day. Examples for such details are holidays or appointments.
   --  That information is shown below each day when
   --  Gtk.Calendar.Gtk_Calendar:show-details is set. A tooltip containing with
   --  full detail information is provided, if the entire text should not fit
   --  into the details area, or if Gtk.Calendar.Gtk_Calendar:show-details is
   --  not set. The size of the details area can be restricted by setting the
   --  Gtk.Calendar.Gtk_Calendar:detail-width-chars and
   --  Gtk.Calendar.Gtk_Calendar:detail-height-rows properties.
   --  Since: gtk+ 2.14
   --  "func": a function providing details for each day.
   --  "data": data to pass to Func invokations.
   --  "destroy": a function for releasing Data.

   procedure Thaw (Calendar : access Gtk_Calendar_Record);
   pragma Obsolescent (Thaw);
   --  Does nothing. Previously defrosted a calendar; all the changes made
   --  since the last Gtk.Calendar.Freeze were displayed.
   --  Deprecated

   function Unmark_Day
      (Calendar : access Gtk_Calendar_Record;
       Day      : Guint) return Boolean;
   --  Removes the visual marker from a particular day. Note that this
   --  function always returns True, and you should ignore the return value. In
   --  GTK+ 3, this function will not return a value.
   --  "day": the day number to unmark between 1 and 31.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Calendar_Record, Gtk_Calendar);
   function "+"
     (Widget : access Gtk_Calendar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Calendar
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Day_Property
   --  Type: Gint
   --  Flags: read-write
   --  The selected day (as a number between 1 and 31, or 0 to unselect the
   --  currently selected day). This property gets initially set to the current
   --  day.
   --
   --  Name: Detail_Height_Rows_Property
   --  Type: Gint
   --  Flags: read-write
   --  Height of a detail cell, in rows. A value of 0 allows any width. See
   --  Gtk.Calendar.Set_Detail_Func.
   --
   --  Name: Detail_Width_Chars_Property
   --  Type: Gint
   --  Flags: read-write
   --  Width of a detail cell, in characters. A value of 0 allows any width.
   --  See Gtk.Calendar.Set_Detail_Func.
   --
   --  Name: Month_Property
   --  Type: Gint
   --  Flags: read-write
   --  The selected month (as a number between 0 and 11). This property gets
   --  initially set to the current month.
   --
   --  Name: No_Month_Change_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether the selected month can be changed.
   --
   --  Name: Show_Day_Names_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether day names are displayed.
   --
   --  Name: Show_Details_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether details are shown directly in the widget, or if they
   --  are available only as tooltip. When this property is set days with
   --  details are marked.
   --
   --  Name: Show_Heading_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether a heading is displayed.
   --
   --  Name: Show_Week_Numbers_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether week numbers are displayed.
   --
   --  Name: Year_Property
   --  Type: Gint
   --  Flags: read-write
   --  The selected year. This property gets initially set to the current
   --  year.

   Day_Property : constant Glib.Properties.Property_Int;
   Detail_Height_Rows_Property : constant Glib.Properties.Property_Int;
   Detail_Width_Chars_Property : constant Glib.Properties.Property_Int;
   Month_Property : constant Glib.Properties.Property_Int;
   No_Month_Change_Property : constant Glib.Properties.Property_Boolean;
   Show_Day_Names_Property : constant Glib.Properties.Property_Boolean;
   Show_Details_Property : constant Glib.Properties.Property_Boolean;
   Show_Heading_Property : constant Glib.Properties.Property_Boolean;
   Show_Week_Numbers_Property : constant Glib.Properties.Property_Boolean;
   Year_Property : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "day-selected"
   --     procedure Handler (Self : access Gtk_Calendar_Record'Class);
   --
   --  "day-selected-double-click"
   --     procedure Handler (Self : access Gtk_Calendar_Record'Class);
   --
   --  "month-changed"
   --     procedure Handler (Self : access Gtk_Calendar_Record'Class);
   --  Emitted when the user clicks a button to change the selected month on a
   --  calendar.
   --
   --  "next-month"
   --     procedure Handler (Self : access Gtk_Calendar_Record'Class);
   --
   --  "next-year"
   --     procedure Handler (Self : access Gtk_Calendar_Record'Class);
   --
   --  "prev-month"
   --     procedure Handler (Self : access Gtk_Calendar_Record'Class);
   --
   --  "prev-year"
   --     procedure Handler (Self : access Gtk_Calendar_Record'Class);

   Signal_Day_Selected : constant Glib.Signal_Name := "day-selected";
   Signal_Day_Selected_Double_Click : constant Glib.Signal_Name := "day-selected-double-click";
   Signal_Month_Changed : constant Glib.Signal_Name := "month-changed";
   Signal_Next_Month : constant Glib.Signal_Name := "next-month";
   Signal_Next_Year : constant Glib.Signal_Name := "next-year";
   Signal_Prev_Month : constant Glib.Signal_Name := "prev-month";
   Signal_Prev_Year : constant Glib.Signal_Name := "prev-year";

private
   Day_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("day");
   Detail_Height_Rows_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("detail-height-rows");
   Detail_Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("detail-width-chars");
   Month_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("month");
   No_Month_Change_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("no-month-change");
   Show_Day_Names_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-day-names");
   Show_Details_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-details");
   Show_Heading_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-heading");
   Show_Week_Numbers_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-week-numbers");
   Year_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("year");
end Gtk.Calendar;
