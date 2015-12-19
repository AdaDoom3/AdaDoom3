------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                        P O S I X . C A L E N D A R                       --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  Copyright (c) 1996 Florida State University (FSU), All Rights Reserved. --
--                     Copyright (C) 1998-2012, AdaCore                     --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5: 1990  and IEEE STD  --
--  1003.5b: 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

--  Note: we used to call c_time in order to keep some compatibility with
--  POSIX.Files.Set_File_Times, but this seems obsolete now with modern
--  file systems, and having a fine grain precision is more important anyway.

with Unchecked_Conversion;

package body POSIX.Calendar is

   package AC renames Standard.Calendar;

   use Standard.Calendar;

   POSIX_Epoch : constant Duration :=
     AC.Time_Of (Year => 2150, Month => 1, Day => 1) -
     AC.Time_Of (Year => 1970, Month => 1, Day => 1);
   --  Time difference between POSIX and GNAT notion of time, measured as a
   --  duration since the Ada mid point

   pragma Warnings (Off);
   --  Disable warning that the representation of Time values may
   --  change between GNAT versions.

   function Duration_To_POSIX_Time is new
     Unchecked_Conversion (Duration, POSIX_Time);

   function POSIX_Time_To_Duration is new
     Unchecked_Conversion (POSIX_Time, Duration);

   pragma Warnings (On);

   -------------
   --  Clock  --
   -------------

   function Clock return POSIX_Time is
   begin
      return To_POSIX_Time (AC.Clock);
   end Clock;

   ---------------
   --  To_Time  --
   ---------------

   function To_Time (Date : POSIX_Time) return AC.Time is
   begin
      return AC.Time (Date) - POSIX_Epoch;
   end To_Time;

   ---------------------
   --  To_POSIX_Time  --
   ---------------------

   function To_POSIX_Time (Date : AC.Time) return POSIX_Time is
   begin
      return POSIX_Time (Date + POSIX_Epoch);
   end To_POSIX_Time;

   ------------
   --  Year  --
   ------------

   function Year (Date : POSIX_Time) return Year_Number is
   begin
      return Year_Number (AC.Year (To_Time (Date)));
   end Year;

   -------------
   --  Month  --
   -------------

   function Month (Date : POSIX_Time) return Month_Number is
   begin
      return AC.Month (To_Time (Date));
   end Month;

   -----------
   --  Day  --
   -----------

   function Day (Date : POSIX_Time) return Day_Number is
   begin
      return AC.Day (To_Time (Date));
   end Day;

   ---------------
   --  Seconds  --
   ---------------

   function Seconds (Date : POSIX_Time) return Day_Duration is
   begin
      return AC.Seconds (To_Time (Date));
   end Seconds;

   -------------
   --  Split  --
   -------------

   procedure Split
     (Date    : POSIX_Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration) is
   begin
      AC.Split (To_Time (Date), Year, Month, Day, Seconds);
   end Split;

   ---------------
   --  Time_Of  --
   ---------------

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return POSIX_Time is
   begin
      return To_POSIX_Time (AC.Time_Of (Year, Month, Day, Seconds));
   end Time_Of;

   -----------
   --  "+"  --
   -----------

   function "+" (L : POSIX_Time; R : Duration) return POSIX_Time is
   begin
      return To_POSIX_Time (To_Time (L) + R);
   end "+";

   -----------
   --  "+"  --
   -----------

   function "+" (L : Duration; R : POSIX_Time) return POSIX_Time is
   begin
      return To_POSIX_Time (L + To_Time (R));
   end "+";

   -----------
   --  "-"  --
   -----------

   function "-" (L : POSIX_Time; R : Duration) return POSIX_Time is
   begin
      return To_POSIX_Time (To_Time (L) - R);
   end "-";

   -----------
   --  "-"  --
   -----------

   function "-" (L : POSIX_Time; R : POSIX_Time) return Duration is
   begin
      return To_Time (L) - To_Time (R);
   end "-";

   -----------
   --  "<"  --
   -----------

   function "<" (L, R : POSIX_Time) return Boolean is
   begin
      return To_Time (L) < To_Time (R);
   end "<";

   ------------
   --  "<="  --
   ------------

   function "<=" (L, R : POSIX_Time) return Boolean is
   begin
      return To_Time (L) <= To_Time (R);
   end "<=";

   -----------
   --  ">"  --
   -----------

   function ">" (L, R : POSIX_Time) return Boolean is
   begin
      return To_Time (L) > To_Time (R);
   end ">";

   ------------
   --  ">="  --
   ------------

   function ">=" (L, R : POSIX_Time) return Boolean is
   begin
      return To_Time (L) >= To_Time (R);
   end ">=";

   ---------------------
   --  To_POSIX_Time  --
   ---------------------

   function To_POSIX_Time (Date : POSIX.Timespec) return POSIX_Time is
   begin
      return Duration_To_POSIX_Time (POSIX.To_Duration (Date));
   end To_POSIX_Time;

   -------------------
   --  To_Timespec  --
   -------------------

   function To_Timespec (Date : POSIX_Time) return POSIX.Timespec is
   begin
      return POSIX.To_Timespec (POSIX_Time_To_Duration (Date));
   end To_Timespec;

end POSIX.Calendar;
