------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                        P O S I X . C A L E N D A R                       --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--  This  file is a component  of FLORIST,  an implementation of the POSIX  --
--  Ada  bindings  for  use with the GNAT Ada compiler and the FSU Gnu Ada  --
--  Runtime Library (GNARL).                                                --
--                                                                          --
--  This package specification contains some text extracted from  IEEE STD  --
--  1003.5: 1990, Information Technology -- POSIX Ada Language  Interfaces  --
--  Part 1: Binding  for  System Application Program Interface, as amended  --
--  by IEEE STD 1003.5b: 1996, Amendment 1: Realtime Extensions, copyright  --
--  1996 by the Institute of Electrical and Electronics Engineers, Inc.     --
--                                                                          --
--  The package specifications in the IEEE standards cited above represent  --
--  only a  portion  of  the  documents  and  are  not to be interpreteted  --
--  outside the context  of  the documents.  The standards must be used in  --
--  conjunction  with  the  package   specifications  in  order  to  claim  --
--  conformance.   The IEEE takes no responsibility for and will assume no  --
--  liability for damages resulting from the reader's misinterpretation of  --
--  said  information resulting from its out-of-context nature.   To order  --
--  copies of the IEEE standards,  please contact the  IEEE Service Center  --
--  at 445 Hoes Lane, PO Box 1331, Piscataway, NJ 08855-1331; via phone at  --
--  1-800-678-IEEE, 908-981-1393; or via fax at 908-981-9667.               --
--                                                                          --
--  These  package  specifications are  distributed in  the hope that they  --
--  will  be useful, but  WITHOUT  ANY  WARRANTY; without even the implied  --
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        --
--                                                                          --
------------------------------------------------------------------------------

with Calendar,
     POSIX;
package POSIX.Calendar is

   --  Time information

   type POSIX_Time is private;
   function Clock return POSIX_Time;
   function To_Time (Date : POSIX_Time) return Standard.Calendar.Time;
   function To_POSIX_Time (Date : Standard.Calendar.Time) return POSIX_Time;

   --  operations on POSIX_Time

   subtype Year_Number    is Standard.Calendar.Year_Number;
   subtype Month_Number   is Standard.Calendar.Month_Number;
   subtype Day_Number     is Standard.Calendar.Day_Number;
   subtype Day_Duration   is Standard.Calendar.Day_Duration;
   function Year (Date : POSIX_Time) return Year_Number;
   function Month (Date : POSIX_Time) return Month_Number;
   function Day (Date : POSIX_Time) return Day_Number;
   function Seconds (Date : POSIX_Time) return Day_Duration;
   procedure Split
     (Date    : POSIX_Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration);
   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return POSIX_Time;
   function "+" (L : POSIX_Time; R : Duration) return POSIX_Time;
   function "+" (L : Duration; R : POSIX_Time) return POSIX_Time;
   function "-" (L : POSIX_Time; R : Duration) return POSIX_Time;
   function "-" (L : POSIX_Time; R : POSIX_Time) return Duration;
   function "<"  (L, R : POSIX_Time) return Boolean;
   function "<=" (L, R : POSIX_Time) return Boolean;
   function ">"  (L, R : POSIX_Time) return Boolean;
   function ">=" (L, R : POSIX_Time) return Boolean;

   Time_Error : exception renames Standard.Calendar.Time_Error;

   function To_POSIX_Time (Date : POSIX.Timespec) return POSIX_Time;
   function To_Timespec (Date : POSIX_Time) return POSIX.Timespec;

private
   type POSIX_Time is new Standard.Calendar.Time;
end POSIX.Calendar;
