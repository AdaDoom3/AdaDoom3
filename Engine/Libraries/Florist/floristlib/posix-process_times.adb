------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                   P O S I X . P R O C E S S _ T I M E S                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

package body POSIX.Process_Times is

   use POSIX.C;

   -------------------------
   --  Elapsed_Real_Time  --
   -------------------------

   function times (buf : tms_ptr) return clock_t;
   pragma Import (C, times, times_LINKNAME);

   function Elapsed_Real_Time return Tick_Count is
      T : Process_Times;
   begin
      return Tick_Count (times (T.tms'Unchecked_Access));
   end Elapsed_Real_Time;

   -------------------------
   --  Get_Process_Times  --
   -------------------------

   function Get_Process_Times return Process_Times is
      t : Process_Times;
   begin
      t.Elapsed_Real_Time := times (t.tms'Unchecked_Access);
      return t;
   end Get_Process_Times;

   ----------------------------
   --  Elapsed_Real_Time_Of  --
   ----------------------------

   function Elapsed_Real_Time_Of (Times : Process_Times) return Tick_Count is
   begin
      return Tick_Count (Times.Elapsed_Real_Time);
   end Elapsed_Real_Time_Of;

   -----------------------
   --  User_CPU_Time_Of --
   -----------------------

   function User_CPU_Time_Of (Times : Process_Times) return Tick_Count is
   begin
      return Tick_Count (Times.tms.tms_utime);
   end User_CPU_Time_Of;

   --------------------------
   --  System_CPU_Time_Of  --
   --------------------------

   function System_CPU_Time_Of (Times : Process_Times) return Tick_Count is
   begin
      return Tick_Count (Times.tms.tms_stime);
   end System_CPU_Time_Of;

   ------------------------------------
   --  Descendants_User_CPU_Time_Of  --
   ------------------------------------

   function Descendants_User_CPU_Time_Of (Times : Process_Times)
      return Tick_Count is
   begin
      return Tick_Count (Times.tms.tms_cutime);
   end Descendants_User_CPU_Time_Of;

   --------------------------------------
   --  Descendants_System_CPU_Time_Of  --
   --------------------------------------

   function Descendants_System_CPU_Time_Of (Times : Process_Times)
      return Tick_Count is
   begin
      return Tick_Count (Times.tms.tms_cstime);
   end Descendants_System_CPU_Time_Of;

end POSIX.Process_Times;
