------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--              P O S I X . P R O C E S S _ S C H E D U L I N G             --
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

with POSIX.Implementation,
     POSIX.Process_Identification,
     Unchecked_Conversion;

package body POSIX.Process_Scheduling is

   use POSIX.C;
   use POSIX.Implementation;

   --------------------
   --  Get_Priority  --
   --------------------

   function Get_Priority (Parameters : Scheduling_Parameters)
     return Scheduling_Priority is
   begin
      return Scheduling_Priority (Parameters.Param.sched_priority);
   end Get_Priority;

   --------------------
   --  Set_Priority  --
   --------------------

   procedure Set_Priority
     (Parameters : in out Scheduling_Parameters;
      Priority   : Scheduling_Priority) is
   begin
      Parameters.Param.sched_priority := int (Priority);
   end Set_Priority;

   ---------------------------------
   --  Set_Scheduling_Parameters  --
   ---------------------------------

   function sched_setparam
     (pid : pid_t;
      param : sched_param_ptr) return int;
   pragma Import (C, sched_setparam, sched_setparam_LINKNAME);

   function To_pid_t is
     new Unchecked_Conversion (POSIX.Process_Identification.Process_ID, pid_t);

   procedure Set_Scheduling_Parameters
     (Process    : POSIX_Process_Identification.Process_ID;
      Parameters : Scheduling_Parameters) is
   begin
      Check (sched_setparam (To_pid_t (Process),
        Parameters.Param'Unchecked_Access));
   end Set_Scheduling_Parameters;

   ---------------------------------
   --  Get_Scheduling_Parameters  --
   ---------------------------------

   function sched_getparam
     (pid : pid_t;
      param : access struct_sched_param) return int;
   pragma Import (C, sched_getparam, sched_getparam_LINKNAME);

   function Get_Scheduling_Parameters
     (Process : POSIX_Process_Identification.Process_ID)
     return Scheduling_Parameters is
      Params : aliased Scheduling_Parameters;
   begin
      Check (sched_getparam
        (To_pid_t (Process), Params.Param'Unchecked_Access));
      return Params;
   end Get_Scheduling_Parameters;

   -----------------------------
   --  Set_Scheduling_Policy  --
   -----------------------------

   function sched_setscheduler
     (pid : pid_t;
      policy : int;
      param : sched_param_ptr) return int;
   pragma Import (C, sched_setscheduler, sched_setscheduler_LINKNAME);

   procedure Set_Scheduling_Policy
     (Process    : POSIX_Process_Identification.Process_ID;
      New_Policy : Scheduling_Policy;
      Parameters : Scheduling_Parameters) is
   begin
      Check (sched_setscheduler
        (To_pid_t (Process),
         int (New_Policy),
         Parameters.Param'Unchecked_Access));
   end Set_Scheduling_Policy;

   -----------------------------
   --  Get_Scheduling_Policy  --
   -----------------------------

   function sched_getscheduler (pid : pid_t) return int;
   pragma Import (C, sched_getscheduler, sched_getscheduler_LINKNAME);

   function Get_Scheduling_Policy
     (Process : POSIX_Process_Identification.Process_ID)
     return Scheduling_Policy is
   begin
      return Scheduling_Policy
        (Check (sched_getscheduler (To_pid_t (Process))));
   end Get_Scheduling_Policy;

   -------------
   --  Yield  --
   -------------

   function sched_yield return int;
   pragma Import (C, sched_yield, sched_yield_LINKNAME);

   procedure Yield is
   begin
      Check (sched_yield);
   end Yield;

   ----------------------------
   --  Get_Maximum_Priority  --
   ----------------------------

   function sched_get_priority_max (policy : int) return int;
   pragma Import (C, sched_get_priority_max, sched_get_priority_max_LINKNAME);

   function Get_Maximum_Priority (Policy : Scheduling_Policy)
     return Scheduling_Priority is
   begin
      return Scheduling_Priority
        (Check (sched_get_priority_max (int (Policy))));
   end Get_Maximum_Priority;

   ----------------------------
   --  Get_Minimum_Priority  --
   ----------------------------

   function sched_get_priority_min (policy : int) return int;
   pragma Import (C, sched_get_priority_min, sched_get_priority_min_LINKNAME);

   function Get_Minimum_Priority (Policy : Scheduling_Policy)
     return Scheduling_Priority is
   begin
      return Scheduling_Priority
        (Check (sched_get_priority_min (int (Policy))));
   end Get_Minimum_Priority;

   ---------------------------------
   --  Get_Round_Robin_Interval  --
   ---------------------------------

   function sched_rr_get_interval
     (pid : pid_t;
      interval : access struct_timespec) return int;
   pragma Import (C, sched_rr_get_interval, sched_rr_get_interval_LINKNAME);

   function Get_Round_Robin_Interval
     (Process : POSIX_Process_Identification.Process_ID)
     return POSIX.Timespec is
      TS : aliased struct_timespec;
   begin
      Check (sched_rr_get_interval (To_pid_t (Process), TS'Unchecked_Access));
      return To_Timespec (TS);
   end Get_Round_Robin_Interval;

end POSIX.Process_Scheduling;
