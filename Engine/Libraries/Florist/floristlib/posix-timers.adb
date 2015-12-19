------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                          P O S I X . T I M E R S                         --
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
     Unchecked_Conversion;

package body POSIX.Timers is

   use POSIX.C;
   use POSIX.Implementation;

   function To_int is new Unchecked_Conversion (Bits, int);
   function To_Struct_Sigevent is new Unchecked_Conversion
     (POSIX.Signals.Signal_Event, POSIX.C.struct_sigevent);

   Zero_Timespec : aliased constant struct_timespec := (0, 0);
   Zero_State : aliased constant struct_itimerspec := ((0, 0), (0, 0));

   -------------------
   --  Set_Initial  --
   -------------------

   procedure Set_Initial
     (State   : in out Timer_State;
      Initial : POSIX.Timespec) is
   begin
      State.State.it_value := To_Struct_Timespec (Initial);
   end Set_Initial;

   -------------------
   --  Get_Initial  --
   -------------------

   function Get_Initial (State : Timer_State) return POSIX.Timespec is
   begin
      return To_Timespec (To_Duration (State.State.it_value));
   end Get_Initial;

   --------------------
   --  Set_Interval  --
   --------------------

   procedure Set_Interval
     (State    : in out Timer_State;
      Interval : POSIX.Timespec) is
   begin
      State.State.it_interval := To_Struct_Timespec (Interval);
   end Set_Interval;

   --------------------
   --  Get_Interval  --
   --------------------

   function Get_Interval (State : Timer_State) return POSIX.Timespec is
   begin
      return To_Timespec (To_Duration (State.State.it_interval));
   end Get_Interval;
   -----------------
   --  Set_Time   --
   -----------------

   function clock_settime
     (clock_id : clockid_t;
      tp : timespec_ptr) return int;
   pragma Import (C, clock_settime, clock_settime_LINKNAME);

   procedure Set_Time
     (Clock : Clock_ID;
      Value : POSIX.Timespec) is
      TS : aliased struct_timespec;
   begin
      TS := To_Struct_Timespec (Value);
      Check (clock_settime (clockid_t (Clock), TS'Unchecked_Access));
   end Set_Time;

   ----------------
   --  Set_Time  --
   ----------------

   procedure Set_Time
     (Value : POSIX.Timespec) is
      TS : aliased struct_timespec;
   begin
      TS := To_Struct_Timespec (Value);
      Check (clock_settime (POSIX.C.CLOCK_REALTIME, TS'Unchecked_Access));
   end Set_Time;

   ----------------
   --  Get_Time  --
   ----------------

   function clock_gettime
     (clock_id : clockid_t;
      tp : access struct_timespec) return int;
   pragma Import (C, clock_gettime, clock_gettime_LINKNAME);

   function Get_Time
     (Clock : Clock_ID := Clock_Realtime) return POSIX.Timespec is
      TS : aliased struct_timespec;
   begin
      Check (clock_gettime (clockid_t (Clock), TS'Unchecked_Access));
      return To_Timespec (To_Duration (TS));
   end Get_Time;

   ----------------------
   --  Get_Resolution  --
   ----------------------

   function Get_Resolution
     (Clock : Clock_ID := Clock_Realtime) return POSIX.Timespec is
      function clock_getres
        (clock_id : clockid_t;
         res : access struct_timespec) return int;
      pragma Import (C, clock_getres, clock_getres_LINKNAME);
      TS : aliased struct_timespec;
   begin
      Check (clock_getres (clockid_t (Clock), TS'Unchecked_Access));
      return To_Timespec (To_Duration (TS));
   end Get_Resolution;

   --------------------
   --  Create_Timer  --
   --------------------

   function Create_Timer
     (Clock : Clock_ID;
      Event : POSIX.Signals.Signal_Event) return Timer_ID is
      function timer_create
        (clock_id : clockid_t;
         evp : sigevent_ptr;
         timerid : access timer_t) return int;
      pragma Import (C, timer_create, timer_create_LINKNAME);
      --  .... Consider making Signal_Event into a tagged type
      --  so that we don't need to make a local copy.
      E : aliased POSIX.C.struct_sigevent := To_Struct_Sigevent (Event);
      TID : aliased timer_t;
   begin
      if E.sigev_notify = POSIX.C.SIGEV_NONE then
         --  make sure the other fields are valid
         E.sigev_signo := SIGUSR1;
         E.sigev_value := null_sigval;
      end if;
      Check (timer_create (clockid_t (Clock),
         E'Unchecked_Access, TID'Unchecked_Access));
      return Timer_ID (TID);
   end Create_Timer;

   --------------------
   --  Delete_Timer  --
   --------------------

   procedure Delete_Timer (Timer : in out Timer_ID) is
      function timer_delete (timer_id : timer_t) return int;
      pragma Import (C, timer_delete, timer_delete_LINKNAME);
   begin
      Check (timer_delete (timer_t (Timer)));
   end Delete_Timer;

   -----------------
   --  Arm_Timer  --
   -----------------

   function timer_settime
     (timer_id : timer_t;
      flags : C.int;
      value : itimerspec_ptr;
      ovalue : itimerspec_ptr) return int;
   pragma Import (C, timer_settime, timer_settime_LINKNAME);

   procedure Arm_Timer
     (Timer     : Timer_ID;
      Options   : Timer_Options;
      New_State : Timer_State;
      Old_State : out Timer_State) is
   begin
      --  ????? Change POSIX.5b?
      --  The following two checks are required by .5b, but
      --  they are inconsistent with one another
      --  and they do not seem to be founded on the .1b specification.
      if Options = Absolute_Timer then
         Check (New_State.State.it_value /= Zero_Timespec, Invalid_Argument);
      else
         Check (New_State.State.it_value.tv_sec > 0, Invalid_Argument);
      end if;
      Check (timer_settime (timer_t (Timer),
        To_int (Option_Set (Options).Option),
        New_State.State'Unchecked_Access,
        Old_State.State'Unchecked_Access));
   end Arm_Timer;

   -----------------
   --  Arm_Timer  --
   -----------------

   procedure Arm_Timer
     (Timer     : Timer_ID;
      Options   : Timer_Options;
      New_State : Timer_State) is
   begin
      Check (New_State.State.it_value /= Zero_Timespec, Invalid_Argument);
      Check (timer_settime (timer_t (Timer),
        To_int (Option_Set (Options).Option),
        New_State.State'Unchecked_Access, null));
   end Arm_Timer;

   -----------------------
   --  Get_Timer_State  --
   -----------------------

   function Get_Timer_State (Timer : Timer_ID) return Timer_State is
      function timer_gettime
         (timer_id : timer_t;
          value : access struct_itimerspec) return int;
      pragma Import (C, timer_gettime, timer_gettime_LINKNAME);
      TS : Timer_State;
   begin
      Check (timer_gettime (timer_t (Timer), TS.State'Unchecked_Access));
      return TS;
   end Get_Timer_State;

   --------------------
   --  Disarm_Timer  --
   --------------------

   procedure Disarm_Timer (Timer : Timer_ID) is
   begin
      Check (timer_settime
        (timer_t (Timer), 0, Zero_State'Unchecked_Access, null));
   end Disarm_Timer;

   --------------------------
   --  Get_Timer_Overruns  --
   --------------------------

   function Get_Timer_Overruns (Timer : Timer_ID) return Natural is
      function timer_getoverrun (timer_id : timer_t) return int;
      pragma Import (C, timer_getoverrun, timer_getoverrun_LINKNAME);
   begin
      return Natural (Check (timer_getoverrun (timer_t (Timer))));
   end Get_Timer_Overruns;

end POSIX.Timers;
