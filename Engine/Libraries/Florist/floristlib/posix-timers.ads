------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                          P O S I X . T I M E R S                         --
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

with POSIX.C,
     POSIX.Signals;
package POSIX.Timers is

   type Clock_ID is private;
   type Timer_ID is private;

   Clock_Realtime : constant Clock_ID;

   type Timer_State is private;
   type Timer_Options is new POSIX.Option_Set;
   Absolute_Timer : constant Timer_Options;

   procedure Set_Initial
     (State   : in out Timer_State;
      Initial : POSIX.Timespec);
   function Get_Initial (State : Timer_State) return POSIX.Timespec;
   procedure Set_Interval
     (State    : in out Timer_State;
      Interval : POSIX.Timespec);
   function Get_Interval (State : Timer_State) return POSIX.Timespec;

   procedure Set_Time
     (Clock : Clock_ID;
      Value : POSIX.Timespec);
   procedure Set_Time (Value : POSIX.Timespec);
   function Get_Time
     (Clock : Clock_ID := Clock_Realtime)
     return POSIX.Timespec;
   function Get_Resolution
     (Clock : Clock_ID := Clock_Realtime)
     return POSIX.Timespec;

   function Create_Timer
     (Clock : Clock_ID;
      Event : POSIX.Signals.Signal_Event) return Timer_ID;
   procedure Delete_Timer (Timer : in out Timer_ID);
   procedure Arm_Timer
     (Timer     : Timer_ID;
      Options   : Timer_Options;
      New_State : Timer_State;
      Old_State : out Timer_State);
   procedure Arm_Timer
     (Timer     : Timer_ID;
      Options   : Timer_Options;
      New_State : Timer_State);
   function Get_Timer_State (Timer : Timer_ID) return Timer_State;
   procedure Disarm_Timer (Timer : Timer_ID);
   function Get_Timer_Overruns (Timer : Timer_ID) return Natural;

private
   type Clock_ID is new POSIX.C.clockid_t;
   Clock_Realtime : constant Clock_ID := POSIX.C.CLOCK_REALTIME;
   type Timer_ID is new POSIX.C.timer_t;
   --  We add a tag to force by-reference parameter passing.
   --  This allows us to pass through to the C interface pointers
   --  directly to the argument, thereby saving copying.
   type Timer_State is tagged record
      State : aliased POSIX.C.struct_itimerspec;
   end record;
   Absolute_Timer : constant Timer_Options :=
      Timer_Options (Option_Set'(Option => POSIX.C.TIMER_ABSTIME));
end POSIX.Timers;
