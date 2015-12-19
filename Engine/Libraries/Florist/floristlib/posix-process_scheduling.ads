------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--              P O S I X . P R O C E S S _ S C H E D U L I N G             --
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
     POSIX_Process_Identification;
package POSIX.Process_Scheduling is

   subtype Scheduling_Priority is Integer;
   type Scheduling_Parameters is private;
   function Get_Priority (Parameters : Scheduling_Parameters)
     return Scheduling_Priority;
   procedure Set_Priority
     (Parameters : in out Scheduling_Parameters;
      Priority   : Scheduling_Priority);

   type Scheduling_Policy is new Integer;

   --  One might consider auto-configuring
   --  the upper and lower bounds of this range to more tightly fit
   --  the range of values supported by the underlying OS.
   --  However, that would not help much.  For example, LynxOS is
   --  reputed to have the following values:
   --    Sched_Fifo : constant := 2097152;
   --    Sched_Other : constant := 4194304;
   --    Sched_Rr : constant := 1048576;
   --  Also, we should not limit it to the just three values shown below,
   --  because we might want to use these interfaces with implementation
   --  defined policies.

   Sched_FIFO  : constant Scheduling_Policy := POSIX.C.SCHED_FIFO;
   Sched_RR    : constant Scheduling_Policy := POSIX.C.SCHED_RR;
   Sched_Other : constant Scheduling_Policy := POSIX.C.SCHED_OTHER;

   procedure Set_Scheduling_Parameters
     (Process    : POSIX_Process_Identification.Process_ID;
      Parameters : Scheduling_Parameters);
   function Get_Scheduling_Parameters
     (Process : POSIX_Process_Identification.Process_ID)
     return Scheduling_Parameters;
   procedure Set_Scheduling_Policy
     (Process    : POSIX_Process_Identification.Process_ID;
      New_Policy : Scheduling_Policy;
      Parameters : Scheduling_Parameters);
   function Get_Scheduling_Policy
     (Process : POSIX_Process_Identification.Process_ID)
     return Scheduling_Policy;
   procedure Yield;
   function Get_Maximum_Priority (Policy : Scheduling_Policy)
     return Scheduling_Priority;
   function Get_Minimum_Priority (Policy : Scheduling_Policy)
     return Scheduling_Priority;
   function Get_Round_Robin_Interval
     (Process : POSIX_Process_Identification.Process_ID)
     return POSIX.Timespec;

private
   type Scheduling_Parameters is record
      Param : aliased POSIX.C.struct_sched_param;
   end record;
end POSIX.Process_Scheduling;
