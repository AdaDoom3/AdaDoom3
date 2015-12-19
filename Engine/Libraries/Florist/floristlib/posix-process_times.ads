------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                   P O S I X . P R O C E S S _ T I M E S                  --
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

with POSIX.C;
package POSIX.Process_Times is

   type Tick_Count is new POSIX.C.clock_t;
   --  Minimally 0 to 24 hours
   Ticks_Per_Second : constant Tick_Count;
   function Elapsed_Real_Time return Tick_Count;
   type Process_Times is private;
   function Get_Process_Times return Process_Times;
   function Elapsed_Real_Time_Of (Times : Process_Times) return Tick_Count;
   function User_CPU_Time_Of (Times : Process_Times) return Tick_Count;
   function System_CPU_Time_Of (Times : Process_Times) return Tick_Count;
   function Descendants_User_CPU_Time_Of (Times : Process_Times)
      return Tick_Count;
   function Descendants_System_CPU_Time_Of (Times : Process_Times)
      return Tick_Count;

private

   function sysconf (c_name : POSIX.C.int) return POSIX.C.long;
   pragma Import (C, sysconf, POSIX.C.sysconf_LINKNAME);

   Ticks_Per_Second : constant Tick_Count :=
     Tick_Count (sysconf (POSIX.C.SC_CLK_TCK));
   type Process_Times is record
      tms : aliased POSIX.C.struct_tms;
      Elapsed_Real_Time : POSIX.C.clock_t;
   end record;

end POSIX.Process_Times;
