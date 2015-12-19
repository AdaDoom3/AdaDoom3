------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--             P O S I X . C O N D I T I O N _ V A R I A B L E S            --
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
     POSIX.Mutexes;

package POSIX.Condition_Variables is

   --  ==========  --
   --   WARNINGS   --
   --  ==========  --

   --  This package is for mixed-language programming, in which
   --  an Ada task needs to synchronize with a C thread.

   --  Do NOT use POSIX CVs to synchronize between Ada tasks.
   --  Instead, use Ada protected objects.

   --  If you use one of these "raw" CVs, you risk undefined
   --  behavior if you violate any of the POSIX.1c rules about CVs,
   --  or if you attempt to abort (including ATC) a task that is performing
   --  a mutex or CV operation.

   type Condition is limited private;
   type Condition_Descriptor is private;

   type Attributes is private;
   procedure Initialize (Attr : in out Attributes);
   procedure Finalize (Attr : in out Attributes);

   function Get_Process_Shared (Attr : Attributes) return Boolean;
   procedure Set_Process_Shared
     (Attr      : in out Attributes;
      Is_Shared : Boolean := False);

   procedure Initialize
     (Cond : in out Condition;
      Attr : Attributes);
   procedure Initialize (Cond : in out Condition);
   function Descriptor_Of (Cond : Condition) return Condition_Descriptor;
   procedure Finalize (Cond : in out Condition);

   procedure Signal (Cond : Condition_Descriptor);
   procedure Broadcast (Cond : Condition_Descriptor);

   procedure Wait
     (Cond : Condition_Descriptor;
      M    : POSIX.Mutexes.Mutex_Descriptor);
   procedure Timed_Wait
     (Cond    : Condition_Descriptor;
      M       : POSIX.Mutexes.Mutex_Descriptor;
      Timeout : POSIX.Timespec);

private
   type Condition_Descriptor is access POSIX.C.pthread_cond_t;
   type Attributes_Descriptor is access POSIX.C.pthread_condattr_t;
   type Attributes is record
      Attr : Attributes_Descriptor;
   end record;
   type Condition is record
      Cond : Condition_Descriptor;
   end record;
end POSIX.Condition_Variables;
