------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                         P O S I X . M U T E X E S                        --
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
package POSIX.Mutexes is

   --  ==========  --
   --   WARNINGS   --
   --  ==========  --

   --  This package is for mixed-language programming, in which
   --  an Ada task needs to synchronize with a C thread.

   --  Do NOT use POSIX mutexes to synchronize between Ada tasks.
   --  Instead, use Ada protected objects.
   --  Protected objects are implemented using mutexes.
   --  The difference is that they are safer.
   --  In particular protected operations are abort-deferred,
   --  and have cleanup code to ensure mutexes are always released,
   --  even if a protected operation completes abnormally due to an exception.
   --  If you use one of these "raw" mutexes, you risk undefined
   --  behavior if you violate any of the POSIX.1c rules about mutexes,
   --  or if you attempt to abort (including ATC) a task that is performing
   --  a mutex or CV operation.

   type Mutex is limited private;
   type Mutex_Descriptor is private;

   type Attributes is private;
   procedure Initialize (Attr : in out Attributes);
   procedure Finalize (Attr : in out Attributes);

   function Get_Process_Shared (Attr : Attributes)
     return Boolean;
   procedure Set_Process_Shared
     (Attr      : in out Attributes;
      Is_Shared : Boolean := False);

   subtype Ceiling_Priority is Integer;
   type Locking_Policy is range 0 .. 2;
   No_Priority_Inheritance : constant Locking_Policy := 0;
   Highest_Blocked_Task : constant Locking_Policy := 1;
   Highest_Ceiling_Priority : constant Locking_Policy := 2;

   procedure Set_Locking_Policy
     (Attr    : in out Attributes;
      Locking : Locking_Policy);
   function Get_Locking_Policy
     (Attr : Attributes)
     return Locking_Policy;
   procedure Set_Ceiling_Priority
     (Attr        : in out Attributes;
      New_Ceiling : Ceiling_Priority);
   function Get_Ceiling_Priority (Attr : Attributes)
     return Ceiling_Priority;

   procedure Initialize
     (M    : in out Mutex;
      Attr : Attributes);
   procedure Initialize (M : in out Mutex);
   function Descriptor_Of (M : Mutex) return Mutex_Descriptor;
   procedure Finalize (M : in out Mutex);

   procedure Set_Ceiling_Priority
     (M           : Mutex_Descriptor;
      New_Ceiling : Ceiling_Priority;
      Old_Ceiling : out Ceiling_Priority);
   function Get_Ceiling_Priority (M : Mutex_Descriptor)
     return Ceiling_Priority;

   procedure Lock (M : Mutex_Descriptor);
   function  Try_Lock (M : Mutex_Descriptor) return Boolean;
   procedure Unlock (M : Mutex_Descriptor);

private
   type Dummy is tagged null record;
   type Attributes is record
      Attr : aliased POSIX.C.pthread_mutexattr_t;
      --  to force by-reference parameter mode:
      D : Dummy;
   end record;
   type Mutex is record
      Mutex : aliased POSIX.C.pthread_mutex_t;
      --  to force by-reference parameter mode:
      D : Dummy;
   end record;
   --  The "access constant" is sometimes a lie, but it allows
   --  us to emulate the POSIX C-language interface without violating
   --  Ada rules about pointers to variables vs. pointers to constants.
   type Mutex_Descriptor is access constant POSIX.C.pthread_mutex_t;
   pragma Convention (C, Mutex_Descriptor);
end POSIX.Mutexes;
