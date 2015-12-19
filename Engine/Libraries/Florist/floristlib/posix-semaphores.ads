------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                      P O S I X . S E M A P H O R E S                     --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2010, AdaCore                     --
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
     POSIX.IO,
     POSIX.Permissions;
package POSIX.Semaphores is

   type Semaphore is limited private;
   type Semaphore_Descriptor is private;
   procedure Initialize
     (Sem       : in out Semaphore;
      Value     : Natural;
      Is_Shared : Boolean := False);
   function Descriptor_Of (Sem : Semaphore) return Semaphore_Descriptor;
   procedure Finalize (Sem : in out Semaphore);
   function Open
     (Name           : POSIX.POSIX_String;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return Semaphore_Descriptor;
   function Open_Or_Create
     (Name           : POSIX.POSIX_String;
      Permissions    : POSIX.Permissions.Permission_Set;
      Value          : Natural;
      Options        : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return Semaphore_Descriptor;
   procedure Close (Sem : in out Semaphore_Descriptor);
   procedure Unlink_Semaphore (Name : POSIX.POSIX_String);
   procedure Wait
     (Sem            : Semaphore_Descriptor;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);
   function Try_Wait (Sem : Semaphore_Descriptor) return Boolean;
   procedure Post (Sem : Semaphore_Descriptor);
   function Get_Value (Sem : Semaphore_Descriptor) return Integer;

   --  .... Change POSIX.5b?
   --  The Wait and Try_Wait operations are allowed to be interruptible
   --  by a signal, according to the C binding.  Here, we have no
   --  Masked_Signals parameter.  If the system support POSIX threads,
   --  we are probably OK, since we will want to keep most signals masked,
   --  in all threads but the corresponding handler (if any).

private
   --  We rely that type Semaphore will be passed by reference,
   --  so that we can use 'Address of a parameter (even an "in" parameter)
   --  to get a pointer to the actual object.
   --  If there is any danger that it will not be passed by reference,
   --  we will need to enclose the "sem_t" value as an aliased component of a
   --  record or even of a tagged type.
   type Dummy is tagged null record;
   type Semaphore is record
      Sem : aliased POSIX.C.sem_t;
      --  to force by-reference parameter mode:
      D : Dummy;
   end record;
   --  The "access constant" is sometimes a lie, but it allows
   --  us to emulate the POSIX C-language interface without violating
   --  Ada rules about pointers to variables vs. pointers to constants.
   type Semaphore_Descriptor is access constant POSIX.C.sem_t;
   pragma Convention (C, Semaphore_Descriptor);
end POSIX.Semaphores;
