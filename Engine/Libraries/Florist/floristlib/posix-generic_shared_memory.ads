------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--           P O S I X . G E N E R I C _ S H A R E D _ M E M O R Y          --
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

--  .... Change POSIX.5b????

   --  ==========  --
   --   WARNINGS   --
   --  ==========  --

   --  1) DO NOT instantiate this package for a controlled type.  If you
   --  do, at best finalization will not work correctly. At worst, you
   --  will crash the entire process.

   --  2) DO NOT instantiate and use Unchecked_Deallocation for the type
   --  Shared_Access belonging to an instantiation of this package.  If
   --  you do, you are likely to end up corrupting the heap, and possibly
   --  crashing the entire process.

with POSIX.IO,
     POSIX.Permissions,
     POSIX.Memory_Mapping;
generic
   type Object_Type is private;
   --  Do not instantiate with a controlled type!
   --  See note below for reasons.
package POSIX.Generic_Shared_Memory is

   type Shared_Access is access Object_Type;

   function Open_And_Map_Shared_Memory
     (Name           : POSIX.POSIX_String;
      Protection     : POSIX.Memory_Mapping.Protection_Options;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return POSIX.IO.File_Descriptor;
   function Open_Or_Create_And_Map_Shared_Memory
     (Name           : POSIX.POSIX_String;
      Protection     : POSIX.Memory_Mapping.Protection_Options;
      Permissions    : POSIX.Permissions.Permission_Set;
      Options        : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return POSIX.IO.File_Descriptor;
   function Access_Shared_Memory
     (File : POSIX.IO.File_Descriptor) return Shared_Access;
   procedure Unmap_And_Close_Shared_Memory
     (File : POSIX.IO.File_Descriptor);
   procedure Lock_Shared_Memory (File : POSIX.IO.File_Descriptor);
   procedure Unlock_Shared_Memory (File : POSIX.IO.File_Descriptor);

end POSIX.Generic_Shared_Memory;
