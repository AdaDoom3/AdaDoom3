------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                  P O S I X . M E M O R Y _ M A P P I N G                 --
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

with POSIX,
     POSIX.C,
     POSIX.IO,
     System,
     System.Storage_Elements;
pragma Elaborate_All (POSIX);
package POSIX.Memory_Mapping is

   use POSIX.C;

   type Protection_Options is new POSIX.Option_Set;
   Allow_Read    : constant Protection_Options;
   Allow_Write   : constant Protection_Options;
   Allow_Execute : constant Protection_Options;

   type Mapping_Options is new POSIX.Option_Set;
   Map_Shared  : constant Mapping_Options;
   Map_Private : constant Mapping_Options;

   type Location_Options is new POSIX.Option_Set;
   Exact_Address  : constant Location_Options;
   Nearby_Address : constant Location_Options;

   function Map_Memory
     (First      : System.Address;
      Length     : System.Storage_Elements.Storage_Offset;
      Protection : Protection_Options;
      Mapping    : Mapping_Options;
      Location   : Location_Options;
      File       : POSIX.IO.File_Descriptor;
      Offset     : POSIX.IO_Count)
      return System.Address;

   function Map_Memory
     (Length     : System.Storage_Elements.Storage_Offset;
      Protection : Protection_Options;
      Mapping    : Mapping_Options;
      File       : POSIX.IO.File_Descriptor;
      Offset     : POSIX.IO_Count)
      return System.Address;

   procedure Unmap_Memory
     (First  : System.Address;
      Length : System.Storage_Elements.Storage_Offset);

   procedure Change_Protection
     (First      : System.Address;
      Length     : System.Storage_Elements.Storage_Offset;
      Protection : Protection_Options);

   type Synchronize_Memory_Options is new POSIX.Option_Set;
   Wait_For_Completion    : constant Synchronize_Memory_Options;
   No_Wait_For_Completion : constant Synchronize_Memory_Options;
   Invalidate_Cached_Data : constant Synchronize_Memory_Options;

   procedure Synchronize_Memory
     (First   : System.Address;
      Length  : System.Storage_Elements.Storage_Offset;
      Options : Synchronize_Memory_Options := Wait_For_Completion);

private
   Allow_Read             : constant Protection_Options :=
     Protection_Options (Option_Set'(Option => POSIX.C.PROT_READ));
   Allow_Write            : constant Protection_Options :=
     Protection_Options (Option_Set'(Option => POSIX.C.PROT_WRITE));
   Allow_Execute          : constant Protection_Options :=
     Protection_Options (Option_Set'(Option => POSIX.C.PROT_EXEC));
   Map_Shared             : constant Mapping_Options :=
     Mapping_Options (Option_Set'(Option => POSIX.C.MAP_SHARED));
   Map_Private            : constant Mapping_Options :=
     Mapping_Options (Option_Set'(Option => POSIX.C.MAP_PRIVATE));
   Exact_Address          : constant Location_Options :=
     Location_Options (Option_Set'(Option => POSIX.C.MAP_FIXED));
   Nearby_Address         : constant Location_Options :=  Empty_Set;
   Wait_For_Completion    : constant Synchronize_Memory_Options :=
     Synchronize_Memory_Options (Option_Set'(Option => POSIX.C.MS_SYNC));
   No_Wait_For_Completion : constant Synchronize_Memory_Options :=
     Synchronize_Memory_Options (Option_Set'(Option => POSIX.C.MS_ASYNC));
   Invalidate_Cached_Data : constant Synchronize_Memory_Options :=
     Synchronize_Memory_Options
       (Option_Set'(Option => POSIX.C.MS_INVALIDATE));
end POSIX.Memory_Mapping;
