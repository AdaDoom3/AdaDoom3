------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--           P O S I X . G E N E R I C _ S H A R E D _ M E M O R Y          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2010, AdaCore                     --
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

--  See the warnings in the package spec.

--  This package presents potential semantic and implementation
--  problems.  We do not want shared objects to be reinitialized for
--  each process that uses them.  We do not want shared objects
--  finalized, or at least not until the "last close" of the
--  shared memory object in which they reside.

--  The present implementation makes no attempt to deal correctly
--  with controlled types.

--  It also relies on the assumption that an "access all" pointer
--  is meaningfully unchecked-convertible to an ordinary "access"
--  value.

--  ....
--  This has several critical sections, to give the effect of atomicity
--  from a series of system calls.
--  We have put exception handlers around these, to make sure the lock
--  gets released if there happens to be an exception.
--  In some cases we may be able to convince ourselves that no exception
--  is possible, but there is still the possibility of Storage_Error.

with POSIX.Implementation,
     POSIX.Memory_Range_Locking,
     POSIX.Shared_Memory_Objects,
     System,
     System.Storage_Elements,
     Unchecked_Conversion;

package body POSIX.Generic_Shared_Memory is

   use POSIX.Implementation;
   use type POSIX.IO.File_Descriptor;
   use type POSIX.Memory_Mapping.Protection_Options;

   Length : constant POSIX.IO_Count :=
     Object_Type'Max_Size_In_Storage_Elements;
   type Private_Ptr is access all Object_Type;
   function To_Shared_Access is
     new Unchecked_Conversion (Private_Ptr, Shared_Access);

   --  One instantiation of this package can be used to open
   --  several shared memory objects, with different file descriptors.
   --  We need a list to keep track of the mapping from file descriptor
   --  to start-address.

   type Node;
   type Node_List is access all Node;
   type Node is record
      FD : POSIX.IO.File_Descriptor;
      Start_addr : System.Address;
      Pointer : Private_Ptr;
      Next : Node_List;
   end record;

   Head : Node_List := null;
   pragma Volatile (Head);
   Avail : Node_List := null;
   pragma Volatile (Avail);

   ------------------------
   --  Local Subprograms --
   ------------------------

   procedure Insert_Node
     (FD    : POSIX.IO.File_Descriptor;
      Start : System.Address);
   function Start_Of_Shared_Memory
     (File : POSIX.IO.File_Descriptor) return System.Address;
   procedure Remove_Node (FD : POSIX.IO.File_Descriptor);

   -------------------
   --  Insert_Node  --
   -------------------

   procedure Insert_Node
     (FD : POSIX.IO.File_Descriptor;
      Start : System.Address) is
      T : Node_List;
      --  The local object is necessary to force initialization.
      --  Unfortunately, it means that if the type has finalization
      --  we also get the finalization, before we return from this call.
      --  .... That is unwanted, but what else can we do?
      X : aliased Object_Type;
      for X'Address use Start;
   begin
      if Avail /= null then
         T := Avail; Avail := Avail.Next;
      else
         T := new Node;
      end if;
      T.FD := FD;
      T.Start_addr := Start;
      T.Pointer := X'Unchecked_Access;
      T.Next := Head;
      Head := T;
   end Insert_Node;

   -------------------
   --  Remove_Node  --
   -------------------

   procedure Remove_Node (FD : POSIX.IO.File_Descriptor) is
      T, Prev : Node_List;
   begin
      T := Head;
      Prev := Head;
      while T /= null loop
         if T.FD = FD then
            if Prev = T then
               Head := T.Next;
            else
               Prev.Next := T.Next;
            end if;
            T.Next := Avail;
            Avail := T;
            return;
         else
            Prev := T; T := T.Next;
         end if;
      end loop;
      Raise_POSIX_Error (POSIX.Bad_File_Descriptor);
   end Remove_Node;

   ------------------------------
   --  Start_Of_Shared_Memory  --
   ------------------------------

   function Start_Of_Shared_Memory
     (File : POSIX.IO.File_Descriptor) return System.Address is
      T : Node_List;
   begin
      Begin_Critical_Section;
      begin
         T := Head;
         while T /= null loop
            if T.FD = File then
               End_Critical_Section;
               return T.Start_addr;
            end if;
            T := T.Next;
         end loop;
         End_Critical_Section;
      exception when others =>
         End_Critical_Section; raise;
      end;
      Raise_POSIX_Error (POSIX.Bad_File_Descriptor);
      --  to suppress compiler warning:
      return System.Null_Address;
   end Start_Of_Shared_Memory;

   ----------------------------------
   --  Open_And_Map_Shared_Memory  --
   ----------------------------------

   --  No adjustment of signal mask in these procedures.
   --  We just pass on the masking information to the "open".

   function Open_And_Map_Shared_Memory
     (Name           : POSIX.POSIX_String;
      Protection     : POSIX.Memory_Mapping.Protection_Options;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return POSIX.IO.File_Descriptor is
      FD : POSIX.IO.File_Descriptor;
      Mode : POSIX.IO.File_Mode;
   begin
      if Protection >= POSIX.Memory_Mapping.Allow_Write then
         Mode := POSIX.IO.Read_Write;
      else
         Mode := POSIX.IO.Read_Only;
      end if;
      Begin_Critical_Section;
      begin
         FD := POSIX.Shared_Memory_Objects.Open_Shared_Memory
           (Name, Mode, POSIX.IO.Empty_Set, Masked_Signals);
         if Protection >= POSIX.Memory_Mapping.Allow_Write then
            POSIX.IO.Truncate_File (FD, Length);
         end if;
         Insert_Node (FD, POSIX.Memory_Mapping.Map_Memory
           (System.Storage_Elements.Storage_Offset (Length),
            Protection, POSIX.Memory_Mapping.Map_Shared, FD, 0));
         End_Critical_Section;
      exception when others =>
         End_Critical_Section; raise;
      end;
      return FD;
   end Open_And_Map_Shared_Memory;

   --------------------------------------------
   --  Open_Or_Create_And_Map_Shared_Memory  --
   --------------------------------------------

   function Open_Or_Create_And_Map_Shared_Memory
     (Name           : POSIX.POSIX_String;
      Protection     : POSIX.Memory_Mapping.Protection_Options;
      Permissions    : POSIX.Permissions.Permission_Set;
      Options        : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
     return POSIX.IO.File_Descriptor is
      FD : POSIX.IO.File_Descriptor;
      Mode : POSIX.IO.File_Mode;
   begin
      if Protection >= POSIX.Memory_Mapping.Allow_Write then
         Mode := POSIX.IO.Read_Write;
      else
         Mode := POSIX.IO.Read_Only;
      end if;
      Begin_Critical_Section;
      begin
         FD := POSIX.Shared_Memory_Objects.Open_Or_Create_Shared_Memory
           (Name, Mode, Permissions, Options, Masked_Signals);
         if Protection >= POSIX.Memory_Mapping.Allow_Write then
            POSIX.IO.Truncate_File (FD, Length);
         end if;
         Insert_Node (FD, POSIX.Memory_Mapping.Map_Memory
           (System.Storage_Elements.Storage_Offset (Length),
            Protection, POSIX.Memory_Mapping.Map_Shared, FD, 0));
         End_Critical_Section;
      exception when others =>
         End_Critical_Section; raise;
      end;
      return FD;
   end Open_Or_Create_And_Map_Shared_Memory;

   ----------------------------
   --  Access_Shared_Memory  --
   ----------------------------

   function Access_Shared_Memory
     (File : POSIX.IO.File_Descriptor) return Shared_Access is
      T : Node_List;
   begin
      Begin_Critical_Section;
      begin
         T := Head;
         while T /= null loop
            if T.FD = File then
               End_Critical_Section;
               return To_Shared_Access (T.Pointer);
            end if;
            T := T.Next;
         end loop;
         End_Critical_Section;
      exception when others =>
         End_Critical_Section; raise;
      end;
      Raise_POSIX_Error (POSIX.Bad_File_Descriptor);
      --  To suppress compiler warning message:
      return null;
   end Access_Shared_Memory;

   -------------------------------------
   --  Unmap_And_Close_Shared_Memory  --
   -------------------------------------

   procedure Unmap_And_Close_Shared_Memory
     (File : POSIX.IO.File_Descriptor) is
   begin
      Begin_Critical_Section;
      begin
         POSIX.Memory_Mapping.Unmap_Memory
           (Start_Of_Shared_Memory (File),
            Object_Type'Max_Size_In_Storage_Elements);
         Remove_Node (File);
         POSIX.IO.Close (File);
         End_Critical_Section;
      exception when others =>
         End_Critical_Section; raise;
      end;
      --  .... If we could detect "last close", and if we could
      --  detect that the type has finalization, we might want to
      --  call finalization here, for the last close.
   end Unmap_And_Close_Shared_Memory;

   --------------------------
   --  Lock_Shared_Memory  --
   --------------------------

   procedure Lock_Shared_Memory
     (File : POSIX.IO.File_Descriptor) is
   begin
      POSIX.Memory_Range_Locking.Lock_Range
        (Start_Of_Shared_Memory (File),
         System.Storage_Elements.Storage_Offset
           (Object_Type'Max_Size_In_Storage_Elements));
   end Lock_Shared_Memory;

   ----------------------------
   --  Unlock_Shared_Memory  --
   ----------------------------

   procedure Unlock_Shared_Memory
     (File : POSIX.IO.File_Descriptor) is
   begin
      POSIX.Memory_Range_Locking.Unlock_Range
        (Start_Of_Shared_Memory (File),
         System.Storage_Elements.Storage_Offset
           (Object_Type'Max_Size_In_Storage_Elements));
   end Unlock_Shared_Memory;

end POSIX.Generic_Shared_Memory;
