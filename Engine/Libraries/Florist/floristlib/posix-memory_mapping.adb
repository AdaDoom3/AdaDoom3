------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                  P O S I X . M E M O R Y _ M A P P I N G                 --
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

package body POSIX.Memory_Mapping is

   use POSIX.Implementation;
   use System;

   function To_Address is
     new Unchecked_Conversion (ptr_as_int, System.Address);

   Zero_Address : constant System.Address := To_Address (0);
   Failure      : constant System.Address := To_Address (MAP_FAILED);

   ------------------
   --  Map_Memory  --
   ------------------

   function mmap
     (addr : System.Address;
      len : size_t;
      prot : int;
      flags : int;
      fildes : int;
      off : off_t) return System.Address;
   pragma Import (C, mmap, mmap_LINKNAME);

   function Map_Memory
     (First      : System.Address;
      Length     : System.Storage_Elements.Storage_Offset;
      Protection : Protection_Options;
      Mapping    : Mapping_Options;
      Location   : Location_Options;
      File       : POSIX.IO.File_Descriptor;
      Offset     : POSIX.IO_Count)
      return System.Address is
      Result : System.Address;
   begin
      Result := mmap
       (First,
        size_t (Length),
        int (Option_Set (Protection).Option),
        int (Option_Set (Mapping).Option or Option_Set (Location).Option
           or MAP_FILE),
        int (File),
        off_t (Offset));
      if Result = Failure then
         Raise_POSIX_Error;
      end if;
      return Result;
   end Map_Memory;

   ------------------
   --  Map_Memory  --
   ------------------

   function Map_Memory
     (Length     : System.Storage_Elements.Storage_Offset;
      Protection : Protection_Options;
      Mapping    : Mapping_Options;
      File       : POSIX.IO.File_Descriptor;
      Offset     : POSIX.IO_Count)
      return System.Address is
      Result : System.Address;
   begin
      Result := mmap
        (Zero_Address,
         size_t (Length),
         int (Option_Set (Protection).Option),
         int (Option_Set (Mapping).Option or MAP_FILE),
         int (File),
         off_t (Offset));
      if Result = Failure then
         Raise_POSIX_Error;
      end if;
      return Result;
   end Map_Memory;

   --------------------
   --  Unmap_Memory  --
   --------------------

   procedure Unmap_Memory
     (First  : System.Address;
      Length : System.Storage_Elements.Storage_Offset) is
      function munmap
        (addr : System.Address;
         len : size_t) return int;
      pragma Import (C, munmap, munmap_LINKNAME);
   begin
      Check (munmap (First, size_t (Length)));
   end Unmap_Memory;

   -------------------------
   --  Change_Protection  --
   -------------------------

   procedure Change_Protection
     (First      : System.Address;
      Length     : System.Storage_Elements.Storage_Offset;
      Protection : Protection_Options) is
      function mprotect
        (addr : System.Address;
         len : size_t;
         prot : int) return int;
      pragma Import (C, mprotect, mprotect_LINKNAME);
   begin
      Check (mprotect (First, size_t (Length),
        int (Option_Set (Protection).Option)));
   end Change_Protection;

   --------------------------
   --  Synchronize_Memory  --
   --------------------------

   procedure Synchronize_Memory
     (First   : System.Address;
      Length  : System.Storage_Elements.Storage_Offset;
      Options : Synchronize_Memory_Options := Wait_For_Completion) is
      function msync
        (address : System.Address;
         len : size_t;
         flags : int) return int;
      pragma Import (C, msync, msync_LINKNAME);
   begin
      Check (msync
        (First, size_t (Length), int (Option_Set (Options).Option)));
   end Synchronize_Memory;

end POSIX.Memory_Mapping;
