------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--       P O S I X . U N S A F E _ P R O C E S S _ P R I M I T I V E S      --
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

with POSIX.C,
     POSIX.Implementation,
     System,
     System.Soft_Links,
     Unchecked_Conversion;

package body POSIX.Unsafe_Process_Primitives is

   use POSIX.C,
       POSIX.Implementation;

   function To_Process_ID is new Unchecked_Conversion
     (pid_t, POSIX.Process_Identification.Process_ID);
   function To_String_List_Ptr is new Unchecked_Conversion
     (POSIX_String_List, String_List_Ptr);
   function To_String_List_Ptr is new Unchecked_Conversion
     (POSIX.Process_Environment.Environment, String_List_Ptr);

   -------------------------
   --  Local Subprograms  --
   -------------------------

   function Make_Path_Name
     (Directory : POSIX_String;
      File : POSIX_String) return POSIX_String;
   pragma Inline (Make_Path_Name);

   --  Concatenate a directory name and a file name.

   function Make_Path_Name
     (Directory : POSIX_String;
      File : POSIX_String) return POSIX_String is
   begin
      if Directory = "" then
         return File;
      end if;
      if Directory (Directory'Last) = '/' then
         return Directory & File;
      end if;
      return Directory & '/' & File;
   end Make_Path_Name;

   ------------
   --  Fork  --
   ------------

   function fork return pid_t;
   pragma Import (C, fork, fork_LINKNAME);

   function Fork return POSIX.Process_Identification.Process_ID is
      Result : pid_t;
      package SSL renames System.Soft_Links;
      --  save local values of soft-link data
      NT_Sec_Stack_Addr : constant System.Address :=
                            SSL.Get_Sec_Stack_Addr.all;
      NT_Jmpbuf_Address : constant System.Address :=
                            SSL.Get_Jmpbuf_Address.all;
   begin
      Result := fork;
      if Result = -1 then
         Raise_POSIX_Error;
      end if;
      if Result = 0 then
         --  reset soft links to non-tasking versions of operations
         SSL.Abort_Defer        := SSL.Abort_Defer_NT'Access;
         SSL.Abort_Undefer      := SSL.Abort_Undefer_NT'Access;
         SSL.Lock_Task          := SSL.Task_Lock_NT'Access;
         SSL.Unlock_Task        := SSL.Task_Unlock_NT'Access;
         SSL.Get_Jmpbuf_Address := SSL.Get_Jmpbuf_Address_NT'Access;
         SSL.Set_Jmpbuf_Address := SSL.Set_Jmpbuf_Address_NT'Access;
         SSL.Get_Sec_Stack_Addr := SSL.Get_Sec_Stack_Addr_NT'Access;
         SSL.Set_Sec_Stack_Addr := SSL.Set_Sec_Stack_Addr_NT'Access;
         --  reset global data to saved local values for this thread
         SSL.Set_Sec_Stack_Addr (NT_Sec_Stack_Addr);
         SSL.Set_Jmpbuf_Address (NT_Jmpbuf_Address);
      end if;
      return To_Process_ID (Result);
   end Fork;

   ------------
   --  Exec  --
   ------------

   function execve
     (path : char_ptr;
      argv : char_ptr_ptr;
      envp : char_ptr_ptr) return int;
   pragma Import (C, execve, execve_LINKNAME);

   procedure Exec
     (Pathname : POSIX.Pathname;
      Arg_List : POSIX.POSIX_String_List
               := POSIX.Empty_String_List;
      Env_List : POSIX.Process_Environment.Environment) is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
      Arg : String_List_Ptr := To_String_List_Ptr (Arg_List);
      Env : String_List_Ptr := To_String_List_Ptr (Env_List);
   begin
      if Arg = null then
         Arg := Null_String_List_Ptr;
      end if;
      if Env = null then
         Env := Null_String_List_Ptr;
      end if;
      Check (execve
        (Pathname_With_NUL (Pathname_With_NUL'First)'Unchecked_Access,
         Arg.Char (1)'Unchecked_Access,
         Env.Char (1)'Unchecked_Access));
   end Exec;

   ------------
   --  Exec  --
   ------------

   function execv
     (path : char_ptr;
      argv : char_ptr_ptr) return int;
   pragma Import (C, execv, execv_LINKNAME);

   procedure Exec
     (Pathname : POSIX.Pathname;
      Arg_List : POSIX.POSIX_String_List
               := POSIX.Empty_String_List) is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
      Arg : String_List_Ptr := To_String_List_Ptr (Arg_List);
   begin
      if Arg = null then
         Arg := Null_String_List_Ptr;
      end if;
      Check (execv
        (Pathname_With_NUL (Pathname_With_NUL'First)'Unchecked_Access,
         Arg.Char (1)'Unchecked_Access));
   end Exec;

   -------------------
   --  Exec_Search  --
   -------------------

   procedure Exec_Search
     (Filename : POSIX.Filename;
      Arg_List : POSIX.POSIX_String_List := POSIX.Empty_String_List;
      Env_List : POSIX.Process_Environment.Environment) is
      Filename_With_NUL : POSIX_String := Filename & NUL;
      Arg : String_List_Ptr := To_String_List_Ptr (Arg_List);
      Env : String_List_Ptr := To_String_List_Ptr (Env_List);
   begin
      --  .... Change POSIX.5?
      --  There is no POSIX.1 function that takes an environment list
      --  and searches for a filename, apparently, so we have to simulate
      --  the effect here.
      if Arg = null then
         Arg := Null_String_List_Ptr;
      end if;
      if Env = null then
         Env := Null_String_List_Ptr;
      end if;
      for I in Filename'Range loop
         if Filename (I) = '/' then
            Check (execve
              (Filename_With_NUL (Filename_With_NUL'First)'Unchecked_Access,
               Arg.Char (1)'Unchecked_Access,
               Env.Char (1)'Unchecked_Access));
            return;
         end if;
      end loop;
      --  filename does not contain "/"
      declare
         Path : constant POSIX_String
              := POSIX.Process_Environment.Environment_Value_Of
                 ("PATH", "/bin:/usr/bin");
         Start : Positive;
         P : Positive;
         Err : Error_Code := No_Such_File_Or_Directory;
      begin
         P := Path'First;
         loop
            Start := P;
            while P <= Path'Last and then Path (P) /= ':' loop
               P := P + 1;
            end loop;
            declare
               Pathname : constant POSIX_String
                 := Make_Path_Name (Path (Start .. P - 1), Filename);
            begin
               Exec (Pathname, Arg_List, Env_List);
            exception
            when POSIX_Error => null;
            end;
            if Get_Error_Code /= No_Such_File_Or_Directory then
               Err := Get_Error_Code;
            end if;
            exit when P > Path'Last;
            P := P + 1; -- skip colon
         end loop;
         Raise_POSIX_Error (Err);
      end;
   end Exec_Search;

   -------------------
   --  Exec_Search  --
   -------------------

   function execvp
     (file : char_ptr;
      argv : char_ptr_ptr) return int;
   pragma Import (C, execvp, execvp_LINKNAME);

   procedure Exec_Search
     (Filename : POSIX.Filename;
      Arg_List : POSIX.POSIX_String_List
               := POSIX.Empty_String_List) is
      Filename_With_NUL : POSIX_String := Filename & NUL;
      Arg : String_List_Ptr := To_String_List_Ptr (Arg_List);
   begin
      if Arg = null then
         Arg := Null_String_List_Ptr;
      end if;
      Check (execvp
        (Filename_With_NUL (Filename_With_NUL'First)'Unchecked_Access,
         Arg.Char (1)'Unchecked_Access));
   end Exec_Search;

end POSIX.Unsafe_Process_Primitives;
