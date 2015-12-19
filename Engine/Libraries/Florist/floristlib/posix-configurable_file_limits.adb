------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--        P O S I X . C O N F I G U R A B L E _ F I L E _ L I M I T S       --
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
     POSIX.Implementation;

package body POSIX.Configurable_File_Limits is

   use POSIX.C;
   use POSIX.Implementation;

   -------------------------
   --  Local Subprograms  --
   -------------------------

   function pathconf (path : char_ptr; name : int) return long;
   pragma Import (C, pathconf, pathconf_LINKNAME);

   function fpathconf (fd : int; name : int) return long;
   pragma Import (C, fpathconf, fpathconf_LINKNAME);

   function Is_Limited
     (Pathname : POSIX.Pathname;
      PC_Code : int) return Boolean;
   function Is_Limited
     (File : POSIX.IO.File_Descriptor;
      PC_Code : int) return Boolean;
   function Is_Supported
     (Pathname : POSIX.Pathname;
      PC_Code : int) return Boolean;
   function Is_Supported
     (File : POSIX.IO.File_Descriptor;
      PC_Code : int) return Boolean;
   function Limit
     (Pathname : POSIX.Pathname;
      PC_Code : int;
      Default_Maximum : long) return long;
   function Limit
     (File : POSIX.IO.File_Descriptor;
      PC_Code : int;
      Default_Maximum : long) return long;

   function Is_Limited
     (Pathname : POSIX.Pathname;
      PC_Code : int) return Boolean is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      Store_Errno (0);
      --  -1 without errno unchanged -> no limit
      --  -1 with errno              -> bad name or other error
      --  other values               -> there exists a limit
      if pathconf (Pathname_With_NUL
        (Pathname_With_NUL'First)'Unchecked_Access, PC_Code) = -1
      then
         if Fetch_Errno /= 0 then
            Raise_POSIX_Error;
         end if;
         return False;
      else
         return True;
      end if;
   end Is_Limited;

   function Is_Limited
     (File : POSIX.IO.File_Descriptor;
      PC_Code : int) return Boolean is
   begin
      Store_Errno (0);
      if fpathconf (int (File), PC_Code) = -1 then
         if Fetch_Errno /= 0 then
            Raise_POSIX_Error;
         end if;
         return False;
      else
         return True;
      end if;
   end Is_Limited;

   function Limit
     (Pathname : POSIX.Pathname;
      PC_Code : int;
      Default_Maximum : long) return long is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
      Result : long;
   begin
      Result := pathconf
       (Pathname_With_NUL (Pathname_With_NUL'First)'Unchecked_Access, PC_Code);
      if Result = -1 then
         if Fetch_Errno /= 0 then
            Raise_POSIX_Error;
         end if;
         return Default_Maximum;
      else
         return Result;
      end if;
   end Limit;

   function Limit
     (File : POSIX.IO.File_Descriptor;
      PC_Code : int;
      Default_Maximum : long) return long is
      Result : long;
   begin
      Result := fpathconf (int (File), PC_Code);
      if Result = -1 then
         if Fetch_Errno /= 0 then
            Raise_POSIX_Error;
         end if;
         return Default_Maximum;
      else
         return Result;
      end if;
   end Limit;

   function Is_Supported
     (Pathname : POSIX.Pathname;
      PC_Code : int) return Boolean is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      return pathconf (Pathname_With_NUL
       (Pathname_With_NUL'First)'Unchecked_Access, PC_Code) /= 0;
   end Is_Supported;

   function Is_Supported
     (File : POSIX.IO.File_Descriptor;
      PC_Code : int) return Boolean is
   begin return fpathconf (int (File), PC_Code) /= 0;
   end Is_Supported;

   -------------------
   --  Link Limits  --
   -------------------

   function Link_Is_Limited (Pathname : POSIX.Pathname)
      return Boolean is
   begin
      return Is_Limited (Pathname, PC_LINK_MAX);
   end Link_Is_Limited;

   function Link_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean is
   begin
      return Is_Limited (File, PC_LINK_MAX);
   end Link_Is_Limited;

   function Link_Limit (Pathname : POSIX.Pathname)
      return Link_Limit_Maxima is
   begin
      return Link_Limit_Maxima (Limit
       (Pathname, PC_LINK_MAX, long (Link_Limit_Maxima'Last)));
   end Link_Limit;

   function Link_Limit (File : POSIX.IO.File_Descriptor)
      return Link_Limit_Maxima is
   begin
      return Link_Limit_Maxima (Limit
       (File, PC_LINK_MAX, long (Link_Limit_Maxima'Last)));
   end Link_Limit;

   ------------------------
   --  Input Line Limits --
   ------------------------

   function Input_Line_Is_Limited (Pathname : POSIX.Pathname)
      return Boolean is
   begin
      return Is_Limited (Pathname, PC_MAX_CANON);
   end Input_Line_Is_Limited;

   function Input_Line_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean is
   begin
      return Is_Limited (File, PC_MAX_CANON);
   end Input_Line_Is_Limited;

   function Input_Line_Limit (Pathname : POSIX.Pathname)
      return Input_Line_Limit_Maxima is
   begin
      return Input_Line_Limit_Maxima (Limit
       (Pathname, PC_MAX_CANON, long (Input_Line_Limit_Maxima'Last)));
   end Input_Line_Limit;

   function Input_Line_Limit (File : POSIX.IO.File_Descriptor)
      return Input_Line_Limit_Maxima is
   begin
      return Input_Line_Limit_Maxima (Limit
       (File, PC_MAX_CANON, long (Input_Line_Limit_Maxima'Last)));
   end Input_Line_Limit;

   -------------------------
   --  Input Queue Limits --
   -------------------------

   function Input_Queue_Is_Limited (Pathname : POSIX.Pathname)
      return Boolean is
   begin
      return Is_Limited (Pathname, PC_MAX_INPUT);
   end Input_Queue_Is_Limited;

   function Input_Queue_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean is
   begin
      return Is_Limited (File, PC_MAX_INPUT);
   end Input_Queue_Is_Limited;

   function Input_Queue_Limit (Pathname : POSIX.Pathname)
      return Input_Queue_Limit_Maxima is
   begin
      return Input_Queue_Limit_Maxima (Limit
       (Pathname, PC_MAX_INPUT, long (Input_Queue_Limit_Maxima'Last)));
   end Input_Queue_Limit;

   function Input_Queue_Limit (File : POSIX.IO.File_Descriptor)
      return Input_Queue_Limit_Maxima is
   begin
      return Input_Queue_Limit_Maxima (Limit
       (File, PC_MAX_INPUT, long (Input_Queue_Limit_Maxima'Last)));
   end Input_Queue_Limit;

   ------------------------------------
   --  Filename And Pathname Limits  --
   ------------------------------------

   function Filename_Is_Limited (Pathname : POSIX.Pathname)
      return Boolean is
   begin
      return Is_Limited (Pathname, PC_NAME_MAX);
   end Filename_Is_Limited;

   function Filename_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean is
   begin
      return Is_Limited (File, PC_NAME_MAX);
   end Filename_Is_Limited;

   function Filename_Limit (Pathname : POSIX.Pathname)
      return Filename_Limit_Maxima is
   begin
      return Filename_Limit_Maxima (Limit
       (Pathname, PC_NAME_MAX, long (Filename_Limit_Maxima'Last)));
   end Filename_Limit;

   function Filename_Limit (File : POSIX.IO.File_Descriptor)
      return Filename_Limit_Maxima is
   begin
      return Filename_Limit_Maxima (Limit
       (File, PC_NAME_MAX, long (Filename_Limit_Maxima'Last)));
   end Filename_Limit;

   function Pathname_Is_Limited (Pathname : POSIX.Pathname)
      return Boolean is
   begin
      return Is_Limited (Pathname, PC_PATH_MAX);
   end Pathname_Is_Limited;

   function Pathname_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean is
   begin
      return Is_Limited (File, PC_PATH_MAX);
   end Pathname_Is_Limited;

   function Pathname_Limit (Pathname : POSIX.Pathname)
      return Pathname_Limit_Maxima is
   begin
      return Pathname_Limit_Maxima (Limit
       (Pathname, PC_PATH_MAX, long (Pathname_Limit_Maxima'Last)));
   end Pathname_Limit;

   function Pathname_Limit (File : POSIX.IO.File_Descriptor)
      return Pathname_Limit_Maxima is
   begin
      return Pathname_Limit_Maxima (Limit
       (File, PC_PATH_MAX, long (Pathname_Limit_Maxima'Last)));
   end Pathname_Limit;

   --------------------------
   --  Pipe Length Limits  --
   --------------------------

   function Pipe_Length_Is_Limited (Pathname : POSIX.Pathname)
      return Boolean is
   begin
      return Is_Limited (Pathname, PC_PIPE_BUF);
   end Pipe_Length_Is_Limited;

   function Pipe_Length_Is_Limited (File : POSIX.IO.File_Descriptor)
      return Boolean is
   begin
      return Is_Limited (File, PC_PIPE_BUF);
   end Pipe_Length_Is_Limited;

   function Pipe_Length_Limit (Pathname : POSIX.Pathname)
      return Pipe_Limit_Maxima is
   begin
      return Pipe_Limit_Maxima (Limit
       (Pathname, PC_PIPE_BUF, long (Pipe_Limit_Maxima'Last)));
   end Pipe_Length_Limit;

   function Pipe_Length_Limit (File : POSIX.IO.File_Descriptor)
      return Pipe_Limit_Maxima is
   begin
      return Pipe_Limit_Maxima (Limit
       (File, PC_PIPE_BUF, long (Pipe_Limit_Maxima'Last)));
   end Pipe_Length_Limit;

   --------------------------------
   --  Change Owner Restriction  --
   --------------------------------

   function Change_Owner_Is_Restricted (Pathname : POSIX.Pathname)
      return Change_Owner_Restriction is
   begin
      return Is_Supported (Pathname, PC_CHOWN_RESTRICTED);
   end Change_Owner_Is_Restricted;

   function Change_Owner_Is_Restricted (File : POSIX.IO.File_Descriptor)
      return Change_Owner_Restriction is
   begin
      return Is_Supported (File, PC_CHOWN_RESTRICTED);
   end Change_Owner_Is_Restricted;

   ---------------------------
   --  Filename Truncation  --
   ---------------------------

   function Filename_Is_Truncated (Pathname : POSIX.Pathname)
      return Filename_Truncation is
   begin
      return Is_Supported (Pathname, PC_NO_TRUNC);
   end Filename_Is_Truncated;

   function Filename_Is_Truncated (File : POSIX.IO.File_Descriptor)
      return Filename_Truncation is
   begin
      return Is_Supported (File, PC_NO_TRUNC);
   end Filename_Is_Truncated;

   -----------------------
   --  Synchronized IO  --
   -----------------------

   function Synchronized_IO_Is_Supported (Pathname : POSIX.Pathname)
      return Boolean is
   begin
      return Is_Supported (Pathname, PC_SYNC_IO);
   end Synchronized_IO_Is_Supported;

   function Synchronized_IO_Is_Supported (File : POSIX.IO.File_Descriptor)
      return Boolean is
   begin
      return Is_Supported (File, PC_SYNC_IO);
   end Synchronized_IO_Is_Supported;

   -----------------------
   --  Asynchronous IO  --
   -----------------------

   function Asynchronous_IO_Is_Supported (Pathname : POSIX.Pathname)
      return Boolean is
   begin
      return Is_Supported (Pathname, PC_ASYNC_IO);
   end Asynchronous_IO_Is_Supported;

   function Asynchronous_IO_Is_Supported (File : POSIX.IO.File_Descriptor)
      return Boolean is
   begin
      return Is_Supported (File, PC_ASYNC_IO);
   end Asynchronous_IO_Is_Supported;

   ----------------------
   --  Prioritized IO  --
   ----------------------

   function Prioritized_IO_Is_Supported (Pathname : POSIX.Pathname)
     return Boolean is
   begin
      return Is_Supported (Pathname, PC_PRIO_IO);
   end Prioritized_IO_Is_Supported;

   function Prioritized_IO_Is_Supported (File : POSIX.IO.File_Descriptor)
     return Boolean is
   begin
      return Is_Supported (File, PC_PRIO_IO);
   end Prioritized_IO_Is_Supported;

   --  POSIX.5c [D2] additions

   function Socket_Buffer_Is_Limited (File : POSIX.IO.File_Descriptor)
     return Boolean is
   begin
      return Is_Limited (File, PC_SOCK_MAXBUF);
   end Socket_Buffer_Is_Limited;

   function Socket_Buffer_Is_Limited (Pathname : POSIX.Pathname)
     return Boolean is
   begin
      return Is_Limited (Pathname, PC_SOCK_MAXBUF);
   end Socket_Buffer_Is_Limited;

   function Socket_Buffer_Limit (Pathname : POSIX.Pathname)
     return POSIX.Limits.Socket_Buffer_Maxima is
   begin
      return POSIX.Limits.Socket_Buffer_Maxima (Limit
       (Pathname, PC_SOCK_MAXBUF,
        long (POSIX.Limits.Socket_Buffer_Maxima'Last)));
   end Socket_Buffer_Limit;

   function Socket_Buffer_Limit (File : POSIX.IO.File_Descriptor)
     return POSIX.Limits.Socket_Buffer_Maxima is
   begin
      return POSIX.Limits.Socket_Buffer_Maxima (Limit
       (File, PC_SOCK_MAXBUF, long (POSIX.Limits.Socket_Buffer_Maxima'Last)));
   end Socket_Buffer_Limit;

end POSIX.Configurable_File_Limits;
