--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source                      Luebeck            --
--  Interface                                      Winter, 2004       --
--                                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--
--
--  This generic package provides abstract interface for the source code
--  readers. The package is abstract and provides no  functionality.  An
--  instance should implement its  interface  by  defining  the  generic
--  parameters:   
--
--  (o)  Source_Type  is  the  type  of  code  source. An implementation
--       should maintain two source code cursors (pointers); 
--  (o)  Location_Type is the type used for  source  code  locations.  A
--       source code location refers a code slice. The slice may  occupy
--       several line if the source is multiline;
--  (o)  Line_Ptr_Type is the pointer type to refer source lines.
--
--  The following generic formal  subroutines  are  used  to  manipulate
--  source code: 
--
--  (o)  End_Of returns True at the source end;
--  (o)  Get_Backup_Pointer  is  used to get the saved cursor. It is one
--       to which Restore_Pointer would return, see below. At the source
--       end 1 is the result;
--  (o)  Get_Line  is a function to get the current source code line. It
--       remains valid until the first call to Next_Line.  End_Error  is
--       propagated when the source end was reached either  because  the
--       source is empty or because of a call to Next_Line before; 
--  (o)  Get_Line  is  another  variant  in the form of procedure. It is
--       similar to  Get_Line  but  returns  a  pointer  to  the  buffer
--       containing  the  current  source  code line, the current cursor
--       position in that buffer and the position of the last  character
--       in  the buffer. It might be more efficient than Get_Line if the
--       compiler optimization is not great. The  pointer  returned  may
--       refer  to  a  string  longer  that  the   current   line.   The
--       implementation shall ensure equivalence of the  value  returned
--       in the Pointer parameter to one  returned  by  Get_Pointer  and
--       accepted by  Set_Pointer.  Usually  it  is  achieved  when  the
--       function Get_Line returns a slice of the buffer returned by the
--       procedure  Get_Line.  Note  that in Ada string slicing does not
--       shift  the  lower  bound of the result to 1. Thus it is safe to
--       use  plain  slicing  there.  Like  the  function, the procedure
--       Get_Line raises End_Error at the source end or  else  when  the
--       source is empty; 
--  (o)  Get_Pointer is used to get the current cursor. The result is an
--       index in the current line which would be returned by  Get_Line.
--       It  is  in the range Line'First..Line'Last+1 provided that Line
--       is  the value of Get_Line. The character pointed by Get_Pointer
--       is the first one  to  parse.  The  characters  before  are  the
--       recognized  ones. At the source end 1 is the result;
--  (o)  Link  gets  the  source  code  location. A location specifies a
--       source code slice between two cursors. The second cursor is one
--       returned by Get_Pointer. The first cursor is the previous value
--       of the second one. The slice in between  is  usually  the  last
--       recognized lexical token. It includes the character pointed  by
--       the  first cursor,  and does not one pointed by the second one.
--       Empty slices are allowed, so Link should never fail even at the
--       end of a source; 
--  (o)  Next_Line  is  used  to  advance to the next source line. After
--       successful  completion Get_Line can be used to access the newly
--       read  source  line.  Both cursors are set to Get_Line'First. So
--       when the line is not empty Get_Pointer will return the index of
--       the first character in  the  new  source  line.  Data_Error  is
--       propagated on I/O errors.  End_Error  is  propagated  when  the
--       source end is reached;
--  (o)  Reset_Pointer  is  used  to  move the second cursor back to the
--       first  cursor.  The  depth of the "unget" need not to be deeper
--       than 1. Consequent calls to Reset_Pointer may have  no  effect.
--       It  is  also  not  required to implement return to the previous
--       line;   
--  (o)  Set_Pointer move  the  second  cursor forward. The new position
--       should be  in  the  range  between  the  position  returned  by
--       Get_Pointer and the position following the  last  character  of
--       the current line, i.e.  Get_Line  (Code)'Last  +  1.  Otherwise
--       Layout_Error is propagated; 
--
--  The following generic formal  subroutines  are  used  to  manipulate
--  source code locations:
--
--  (o)  Image returns a text description of a location. The result is
--       a string;
--  (o)  "&" is used to combine two,  usually  adjacent  a  source  code
--       locations. The result is a consecutive code fragment containing
--       positions  from  both  Left and Right locations. For example if
--       Left and Right are locations of "(" and ")" then the result  is
--       everything in the brackets including the brackets. 
--
--  The  following  small  example  illustrates  an  implementation of a
--  routine to skip spaces in the source line:  
--
--     procedure Skip (Code : in out Source_Type) is
--        Line    : String renames Get_Line (Code);
--        Pointer : Integer := Get_Pointer (Code);
--     begin
--        while Pointer <= Line'Last and then Line (Pointer) = ' ' loop
--           Pointer := Pointer + 1;
--        end loop;
--        Set_Pointer (Code, Pointer);
--     end Skip;
--
--  Should  Link  (Code) be called immediately after this implementation
--  of  Skip  it  would  return  a  location identifying the blank slice
--  matched  by  Skip  in  the  source code line. The same example using
--  Get_Line_Ptr instead of Get_Line: 
--
--     procedure Skip (Code : in out Source_Type) is
--        Line    : Line_Ptr_Type;
--        Pointer : Integer;
--        Last    : Integer;
--     begin
--        Get_Line (Line, Pointer, Last);
--        while Pointer <= Last and then Line (Pointer) = ' ' loop
--           Pointer := Pointer + 1;
--        end loop;
--        Set_Pointer (Code, Pointer);
--     end Skip;
--
generic
   type Source_Type (<>) is limited private;
   type Line_Ptr_Type is access constant String;
   type Location_Type is private;
   with function End_Of (Link : Source_Type) return Boolean is <>;
   with function Get_Line (Code : Source_Type) return String is <>;
   with procedure Get_Line
                  (  Code    : Source_Type;
                     Line    : out Line_Ptr_Type;
                     Pointer : out Integer;
                     Last    : out Integer
                  )  is <>;
   with function Get_Pointer (Code : Source_Type) return Integer is <>;
   with function Get_Backup_Pointer (Code : Source_Type)
      return Integer is <>;
   with function Image (Link : Location_Type) return String is <>;
   with function Link (Code : Source_Type) return Location_Type is <>;
   with procedure Next_Line (Code : in out Source_Type) is <>;
   with procedure Reset_Pointer (Code : in out Source_Type) is <>;
   with procedure Set_Pointer
                  (  Code    : in out Source_Type;
                     Pointer : Integer
                  )  is <>;
   with function "&" (Left, Right : Location_Type)
      return Location_Type is <>;
package Parsers.Generic_Source is

end Parsers.Generic_Source;
