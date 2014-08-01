--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.                     Luebeck            --
--        Get_Cpp_Blank                            Winter, 2004       --
--  Interface                                                         --
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
--  Get_Cpp_Blank -- Skip blanks in the source
--
--     Code     - The source code
--     Got_It   - Set to false if the source end was reached
--     Error    - Set to true if an unclosed /*..*/ detected
--     Error_At - The location of erroneous comment
--
--  This procedure skips blanks by consuming C++ comments and characters
--  CR,  HT,  VT,  LF,  FF,  SP (carriage return, horizontal tabulation,
--  vertical tabulation, line feed, form feed, space). It also skips  to
--  the  next  source  line  when necessary. A C++ comment either starts
--  with // (double forward slash) and  continues  to  the  end  of  the
--  current  line  or with /* (forward slash, asterisk) and continues to
--  the first appearance of closing */. This may include several  source
--  code  lines. Upon an unclosed /*..*/ comment the source is read till
--  the end. In this case Got_It is false, Error is true and Error_At is
--  the  location of /* in the source. In all other cases Error is false
--  and Error_At should be ignored.    
--
generic
procedure Parsers.Generic_Source.Get_Cpp_Blank
          (  Code     : in out Source_Type;
             Got_It   : out Boolean;
             Error    : out Boolean;
             Error_At : out Location_Type
          );
