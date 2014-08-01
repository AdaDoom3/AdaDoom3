--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.                     Luebeck            --
--        Get_Ada_Blank                            Winter, 2004       --
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
--  Get_Ada_Blank -- Skip blanks in the source
--
--     Code   - The source code
--     Got_It - Set to false if the source end was reached
--
--  This procedure skips Ada comments, characters CR, HT, VT, LF, FF, SP
--  (carriage  return,  horizontal tabulation, vertical tabulation, line
--  feed, form feed, space). A comment ends either at the line end or in
--  the one of the format effectors as ARM 2.2(2) requires. It skips  to
--  the next source line when necessary.
--
generic
procedure Parsers.Generic_Source.Get_Ada_Blank
          (  Code   : in out Source_Type;
             Got_It : out Boolean
          );
