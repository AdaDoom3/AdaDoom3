--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source                      Luebeck            --
--  Interface                                      Winter, 2004       --
--                                                                    --
--                                Last revision :  15:03 28 Mar 2009  --
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

with Ada.Text_IO;  use Ada.Text_IO;

generic
package Parsers.Generic_Source.Text_IO is
--
-- Put_Line -- Put the current code slice into a text file
--
--  [ File ]      - The text file (the standard output if missing)
--    Code        - The source code
--    Expand_Tabs - Tabulation expansion flag
--
-- This procedure outputs the current source code line following current
-- source cursors. The output might look like:
--
--      123.0 + ( Value - 1)
--                ^^^^^|
--
-- The  parameter  Expand_Tabs when true forms the second output line in
-- accordance with the tabulations expanded in the first line.
--
   procedure Put_Line
             (  File        : File_Type;
                Code        : Source_Type;
                Expand_Tabs : Boolean := False
             );
   procedure Put_Line
             (  Code        : Source_Type;
                Expand_Tabs : Boolean := False
             );
end Parsers.Generic_Source.Text_IO;
