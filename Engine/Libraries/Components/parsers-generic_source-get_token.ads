--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Get_Token            Luebeck            --
--  Interface                                      Summer, 2005       --
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
--  Get_Token -- Get a token in the source
--
--     Code   - The source code
--     Folder - The table of tokens to match the source against
--     Token  - The token matched
--     Got_It - Set to false if no token was matched
--
--  This  generic  procedure  matches  the  tokens from the table Folder
--  against  Code.  When  a  token there is matched the value associated
--  with  it  is  set  into  Token and Got_It is set to True. The source
--  cursor  is  then  advanced  behind  the  token  matched. The longest
--  possible  token  is always matched. When no token matches the source
--  Got_It is set to False. The procedure is generic. The  parameter  is
--  an instance of the package Tables.  Note  that  Folder  can  be  any
--  descendant of the table type defined in Tables.  This  includes  the
--  case-insensitive tables provided by Tables.Names. 
--
with Tables;

generic
   with package Tokens is new Tables (<>);
procedure Parsers.Generic_Source.Get_Token
          (  Code   : in out Source_Type;
             Folder : Tokens.Table'Class;
             Token  : out Tokens.Tag;
             Got_It : out Boolean
          );
