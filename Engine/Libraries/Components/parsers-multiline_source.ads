--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Multiline_Source                    Luebeck            --
--  Interface                                      Winter, 2004       --
--                                                                    --
--                                Last revision :  12:54 06 Jun 2009  --
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
--  This  package  provides an implementation of code sources consisting
--  of several lines. The package defines an abstract base type Source.
--
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Parsers.Generic_Source;

package Parsers.Multiline_Source is
   type Line_Number is new Natural;
--
-- Position -- File postion
--
   type Position is record
      Line   : Line_Number;
      Column : Integer;
   end record;
   function "<" (Left, Right : Position) return Boolean;
--
-- Location -- File slice from First to previous to Next
--
   type Location is record
      First : Position;
      Next  : Position;
   end record;

   type String_Ptr is access all String;
--
-- Source -- The source containing a file
--
--    Buffer  - The source line buffer
--    Line    - The current source line number
--    Length  - The current line length
--    Pointer - The second cursor
--    Last    - The first (backup) cursor
--
-- The field Buffer points to a  string,  which  is  used  to  keep  the
-- current source line. The constructor allocates  the  buffer  of  some
-- reasonable size. When a new line  is  requested  the  buffer  can  be
-- replaced  by  a  greater one if necessary. The destructor deallocates
-- the buffer.
--
   type Source is
      abstract new Ada.Finalization.Limited_Controlled with
   record
      Buffer  : String_Ptr;
      Line    : Line_Number := 0;
      Length  : Natural;
      Pointer : Integer;
      Last    : Integer;
   end record;
   type Line_Ptr is access constant String;
--
-- Finalize -- Destruction
--
--    Code - The source code
--
   procedure Finalize (Code : in out Source);
--
-- Initialize -- Construction
--
--    Code - The source code
--
   procedure Initialize (Code : in out Source);
--
-- Get_Line -- Read the next line into the buffer
--
--    Code - The source code
--
-- An   implementation   should   read   a   complete   next  line  into
-- Code.Buffer.all.  It  may  reallocate  the buffer if necessary. After
-- successful completion Code.Buffer should point to a buffer containing
-- the  line  and Code.Length should be the line length. The rest of the
-- buffer is ignored.
--
-- Exceptions :
--
--    End_Error - No more lines available
--    Any other - I/O error etc
--
   procedure Get_Line (Code : in out Source) is abstract;
--
-- Get_Location -- Of an error from an error message string
--
--    Message - An error message
--    Prefix  - Introducing error location image
--
-- This  function  searches  for  a  location  image in an error message
-- string. The image is searched backwards for an appearance of  Prefix.
-- If  an  image  does not follow Prefix search continues. The result is
-- the location decoded according to the format used  by  Image.  If  no
-- image found the result is ((0,0), (0,0).
--
-- Returns :
--
--    The rightmost location
--
   function Get_Location
            (  Message : String;
               Prefix  : String := "at "
            )  return Location;
--
-- Skip -- Advance source to the specified location
--
--    Code - The source code
--    Link - Location
--
-- This procedure advances the source Code to the location Link, so that
-- the  result  of Link (Code) would equal to the value of the parameter
-- Link. Layout_Error is propagated when the source is beyond the  first
-- position of Link. It is also propagated when some parts  of  Link  do
-- not belong to the source Code.
--
-- Exceptions :
--
--    Layout_Error - Link is beyond the actual position
--    Any other    - I/O error etc
--
   procedure Skip (Code : in out Source'Class; Link : Location);
--
-- Implementations of the Parsers.Generic_Source interface
--
   function "&" (Left, Right : Location) return Location;
   function End_Of (Code : Source'Class) return Boolean;
   function Get_Backup_Pointer (Code : Source'Class) return Integer;
   function Get_Line (Code : Source'Class) return String;
   procedure Get_Line
             (  Code    : Source'Class;
                Line    : out Line_Ptr;
                Pointer : out Integer;
                Last    : out Integer
             );
   function Get_Pointer (Code : Source'Class) return Integer;
   function Image (Link : Location) return String;
   function Link (Code : Source'Class) return Location;
   procedure Next_Line (Code : in out Source'Class);
   procedure Reset_Pointer (Code : in out Source'Class);
   procedure Set_Pointer
             (  Code    : in out Source'Class;
                Pointer : Integer
             );
--
-- Code -- The implementation
--
   package Code is new
      Parsers.Generic_Source
      (  Location_Type => Location,
         Source_Type   => Source'Class,
         Line_Ptr_Type => Line_Ptr
      );
private
   pragma Inline ("<");
   pragma Inline (End_Of);
   pragma Inline (Get_Backup_Pointer);
   pragma Inline (Get_Line);
   pragma Inline (Get_Pointer);
   pragma Inline (Link);
   pragma Inline (Reset_Pointer);

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

end Parsers.Multiline_Source;
