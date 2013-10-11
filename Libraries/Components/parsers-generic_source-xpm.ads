--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.XPM                  Luebeck            --
--  Implementation                                 Summer, 2006       --
--                                                                    --
--                                Last revision :  19:53 12 Jan 2008  --
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
--  This  generic  package  is an XPM parser. It can be instantiated for
--  any  type  of  sources,  but  usually  it makes sense for multi-line
--  sources only. The package provides three subprograms for parsing XPM
--  files.  An XPM file is a C program containing an XPM image. A source
--  containing such image is usually parsed this way:
--
--     Header : Descriptor         := Get (Source);
--     Map    : Color_Tables.Table := Get (Source, Header);
--     Image  : Pixel_Buffer       := Get (Source, Header, Map);
--
with Tables;
with Parsers.Generic_Source.Get_Cpp_Blank;
with Parsers.Generic_Source.Get_Text;
with Parsers.Generic_Source.Keywords;

generic
package Parsers.Generic_Source.XPM is
--
-- Descriptor -- Of an XPM file
--
-- The descriptor contains the information about an XPM image.
--
--    Name       - The name of the image, as found in the file
--    Width      - In pixels
--    Height     - In pixels
--    Pixel_Size - Number of characters per pixel used in the file
--    Map_Size   - Number of colors in the colormap
--    Extended   - Has XPMEXT part
--    X_Hotspot  - Hostspot co-ordinate 0..
--    Y_Hotspot  - Hostspot co-ordinate 0..
--
   type Descriptor
        (  Has_Hotspot : Boolean;
           Length      : Positive
        )  is record
      Name       : String (1..Length);
      Width      : Positive;
      Height     : Positive;
      Pixel_Size : Positive;
      Map_Size   : Positive;
      Extended   : Boolean;
      case Has_Hotspot is
         when True =>
            X_Hotspot : Natural;
            Y_Hotspot : Natural;
         when False =>
            null;
      end case;
   end record;
--
-- RGB_Color -- The type of a pixel
--
-- The  values  are  encoded  as  RGB,  big  endian. For example, Red is
-- 16#FF0000#. The value 2**24 is used for the transparent color.
--
   type RGB_Color is range 0..2**24;
   Transparent : constant RGB_Color := RGB_Color'Last;
--
-- Color_Tables -- Colormaps
--
-- The type Color_Table.Table is a mapping String->RGB_Color.
--
   package Color_Tables is new Tables (RGB_Color);
--
-- Pixel_Buffer -- Image in Row x Column format
--
   type Pixel_Buffer is array (Positive range <>, Positive range <>)
      of RGB_Color;
--
-- Get -- Descriptor from source
--
--    Code - The source to parse
--
-- Returns :
--
--    The image descriptor
--
-- Exceptions :
--
--    Syntax_Error - Syntax error during parsing
--    othes        - Source related exception
--
   function Get (Code : access Source_Type) return Descriptor;
--
-- Get -- Colormap from source
--
--    Code   - The source to parse
--    Header - Parsed before
--
-- Returns :
--
--    The image colormap
--
-- Exceptions :
--
--    Syntax_Error - Syntax error during parsing
--    othes        - Source related exception
--
   function Get
            (  Code   : access Source_Type;
               Header : Descriptor
            )  return Color_Tables.Table;
--
-- Get -- Image from source
--
--    Code   - The source to parse
--    Header - The image descriptor parsed before
--    Map    - The image colormap parsed before
--
-- Returns :
--
--    The image
--
-- Exceptions :
--
--    Syntax_Error - Syntax error during parsing
--    othes        - Source related exception
--
   function Get
            (  Code   : access Source_Type;
               Header : Descriptor;
               Map    : Color_Tables.Table
            )  return Pixel_Buffer;

private
   type Color_Type is (m, s, g4, g, c);
   package Color_Types is new Keywords (Color_Type);

   type Color_Name is (none, white, black, red, green, blue);
   package Color_Names is new Keywords (Color_Name);

   procedure Skip is new Parsers.Generic_Source.Get_Cpp_Blank;
   procedure Get  is new Parsers.Generic_Source.Get_Text;

end Parsers.Generic_Source.XPM;
