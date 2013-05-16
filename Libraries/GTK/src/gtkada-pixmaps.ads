-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                        Copyright (C) 2000                         --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--
--  This package provides a collection of "standard" pixmaps
--
--  </description>

with Interfaces.C.Strings;
with Gtkada.Types;

package Gtkada.Pixmaps is

   function "+" (Str : in String) return Gtkada.Types.Chars_Ptr
     renames Interfaces.C.Strings.New_String;

   Warning_Xpm : Gtkada.Types.Chars_Ptr_Array :=
   --  Provide a yellow exclamation point
     (+"32 32 7 1",
      --  colors
      +". c #000000",
      +"# c #808000",
      +"a c #000080",
      +"b c none g none m none s Background",
      +"c c #808080",
      +"d c #f8fc00",
      +"e c #f8fcf8",
      --  pixels
      +"bbbbbbbbbbbbb###bbbbbbbbbbbbbbbb",
      +"bbbbbbbbbbbb#ddb.bbbbbbbbbbbbbbb",
      +"bbbbbbbbbbb#ddddb.cbbbbbbbbbbbbb",
      +"bbbbbbbbbbb#ddddd.ccbbbbbbbbbbbb",
      +"bbbbbbbbbb#ddddddb.ccbbbbbbbbbbb",
      +"bbbbbbbbbb#ddddddd.ccbbbbbbbbbbb",
      +"bbbbbbbbb#ddddddddb.ccbbbbbbbbbb",
      +"bbbbbbbbb#ddddddddd.ccbbbbbbbbbb",
      +"bbbbbbbb#ddddddddddb.ccbbbbbbbbb",
      +"bbbbbbbb#dddb...bddd.ccbbbbbbbbb",
      +"bbbbbbb#dddd.....dddb.ccbbbbbbbb",
      +"bbbbbbb#dddd.....dddd.ccbbbbbbbb",
      +"bbbbbb#ddddd.....ddddb.ccbbbbbbb",
      +"bbbbbb#ddddd.....ddddd.ccbbbbbbb",
      +"bbbbb#dddddd.....dddddb.ccbbbbbb",
      +"bbbbb#dddddd#...#dddddd.ccbbbbbb",
      +"bbbb#dddddddb...bddddddb.ccbbbbb",
      +"bbbb#dddddddd...dddddddd.ccbbbbb",
      +"bbb#ddddddddd#.#ddddddddb.ccbbbb",
      +"bbb#dddddddddb.bddddddddd.ccbbbb",
      +"bb#ddddddddddd.ddddddddddb.ccbbb",
      +"bb#ddddddddddddddddddddddd.ccbbb",
      +"b#dddddddddddb..bdddddddddb.ccbb",
      +"b#ddddddddddd....dddddddddd.ccbb",
      +"#dddddddddddd....ddddddddddb.ccb",
      +"#ddddddddddddb..bddddddddddd.ccb",
      +"#ddddddddddddddddddddddddddd.ccc",
      +"#ddddddddddddddddddddddddddb.ccc",
      +"b#ddddddddddddddddddddddddb.cccc",
      +"bb#........................ccccc",
      +"bbbbcccccccccccccccccccccccccccb",
      +"bbbbbcccccccccccccccccccccccccbb");

   Error_Xpm : Gtkada.Types.Chars_Ptr_Array :=
   --  Provide a red stop sign
     (+"32 32 7 1",
      --  colors
      +". c #000000",
      +"# c #800000",
      +"a c #000080",
      +"b c none g none m none s Background",
      +"c c #808080",
      +"d c #f80000",
      +"e c #f8fcf8",
      --  pixels
      +"bbbbbbbbbbb########bbbbbbbbbbbbb",
      +"bbbbbbbb###dddddddd###bbbbbbbbbb",
      +"bbbbbbb#dddddddddddddd#bbbbbbbbb",
      +"bbbbb##dddddddddddddddd##bbbbbbb",
      +"bbbb#dddddddddddddddddddd#bbbbbb",
      +"bbb#dddddddddddddddddddddd#bbbbb",
      +"bbb#dddddddddddddddddddddd#cbbbb",
      +"bb#ddddddeddddddddddedddddd#cbbb",
      +"b#ddddddeeeddddddddeeedddddd#bbb",
      +"b#dddddeeeeeddddddeeeeeddddd#cbb",
      +"b#ddddddeeeeeddddeeeeedddddd#ccb",
      +"#ddddddddeeeeeddeeeeedddddddd#cb",
      +"#dddddddddeeeeeeeeeeddddddddd#cb",
      +"#ddddddddddeeeeeeeedddddddddd#cc",
      +"#dddddddddddeeeeeeddddddddddd#cc",
      +"#dddddddddddeeeeeeddddddddddd#cc",
      +"#ddddddddddeeeeeeeedddddddddd#cc",
      +"#dddddddddeeeeeeeeeeddddddddd#cc",
      +"#ddddddddeeeeeddeeeeedddddddd#cc",
      +"b#ddddddeeeeeddddeeeeedddddd#ccc",
      +"b#dddddeeeeeddddddeeeeeddddd#ccc",
      +"b#ddddddeeeddddddddeeedddddd#ccb",
      +"bb#ddddddeddddddddddedddddd#cccb",
      +"bbb#dddddddddddddddddddddd#ccccb",
      +"bbb#dddddddddddddddddddddd#cccbb",
      +"bbbb#dddddddddddddddddddd#cccbbb",
      +"bbbbb##dddddddddddddddd##ccccbbb",
      +"bbbbbbc#dddddddddddddd#cccccbbbb",
      +"bbbbbbbc###dddddddd###cccccbbbbb",
      +"bbbbbbbbbcc########ccccccbbbbbbb",
      +"bbbbbbbbbbccccccccccccccbbbbbbbb",
      +"bbbbbbbbbbbbbccccccccbbbbbbbbbbb");

   Information_Xpm : Gtkada.Types.Chars_Ptr_Array :=
   --  Provide a blue "i" sign
     (+"32 32 6 1",
      --  colors
      +". c #000000",
      +"# c #000080",
      +"a c none g none m none s Background",
      +"b c #808080",
      +"c c #0000f8",
      +"d c #f8fcf8",
      --  pixels
      +"aaaaaaaaaaabbbbbbbbaaaaaaaaaaaaa",
      +"aaaaaaaabbbaddddddabbbaaaaaaaaaa",
      +"aaaaaabbaddddddddddddabbaaaaaaaa",
      +"aaaaabaddddddddddddddddabaaaaaaa",
      +"aaaabdddddddaccccaddddddd.aaaaaa",
      +"aaabddddddddccccccdddddddd.aaaaa",
      +"aabdddddddddccccccddddddddd.aaaa",
      +"abadddddddddaccccaddddddddda.aaa",
      +"abdddddddddddddddddddddddddd.baa",
      +"badddddddddddddddddddddddddda.ba",
      +"bddddddddddcccccccddddddddddd.ba",
      +"bddddddddddddcccccddddddddddd.bb",
      +"bddddddddddddcccccddddddddddd.bb",
      +"bddddddddddddcccccddddddddddd.bb",
      +"bddddddddddddcccccddddddddddd.bb",
      +"badddddddddddcccccdddddddddda.bb",
      +"abdddddddddddcccccdddddddddd.bbb",
      +"abaddddddddddcccccddddddddda.bbb",
      +"aabddddddddcccccccccddddddd.bbba",
      +"aaa.dddddddddddddddddddddd.bbbba",
      +"aaaa.dddddddddddddddddddd.bbbbaa",
      +"aaaaa.adddddddddddddddda.bbbbaaa",
      +"aaaaaa..adddddddddddda..bbbbaaaa",
      +"aaaaaaab...adddddda...bbbbbaaaaa",
      +"aaaaaaaabbb...addd.bbbbbbbaaaaaa",
      +"aaaaaaaaaabbbb.ddd.bbbbbaaaaaaaa",
      +"aaaaaaaaaaaaab.ddd.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaa.dd.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaa.d.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaa..bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaaabbbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaaaabbaaaaaaaaaaa");

   Confirmation_Xpm : Gtkada.Types.Chars_Ptr_Array :=
   --  Provide a blue question mark
     (+"32 32 6 1",
      --  colors
      +". c #000000",
      +"# c #000080",
      +"a c none g none m none s Background",
      +"b c #808080",
      +"c c #0000f8",
      +"d c #f8fcf8",
      --  pixels
      +"aaaaaaaaaaabbbbbbbbaaaaaaaaaaaaa",
      +"aaaaaaaabbbaddddddabbbaaaaaaaaaa",
      +"aaaaaabbaddddddddddddabbaaaaaaaa",
      +"aaaaabaddddddddddddddddabaaaaaaa",
      +"aaaabdddddddddddddddddddd.aaaaaa",
      +"aaabdddddddaccccccaddddddd.aaaaa",
      +"aabdddddddacaddccccaddddddd.aaaa",
      +"abadddddddccddddccccddddddda.aaa",
      +"abddddddddccccddccccdddddddd.baa",
      +"baddddddddccccdaccccdddddddda.ba",
      +"bdddddddddaccadccccdddddddddd.ba",
      +"bdddddddddddddacccddddddddddd.bb",
      +"bdddddddddddddcccdddddddddddd.bb",
      +"bdddddddddddddccadddddddddddd.bb",
      +"bdddddddddddddccddddddddddddd.bb",
      +"badddddddddddddddddddddddddda.bb",
      +"abdddddddddddaccaddddddddddd.bbb",
      +"abaddddddddddccccdddddddddda.bbb",
      +"aabddddddddddccccdddddddddd.bbba",
      +"aaa.dddddddddaccaddddddddd.bbbba",
      +"aaaa.dddddddddddddddddddd.bbbbaa",
      +"aaaaa.adddddddddddddddda.bbbbaaa",
      +"aaaaaa..adddddddddddda..bbbbaaaa",
      +"aaaaaaab...adddddda...bbbbbaaaaa",
      +"aaaaaaaabbb...addd.bbbbbbbaaaaaa",
      +"aaaaaaaaaabbbb.ddd.bbbbbaaaaaaaa",
      +"aaaaaaaaaaaaab.ddd.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaa.dd.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaa.d.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaa..bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaaabbbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaaaabbaaaaaaaaaaa");

end Gtkada.Pixmaps;
