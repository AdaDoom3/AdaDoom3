------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                                 P O S I X                                --
--                                                                          --
--                                  B o d y                                 --
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
     Unchecked_Conversion,
     Unchecked_Deallocation;

pragma Elaborate (POSIX.C);
pragma Elaborate (POSIX.Implementation);
package body POSIX is

   use Ada.Streams;
   use POSIX.C;
   use POSIX.Implementation;

   type String_List is new POSIX.Implementation.String_List;

   -----------------------------
   --  Unchecked Conversions  --
   -----------------------------

   type Big_POSIX_String_Ptr is access all POSIX_String (Positive'Range);
   type Big_String_Ptr is access all String (Positive'Range);
   type Big_Stream_Element_Array_Ptr is access all
     Ada.Streams.Stream_Element_Array
     (Ada.Streams.Stream_Element_Offset'Range);

   function From_Address is new Unchecked_Conversion
     (System.Address, Big_String_Ptr);
   function From_Address is new Unchecked_Conversion
     (System.Address, Big_POSIX_String_Ptr);
   function From_Address is new Unchecked_Conversion
     (System.Address, Big_Stream_Element_Array_Ptr);

   -----------------------
   --  To_POSIX_String  --
   -----------------------

   function To_POSIX_String (Str : String)
      return POSIX_String is
   begin
      return From_Address (Str'Address) (1 .. Str'Length);
   end To_POSIX_String;

   -----------------
   --  To_String  --
   -----------------

   function To_String (Str : POSIX_String) return String is
   begin
      return From_Address (Str'Address) (1 .. Str'Length);
   end To_String;

   ----------------------
   --  To_Wide_String  --
   ----------------------

   function To_Wide_String (Str : POSIX_String)
      return Wide_String is
      Result : Wide_String (Str'Range);
   begin
      for I in Str'Range loop
         Result (I) :=
           Wide_Character'Val (POSIX_Character'Pos (Str (I)));
      end loop;
      return Result;
   end To_Wide_String;

   --  We cannot use direct unchecked conversion here,
   --  since the sizes of the characters are different.
   --  However, we rely that the integer codes for the
   --  first 256 wide characters are the same as those
   --  of the ordinary characters. [See ARM A.1 (36)]

   -----------------------
   --  To_POSIX_String  --
   -----------------------

   function To_POSIX_String (Str : Wide_String)
      return POSIX_String is
      Result : POSIX_String (Str'Range);
   begin
      for I in Str'Range loop
         Result (I) := POSIX_Character'Val
           (Wide_Character'Pos (Str (I)) rem 256);
      end loop;
      return Result;
   end To_POSIX_String;

   -------------------------------
   --  To_Stream_Element_Array  --
   -------------------------------

   function To_Stream_Element_Array (Buffer : POSIX_String)
      return Ada.Streams.Stream_Element_Array
   is
      subtype Offset is Stream_Element_Offset;
   begin
      return From_Address (Buffer'Address)
        ((Offset (Buffer'First) + Offset'First - 1) ..
         (Offset (Buffer'Last) + Offset'First - 1));
   end To_Stream_Element_Array;

   -----------------------
   --  To_POSIX_String  --
   -----------------------

   function To_POSIX_String
     (Buffer : Ada.Streams.Stream_Element_Array) return POSIX_String
   is
      subtype Offset is Stream_Element_Offset;
   begin
      return From_Address (Buffer'Address)
        (Positive (Buffer'First - Offset'First + 1) ..
         Positive (Buffer'Last - Offset'First + 1));
   end To_POSIX_String;

   -------------------
   --  Is_Filename  --
   -------------------

   function Is_Filename (Str : POSIX_String) return Boolean is
   begin
      if To_String (Str)'Length = 0 then
         return False;
      end if;
      for I in Str'Range loop
         if Str (I) = '/' or Str (I) = NUL or Str (I) = ' ' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Filename;

   --  These two functions (Is_Pathname and Is_Filename) seem
   --  not to be unimplementable in a portable way, since they are
   --  supposed to "check all constraints set on filename and
   --  pathname by the implementation that can be checked without
   --  accessing the file system directly.

   -------------------
   --  Is_Pathname  --
   -------------------

   function Is_Pathname (Str : POSIX_String) return Boolean is
   begin
      if To_String (Str)'Length = 0 then
         return False;
      end if;
      for I in Str'Range loop
         if Str (I) = NUL or Str (I) = ' ' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Pathname;

   ----------------------------
   --  Is_Portable_filename  --
   ----------------------------

   function Is_Portable_Filename (Str : POSIX_String)
      return Boolean is
   begin
      if To_String (Str)'Length = 0 then
         return False;
      end if;
      for I in Str'Range loop
         case Str (I) is
            when 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' =>
               null;
            when '-' =>
               if I = Str'First then
                  return False;
               end if;
            when others =>
               return False;
         end case;
      end loop;
      return True;
   end Is_Portable_Filename;

   ----------------------------
   --  Is_Portable_Pathname  --
   ----------------------------

   function Is_Portable_Pathname (Str : POSIX_String)
      return Boolean is
      Start : Positive;
      P : Positive;
   begin
      if To_String (Str)'Length = 0 then
         return False;
      end if;
      Start := Str'First;
      P := Str'First;
      loop
         if P > Str'Last or else Str (P) = '/' then
            if Start < P and then not
               Is_Portable_Filename (Str (Start .. P - 1))
            then
               return False;
            end if;
            if P > Str'Last then
               return True;
            end if;
            Start := P + 1;
         end if;
         P := P + 1;
      end loop;
   end Is_Portable_Pathname;

   ------------------
   --  Make_Empty  --
   ------------------

   procedure Free is
     new Unchecked_Deallocation (POSIX_String, POSIX_String_Ptr);

   procedure Free is
     new Unchecked_Deallocation (String_List, POSIX_String_List);

   procedure Make_Empty (List : in out POSIX_String_List) is
   begin
      if List = null then
         return;
      end if;
      for I in 1 .. List.Length loop
         if List.List (I) = null then
            exit;
         end if;
         Free (List.List (I));
      end loop;
      Free (List);
   end Make_Empty;

   --------------
   --  Append  --
   --------------

   procedure Append
     (List : in out POSIX_String_List;
      Str  : POSIX_String) is
      Tmp  : POSIX_String_List;
      Len  : constant Integer := Str'Length;
   begin
      if List = null then
         List := new String_List (Min_String_List_Length);
         --  rely that pointers all initialized to null
      end if;
      for I in 1 .. List.Length loop
         if List.List (I) = null then
            if I = List.Length then
               Tmp := new String_List (2 * List.Length);
               Tmp.List (List.List'Range) := List.List;
               Tmp.Char (List.List'Range) := List.Char;
               Free (List); List := Tmp;
            end if;
            List.List (I) := new POSIX_String (1 .. Len + 1);
            List.List (I)(1 .. Len) := Str;
            List.List (I)(Len + 1) := NUL;
            List.Char (I) := List.List (I)(1)'Unchecked_Access;
            return;
         end if;
      end loop;
   end Append;

   ----------------------
   --  For_Every_Item  --
   ----------------------

   --  generic
   --   with procedure Action
   --     (Item: POSIX_String; Quit: in out Boolean);
   procedure For_Every_Item (List : POSIX_String_List) is
      Quit : Boolean := False;
   begin
      if List = null then
         return;
      end if;
      for I in 1 .. List.Length loop
         exit when List.List (I) = null;
         declare
            L : constant Integer := List.List (I)'Length;
         begin
            Action (List.List (I)(1 .. L - 1), Quit);
         end;
         exit when Quit;
      end loop;
   end For_Every_Item;

   --------------
   --  Length  --
   --------------

   function Length (List : POSIX_String_List)
      return Natural is
   begin
      if List = null then
         return 0;
      end if;
      for I in 1 .. List.Length loop
         if List.List (I) = null then
            return Natural (I - 1);
         end if;
      end loop;
      raise Program_Error;
      return 0;
   end Length;

   -------------
   --  Value  --
   -------------

   function Value
     (List  : POSIX_String_List;
      Index : Positive)
      return POSIX_String is
      I : constant Positive := Index;
   begin
      if List = null
        or else not (I <= List.Length)
        or else List.List (I) = null
      then
         raise Constraint_Error;
      end if;

      declare
         L : constant Integer := List.List (I).all'Length;
      begin
         return List.List (I)(1 .. L - 1);
      end;
   end Value;

   -----------------
   --  Empty_set  --
   -----------------

   function Empty_Set return Option_Set is
   begin
      return (Option => 0);
   end Empty_Set;

   -----------
   --  "+"  --
   -----------

   function "+" (L, R : Option_Set) return Option_Set is
   begin
      return (Option => Bits (unsigned (L.Option) or unsigned (R.Option)));
   end "+";

   -----------
   --  "-"  --
   -----------

   function "-" (L, R : Option_Set) return Option_Set is
   begin
      return (Option =>
        Bits (unsigned (L.Option) and not (unsigned (R.Option))));
   end "-";

   ---------
   --  <  --
   ---------

   function "<"  (Left, Right : Option_Set) return Boolean is
   begin
      return (Left <= Right) and (Left /= Right);
   end "<";

   ---------
   --  <= --
   ---------

   function "<=" (Left, Right : Option_Set) return Boolean is
   begin
      return (((not Bits (unsigned (Right.Option)))) and
        Bits (unsigned (Left.Option))) = 0;
   end "<=";

   ---------
   --  >  --
   ---------

   function ">"  (Left, Right : Option_Set) return Boolean is
   begin
      return Right < Left;
   end ">";

   ----------
   --  >=  --
   ----------

   function ">=" (Left, Right : Option_Set) return Boolean is
   begin
      return Right <= Left;
   end ">=";

   ----------------------
   --  Get_Error_Code  --
   ----------------------

   function Get_Error_Code return Error_Code is
   begin
      return POSIX.Implementation.Get_Ada_Error_Code;
   end Get_Error_Code;

   ----------------------
   --  Set_Error_Code  --
   ----------------------

   procedure Set_Error_Code (Error : Error_Code) is
   begin
      POSIX.Implementation.Set_Ada_Error_Code (Error);
   end Set_Error_Code;

   ----------------------
   --  Is_POSIX_Error  --
   ----------------------

   function Is_POSIX_Error (Error : Error_Code) return Boolean is
      use Bogus_Error_Codes;
   begin
      for I in Error_Array'Range loop
         if Error = Error_Array (I) then
            return True;
         end if;
      end loop;
      return False;
   end Is_POSIX_Error;

   -------------
   --  Image  --
   -------------

   function Image (Error : Error_Code) return String is
      use Bogus_Error_Codes;
   begin
      for I in Error_Array'Range loop
         if Error = Error_Array (I) then
            return Error_Name_Enum'Image (I);
         end if;
      end loop;
      declare
         Tmp : constant String := Error_Code'Image (Error);
      begin
         if Tmp (Tmp'First) /= ' ' then
            return Tmp;
         end if;
         return Tmp (Tmp'First + 1 .. Tmp'Last);
      end;
   end Image;

   function uname (name : access struct_utsname)
     return int;
   pragma Import (C, uname, uname_LINKNAME);

   -------------------
   --  System_Name  --
   -------------------

   function System_Name return POSIX_String is
      Name : aliased struct_utsname;
   begin
      Check (uname (Name'Unchecked_Access));
      return Form_POSIX_String (Name.sysname (1)'Unchecked_Access);
   end System_Name;

   -----------------
   --  Node_Name  --
   -----------------

   function Node_Name return POSIX_String is
      Name : aliased struct_utsname;
   begin
      Check (uname (Name'Unchecked_Access));
      return Form_POSIX_String (Name.nodename (1)'Unchecked_Access);
   end Node_Name;

   ---------------
   --  Release  --
   ---------------

   function Release return POSIX_String is
      Name : aliased struct_utsname;
   begin
      Check (uname (Name'Unchecked_Access));
      return Form_POSIX_String (Name.release (1)'Unchecked_Access);
   end Release;

   ---------------
   --  Version  --
   ---------------

   function Version return POSIX_String is
      Name : aliased struct_utsname;
   begin
      Check (uname (Name'Unchecked_Access));
      return Form_POSIX_String (Name.version (1)'Unchecked_Access);
   end Version;

   ---------------
   --  Machine  --
   ---------------

   function Machine return POSIX_String is
      Name : aliased struct_utsname;
   begin
      Check (uname (Name'Unchecked_Access));
      return Form_POSIX_String (Name.machine (1)'Unchecked_Access);
   end Machine;

   -----------------------------------------
   --  Timespec Composition/Decomposition --
   -----------------------------------------

   procedure Split
     (D  : Duration;
      S  : out Duration;
      NS : out Duration);
   pragma Inline (Split);
   --  Decompose D into seconds (S) and nanoseconds (NS) parts,
   --  with the nanosecond part in the range 0.0 .. 0.999999999.

   procedure Split
     (D  : Duration;
      S  : out Duration;
      NS : out Duration) is
   begin
      S := POSIX.Implementation.To_Duration
        (To_D_Int (D / NS_per_S) * NS_per_S);
      NS := D - S;
      if NS < 0.0 then
         S := S - 1.0;
         NS := NS + 1.0;
      end if;
   end Split;

   -------------
   --  Split  --
   -------------

   procedure Split
      (Time : Timespec;
       S    : out Seconds;
       NS   : out Nanoseconds) is
      SD, NSD : Duration;
   begin
      Split (Time.Val, S => SD, NS => NSD);
      S := Seconds (SD); NS := Nanoseconds (NSD * NS_per_S);
   end Split;

   -------------------
   --  To_Timespec  --
   -------------------

   function To_Timespec
     (S  : Seconds;
      NS : Nanoseconds) return Timespec is
   begin
      return Timespec'
        (Val => Duration (S) + Duration (NS) / NS_per_S);
   end To_Timespec;

   -------------------
   --  Get_Seconds  --
   -------------------

   function Get_Seconds (Time : Timespec) return Seconds is
      SD, NSD : Duration;
   begin
      Split (Time.Val, S => SD, NS => NSD);
      return Seconds (SD);
   end Get_Seconds;

   -----------------------
   --  Get_Nanoseconds  --
   -----------------------

   function Get_Nanoseconds (Time : Timespec) return Nanoseconds is
      SD, NSD : Duration;
   begin
      Split (Time.Val, S => SD, NS => NSD);
      return Nanoseconds (NSD * NS_per_S);
   end Get_Nanoseconds;

   -----------------------
   --  Set_Nanoseconds  --
   -----------------------

   procedure Set_Nanoseconds
     (Time : in out Timespec;
      NS   : Nanoseconds)
   is
      SD, NSD : Duration;
   begin
      Split (Time.Val, S => SD, NS => NSD);
      Time.Val := SD + Duration (NS) / NS_per_S;
   end Set_Nanoseconds;

   -------------------
   --  Set_Seconds  --
   -------------------

   procedure Set_Seconds
     (Time : in out Timespec;
      S    : Seconds)
   is
      SD, NSD : Duration;
   begin
      Split (Time.Val, S => SD, NS => NSD);
      Time.Val :=  Duration (S) + NSD;
   end Set_Seconds;

   -----------
   --  "+"  --
   -----------

   function "+" (Left, Right : Timespec) return Timespec is
   begin
      return Timespec'(Val => Left.Val + Right.Val);
   end "+";

   -----------
   --  "+"  --
   -----------

   function "+" (Left : Timespec; Right : Nanoseconds)
     return Timespec is
   begin
      return Timespec'
        (Val => Left.Val + Duration (Right) / NS_per_S);
   end "+";

   -----------
   --  "-"  --
   -----------

   function "-" (Right : Timespec) return Timespec is
   begin
      return Timespec'(Val => -Right.Val);
   end "-";

   -----------
   --  "-"  --
   -----------

   function "-" (Left, Right : Timespec) return Timespec is
   begin
      return Timespec'(Val => Left.Val - Right.Val);
   end "-";

   -----------
   --  "-"  --
   -----------

   function "-" (Left : Timespec; Right : Nanoseconds)
     return Timespec is
   begin
      return Timespec'(Val => Left.Val - Duration (Right) / NS_per_S);
   end "-";

   -----------
   --  "*"  --
   -----------

   function "*" (Left : Timespec; Right : Integer)
     return Timespec is
   begin
      return Timespec'(Val => Left.Val * Duration (Right));
   end "*";

   -----------
   --  "*"  --
   -----------

   function "*" (Left : Integer; Right : Timespec)
     return Timespec is
   begin
      return Timespec'(Val => Left * Right.Val);
   end "*";

   -----------
   --  "/"  --
   -----------

   function "/" (Left : Timespec; Right : Integer)
     return Timespec is
   begin
      return Timespec'(Val => Left.Val / Right);
   end "/";

   -----------
   --  "/"  --
   -----------

   function "/" (Left, Right : Timespec) return Integer is
   begin
      return Integer (Left.Val / Right.Val);
   end "/";

   -----------
   --  "<"  --
   -----------

   function "<" (Left, Right : Timespec) return Boolean is
   begin
      return Left.Val < Right.Val;
   end "<";

   -----------
   --  "<="  --
   -----------

   function "<=" (Left, Right : Timespec) return Boolean is
   begin
      return Left.Val <= Right.Val;
   end "<=";

   -----------
   --  ">"  --
   -----------

   function ">" (Left, Right : Timespec) return Boolean is
   begin
      return Right <= Left;
   end ">";

   ------------
   --  ">="  --
   ------------

   function ">=" (Left, Right : Timespec) return Boolean is
   begin
      return Right < Left;
   end ">=";

   -------------------
   --  To_Timespec  --
   -------------------

   function To_Timespec (D : Duration)  return Timespec is
   begin
      return Timespec'(Val => D);
   end To_Timespec;

   -------------------
   --  To_Duration  --
   -------------------

   function To_Duration (Time : Timespec)  return Duration is
   begin
      return Time.Val;
   end To_Duration;

   --------------------------------
   -- Host_To_Network_Byte_Order --
   --------------------------------

   function Host_To_Network_Byte_Order
     (Host_32 : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
   is
      function c_htonl
        (host_32 : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
      pragma Import (C, c_htonl, "c_htonl");

   begin
      return c_htonl (Host_32);
   end Host_To_Network_Byte_Order;

   function Host_To_Network_Byte_Order
     (Host_16 : Interfaces.Unsigned_16) return Interfaces.Unsigned_16
   is
      function c_htons
        (host_16 : Interfaces.Unsigned_16) return Interfaces.Unsigned_16;
      pragma Import (C, c_htons, "c_htons");

   begin
      return c_htons (Host_16);
   end Host_To_Network_Byte_Order;

   --------------------------------
   -- Host_To_Network_Byte_Order --
   --------------------------------

   function Network_To_Host_Byte_Order
     (Host_32 : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
   is
      function c_ntohl
        (host_32 : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
      pragma Import (C, c_ntohl, "c_ntohl");

   begin
      return c_ntohl (Host_32);
   end Network_To_Host_Byte_Order;

   function Network_To_Host_Byte_Order
     (Host_16 : Interfaces.Unsigned_16) return Interfaces.Unsigned_16
   is
      function c_ntohs
        (host_16 : Interfaces.Unsigned_16) return Interfaces.Unsigned_16;
      pragma Import (C, c_ntohs, "c_ntohs");

   begin
      return c_ntohs (Host_16);
   end Network_To_Host_Byte_Order;

begin
   --  We rely on Duration being an exact count of nanoseconds

   pragma Assert (Duration'Small = 1.0 / NS_per_S);
   null;
end POSIX;
