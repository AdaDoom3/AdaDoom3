with
  Ada.IO_Exceptions,
  Ada.Strings.Fixed;
package body CSV is
   use Ada.Strings.Fixed;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds (Item : String; Separator : Character := ',') return Fields_Bounds is
      In_Quotes : Boolean := False;
   begin
      for I in Item'Range loop
         if Item(I) = '"' then
            In_Quotes := not In_Quotes;
         elsif not In_Quotes and Item (I) = Separator then
            return Bounds'(Item'First, I-1) & Get_Bounds (Item (I+1 .. Item'Last), Separator);
         end if;
      end loop;

      return (1 => (Item'First, Item'Last));
   end Get_Bounds;

   -------------
   -- Extract --
   -------------

   function Extract (
      Item   : String;
      Fields : Fields_Bounds;
      Column : Positive;
      Unquote: Boolean:= True
   )
   return String
   is
      Extracted: constant String:=
         Item( Fields(Column).Start .. Fields(Column).Stop );
   begin
      if Unquote then
        return CSV.Unquote(Extracted);
      else
        return Extracted;
      end if;
   end Extract;

   -----------
   -- Quote --
   -----------

   function Quote (Item : String) return String is
      Result : String (Item'First .. Item'Last + Count (Item, """") + 2);
      Index  : Positive;
   begin
      Index := Result'First;
      Result (Index) := '"';

      for I in Item'Range loop
         if Item (I) = '"' then
            Index := Index + 1;
            Result (Index) := '"';
         end if;
         Index := Index + 1;
         Result (Index) := Item (I);
      end loop;
      Result (Result'Last) := '"';

      return Result;
   end Quote;

   -------------
   -- Unquote --
   -------------

   function Unquote (Item : String) return String is
      use Ada.IO_Exceptions;

      Result    : String(Item'Range);
      Index_In  : Positive;
      Index_Out : Natural;
   begin
      if Item = "" or else Item (Item'First) /= '"' then
         return Item;
      end if;

      Index_In  := Item'First+1;
      Index_Out := Result'First-1;
      while Index_In <= Item'Last-1 loop
         if Item (Index_In) = '"' then
            Index_Out := Index_Out + 1;
            Result (Index_Out) := '"';
            if Item (Index_In+1) ='"' then
               Index_In := Index_In + 1;
            end if;
         else
            Index_Out := Index_Out + 1;
            Result (Index_Out) := Item (Index_In);
         end if;
         Index_In := Index_In + 1;
      end loop;

      if Item (Item'Last) /= '"' then
         raise End_Error;
      end if;

      return Result (Result'First .. Index_Out);
   end Unquote;

   -------------
   -- Unquote --
   -------------

   function Unquote (Item : String; Slice : Bounds; Size : Natural := 0) return String is
      use Ada.Strings;
      Raw_Line : constant String := Unquote (Item (Slice.Start .. Slice.Stop));
   begin
      if Size = 0 then
         return Trim (Raw_line, Both);
      elsif Raw_Line'Length < Size then
         return Raw_Line & (Size - Raw_Line'Length) * ' ';
      else
         return Raw_Line (Raw_Line'First .. Raw_Line'First + Size - 1);
      end if;
   end Unquote;

end CSV;
