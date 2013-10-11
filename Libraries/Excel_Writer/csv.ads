-- Freeware, author: J-P. Rosen, http://www.adalog.fr/

package CSV is

   type Bounds is
      record
         Start : Positive;
         Stop  : Natural;
      end record;
   type Fields_Bounds is array (Positive range <>) of Bounds;
   function Get_Bounds (Item : String; Separator : Character := ',') return Fields_Bounds;

   function Extract (
      Item   : String;
      Fields : Fields_Bounds;
      Column : Positive;
      Unquote: Boolean:= True
   )
   return String;

   function Quote   (Item : String) return String;
   function Unquote (Item : String) return String;
   function Unquote (Item : String; Slice : Bounds; Size : Natural := 0) return String;

end CSV;
