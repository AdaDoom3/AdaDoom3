-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                 Copyright (C) 2000-2013, AdaCore                  --
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

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Glib is

   ----------------------
   -- To_Boolean_Array --
   ----------------------

   function To_Boolean_Array (A : in Gboolean_Array) return Boolean_Array is
      Result : Boolean_Array (A'Range);
   begin
      for Index in A'Range loop
         Result (Index) := A (Index) /= 0;
      end loop;

      return Result;
   end To_Boolean_Array;

   -------------
   -- To_Gint --
   -------------

   function To_Gint (Bool : in Boolean) return Gint is
   begin
      if Bool then
         return 1;
      else
         return 0;
      end if;
   end To_Gint;

   -----------------------
   -- Quark_From_String --
   -----------------------

   function Quark_From_String (Id : in String) return GQuark is
      function Internal (Id : String) return GQuark;
      pragma Import (C, Internal, "g_quark_from_string");
   begin
      return Internal (Id & ASCII.NUL);
   end Quark_From_String;

   ----------------------
   -- Quark_Try_String --
   ----------------------

   function Quark_Try_String (Id : in String) return GQuark is
      function Internal (Id : String) return GQuark;
      pragma Import (C, Internal, "g_quark_try_string");
   begin
      return Internal (Id & ASCII.NUL);
   end Quark_Try_String;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (Type_Num : in GType) return String is
      function Internal (Type_Num : GType) return chars_ptr;
      pragma Import (C, Internal, "g_type_name");
      Ret : constant chars_ptr := Internal (Type_Num);
   begin
      if Ret = Null_Ptr then
         return "";
      else
         return Value (Ret);
      end if;
   end Type_Name;

   --------------------
   -- Type_From_Name --
   --------------------

   function Type_From_Name (Name : in String) return GType is
      function Internal (Name : String) return GType;
      pragma Import (C, Internal, "g_type_from_name");
   begin
      return Internal (Name & ASCII.NUL);
   end Type_From_Name;

   -----------
   -- Build --
   -----------

   function Build (Name : String) return Property is
   begin
      if Name (Name'Last) /= ASCII.NUL then
         return Property (Name & ASCII.NUL);
      else
         return Property (Name);
      end if;
   end Build;

   -------------------
   -- Property_Name --
   -------------------

   function Property_Name (Prop : Property) return String is
   begin
      return String (Prop);
   end Property_Name;

   --------------------------------
   -- Boxed_Type_Register_Static --
   --------------------------------

   function Boxed_Type_Register_Static
     (Name : String;
      Copy : Boxed_Copy;
      Free : Boxed_Free) return GType
   is
      function Internal
        (N : String; Copy : Boxed_Copy; Free : Boxed_Free) return GType;
      pragma Import (C, Internal, "g_boxed_type_register_static");
   begin
      return Internal (Name & ASCII.NUL, Copy, Free);
   end Boxed_Type_Register_Static;

end Glib;
