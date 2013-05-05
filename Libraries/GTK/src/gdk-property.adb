-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

with Interfaces.C.Pointers;
with Interfaces.C.Strings;

package body Gdk.Property is

   ------------------------
   -- Local declarations --
   ------------------------

   type Local_Guchar_Array is array (Natural range <>) of aliased Guchar;

   package Guchars_Ptr is new Interfaces.C.Pointers
     (Index => Natural,
      Element => Guchar,
      Element_Array => Local_Guchar_Array,
      Default_Terminator => 0);

   procedure C_Free (P : Guchars_Ptr.Pointer);
   pragma Import (C, C_Free, "free");

   function To_Guchar_Array (Value : Local_Guchar_Array) return Guchar_Array;
   pragma Inline (To_Guchar_Array);

   -----------------
   -- Atom_Intern --
   -----------------

   function Atom_Intern
     (Atom_Name      : String;
      Only_If_Exists : Boolean := True) return Gdk.Types.Gdk_Atom
   is
      function Internal
        (Atom_Name      : String;
         Only_If_Exists : Gint) return Gdk.Types.Gdk_Atom;
      pragma Import (C, Internal, "gdk_atom_intern");

   begin
      return Internal (Atom_Name & ASCII.NUL, To_Gint (Only_If_Exists));
   end Atom_Intern;

   ---------------
   -- Atom_Name --
   ---------------

   function Atom_Name (Atom : Gdk.Types.Gdk_Atom) return String is
      function Internal
        (Atom : Gdk.Types.Gdk_Atom) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gdk_atom_name");

   begin
      return Interfaces.C.Strings.Value (Internal (Atom));
   end Atom_Name;

   ------------
   -- Change --
   ------------

   procedure Change
     (Window    : Gdk.Window.Gdk_Window;
      Property  : Gdk.Types.Gdk_Atom;
      The_Type  : Gdk.Types.Gdk_Atom;
      Format    : Gint;
      Mode      : Gdk_Prop_Mode;
      Data      : Guchar_Array)
   is
      procedure Internal
        (Window    : Gdk.Window.Gdk_Window;
         Property  : Gdk.Types.Gdk_Atom;
         The_Type  : Gdk.Types.Gdk_Atom;
         Format    : Gint;
         Mode      : Gdk_Prop_Mode;
         Data      : Guchar_Array;
         Nelements : Gint);
      pragma Import (C, Internal, "gdk_property_change");

   begin
      Internal (Window, Property, The_Type, Format, Mode, Data, Data'Length);
   end Change;

   ---------
   -- Get --
   ---------

   procedure Get
     (Window               : Gdk.Window.Gdk_Window;
      Property             : Gdk.Types.Gdk_Atom;
      The_Type             : Gdk.Types.Gdk_Atom;
      Offset               : Gulong;
      Length               : Gulong;
      Pdelete              : Boolean;
      Actual_Property_Type : out Gdk.Types.Gdk_Atom;
      Actual_Format        : out Gint;
      Data                 : out Guchar_Array_Access;
      Success              : out Boolean)
   is
      procedure Internal
        (Window               : Gdk.Window.Gdk_Window;
         Property             : Gdk.Types.Gdk_Atom;
         The_Type             : Gdk.Types.Gdk_Atom;
         Offset               : Gulong;
         Length               : Gulong;
         Pdelete              : Gint;
         Actual_Property_Type : out Gdk.Types.Gdk_Atom;
         Actual_Format        : out Gint;
         Actual_Length        : out Gint;
         Data                 : out Guchars_Ptr.Pointer;
         Success              : out Gint);
      pragma Import (C, Internal, "ada_gdk_property_get");

      Actual_Length : Gint;
      Tmp_Success   : Gint;
      Tmp_Result    : Guchars_Ptr.Pointer;

   begin
      Internal
        (Window,
         Property,
         The_Type,
         Offset,
         Length,
         Boolean'Pos (Pdelete),
         Actual_Property_Type,
         Actual_Format,
         Actual_Length,
         Tmp_Result,
         Tmp_Success);

      Success := Boolean'Val (Tmp_Success);

      Data := new Guchar_Array'
        (To_Guchar_Array
         (Guchars_Ptr.Value
          (Ref => Tmp_Result,
           Length => Interfaces.C.ptrdiff_t (Actual_Length))));
      C_Free (Tmp_Result);
   end Get;

   -----------------------
   --  To_Guchar_Array  --
   -----------------------

   function To_Guchar_Array
     (Value : Local_Guchar_Array) return Guchar_Array
   is
      Result : Guchar_Array (Value'Range);
   begin
      for Index in Value'Range loop
         Result (Index) := Value (Index);
      end loop;

      return Result;
   end To_Guchar_Array;

end Gdk.Property;
