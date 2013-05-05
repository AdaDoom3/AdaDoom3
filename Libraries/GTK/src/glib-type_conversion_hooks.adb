-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2001-2013, AdaCore                   --
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

package body Glib.Type_Conversion_Hooks is

   type Conversion_Support_Hook_Type is
     access function return Glib.GType;
   pragma Convention (C, Conversion_Support_Hook_Type);
   --  This variable can be point to one of your functions.
   --  It returns the GType corresponding to the handled widget

   type Hook_List;
   type Hook_List_Access is access Hook_List;
   type Hook_List is record
      Get_GType : Conversion_Support_Hook_Type;
      Creator   : Conversion_Creator_Hook_Type;
      Next : Hook_List_Access := null;
   end record;
   --  Internal structure used for the list.

   Conversion_Hooks : Hook_List_Access := null;

   ----------------------
   -- Hook_Registrator --
   ----------------------

   package body Hook_Registrator is

      -------------
      -- Creator --
      -------------

      function Creator
        (Expected_Object : GObject_Record'Class) return GObject is
      begin
         if Expected_Object in Handled_Type'Class then
            return new GObject_Record'Class'(Expected_Object);
         else
            return new Handled_Type;
         end if;
      end Creator;

   begin
      Conversion_Hooks :=
        new Hook_List'
          (Get_GType  => Conversion_Support_Hook_Type (Get_GType),
           Creator    => Creator_Access,
           Next       => Conversion_Hooks);
   end Hook_Registrator;

   -------------------------
   -- Conversion_Function --
   -------------------------

   function Conversion_Function
     (Obj : System.Address; Stub : GObject_Record'Class) return GObject
   is
      function Get_Type (Obj : System.Address) return GType;
      pragma Import (C, Get_Type, "ada_gobject_get_type");

      The_Type  : GType := Get_Type (Obj);
      Hooks     : Glib.Type_Conversion_Hooks.Hook_List_Access;

      use type Glib.Type_Conversion_Hooks.Hook_List_Access;

   begin
      while The_Type > GType_Object loop
         Hooks := Glib.Type_Conversion_Hooks.Conversion_Hooks;

         while Hooks /= null loop
            if The_Type = Hooks.Get_GType.all then
               return Hooks.Creator (Stub);
            end if;

            Hooks := Hooks.Next;
         end loop;

         The_Type := Parent (The_Type);
      end loop;

      return new GObject_Record'Class'(Stub);
   end Conversion_Function;

end Glib.Type_Conversion_Hooks;
