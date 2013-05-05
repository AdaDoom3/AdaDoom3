-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Glib.Types;          use Glib.Types;
with Gtk.Cell_Renderer;   use Gtk.Cell_Renderer;
with Gtk.Tree_Model;      use Gtk.Tree_Model;

package body Gtk.Cell_Layout is

   procedure Internal_Cell_Data_Func
     (Cell_Layout : Gtk_Cell_Layout;
      Cell, Model, Iter : System.Address; Data : Cell_Data_Func);
   --  Internal proxu for Cell_Data_Func

   type Gtk_Tree_Iter_Access is access all Gtk_Tree_Iter;
   function To_Iter is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Iter_Access);

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Expand      : Boolean)
   is
      procedure Internal
        (Cell_Layout : Gtk_Cell_Layout;
         Cell        : System.Address;
         Expand      : Gboolean);
      pragma Import (C, Internal, "gtk_cell_layout_pack_start");
   begin
      Internal (Cell_Layout, Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_Start;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Expand      : Boolean)
   is
      procedure Internal
        (Cell_Layout : Gtk_Cell_Layout;
         Cell        : System.Address;
         Expand      : Gboolean);
      pragma Import (C, Internal, "gtk_cell_layout_pack_end");
   begin
      Internal (Cell_Layout, Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_End;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Attribute   : String;
      Column      : Gint)
   is
      procedure Internal
        (Cell_Layout : Gtk_Cell_Layout;
         Cell        : System.Address;
         Attribute   : String;
         Column      : Gint);
      pragma Import (C, Internal, "gtk_cell_layout_add_attribute");
   begin
      Internal (Cell_Layout, Get_Object (Cell), Attribute & ASCII.NUL, Column);
   end Add_Attribute;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal
        (Cell_Layout : Gtk_Cell_Layout;
         Cell        : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear_attributes");
   begin
      Internal (Cell_Layout, Get_Object (Cell));
   end Clear_Attributes;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Position    : Gint)
   is
      procedure Internal
        (Cell_Layout : Gtk_Cell_Layout;
         Cell        : System.Address;
         Position    : Gint);
      pragma Import (C, Internal, "gtk_cell_layout_reorder");
   begin
      Internal (Cell_Layout, Get_Object (Cell), Position);
   end Reorder;

   -----------------------------
   -- Internal_Cell_Data_Func --
   -----------------------------

   procedure Internal_Cell_Data_Func
     (Cell_Layout : Gtk_Cell_Layout;
      Cell, Model, Iter : System.Address; Data : Cell_Data_Func)
   is
      M_Stub : Gtk_Tree_Model_Record;
      C_Stub : Gtk_Cell_Renderer_Record;

      C : constant Gtk_Cell_Renderer :=
        Gtk_Cell_Renderer (Get_User_Data_Fast (Cell, C_Stub));
      M : constant Gtk_Tree_Model :=
        Gtk_Tree_Model (Get_User_Data_Fast (Model, M_Stub));
      I : constant Gtk_Tree_Iter_Access := To_Iter (Iter);
   begin
      Data (Cell_Layout, C, M, I.all);
   end Internal_Cell_Data_Func;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Func        : Cell_Data_Func)
   is
      procedure Internal
        (Cell_Layout   : Gtk_Cell_Layout;
         Cell_Renderer : System.Address;
         Func          : System.Address;
         Func_Data     : System.Address;
         Destroy       : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_set_cell_data_func");

   begin
      Internal
        (Cell_Layout, Get_Object (Cell),
         Internal_Cell_Data_Func'Address,
         Func.all'Address, System.Null_Address);
   end Set_Cell_Data_Func;

   -------------------------
   -- Cell_Data_Functions --
   -------------------------

   package body Cell_Data_Functions is

      procedure Free is new Ada.Unchecked_Deallocation
        (Data_Type, Data_Type_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (Data_Type_Record, Data_Type_Record_Access);

      procedure Internal
        (Cell_Layout   : Gtk_Cell_Layout;
         Cell_Renderer : System.Address;
         Func          : System.Address;
         Func_Data     : Data_Type_Record_Access;
         Destroy       : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_set_cell_data_func");

      ----------------------------------
      -- Internal_Data_Cell_Data_Func --
      ----------------------------------

      procedure Internal_Data_Cell_Data_Func
        (Cell_Layout       : Gtk_Cell_Layout;
         Cell, Model, Iter : System.Address;
         Data              : Data_Type_Record_Access)
      is
         C_Stub : Gtk_Cell_Renderer_Record;
         M_Stub : Gtk_Tree_Model_Record;

         C : constant Gtk_Cell_Renderer :=
           Gtk_Cell_Renderer (Get_User_Data_Fast (Cell, C_Stub));
         M : constant Gtk_Tree_Model :=
           Gtk_Tree_Model (Get_User_Data_Fast (Model, M_Stub));
         I : constant Gtk_Tree_Iter_Access := To_Iter (Iter);

      begin
         Data.Func (Cell_Layout, C, M, I.all, Data.Data.all);
      end Internal_Data_Cell_Data_Func;

      -----------------------------
      -- Internal_Destroy_Notify --
      -----------------------------

      procedure Internal_Destroy_Notify (Data : Data_Type_Record_Access) is
         D : Data_Type_Record_Access := Data;
      begin
         if D.Destroy /= null then
            D.Destroy (D.Data.all);
         end if;
         Free (D.Data);
         Free (D);
      end Internal_Destroy_Notify;

      ------------------------
      -- Set_Cell_Data_Func --
      ------------------------

      procedure Set_Cell_Data_Func
        (Cell_Layout : Gtk_Cell_Layout;
         Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Func        : Cell_Data_Func;
         Data        : Data_Type;
         Destroy     : Destroy_Notify := null)
      is
      begin
         Internal
           (Cell_Layout, Get_Object (Cell),
            Internal_Data_Cell_Data_Func'Address,
            new Data_Type_Record'
              (Func => Func, Destroy => Destroy, Data => new Data_Type'(Data)),
            Internal_Destroy_Notify'Address);
      end Set_Cell_Data_Func;
   end Cell_Data_Functions;

end Gtk.Cell_Layout;
