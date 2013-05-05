-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--               Copyright (C) 2006-2013, AdaCore                    --
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

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Gtk;                      use Gtk;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;    use Gtk.Tree_Model_Filter;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Frame;                use Gtk.Frame;

package body Create_Tree_Filter is

   Column_0 : constant := 0;

   function Custom_Filter
     (Model : access Gtk_Tree_Model_Record'Class;
      Iter  : Gtk_Tree_Iter) return Boolean;
   --  Decide whether a row should be made visible or not

   procedure Custom_Appearance
     (Model  : access Gtk_Tree_Model_Filter_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Value  : out GValue;
      Column : Gint);
   --  Change the appearance of the view dynamically

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This example demonstrates a special tree modeL: it wraps another"
        & " model, and can be used to filter out lines, or even modify its"
        & " appearance on the fly." & ASCII.LF
        & "In this example, we have creates a model that contains the sequence"
        & " 1, 2, ... 9. Another model is applied on top of it, and filters"
        & " all odd numbers rows. It also changes the appearance to display"
        & " some extra text." & ASCII.LF
        & "The underlying model itself is never modified, and by changing a"
        & " few properties we can decide to show the whole underlying model"
        & " itself." & ASCII.LF
        & "Modifying the appearance on the fly is not efficient. It is"
        & " generally better to use the functions from @bGtk_Cell_Layout@B to"
        & " create ""virtual"" columns in the model. See the Cell View demo.";
   end Help;

   -------------------
   -- Custom_Filter --
   -------------------

   function Custom_Filter
     (Model : access Gtk_Tree_Model_Record'Class;
      Iter  : Gtk_Tree_Iter) return Boolean
   is
      Value : constant Gint := Get_Int (Model, Iter, Column_0);
   begin
      return Value mod 2 /= 1;
   end Custom_Filter;

   -----------------------
   -- Custom_Appearance --
   -----------------------

   procedure Custom_Appearance
     (Model  : access Gtk_Tree_Model_Filter_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Value  : out GValue;
      Column : Gint)
   is
      Val        : Gint;
      Child_Iter : Gtk_Tree_Iter;
   begin
      Convert_Iter_To_Child_Iter (Model, Child_Iter, Iter);
      Val := Get_Int (Get_Model (Model), Child_Iter, Column);
      Set_String (Value, "This is line" & Gint'Image (Val));
   end Custom_Appearance;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Model    : Gtk_List_Store;
      Filter   : Gtk_Tree_Model_Filter;
      Tree     : Gtk_Tree_View;
      Scrolled : Gtk_Scrolled_Window;
      Col      : Gtk_Tree_View_Column;
      Num      : Gint;
      Text     : Gtk_Cell_Renderer_Text;
      Iter     : Gtk_Tree_Iter;
      pragma Unreferenced (Num);

   begin
      Set_Label (Frame, "Tree Model Filter");

      Gtk_New (Scrolled);
      Add (Frame, Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      --  Create the model that contains the actual data. This model will
      --  never be modified

      Gtk_New (Model, (Column_0 => GType_Int));

      for N in 1 .. 10 loop
         Append (Model, Iter);
         Set (Model, Iter, Column_0, Gint (N));
      end loop;

      --  Now creates a filter around this model. We filter through a custom
      --  function, but that could be a simple row in the model as well. The
      --  function is slightly more flexible, though.

      Gtk_New (Filter, Model);
      Set_Visible_Func (Filter, Custom_Filter'Access);
      Set_Modify_Func  (Filter, (0 => GType_String), Custom_Appearance'Access);

      --  And now a view that displays the filter. A single column is displayed

      Gtk_New (Tree, Filter);
      Add (Scrolled, Tree);
      Set_Headers_Visible (Tree, False);

      Gtk_New (Text);

      Gtk_New (Col);
      Num := Append_Column (Tree, Col);
      Pack_Start (Col, Text, True);
      Add_Attribute (Col, Text, "text", Column_0);

      Show_All (Frame);
   end Run;

end Create_Tree_Filter;
