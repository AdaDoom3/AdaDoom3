-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2006 AdaCore                    --
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

with Glib;                   use Glib;
with Glib.Properties;        use Glib.Properties;
with Gtk.Button;             use Gtk.Button;
with Gtk.Box;                use Gtk.Box;
with Gtk.Frame;              use Gtk.Frame;
with Gtk.Cell_Renderer;      use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Cell_View;          use Gtk.Cell_View;
with Gtk.Cell_Layout;        use Gtk.Cell_Layout;
with Gtk.List_Store;         use Gtk.List_Store;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.Handlers;        use Gtkada.Handlers;

package body Create_Cell_View is

   Column_0 : constant := 0;
   Column_1 : constant := 1;
   --  The columns in the model

   Path : Gtk_Tree_Path;

   procedure Move_To_Previous (View : access Gtk_Widget_Record'Class);
   procedure Move_To_Next (View : access Gtk_Widget_Record'Class);
   --  Display the previous or next function in the cell view

   procedure Compute_Column_3
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Compute dynamically what to display in the third column of the view,
   --  based on the contents of the model. In this example, this is a
   --  concatenation of the two model columns

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Cell_View@B provides a view of a single row of a tree"
        & " model." & ASCII.LF
        & "In this example, the first two columns displayed come straight from"
        & " the model, where they are stored as a string and an integer. The"
        & " third column is computed dynamically, and doesn't exist in the"
        & " model itself.";
   end Help;

   ----------------------
   -- Move_To_Previous --
   ----------------------

   procedure Move_To_Previous (View : access Gtk_Widget_Record'Class) is
   begin
      if Prev (Path) then
         Set_Displayed_Row (Gtk_Cell_View (View), Path);
      end if;
   end Move_To_Previous;

   ------------------
   -- Move_To_Next --
   ------------------

   procedure Move_To_Next (View : access Gtk_Widget_Record'Class) is
   begin
      Next (Path);
      Set_Displayed_Row (Gtk_Cell_View (View), Path);
   end Move_To_Next;

   ----------------------
   -- Compute_Column_3 --
   ----------------------

   procedure Compute_Column_3
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Unreferenced (Cell_Layout);
   begin
      Set_Property
        (Cell, Gtk.Cell_Renderer_Text.Text_Property,
         "--" & Get_String (Model, Iter, 0) & "--"
         & Gint'Image (Get_Int (Model, Iter, 1)) & "--");
   end Compute_Column_3;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box, Hbox : Gtk_Box;
      Prev, Next : Gtk_Button;
      Model     : Gtk_List_Store;
      Iter      : Gtk_Tree_Iter;
      View      : Gtk_Cell_View;
      Render    : Gtk_Cell_Renderer_Text;
   begin
      Set_Label (Frame, "Cell View");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Gtk_New_Hbox (Hbox, Homogeneous => True);
      Pack_Start (Box, Hbox, Expand => False);

      Gtk_New (Prev, "Previous row");
      Pack_Start (Hbox, Prev, Expand => False);

      Gtk_New (Next, "Next row");
      Pack_Start (Hbox, Next, Expand => False);

      --  Create a model. This is a set of rows, each with two columns in this
      --  specific case.
      Gtk_New (Model, (Column_0 => GType_String,
                       Column_1 => GType_Int));
      Append (Model, Iter);
      Set (Model, Iter, Column_0, "row 1");
      Set (Model, Iter, Column_1, 1);

      Append (Model, Iter);
      Set (Model, Iter, Column_0, "row 2");
      Set (Model, Iter, Column_1, 2);

      Append (Model, Iter);
      Set (Model, Iter, Column_0, "row 3");
      Set (Model, Iter, Column_1, 3);

      --  Create the cell view. We need to specify the renderer that will show
      --  the columns of the model. Note that we do not have to display all
      --  the columns.
      --  Adding renderers is done through the Gtk_Cell_Layout interface, not
      --  directly through Gtk_Cell_View.

      Gtk_New (View);
      Pack_Start (Box, View, Expand => True, Fill => False);
      Set_Model (View, Gtk_Tree_Model (Model));

      Gtk_New (Render);
      Pack_Start    (+View, Render, Expand => True);
      Add_Attribute (+View, Render, "text", Column_0);

      Gtk_New (Render);
      Pack_Start (+View, Render, Expand => True);
      Add_Attribute (+View, Render, "text", Column_1);

      --  The third column in the view is computed dynamically from the others
      --  in the model, and doesn't have to exist in the model itself.
      Gtk_New (Render);
      Pack_Start (+View, Render, Expand => True);
      Set_Cell_Data_Func (+View, Render, Compute_Column_3'Access);

      Path := Gtk_New_First;
      Set_Displayed_Row (View, Path);

      Widget_Callback.Object_Connect
        (Prev, "clicked", Move_To_Previous'Access, View);
      Widget_Callback.Object_Connect
        (Next, "clicked", Move_To_Next'Access, View);

      Show_All (Frame);
   end Run;

end Create_Cell_View;
