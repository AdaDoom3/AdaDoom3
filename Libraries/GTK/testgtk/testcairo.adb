-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2010-2013, AdaCore                  --
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

pragma Ada_2005;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glib; use Glib;
with Glib.Object; use Glib.Object;

with Cairo;         use Cairo;

with Gdk.Cairo;    use Gdk.Cairo;
with Gdk.Event;    use Gdk.Event;
with Gdk.Window;   use Gdk.Window;

with Gtk.Main;
with Gtk.Box;           use Gtk.Box;
with Gtk.Button;        use Gtk.Button;
with Gtk.Drawing_Area;  use Gtk.Drawing_Area;
with Gtk.Label;         use Gtk.Label;

with Gtk.Widget;   use Gtk.Widget;
with Gtk.Window;   use Gtk.Window;
with Gtk.Handlers; use Gtk.Handlers;

with Gtk.Print_Operation;    use Gtk.Print_Operation;
with Gtkada.Printing;        use Gtkada.Printing;

with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_Selection;     use Gtk.Tree_Selection;
with Gtk.Tree_Store;         use Gtk.Tree_Store;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;

with Testcairo_Drawing;    use Testcairo_Drawing;
with Testcairo_Printing;   use Testcairo_Printing;

procedure Testcairo is

   --  The tests implemented in this example program

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Drawing_Area_Record, Boolean);

   package Button_Cb is new Gtk.Handlers.Callback
     (Gtk_Button_Record);

   package Selection_Cb is new Gtk.Handlers.Callback
     (Gtk_Tree_Selection_Record);

   --  Pi : constant Gdouble := Gdouble (Ada.Numerics.Pi);

   Win  : Gtk_Window;
   Area : Gtk_Drawing_Area;
   Test : Test_Type := Test_Type'First;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Pretty (T : Test_Type) return String;
   --  Pretty print T

   function Expose_Cb (Area  : access Gtk_Drawing_Area_Record'Class;
                       Event : Gdk_Event) return Boolean;
   --  Callback on an expose event on Win

   procedure On_Print_Cb  (Widget : access Gtk_Button_Record'Class);
   --  Callback on a click on the "Print" button

   type Doc_Array is array (Test_Type) of Unbounded_String;

   function "-"
     (S : String) return Unbounded_String
      renames To_Unbounded_String;

   Docs : constant Doc_Array :=
     (Rectangles      => -"Simple rectangles",
      Transparency    => -"Transparency",
      Operators       => -"Compositing operators",
      Testcairo_Drawing.Matrix   =>
      -"Translating, rotating and scaling using matrix transformations",
      Transformations => -"Direct transformations",
      Paths           => -"Paths",
      Patterns        => -"Patterns",
      Toy_Text        => -"The Cairo 'toy' text API",
      Pango_Text      => -"Rendering sophisticated text using Pango",
      Clip_And_Paint  => -"Painting and clipping",
      Surface_And_Png => -"Using surfaces and saving to PNG",
      Image           => -"Rendering an image as background");

   ---------------
   -- Expose_Cb --
   ---------------

   function Expose_Cb (Area  : access Gtk_Drawing_Area_Record'Class;
                       Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Cr : Cairo_Context;

   begin
      Cr := Create (Get_Window (Area));
      Draw_On_Context (Cr, Gtk_Widget (Win), Test);
      Destroy (Cr);
      return False;
   end Expose_Cb;

   Tree    : Gtk_Tree_View;
   Model   : Gtk_Tree_Store;
   Col     : Gtk_Tree_View_Column;
   Rend    : Gtk_Cell_Renderer_Text;
   Col_Num : Gint;
   Iter    : Gtk_Tree_Iter;
   Label   : Gtk_Label;
   Button  : Gtk_Button;

   Box     : Gtk_Box;
   Hbox    : Gtk_Box;
   Vbox    : Gtk_Box;

   pragma Unreferenced (Col_Num);

   -----------------
   -- On_Print_Cb --
   -----------------

   procedure On_Print_Cb (Widget : access Gtk_Button_Record'Class) is
      Print_Op : Testcairo_Print_Operation;
      Result   : Gtk_Print_Operation_Result;
      pragma Unreferenced (Result);
   begin
      Print_Op := new Testcairo_Print_Operation_Record;
      Gtkada.Printing.Initialize (Print_Op);
      Print_Op.Test := Test;
      Print_Op.Win  := Gtk_Widget (Win);

      Print_Op.Set_N_Pages (1);

      --  Call up the print operation dialog
      Result :=
        Connect_And_Run (Print_Op, Action_Print_Dialog,
                         Gtk_Window (Get_Toplevel (Widget)));

   end On_Print_Cb;

   ------------
   -- Pretty --
   ------------

   function Pretty (T : Test_Type) return String is
      Capitalize : Boolean := True;
      Img        : String := Test_Type'Image (T);
   begin
      for J in Img'Range loop
         if not Capitalize then
            if Img (J) = '_' then
               Img (J) := ' ';
               Capitalize := True;
            else
               Img (J) := To_Lower (Img (J));
            end if;
         else
            Capitalize := False;
         end if;
      end loop;
      return Img;
   end Pretty;

   procedure S_Changed
     (Widget : access Gtk_Tree_Selection_Record'Class);

   ---------------
   -- S_Changed --
   ---------------

   procedure S_Changed
     (Widget : access Gtk_Tree_Selection_Record'Class)
   is
      pragma Unreferenced (Widget);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Ind   : Gint_Array (0 .. 0);
      W, H  : Gint;
   begin
      Get_Selected (Get_Selection (Tree), Model, Iter);
      Path := Get_Path (Model, Iter);
      Ind := Get_Indices (Path);
      Test := Test_Type'Val (Ind (0));
      Path_Free (Path);

      W := Area.Get_Allocation_Width;
      H := Area.Get_Allocation_Height;

      Invalidate_Rect (Get_Window (Area),
                       (0, 0, W, H), Invalidate_Children => False);

      Set_Text (Label, To_String (Docs (Test)));
   end S_Changed;

begin
   Gtk.Main.Init;
   Gtk_New (Win);
   Set_Default_Size (Win, 960, 400);

   --  Create the tree and model
   Gtk_New (Model, (0 => GType_String));
   Gtk_New (Tree, Model);
   Tree.Set_Headers_Visible (False);
   Tree.Set_Show_Expanders (False);
   Gtk_New (Rend);

   Gtk_New (Col);
   Pack_Start (Col, Rend, False);
   Add_Attribute (Col, Rend, "text", 0);
   Col_Num := Tree.Append_Column (Col);

   for T in Test_Type loop
      Append (Model, Iter, Null_Iter);
      Set (Model, Iter, 0, Pretty (T));
   end loop;

   Selection_Cb.Connect
     (Get_Selection (Tree),
      Gtk.Tree_Selection.Signal_Changed,
      S_Changed'Access,
      After => True);

   Gtk_New_Hbox (Box);
   Win.Add (Box);
   Box.Pack_Start (Tree, False, False, 3);

   Gtk_New (Area);

   Gtk_New_Vbox (Vbox);
   Vbox.Pack_Start (Area, True, True, 3);

   Gtk_New_Hbox (Hbox);
   Vbox.Pack_Start (Hbox, False, False, 3);

   Gtk_New (Label);
   Hbox.Pack_Start (Label, True, True, 3);

   Gtk_New (Button, "Print");
   Hbox.Pack_End (Button, False, False, 3);
   Button_Cb.Connect
     (Button,
      Gtk.Button.Signal_Clicked,
      On_Print_Cb'Access,
      After => True);

   Box.Pack_Start (Vbox, True, True, 3);

   --  Connect to the "expose" event.

   Event_Cb.Connect (Area, "expose_event",
                     Event_Cb.To_Marshaller (Expose_Cb'Access));

   Show_All (Win);
   Gtk.Main.Main;
end Testcairo;
