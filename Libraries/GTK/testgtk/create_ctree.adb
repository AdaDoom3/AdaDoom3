-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                Copyright (C) 2003-2013, AdaCore                   --
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

with Glib; use Glib;
with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Pixmap;
with Gtk; use Gtk;
with Gtk.Adjustment;
with Gtk.Box;
with Gtk.Button;
with Gtk.Check_Button;
with Gtk.Ctree; use Gtk.Ctree;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Label;
with Gtk.Option_Menu;
with Gtk.Radio_Menu_Item;
with Gtk.Scrolled_Window;
with Gtk.Handlers;
with Gtk.Spin_Button;
with Gtk.Style;   use Gtk.Style;
with Gtk.Tooltips;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.Types;
with Pango.Font; use Pango.Font;

with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Common;

package body Create_Ctree is

   use type Gtk.Window.Gtk_Window;
   use Gtkada.Types;

   package Ctree_Style_Row_Data is
     new Gtk.Ctree.Row_Data (Gtk.Style.Gtk_Style);

   package Ctree_User_Cb is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
      User_Type   => Gtk.Ctree.Gtk_Ctree);

   package Ctree_Cb is new Gtk.Handlers.Callback
     (Widget_Type => Gtk.Ctree.Gtk_Ctree_Record);

   package Gint_Ctree_Cb is new Gtk.Handlers.Return_Callback
     (Widget_Type => Gtk.Ctree.Gtk_Ctree_Record, Return_Type => Gint);

   package Adjustment_Cb is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Adjustment.Gtk_Adjustment_Record,
      User_Type   => Gtk.Ctree.Gtk_Ctree);

   Ctree : Gtk.Ctree.Gtk_Ctree;
   Clist_Omenu_Group1  : Gtk.Widget.Widget_SList.GSlist;
   Clist_Omenu_Group2  : Gtk.Widget.Widget_SList.GSlist;
   Clist_Omenu_Group3  : Gtk.Widget.Widget_SList.GSlist;
   Clist_Omenu_Group4  : Gtk.Widget.Widget_SList.GSlist;
   Pixmap1, Pixmap2, Pixmap3 : Gdk.Pixmap.Gdk_Pixmap;
   Mask1, Mask2, Mask3 : Gdk.Bitmap.Gdk_Bitmap;
   Book_Label, Page_Label, Sel_Label, Vis_Label : Gtk.Label.Gtk_Label;
   Spin1, Spin2, Spin3 : Gtk.Spin_Button.Gtk_Spin_Button;

   Style1 : Gtk.Style.Gtk_Style := Null_Style;
   Style2 : Gtk.Style.Gtk_Style := Null_Style;

   Books : Gint := 0;
   Pages : Gint := 0;

   Title : constant Chars_Ptr_Array := "Tree" + "Info";

   Items1 : constant Chars_Ptr_Array :=
     "No lines" + "Solid" + "Dotted" + "Tabbed";

   Items2 : constant Chars_Ptr_Array :=
     "None" + "Square" + "Triangle" + "Circular";

   Items3 : constant Chars_Ptr_Array := "Left" + "Right";

   Items4 : constant Chars_Ptr_Array :=
     "Single" + "Browse" + "Multiple" + "Extended";

   --  Put at the package level, because we would like to avoid allocating
   --  then freeing the memory every time we click on the "ctree" button.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "!!!! Consider using a @bGtk_Tree_View@B instead !!!!"
        & ASCII.LF
        & ASCII.LF
        & "A @bGtk_Ctree@B is a widget similar to a @bGtk_Clist@B, except"
        & " that the information is presented in a hierarchical manner, as in"
        & " a @bGtk_Tree@B.";
   end Help;

   -----------------
   -- After_Press --
   -----------------

   procedure After_Press (Ctree : access Gtk.Ctree.Gtk_Ctree_Record'Class) is
      N_Sel : Guint renames Gint_List.Length (Gtk.Ctree.Get_Selection (Ctree));
      N_Vis : Guint renames Gtk.Ctree.Row_List.Length
        (Gtk.Ctree.Get_Row_List (Ctree));
   begin
      Gtk.Label.Set_Text (Sel_Label, Str => Common.Image_Of (Gint (N_Sel)));
      Gtk.Label.Set_Text (Vis_Label, Str => Common.Image_Of (Gint (N_Vis)));
      Gtk.Label.Set_Text (Book_Label, Str => Common.Image_Of (Books));
      Gtk.Label.Set_Text (Page_Label, Str => Common.Image_Of (Pages));
   end After_Press;

   ------------------------
   -- After_Press_return --
   ------------------------

   function After_Press_Return
      (Ctree : access Gtk.Ctree.Gtk_Ctree_Record'Class)
      return Gint is
   begin
      After_Press (Ctree);
      return 0;
   end After_Press_Return;

   -----------------
   -- Count_Items --
   -----------------

   procedure Count_Items
     (Ctree      : access Gtk.Ctree.Gtk_Ctree_Record'Class;
      Node       :        Gtk.Ctree.Gtk_Ctree_Node;
      Dummy_Data :        Ctree_Style_Row_Data.Data_Type_Access)
   is
      pragma Warnings (Off, Ctree);
      pragma Warnings (Off, Dummy_Data);
   begin
      if Gtk.Ctree.Row_Get_Is_Leaf (Gtk.Ctree.Node_Get_Row (Node)) then
         Pages := Pages - 1;
      else
         Books := Books - 1;
      end if;
   end Count_Items;

   -------------------
   -- Change_Indent --
   -------------------

   procedure Change_Indent
     (Adj   : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Ctree :        Gtk.Ctree.Gtk_Ctree)
   is
   begin
      Gtk.Ctree.Set_Indent
        (Ctree, Indent => Gint (Gtk.Adjustment.Get_Value (Adj)));
   end Change_Indent;

   --------------------
   -- Change_Spacing --
   --------------------

   procedure Change_Spacing
     (Adj   : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Ctree :        Gtk.Ctree.Gtk_Ctree)
   is
   begin
      Gtk.Ctree.Set_Spacing
        (Ctree, Spacing => Gint (Gtk.Adjustment.Get_Value (Adj)));
   end Change_Spacing;

   -----------------------
   -- Change_Row_Height --
   -----------------------

   procedure Change_Row_Height
     (Adj   : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Ctree :        Gtk.Ctree.Gtk_Ctree)
   is
   begin
      Gtk.Ctree.Set_Row_Height
        (Ctree, Height => Gint (Gtk.Adjustment.Get_Value (Adj)));
   end Change_Row_Height;

   ----------------
   -- Expand_All --
   ----------------

   procedure Expand_All (Ctree : access Gtk.Ctree.Gtk_Ctree_Record'Class) is
   begin
      Gtk.Ctree.Expand_Recursive (Ctree);
      After_Press (Ctree);
   end Expand_All;

   ------------------
   -- Collapse_All --
   ------------------

   procedure Collapse_All (Ctree : access Gtk.Ctree.Gtk_Ctree_Record'Class) is
   begin
      Gtk.Ctree.Collapse_Recursive (Ctree);
      After_Press (Ctree);
   end Collapse_All;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Ctree : access Gtk.Ctree.Gtk_Ctree_Record'Class) is
   begin
      Gtk.Ctree.Select_Recursive (Ctree);
      After_Press (Ctree);
   end Select_All;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All (Ctree : access Gtk.Ctree.Gtk_Ctree_Record'Class) is
   begin
      Gtk.Ctree.Unselect_Recursive (Ctree);
      After_Press (Ctree);
   end Unselect_All;

   ------------------
   -- Change_Style --
   ------------------

   procedure Change_Style (Ctree : access Gtk.Ctree.Gtk_Ctree_Record'Class)
   is
      Node : Gtk.Ctree.Gtk_Ctree_Node;
      Child : Gtk.Ctree.Gtk_Ctree_Node;
      Col1, Col2 : Gdk.Color.Gdk_Color;
      Pos : Gint := Gtk.Ctree.Get_Focus_Row (Ctree);
   begin

      if Pos < 0 then
         Pos := 0;
      end if;

      Node := Gtk.Ctree.Node_List.Get_Gpointer
        (Gtk.Ctree.Node_List.Nth (Gtk.Ctree.Get_Node_List (Ctree),
                                  Guint (Pos)));

      if Node = null then
         return;
      end if;

      if Style1 = Null_Style then
         Gdk.Color.Set_Rgb (Color => Col1,
                            Red => 0,
                            Green => 56_000,
                            Blue => 0);
         Gdk.Color.Set_Rgb (Color => Col2,
                            Red => 32_000,
                            Green => 0,
                            Blue => 56_000);

         Gtk.Style.Gtk_New (Style1);
         Gtk.Style.Set_Base (Style      => Style1,
                             State_Type => State_Normal,
                             Color      => Col1);
         Gtk.Style.Set_Foreground (Style      => Style1,
                                   State_Type => State_Selected,
                                   Color      => Col2);

         Gtk.Style.Gtk_New (Style2);
         Gtk.Style.Set_Base (Style      => Style2,
                             State_Type => State_Selected,
                             Color      => Col2);
         Gtk.Style.Set_Foreground (Style      => Style2,
                                   State_Type => State_Normal,
                                   Color      => Col1);
         Gtk.Style.Set_Background (Style      => Style2,
                                   State_Type => State_Normal,
                                   Color      => Col2);

         Set_Font_Description (Style2, From_String ("Courier 30"));

      end if;

      Gtk.Ctree.Node_Set_Cell_Style (Ctree, Node,
                                     Column => 1,
                                     Style => Style1);
      Gtk.Ctree.Node_Set_Cell_Style (Ctree, Node,
                                     Column => 0,
                                     Style => Style2);

      Child := Gtk.Ctree.Row_Get_Children (Gtk.Ctree.Node_Get_Row (Node));
      if Child /= null then
         Gtk.Ctree.Node_Set_Row_Style (Ctree, Child, Style2);
      end if;
   end Change_Style;

   ----------------------
   -- Remove_Selection --
   ----------------------

   procedure Remove_Selection (Ctree : access Gtk.Ctree.Gtk_Ctree_Record'Class)
   is
      use Gtk.Ctree.Node_List;

      Node : Gtk.Ctree.Gtk_Ctree_Node;
      Selection : Gtk.Ctree.Node_List.Glist;

   begin

      Gtk.Ctree.Freeze (Ctree);

      loop

         Selection := Gtk.Ctree.Get_Selection (Ctree);
         exit when Gtk.Ctree.Node_List.Length (Selection) = 0;

         Node := Gtk.Ctree.Node_List.Get_Data (Selection);

         if Gtk.Ctree.Row_Get_Is_Leaf (Gtk.Ctree.Node_Get_Row (Node)) then
            Pages := Pages - 1;
         else
            Ctree_Style_Row_Data.Post_Recursive (Ctree, Node,
                                                 Count_Items'Access,
                                                 null);
         end if;

         Gtk.Ctree.Remove_Node (Ctree, Node);

         exit when Gtk.Ctree.Get_Selection_Mode (Ctree) = Selection_Browse;

      end loop;

      if Gtk.Ctree.Get_Selection_Mode (Ctree) = Selection_Multiple and then
        not Is_Created (Gtk.Ctree.Get_Selection (Ctree)) and then
        Gtk.Ctree.Get_Focus_Row (Ctree) >= 0
      then

         Node := Gtk.Ctree.Node_Nth (Ctree,
                                     Guint (Gtk.Ctree.Get_Focus_Row (Ctree)));
         if Node /= null then
            Gtk.Ctree.Gtk_Select (Ctree, Node);
         end if;

      end if;

      Gtk.Ctree.Thaw (Ctree);
      After_Press (Ctree);

   end Remove_Selection;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Ctree : access Gtk.Ctree.Gtk_Ctree_Record'Class;
      Node  :        Gtk.Ctree.Gtk_Ctree_Node;
      Dummy :        Ctree_Style_Row_Data.Data_Type_Access)
   is
      pragma Warnings (Off, Dummy);
      Style : Gtk.Style.Gtk_Style;
   begin
      if Node = null then
         return;
      end if;

      if Gtk.Ctree.Get_Line_Style (Ctree) /= Ctree_Lines_Tabbed then

         if not Gtk.Ctree.Row_Get_Is_Leaf (Gtk.Ctree.Node_Get_Row (Node)) then
            Style := Ctree_Style_Row_Data.Node_Get_Row_Data (Ctree,
                                                             Node => Node);
         else
            declare
               Parent : constant Gtk.Ctree.Gtk_Ctree_Node :=
                 Gtk.Ctree.Row_Get_Parent (Gtk.Ctree.Node_Get_Row (Node));
            begin
               if Parent /= null then
                  Style :=
                    Ctree_Style_Row_Data.Node_Get_Row_Data (Ctree, Parent);
               end if;
            end;
         end if;

      end if;

      Gtk.Ctree.Node_Set_Row_Style (Ctree, Node => Node, Style =>  Style);

   end Set_Background;

   -----------------------
   -- Toggle_Line_Style --
   -----------------------

   procedure Toggle_Line_Style
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Current_Line_Style : constant Gtk_Ctree_Line_Style :=
        Gtk.Ctree.Get_Line_Style (Ctree);
      New_Line_Style : constant Gtk_Ctree_Line_Style :=
        Gtk_Ctree_Line_Style'Val (3 - Gtk.Radio_Menu_Item.Selected_Button
                                  (Clist_Omenu_Group1));
   begin
      if not Gtk.Widget.Mapped_Is_Set (Widget) then
         return;
      end if;

      if New_Line_Style /= Current_Line_Style and then
        (New_Line_Style = Ctree_Lines_Tabbed or
         Current_Line_Style = Ctree_Lines_Tabbed) then
         Ctree_Style_Row_Data.Pre_Recursive (Ctree,
                                             Node => Gtk.Ctree.Null_Ctree_Node,
                                             Func => Set_Background'Access,
                                             Data => null);
      end if;

      Gtk.Ctree.Set_Line_Style (Ctree, Line_Style => New_Line_Style);
   end Toggle_Line_Style;

   ---------------------------
   -- Toggle_Expander_Style --
   ---------------------------

   procedure Toggle_Expander_Style
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      if not Gtk.Widget.Mapped_Is_Set (Widget) then
         return;
      end if;
      Gtk.Ctree.Set_Expander_Style
        (Ctree,
         Expander_Style => Gtk_Ctree_Expander_Style'Val
           (3 - Gtk.Radio_Menu_Item.Selected_Button (Clist_Omenu_Group2)));
   end Toggle_Expander_Style;

   --------------------
   -- Toggle_Justify --
   --------------------

   procedure Toggle_Justify
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      if not Gtk.Widget.Mapped_Is_Set (Widget) then
         return;
      end if;
      Gtk.Ctree.Set_Column_Justification
        (Ctree,
         Column => Gtk.Ctree.Get_Tree_Column (Ctree),
         Justification => Gtk_Justification'Val
           (1 - Gtk.Radio_Menu_Item.Selected_Button (Clist_Omenu_Group3)));
   end Toggle_Justify;

   ---------------------
   -- Toggle_Sel_Mode --
   ---------------------

   procedure Toggle_Sel_Mode
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      if not Gtk.Widget.Mapped_Is_Set (Widget) then
         return;
      end if;
      Gtk.Ctree.Set_Selection_Mode
        (Ctree,
         Mode => Gtk_Selection_Mode'Val
           (3 - Gtk.Radio_Menu_Item.Selected_Button (Clist_Omenu_Group4)));
      After_Press (Ctree);
   end Toggle_Sel_Mode;

   -----------------
   -- Gint_Random --
   -----------------

   package Gint_Random is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Gint);

   ---------------------
   -- Build_Recursive --
   ---------------------

   procedure Build_Recursive (Ctree     : Gtk.Ctree.Gtk_Ctree;
                              Cur_Depth :     Gint;
                              Depth     :     Gint;
                              Num_Books :     Gint;
                              Num_Pages :     Gint;
                              Parent    :     Gtk.Ctree.Gtk_Ctree_Node) is

      Text : Chars_Ptr_Array (Title'Range);
      Gen : Gint_Random.Generator;
      Sibling : Gtk.Ctree.Gtk_Ctree_Node;
      Style : Gtk.Style.Gtk_Style;
      Tmp_Color : Gdk.Color.Gdk_Color;

   begin

      Gint_Random.Reset (Gen);

      for I in reverse Num_Books + 1 .. Num_Pages + Num_Books loop
         Pages := Pages + 1;
         Text := "Page" & Gint'Image (Gint_Random.Random (Gen) mod 100) +
           ("Item" & Gint'Image (Cur_Depth) & "-" & Common.Image_Of (I));
         Sibling := Gtk.Ctree.Insert_Node
           (Ctree,
            Parent => Parent,
            Sibling => Sibling,
            Text => Text,
            Spacing => 5,
            Pixmap_Closed => Pixmap3,
            Mask_Closed => Mask3,
            Pixmap_Opened => Gdk.Pixmap.Null_Pixmap,
            Mask_Opened => Gdk.Bitmap.Null_Bitmap,
            Is_Leaf => True,
            Expanded => False);

         if Parent /= null and then
           Gtk.Ctree.Get_Line_Style (Ctree) = Ctree_Lines_Tabbed then
            Gtk.Ctree.Node_Set_Row_Style
              (Ctree,
               Node => Sibling,
               Style => Gtk.Ctree.Node_Get_Row_Style (Ctree, Node => Parent));
         end if;

         Free (Text);
      end loop;

      if Cur_Depth = Depth then
         return;
      end if;

      for I in reverse 1 .. Num_Books loop
         Books := Books + 1;
         Text := "Book" & Gint'Image (Gint_Random.Random (Gen) mod 100) +
           ("Item" & Gint'Image (Cur_Depth) & "-" & Common.Image_Of (I));
         Sibling := Gtk.Ctree.Insert_Node
           (Ctree,
            Parent => Parent,
            Sibling => Sibling,
            Text => Text,
            Spacing => 5,
            Pixmap_Closed => Pixmap1,
            Mask_Closed => Mask1,
            Pixmap_Opened => Pixmap2,
            Mask_Opened => Mask2,
            Is_Leaf => False,
            Expanded => False);

         Gtk.Style.Gtk_New (Style);

         case Cur_Depth mod 3 is
            when 0 =>
               Gdk.Color.Set_Rgb
                 (Tmp_Color,
                  Red => 10_000 * Guint16 (Cur_Depth mod 6),
                  Green => 0,
                  Blue => 65_535 - Guint16 ((I * 10_000) mod 65_535));
               Gtk.Style.Set_Base (Style,
                                   State_Type => State_Normal,
                                   Color => Tmp_Color);
            when 1 =>
               Gdk.Color.Set_Rgb
                 (Tmp_Color,
                  Red => 10_000 * Guint16 (Cur_Depth mod 6),
                  Green => 65_535 - Guint16 ((I * 10_000) mod 65_535),
                  Blue => 0);
               Gtk.Style.Set_Base (Style,
                                   State_Type => State_Normal,
                                   Color => Tmp_Color);
            when others =>
               Gdk.Color.Set_Rgb
                 (Tmp_Color,
                  Red => 65_535 - Guint16 ((I * 10_000) mod 65_535),
                  Green => 0,
                  Blue => 10_000 * Guint16 (Cur_Depth mod 6));
               Gtk.Style.Set_Base (Style,
                                   State_Type => State_Normal,
                                   Color => Tmp_Color);
         end case;

         Ctree_Style_Row_Data.Node_Set_Row_Data
           (Ctree, Node => Sibling, Data => Style);

         if Gtk.Ctree.Get_Line_Style (Ctree) = Ctree_Lines_Tabbed then
            Gtk.Ctree.Node_Set_Row_Style
              (Ctree, Node => Sibling, Style => Style);
         end if;

         Free (Text);

         Build_Recursive (Ctree, Cur_Depth + 1, Depth, Num_Books,
                          Num_Pages, Sibling);
      end loop;
   end Build_Recursive;

   ------------------
   -- Rebuild_Tree --
   ------------------

   procedure Rebuild_Tree (Ctree : Gtk.Ctree.Gtk_Ctree) is
      B, D, P, N : Gint;
      Text : Chars_Ptr_Array (Title'Range);
      Parent : Gtk.Ctree.Gtk_Ctree_Node;
      Style : Gtk.Style.Gtk_Style;
      Tmp_Color : Gdk.Color.Gdk_Color;

   begin
      Text := "Root" + "";

      D := Gtk.Spin_Button.Get_Value_As_Int (Spin1);
      B := Gtk.Spin_Button.Get_Value_As_Int (Spin2);
      P := Gtk.Spin_Button.Get_Value_As_Int (Spin3);

      if B = 1 then
         N := D * (P + 1);
      else
         N := ((B ** Integer (D) - 1) / (B - 1)) * (P + 1);
      end if;

      if N > 100_000 then
         Ada.Text_IO.Put_Line (Gint'Image (N) & " total items? Try less");
         return;
      end if;

      Gtk.Ctree.Freeze (Ctree);
      Gtk.Ctree.Clear (Ctree);

      Books := 1;
      Pages := 0;

      Parent := Gtk.Ctree.Insert_Node (Ctree,
                                       Parent => Gtk.Ctree.Null_Ctree_Node,
                                       Sibling => Gtk.Ctree.Null_Ctree_Node,
                                       Text => Text,
                                       Spacing => 5,
                                       Pixmap_Closed => Pixmap1,
                                       Mask_Closed => Mask1,
                                       Pixmap_Opened => Pixmap2,
                                       Mask_Opened => Mask2,
                                       Is_Leaf => False,
                                       Expanded => True);
      Free (Text);

      Gtk.Style.Gtk_New (Style);
      Gdk.Color.Set_Rgb (Tmp_Color, Red => 0, Green => 45_000, Blue => 55_000);
      Gtk.Style.Set_Base (Style,
                          State_Type => State_Normal,
                          Color => Tmp_Color);
      Ctree_Style_Row_Data.Node_Set_Row_Data
        (Ctree, Node => Parent, Data => Style);

      if Gtk.Ctree.Get_Line_Style (Ctree) = Ctree_Lines_Tabbed then
         Gtk.Ctree.Node_Set_Row_Style (Ctree, Node => Parent, Style => Style);
      end if;

      Build_Recursive (Ctree, Cur_Depth => 1, Depth => D,
                       Num_Books => B, Num_Pages => P, Parent => Parent);

      Gtk.Ctree.Thaw (Ctree);
      After_Press (Ctree);
   end Rebuild_Tree;

   ------------------
   -- Rebuild_Tree --
   ------------------

   procedure Rebuild_Tree (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                           Ctree  :        Gtk.Ctree.Gtk_Ctree)
   is
      pragma Warnings (Off, Widget);
   begin
      Rebuild_Tree (Ctree);
   end Rebuild_Tree;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is

      Tooltips : Gtk.Tooltips.Gtk_Tooltips;
      Hbox, Bbox : Gtk.Box.Gtk_Box;
      Hbox2      : Gtk.Box.Gtk_Box;
      Vbox, Mbox : Gtk.Box.Gtk_Box;
      Label : Gtk.Label.Gtk_Label;
      Adj : Gtk.Adjustment.Gtk_Adjustment;
      Button : Gtk.Button.Gtk_Button;
      Scrolled_Win : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Spinner : Gtk.Spin_Button.Gtk_Spin_Button;
      Check : Gtk.Check_Button.Gtk_Check_Button;
      Omenu1, Omenu2, Omenu3, Omenu4 : Gtk.Option_Menu.Gtk_Option_Menu;
      Transparent : Gdk.Color.Gdk_Color;
      Frame2 : Gtk.Frame.Gtk_Frame;

      pragma Warnings (Off, Transparent);
   begin
      Set_Label (Frame, "Ctree");
      Gtk.Tooltips.Gtk_New (Tooltips);

      --  gtk_object_ref (tooltips)
      --  gtk_oject_sink (tooltips)
      --  gtk_object_data_full (window, [...] )

      Gtk.Box.Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Gtk.Frame.Add (Frame, Widget => Vbox);

      Gtk.Box.Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 5);
      Gtk.Box.Set_Border_Width (Hbox, Border_Width => 5);
      Gtk.Box.Pack_Start (In_Box => Vbox, Child => Hbox, Expand => False);

      Gtk.Label.Gtk_New (Label, Str => "Depth :");
      Gtk.Box.Pack_Start (In_Box => Hbox, Child => Label, Expand => False);

      Gtk.Adjustment.Gtk_New (Adj,
                              Value => 4.0,
                              Lower => 1.0,
                              Upper => 10.0,
                              Step_Increment => 1.0,
                              Page_Increment => 5.0,
                              Page_Size => 0.0);
      Gtk.Spin_Button.Gtk_New (Spin1,
                               Adjustment => Adj,
                               Climb_Rate => 0.0,
                               The_Digits => 0);
      Gtk.Box.Pack_Start (In_Box => Hbox,
                          Child => Spin1,
                          Expand => False,
                          Padding => 5);

      Gtk.Label.Gtk_New (Label, Str => "Books :");
      Gtk.Box.Pack_Start (In_Box => Hbox, Child => Label, Expand => False);

      Gtk.Adjustment.Gtk_New (Adj,
                              Value => 3.0,
                              Lower => 1.0,
                              Upper => 20.0,
                              Step_Increment => 1.0,
                              Page_Increment => 5.0,
                              Page_Size => 0.0);
      Gtk.Spin_Button.Gtk_New (Spin2,
                               Adjustment => Adj,
                               Climb_Rate => 0.0,
                               The_Digits => 0);
      Gtk.Box.Pack_Start (In_Box => Hbox,
                          Child => Spin2,
                          Expand => False,
                          Padding => 5);

      Gtk.Label.Gtk_New (Label, Str => "Pages :");
      Gtk.Box.Pack_Start (Hbox, Child => Label, Expand => False);

      Gtk.Adjustment.Gtk_New (Adj,
                              Value => 5.0,
                              Lower => 1.0,
                              Upper => 20.0,
                              Step_Increment => 1.0,
                              Page_Increment => 5.0,
                              Page_Size => 0.0);
      Gtk.Spin_Button.Gtk_New (Spin3,
                               Adjustment => Adj,
                               Climb_Rate => 0.0,
                               The_Digits => 0);
      Gtk.Box.Pack_Start (In_Box => Hbox,
                          Child => Spin3,
                          Expand => False,
                          Padding => 5);

      Gtk.Button.Gtk_New (Button, "Rebuild Tree");
      Gtk.Box.Pack_Start (Hbox, Child => Button);

      Gtk.Scrolled_Window.Gtk_New (Scrolled_Win);
      Gtk.Scrolled_Window.Set_Border_Width (Scrolled_Win, Border_Width => 5);
      Gtk.Scrolled_Window.Set_Policy (Scrolled_Win,
                                      H_Scrollbar_Policy => Policy_Automatic,
                                      V_Scrollbar_Policy => Policy_Always);
      Gtk.Box.Pack_Start (Vbox, Child => Scrolled_Win);

      Gtk.Ctree.Gtk_New (Ctree, Titles => Title, Tree_Column => 0);
      Gtk.Scrolled_Window.Add (Scrolled_Win, Ctree);

      Gtk.Ctree.Set_Column_Auto_Resize (Ctree, Column => 0,
                                        Auto_Resize => True);
      Gtk.Ctree.Set_Column_Width (Ctree, Column => 1, Width => 200);
      Gtk.Ctree.Set_Selection_Mode (Ctree, Mode => Selection_Multiple);
      Gtk.Ctree.Set_Line_Style (Ctree, Line_Style => Ctree_Lines_Dotted);

      Ctree_User_Cb.Connect
        (Button,
         Name => "clicked",
         Marsh => Ctree_User_Cb.To_Marshaller (Rebuild_Tree'Access),
         User_Data => Ctree);

      Gint_Ctree_Cb.Connect
        (Ctree,
         Name => "button_press_event",
         Marsh => Gint_Ctree_Cb.To_Marshaller (After_Press_Return'Access),
         After => True);

      Gint_Ctree_Cb.Connect
        (Ctree,
         Name => "button_release_event",
         Marsh => Gint_Ctree_Cb.To_Marshaller (After_Press_Return'Access),
         After => True);

      --       gtk_signal_connect_after (GTK_OBJECT (ctree), "tree_move",
      --                                 GTK_SIGNAL_FUNC (after_move), NULL);

      Ctree_Cb.Connect
        (Ctree,
         Name => "end_selection",
         Marsh => Ctree_Cb.To_Marshaller (After_Press'Access),
         After => True);

      Ctree_Cb.Connect
        (Ctree,
         Name => "toggle_focus_row",
         Marsh => Ctree_Cb.To_Marshaller (After_Press'Access),
         After => True);

      Ctree_Cb.Connect
        (Ctree,
         Name => "select_all",
         Marsh => Ctree_Cb.To_Marshaller (After_Press'Access),
         After => True);

      Ctree_Cb.Connect
        (Ctree,
         Name => "unselect_all",
         Marsh => Ctree_Cb.To_Marshaller (After_Press'Access),
         After => True);

      Ctree_Cb.Connect
        (Ctree,
         Name => "scroll_vertical",
         Marsh => Ctree_Cb.To_Marshaller (After_Press'Access),
         After => True);

      Gtk.Box.Gtk_New_Hbox (Bbox, Homogeneous => False, Spacing => 5);
      Gtk.Box.Set_Border_Width (Bbox, Border_Width => 5);
      Gtk.Box.Pack_Start (Vbox, Child => Bbox, Expand => False);

      Gtk.Box.Gtk_New_Vbox (Mbox, Homogeneous => True, Spacing => 5);
      Gtk.Box.Pack_Start (Bbox, Child => Mbox, Expand => False);

      Gtk.Label.Gtk_New (Label, Str => "Row Height :");
      Gtk.Box.Pack_Start (Mbox, Child => Label,
                          Expand => False, Fill => False);

      Gtk.Label.Gtk_New (Label, Str => "Indent :");
      Gtk.Box.Pack_Start (Mbox, Child => Label,
                          Expand => False, Fill => False);

      Gtk.Label.Gtk_New (Label, Str => "Spacing :");
      Gtk.Box.Pack_Start (Mbox, Child => Label,
                          Expand => False, Fill => False);

      Gtk.Box.Gtk_New_Vbox (Mbox, Homogeneous => True, Spacing => 5);
      Gtk.Box.Pack_Start (Bbox, Child => Mbox, Expand => False);

      Gtk.Box.Gtk_New_Vbox (Mbox, Homogeneous => True, Spacing => 5);
      Gtk.Box.Pack_Start (Bbox, Child => Mbox, Expand => False);

      Gtk.Adjustment.Gtk_New (Adj,
                              Value => 20.0,
                              Lower => 12.0,
                              Upper => 100.0,
                              Step_Increment => 1.0,
                              Page_Increment => 10.0,
                              Page_Size => 0.0);
      Gtk.Spin_Button.Gtk_New (Spinner,
                               Adjustment => Adj,
                               Climb_Rate => 0.0,
                               The_Digits => 0);
      Gtk.Box.Pack_Start (Mbox, Child => Spinner,
                          Expand => False, Fill => False, Padding => 5);
      Gtk.Tooltips.Set_Tip (Tooltips, Widget => Spinner,
                            Tip_Text => "Row height of list items");
      Adjustment_Cb.Connect
        (Adj,
         Name  => "value_changed",
         Marsh => Adjustment_Cb.To_Marshaller (Change_Row_Height'Access),
         User_Data => Ctree);
      Gtk.Ctree.Set_Row_Height
        (Ctree, Height => Gint (Gtk.Adjustment.Get_Value (Adj)));

      Gtk.Adjustment.Gtk_New (Adj,
                              Value => 20.0,
                              Lower => 0.0,
                              Upper => 60.0,
                              Step_Increment => 1.0,
                              Page_Increment => 10.0,
                              Page_Size => 0.0);
      Gtk.Spin_Button.Gtk_New (Spinner,
                               Adjustment => Adj,
                               Climb_Rate => 0.0,
                               The_Digits => 0);
      Gtk.Box.Pack_Start (Mbox, Child => Spinner,
                          Expand => False, Fill => False, Padding => 5);
      Gtk.Tooltips.Set_Tip (Tooltips, Widget => Spinner,
                            Tip_Text => "Tree Indentation.");
      Adjustment_Cb.Connect
        (Adj,
         Name => "value_changed",
         Marsh => Adjustment_Cb.To_Marshaller (Change_Indent'Access),
         User_Data => Ctree);

      Gtk.Adjustment.Gtk_New (Adj,
                              Value => 5.0,
                              Lower => 0.0,
                              Upper => 60.0,
                              Step_Increment => 1.0,
                              Page_Increment => 10.0,
                              Page_Size => 0.0);
      Gtk.Spin_Button.Gtk_New (Spinner,
                               Adjustment => Adj,
                               Climb_Rate => 0.0,
                               The_Digits => 0);
      Gtk.Box.Pack_Start (Mbox, Child => Spinner,
                          Expand => False, Fill => False, Padding => 5);
      Gtk.Tooltips.Set_Tip (Tooltips, Widget => Spinner,
                            Tip_Text => "Tree Spacing.");
      Adjustment_Cb.Connect
        (Adj,
         Name => "value_changed",
         Marsh => Adjustment_Cb.To_Marshaller (Change_Spacing'Access),
         User_Data => Ctree);

      Gtk.Box.Gtk_New_Vbox (Mbox, Homogeneous => True, Spacing => 5);
      Gtk.Box.Pack_Start (Bbox, Child => Mbox, Expand => False);

      Gtk.Box.Gtk_New_Hbox (Hbox, Homogeneous =>  False, Spacing => 5);
      Gtk.Box.Pack_Start (Mbox, Child => Hbox,
                          Expand => False,  Fill => False);

      Gtk.Button.Gtk_New (Button, Label => "Expand All");
      Gtk.Box.Pack_Start (Hbox, Child => Button);
      Ctree_Cb.Object_Connect
        (Button,
         Name  => "clicked",
         Marsh => Ctree_Cb.To_Marshaller (Expand_All'Access),
         Slot_Object => Ctree);

      Gtk.Button.Gtk_New (Button, Label => "Collapse All");
      Gtk.Box.Pack_Start (Hbox, Child => Button);
      Ctree_Cb.Object_Connect
        (Button,
         Name => "clicked",
         Marsh => Ctree_Cb.To_Marshaller (Collapse_All'Access),
         Slot_Object => Ctree);

      Gtk.Button.Gtk_New (Button, Label => "Change Style");
      Gtk.Box.Pack_Start (Hbox, Child => Button);
      Ctree_Cb.Object_Connect
        (Button,
         Name => "clicked",
         Marsh => Ctree_Cb.To_Marshaller (Change_Style'Access),
         Slot_Object => Ctree);

      --       Gtk.Button.Gtk_New (Button, Label => "Export Tree");
      --       Gtk.Box.Pack_Start (Hbox, Child => Button);
      --       gtk_signal_connect (GTK_OBJECT (button), "clicked",
      --                           GTK_SIGNAL_FUNC (export_ctree), ctree);

      Gtk.Box.Gtk_New_Hbox (Hbox, Homogeneous =>  False, Spacing => 5);
      Gtk.Box.Pack_Start (Mbox, Child => Hbox,
                          Expand => False,  Fill => False);

      Gtk.Button.Gtk_New (Button, Label => "Select All");
      Gtk.Box.Pack_Start (Hbox, Child => Button);
      Ctree_Cb.Object_Connect
        (Button,
         Name => "clicked",
         Marsh => Ctree_Cb.To_Marshaller (Select_All'Access),
         Slot_Object => Ctree);

      Gtk.Button.Gtk_New (Button, Label => "Unselect All");
      Gtk.Box.Pack_Start (Hbox, Child => Button);
      Ctree_Cb.Object_Connect
        (Button,
         Name => "clicked",
         Marsh => Ctree_Cb.To_Marshaller (Unselect_All'Access),
         Slot_Object => Ctree);

      Gtk.Button.Gtk_New (Button, Label => "Remove Selection");
      Gtk.Box.Pack_Start (Hbox, Child => Button);
      Ctree_Cb.Object_Connect
        (Button,
         Name => "clicked",
         Marsh => Ctree_Cb.To_Marshaller (Remove_Selection'Access),
         Slot_Object => Ctree);

      Gtk.Check_Button.Gtk_New (Check, Label => "Reorderable");
      Gtk.Box.Pack_Start (Hbox, Child => Check, Expand => False);
      Gtk.Tooltips.Set_Tip
        (Tooltips,
         Widget => Check,
         Tip_Text => "Tree items can be reordered by dragging.");
      --       gtk_signal_connect (GTK_OBJECT (check), "clicked",
      --  GTK_SIGNAL_FUNC (toggle_reorderable), ctree);
      Gtk.Check_Button.Set_Active (Check, Is_Active => True);

      Gtk.Box.Gtk_New_Hbox (Hbox, Homogeneous => True, Spacing => 5);
      Gtk.Box.Pack_Start (Mbox, Child => Hbox,
                          Expand => False, Fill => False);

      Clist_Omenu_Group1 := Gtk.Widget.Widget_SList.Null_List;
      --  FIXME : I wonder if there is not a memory leak here...
      Common.Build_Option_Menu (Omenu1,
                                Gr => Clist_Omenu_Group1,
                                Items => Items1,
                                History => 2,
                                Cb => Toggle_Line_Style'Access);
      Gtk.Box.Pack_Start (Hbox, Child => Omenu1, Expand => False);
      Gtk.Tooltips.Set_Tip (Tooltips,
                            Widget => Omenu1,
                            Tip_Text => "The tree's line style.");

      Clist_Omenu_Group2 := Gtk.Widget.Widget_SList.Null_List;
      --  FIXME : I wonder if there is not a memory leak here...
      Common.Build_Option_Menu (Omenu2,
                                Gr => Clist_Omenu_Group2,
                                Items => Items2,
                                History => 1,
                                Cb => Toggle_Expander_Style'Access);
      Gtk.Box.Pack_Start (Hbox, Child => Omenu2, Expand => False);
      Gtk.Tooltips.Set_Tip (Tooltips,
                            Widget => Omenu2,
                            Tip_Text => "The tree's expander style.");

      Clist_Omenu_Group3 := Gtk.Widget.Widget_SList.Null_List;
      --  FIXME : I wonder if there is not a memory leak here...
      Common.Build_Option_Menu (Omenu3,
                                Gr => Clist_Omenu_Group3,
                                Items => Items3,
                                History => 0,
                                Cb => Toggle_Justify'Access);
      Gtk.Box.Pack_Start (Hbox, Child => Omenu3, Expand => False);
      Gtk.Tooltips.Set_Tip (Tooltips,
                            Widget => Omenu3,
                            Tip_Text => "The tree's justification.");

      Clist_Omenu_Group4 := Gtk.Widget.Widget_SList.Null_List;
      --  FIXME : I wonder if there is not a memory leak here...
      Common.Build_Option_Menu (Omenu4,
                                Gr => Clist_Omenu_Group4,
                                Items => Items4,
                                History => 3,
                                Cb => Toggle_Sel_Mode'Access);
      Gtk.Box.Pack_Start (Hbox, Child => Omenu4, Expand => False);
      Gtk.Tooltips.Set_Tip (Tooltips,
                            Widget => Omenu4,
                            Tip_Text => "The list's selection mode.");

      Gtk.Frame.Realize (Frame);

      Gdk.Pixmap.Create_From_Xpm_D (Pixmap1,
                                    Window => Gtk.Frame.Get_Window (Frame),
                                    Mask => Mask1,
                                    Transparent => Transparent,
                                    Data => Common.Book_Closed_Xpm);
      Gdk.Pixmap.Create_From_Xpm_D (Pixmap2,
                                    Window => Gtk.Frame.Get_Window (Frame),
                                    Mask => Mask2,
                                    Transparent => Transparent,
                                    Data => Common.Book_Open_Xpm);
      Gdk.Pixmap.Create_From_Xpm_D (Pixmap3,
                                    Window => Gtk.Frame.Get_Window (Frame),
                                    Mask => Mask3,
                                    Transparent => Transparent,
                                    Data => Common.Mini_Page_Xpm);

      Gtk.Ctree.Set_USize (Ctree, Width => 0, Height => 300);

      Gtk.Frame.Gtk_New (Frame2);
      Gtk.Frame.Set_Border_Width (Frame2, Border_Width => 0);
      Gtk.Frame.Set_Shadow_Type (Frame2, The_Type => Shadow_Out);
      Gtk.Box.Pack_Start (Vbox, Child => Frame2, Expand => False);

      Gtk.Box.Gtk_New_Hbox (Hbox, Homogeneous => True, Spacing => 2);
      Gtk.Box.Set_Border_Width (Hbox, Border_Width => 2);
      Gtk.Frame.Add (Frame2, Widget => Hbox);

      Gtk.Frame.Gtk_New (Frame2);
      Gtk.Frame.Set_Shadow_Type (Frame2, The_Type => Shadow_In);
      Gtk.Box.Pack_Start (Hbox, Child => Frame2, Expand => False);

      Gtk.Box.Gtk_New_Hbox (Hbox2, Homogeneous => False, Spacing => 0);
      Gtk.Box.Set_Border_Width (Hbox2, Border_Width => 2);
      Gtk.Frame.Add (Frame2, Widget => Hbox2);

      Gtk.Label.Gtk_New (Label, Str => "Books :");
      Gtk.Box.Pack_Start (Hbox2, Child => Label, Expand => False);

      Gtk.Label.Gtk_New (Book_Label, Str => Common.Image_Of (Books));
      Gtk.Box.Pack_End (Hbox2, Child => Book_Label,
                        Expand => False, Padding => 5);

      Gtk.Frame.Gtk_New (Frame2);
      Gtk.Frame.Set_Shadow_Type (Frame2, The_Type => Shadow_In);
      Gtk.Box.Pack_Start (Hbox, Child => Frame2, Expand => False);

      Gtk.Box.Gtk_New_Hbox (Hbox2, Homogeneous => False, Spacing => 0);
      Gtk.Box.Set_Border_Width (Hbox2, Border_Width => 2);
      Gtk.Frame.Add (Frame2, Widget => Hbox2);

      Gtk.Label.Gtk_New (Label, Str => "Pages :");
      Gtk.Box.Pack_Start (Hbox2, Child => Label, Expand => False);

      Gtk.Label.Gtk_New (Page_Label, Str => Common.Image_Of (Pages));
      Gtk.Box.Pack_End (Hbox2, Child => Page_Label,
                        Expand => False, Padding => 5);

      Gtk.Frame.Gtk_New (Frame2);
      Gtk.Frame.Set_Shadow_Type (Frame2, The_Type => Shadow_In);
      Gtk.Box.Pack_Start (Hbox, Child => Frame2, Expand => False);

      Gtk.Box.Gtk_New_Hbox (Hbox2, Homogeneous => False, Spacing => 0);
      Gtk.Box.Set_Border_Width (Hbox2, Border_Width => 2);
      Gtk.Frame.Add (Frame2, Widget => Hbox2);

      Gtk.Label.Gtk_New (Label, Str => "Selected :");
      Gtk.Box.Pack_Start (Hbox2, Child => Label, Expand => False);

      Gtk.Label.Gtk_New
        (Sel_Label,
         Str => Common.Image_Of (Gint (Gint_List.Length
                                       (Gtk.Ctree.Get_Selection (Ctree)))));
      Gtk.Box.Pack_End (Hbox2, Child => Sel_Label,
                        Expand => False, Padding => 5);

      Gtk.Frame.Gtk_New (Frame2);
      Gtk.Frame.Set_Shadow_Type (Frame2, The_Type => Shadow_In);
      Gtk.Box.Pack_Start (Hbox, Child => Frame2, Expand => False);

      Gtk.Box.Gtk_New_Hbox (Hbox2, Homogeneous => False, Spacing => 0);
      Gtk.Box.Set_Border_Width (Hbox2, Border_Width => 2);
      Gtk.Frame.Add (Frame2, Widget => Hbox2);

      Gtk.Label.Gtk_New (Label, Str => "Visible :");
      Gtk.Box.Pack_Start (Hbox2, Child => Label, Expand => False);

      Gtk.Label.Gtk_New
        (Vis_Label,
         Str => Common.Image_Of (Gint (Gtk.Ctree.Row_List.Length
                                       (Gtk.Ctree.Get_Row_List (Ctree)))));
      Gtk.Box.Pack_End (Hbox2, Child => Vis_Label,
                        Expand => False, Padding => 5);

      Rebuild_Tree (Ctree => Ctree);

      Show_All (Frame);
   end Run;

end Create_Ctree;
