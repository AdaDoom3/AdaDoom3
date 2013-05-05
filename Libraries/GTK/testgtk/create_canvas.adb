-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                Copyright (C) 2000-2013, AdaCore                   --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Ada.Numerics.Discrete_Random;

with Glib;                use Glib;

with Cairo;               use Cairo;
with Cairo.Pattern;       use Cairo.Pattern;
with Cairo.Png;           use Cairo.Png;
with Cairo.Region;        use Cairo.Region;
with Cairo.Surface;       use Cairo.Surface;

with Pango.Cairo;         use Pango.Cairo;

with Gdk.Cairo;           use Gdk.Cairo;
with Gdk.Color;           use Gdk.Color;
with Gdk.Event;           use Gdk.Event;

with Gtk.Arrow;           use Gtk.Arrow;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Canvas;       use Gtkada.Canvas;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtk.Label;           use Gtk.Label;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Pango.Layout;        use Pango.Layout;
with Gtk.Style;           use Gtk.Style;

package body Create_Canvas is

   Max_Size : constant := 400;
   --  Size of the canvas;

   Item_Width  : constant Gint := 50;
   Item_Height : constant Gint := 40;

   ----------------------------------------------------------------
   --  Redefine our own item type, since we want to provide our own
   --  graphics.
   ----------------------------------------------------------------

   type Display_Item_Record is new Canvas_Item_Record with record
      Canvas : Interactive_Canvas;
      Color  : Gdk.Color.Gdk_Color;
      Title  : Gdk.Color.Gdk_Color;
      W, H   : Gint;
      Num    : Positive;
   end record;
   type Display_Item is access all Display_Item_Record'Class;

   procedure Initialize
     (Item   : access Display_Item_Record'Class;
      Canvas : access Interactive_Canvas_Record'Class);
   --  Initialize Item with a random size and color.
   --  Canvas must have been realized

   procedure Draw
     (Item   : access Display_Item_Record;
      Cr     : Cairo_Context);
   --  Draw the item to the double-buffer

   -----------------------------------------------------------
   -- A new non-rectangular item, with a hole in the middle --
   -----------------------------------------------------------

   type Hole_Item_Record is new Display_Item_Record with null record;

   procedure Draw
     (Item   : access Hole_Item_Record;
      Cr     : Cairo_Context);
   function Point_In_Item
     (Item   : access Hole_Item_Record;
      X, Y   : Glib.Gint) return Boolean;
   --  Override the inherited subprograms

   ----------------------
   -- A resizable item --
   ----------------------

   type Resizable_Item_Record is new Display_Item_Record with null record;
   type Resizable_Item is access all Resizable_Item_Record'Class;

   procedure Draw
     (Item   : access Resizable_Item_Record;
      Cr     : Cairo.Cairo_Context);
   function On_Button_Click
     (Item   : access Resizable_Item_Record;
      Event  : Gdk.Event.Gdk_Event_Button) return Boolean;
   --  Override the inherited subprograms

   ----------------------------------------------------
   -- Our own canvas, with optional background image --
   ----------------------------------------------------

   type Image_Canvas_Record is new Interactive_Canvas_Record with record
      Background : Cairo_Pattern := Null_Pattern;
      Draw_Grid  : Boolean := True;
   end record;
   type Image_Canvas is access all Image_Canvas_Record'Class;

   procedure Draw_Background
     (Canvas : access Image_Canvas_Record;
      Cr     : Cairo.Cairo_Context);
   --  Draw the background image

   procedure Draw_Grid
     (Canvas : access Image_Canvas_Record;
      Cr     : Cairo.Cairo_Context);
   --  Draw the background grid

   -----------------------------
   --  Misc. types and variables
   -----------------------------

   package Canvas_Cb is new Gtk.Handlers.Callback
     (Interactive_Canvas_Record);
   package Canvas_User_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Image_Canvas);

   procedure Add_Canvas_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Item1, Item2 : access Canvas_Item_Record'Class; Text : String := "");
   --  Add a link between Item1 and Item2

   Max_Colors : constant := 20;

   Zoom_Levels : constant array (Positive range <>) of Gdouble :=
     (0.1, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 2.0, 3.0, 4.0);

   Start_Spin, End_Spin, Num_Spin : Gtk_Spin_Button;
   Num_Items_Label, Num_Links_Label : Gtk_Label;
   Layout : Pango_Layout;

   type Color_Type is range 1 .. Max_Colors;
   package Color_Random is new Ada.Numerics.Discrete_Random (Color_Type);

   package Items_Random is new Ada.Numerics.Discrete_Random (Positive);
   use Items_Random;

   subtype Coordinate_Type is Gint range Default_Grid_Size + 1 .. Max_Size;
   package Coordinate_Random is new
     Ada.Numerics.Discrete_Random (Coordinate_Type);
   use Coordinate_Random;

   subtype Zoom_Type is Gint range 1 .. 2;
   package Zoom_Random is new Ada.Numerics.Discrete_Random (Zoom_Type);
   use Zoom_Random;

   type String_Access is access String;
   Color_Names : constant array (Color_Type) of String_Access :=
     (new String'("forest green"),
      new String'("red"),
      new String'("blue"),
      new String'("yellow"),
      new String'("peach puff"),
      new String'("azure"),
      new String'("seashell"),
      new String'("lavender"),
      new String'("grey"),
      new String'("turquoise"),
      new String'("khaki"),
      new String'("tan"),
      new String'("orange red"),
      new String'("MediumPurple"),
      new String'("ivory1"),
      new String'("DeepSkyBlue1"),
      new String'("burlywood1"),
      new String'("wheat1"),
      new String'("orange1"),
      new String'("pink"));

   Colors : array (Color_Type) of Gdk_Color;

   Items_List : array (1 .. 500) of Canvas_Item;
   Last_Item : Positive;
   Last_Link : Positive;

   Item_Gen : Items_Random.Generator;
   Gen : Coordinate_Random.Generator;
   Color_Gen : Color_Random.Generator;
   Zoom_Gen : Zoom_Random.Generator;
   --  Note: All the generators above are intentionally not reset, so that
   --  we can get the same events every time and thus can reproduce behaviors.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "An @bInteractive_Canvas@B is an interactive widgets, on which"
        & " you can put items that the user will be able to manipulate"
        & " dynamically with the mouse."
        & ASCII.LF
        & "As you can see in this demo, the items can be linked together, and"
        & " the items remain connected when they are moved."
        & ASCII.LF
        & "The canvas also support @bscrolling@B, if put in a "
        & " @bGtk_Scrolled_Window@B, as you can see if you move the items"
        & " outside of the visible part of the canvas."
        & "There is a small area on each side of the canvas. If you leave the"
        & " mouse in this area while dragging an item, the canvas will"
        & " keep scrolling until the mouse is moved outside of this area."
        & ASCII.LF
        & "The canvas provides @bzooming@B capabilities. Try clicking on the"
        & " two arrow buttons at the top of this demo."
        & ASCII.LF
        & "The canvas includes a simple @blayout scheme@B, that can be"
        & " overriden with more complex algorithms. Items are stored in a"
        & " graph structure, tha includes a number of useful algorithms for"
        & " layout: topological sort,..."
        & ASCII.LF
        & "@bNon-rectangular items@B can also be used, see for instance the"
        & " two items 2 and 4 in the default layout."
        & ASCII.LF
        & "You can also redefine your own @btype of links@B. By default, links"
        & " are either straight or arc links, that may optionaly have arrows"
        & " on either end.";
   end Help;

   ---------------------------
   -- Draw_To_Double_Buffer --
   ---------------------------

   procedure Draw
     (Item   : access Display_Item_Record;
      Cr     : Cairo_Context)
   is
   begin
      Gdk.Cairo.Set_Source_Color (Cr, Item.Color);
      Cairo.Rectangle
        (Cr, 0.5, 0.5, Gdouble (Item.W) - 1.0, Gdouble (Item.H) - 1.0);
      Cairo.Fill (Cr);

      Gdk.Cairo.Set_Source_Color (Cr, Item.Title);
      Rectangle
        (Cr, 0.5, 0.5, Gdouble (Item.W) - 1.0, Gdouble (Item.H) - 1.0);
      Cairo.Stroke (Cr);

      Set_Text (Layout, "Item" & Positive'Image (Display_Item (Item).Num));
      Cairo.Move_To (Cr, 10.0, 10.0);
      Pango.Cairo.Show_Layout (Cr, Layout);
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Item   : access Resizable_Item_Record;
      Cr     : Cairo_Context)
   is
      Rect : constant Cairo_Rectangle_Int := Get_Coord (Item);
      W    : constant Gdouble := Gdouble (Rect.Width);
      H    : constant Gdouble := Gdouble (Rect.Height);
      Arrow : constant Gdouble := 4.0;
   begin
      Cairo.Save (Cr);

      Set_Source_Color (Cr, Item.Color);
      Rectangle
        (Cr, 0.5, 0.5,
         W - 2.0 * Arrow - 1.0,
         H - 1.0);
      Cairo.Fill (Cr);

      Gdk.Cairo.Set_Source_Color (Cr, Item.Title);
      Rectangle
        (Cr, 0.5, 0.5,
         W - 2.0 * Arrow - 1.0,
         H - 1.0);
      Cairo.Stroke (Cr);

      Cairo.Restore (Cr);

      Cairo.Translate (Cr, W - Arrow - 0.5, 0.0);

      Cairo.Move_To (Cr, -Arrow / 2.0, Arrow + 0.5);
      Cairo.Line_To (Cr, 0.0, 0.5);
      Cairo.Line_To (Cr, Arrow / 2.0, Arrow + 0.5);
      Cairo.Stroke (Cr);

      Cairo.Move_To (Cr, -Arrow / 2.0, H - Arrow  - 0.5);
      Cairo.Line_To (Cr, 0.0, H - 1.0);
      Cairo.Line_To (Cr, Arrow / 2.0, H - Arrow - 0.5);
      Cairo.Stroke (Cr);

      Cairo.Move_To (Cr, 0.0, 0.5);
      Cairo.Line_To (Cr, 0.0, H - 0.5);
      Cairo.Stroke (Cr);

      Cairo.Move_To (Cr, -Arrow, 0.5);
      Cairo.Line_To (Cr, Arrow, 0.5);
      Cairo.Stroke (Cr);
      Cairo.Move_To (Cr, -Arrow, H - 0.5);
      Cairo.Line_To (Cr, Arrow, H - 0.5);
      Cairo.Stroke (Cr);
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Item   : access Hole_Item_Record;
      Cr     : Cairo_Context)
   is
      Item_Width : constant Gdouble := Gdouble (Get_Coord (Item).Width);
      Item_Height : constant Gdouble := Gdouble (Get_Coord (Item).Height);
      Item_Width_10 : constant Gdouble := Item_Width / 2.0 - 10.0;
      Item_Height_10 : constant Gdouble := Item_Height / 2.0 - 10.0;
   begin
      --  The trick to drawing non-rectangular items is to change the clip mask
      --  of the graphic context before calling the inherited subprogram.

      Cairo.Set_Fill_Rule (Cr, Cairo_Fill_Rule_Even_Odd);
      Cairo.Rectangle (Cr, 0.0, 0.0, Item_Width, Item_Height);
      Cairo.Rectangle (Cr, Item_Width_10, Item_Height_10, 20.0, 20.0);
      Clip (Cr);
      Cairo.Set_Fill_Rule (Cr, Cairo_Fill_Rule_Winding);

      Draw (Display_Item_Record (Item.all)'Access, Cr);

      Gdk.Cairo.Set_Source_Color (Cr, Item.Title);
      Cairo.Rectangle
        (Cr, Item_Width_10 - 0.5, Item_Height_10 - 0.5, 21.0, 21.0);
      Cairo.Stroke (Cr);
   end Draw;

   ---------------------
   -- On_Button_Click --
   ---------------------

   function On_Button_Click
     (Item   : access Resizable_Item_Record;
      Event  : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      Rect : constant Cairo_Rectangle_Int := Get_Coord (Item);
   begin
      if Get_Event_Type (Event) = Button_Press
        and then Get_Button (Event) = 1
      then
         if Gint (Get_Y (Event)) > Rect.Height - 4 then
            return True;
         end if;

      elsif Get_Event_Type (Event) = Motion_Notify then
         if Get_Y (Event) > 4.0 then
            Set_Screen_Size
              (Item, Get_Coord (Item).Width,
               Gint (Get_Y (Event)));
         end if;

         return True;
      end if;

      return False;
   end On_Button_Click;

   ---------------------
   -- Draw_Background --
   ---------------------

   procedure Draw_Background
     (Canvas : access Image_Canvas_Record;
      Cr     : Cairo.Cairo_Context)
   is
   begin
      if Canvas.Background /= Null_Pattern then
         Cairo.Save (Cr);
         Cairo.Set_Source (Cr, Canvas.Background);
         Cairo.Paint (Cr);
         Cairo.Restore (Cr);
      else
         Cairo.Save (Cr);
         Gdk.Cairo.Set_Source_Color
           (Cr, Get_Bg (Get_Style (Canvas), State_Normal));
         Cairo.Paint (Cr);
         Cairo.Restore (Cr);
      end if;
   end Draw_Background;

   ---------------
   -- Draw_Grid --
   ---------------

   procedure Draw_Grid
     (Canvas : access Image_Canvas_Record;
      Cr     : Cairo.Cairo_Context) is
   begin
      if Canvas.Draw_Grid then
         Draw_Grid (Interactive_Canvas_Record (Canvas.all)'Access, Cr);
      end if;
   end Draw_Grid;

   -------------------
   -- Point_In_Item --
   -------------------

   function Point_In_Item
     (Item   : access Hole_Item_Record;
      X, Y   : Glib.Gint) return Boolean
   is
      W : constant Gint := Get_Coord (Item).Width / 2;
      H : constant Gint := Get_Coord (Item).Height / 2;
      X2 : constant Gint := X - Get_Coord (Item).X;
      Y2 : constant Gint := Y - Get_Coord (Item).Y;
   begin
      if X2 >= W - 10
        and then X2 <= W + 10
        and then Y2 >= H - 10
        and then Y2 <= H + 10
      then
         return False;
      else
         return Point_In_Item (Display_Item_Record (Item.all)'Access, X, Y);
      end if;
   end Point_In_Item;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item   : access Display_Item_Record'Class;
      Canvas : access Interactive_Canvas_Record'Class)
   is
   begin
      Item.Canvas := Interactive_Canvas (Canvas);
      Item.Color := Colors (Color_Random.Random (Color_Gen));
      Item.Title := Get_Black (Get_Default_Style);
      Item.W := Item_Width * Random (Zoom_Gen);
      Item.H := Item_Height * Random (Zoom_Gen);
      Item.Num := Last_Item;
      if Last_Item <= Items_List'Last then
         Items_List (Item.Num) := Canvas_Item (Item);
      end if;
      Last_Item := Last_Item + 1;
      Set_Screen_Size (Item, Item.W, Item.H);
      Set_Text (Num_Items_Label, Positive'Image (Last_Item - 1) & " items");
   end Initialize;

   ---------------------
   -- Add_Random_Item --
   ---------------------

   procedure Add_Random_Item
     (Canvas : access Interactive_Canvas_Record'Class)
   is
      Item : constant Display_Item := new Display_Item_Record;
   begin
      Initialize (Item, Canvas);
      Put (Canvas, Item, Random (Gen), Random (Gen));
      Refresh_Canvas (Canvas);
      Show_Item (Canvas, Item);
   end Add_Random_Item;

   -----------
   -- Clear --
   -----------

   procedure Clear (Canvas : access Interactive_Canvas_Record'Class) is
      function Remove_Internal
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean is
      begin
         Remove (Canvas, Item);
         return True;
      end Remove_Internal;

   begin
      For_Each_Item (Canvas, Remove_Internal'Unrestricted_Access);
      Refresh_Canvas (Canvas);
      Last_Item := 1;
      Last_Link := 1;
      Set_Text (Num_Items_Label, Positive'Image (Last_Item - 1) & " items");
      Set_Text (Num_Links_Label, Positive'Image (Last_Link - 1) & " links");
   end Clear;

   ---------------
   -- Add_Items --
   ---------------

   procedure Add_Items
     (Canvas : access Interactive_Canvas_Record'Class)
   is
      Max : constant Positive := Last_Item
        + Positive (Get_Value_As_Int (Num_Spin)) - 1;
   begin
      for J in Last_Item .. Max loop
         Add_Random_Item (Canvas);
         Add_Canvas_Link
           (Canvas, Items_List (J), Items_List (Random (Item_Gen) mod J + 1));
         Add_Canvas_Link
           (Canvas, Items_List (J), Items_List (Random (Item_Gen) mod J + 1));
      end loop;
      Refresh_Canvas (Canvas);
   end Add_Items;

   ---------------------
   -- Add_Single_Item --
   ---------------------

   procedure Add_Single_Item
     (Canvas : access Interactive_Canvas_Record'Class;
      With_Link : Boolean)
   is
      Item : constant Display_Item := new Display_Item_Record;
      Num  : constant Positive := Positive (Get_Value_As_Int (Start_Spin));
   begin
      Initialize (Item, Canvas);

      if With_Link and then Num < Last_Item then
         Add_Canvas_Link (Canvas, Item, Item, "0");
         Add_Canvas_Link (Canvas, Items_List (Num), Item, "1");
         Add_Canvas_Link (Canvas, Items_List (Num), Item, "2");
      end if;

      Put (Canvas, Item);
      Refresh_Canvas (Canvas);
      Show_Item (Canvas, Item);
   end Add_Single_Item;

   -------------------------------
   -- Add_Single_Item_With_Link --
   -------------------------------

   procedure Add_Single_Item_With_Link
     (Canvas : access Interactive_Canvas_Record'Class) is
   begin
      Add_Single_Item (Canvas, True);
   end Add_Single_Item_With_Link;

   -----------------------------
   -- Add_Single_Item_No_Link --
   -----------------------------

   procedure Add_Single_Item_No_Link
     (Canvas : access Interactive_Canvas_Record'Class) is
   begin
      Add_Single_Item (Canvas, False);
   end Add_Single_Item_No_Link;

   ---------------------
   -- Add_Canvas_Link --
   ---------------------

   procedure Add_Canvas_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Item1, Item2 : access Canvas_Item_Record'Class; Text : String := "")
   is
      Link : constant Canvas_Link := new Canvas_Link_Record;
   begin
      Add_Link (Canvas, Link, Item1, Item2, Both_Arrow, Text);
      Last_Link := Last_Link + 1;
      Set_Text (Num_Links_Label, Positive'Image (Last_Link - 1) & " links");
   end Add_Canvas_Link;

   -----------------
   -- Remove_Link --
   -----------------

   procedure Remove_Link
     (Canvas : access Interactive_Canvas_Record'Class)
   is
      It1, It2 : Canvas_Item;

      function Remove_Internal
        (Canvas : access Interactive_Canvas_Record'Class;
         Link : access Canvas_Link_Record'Class) return Boolean
      is
         pragma Warnings (Off, Canvas);
      begin
         if (Canvas_Item (Get_Src (Link)) = It1
             and then Canvas_Item (Get_Dest (Link)) = It2)
           or else
           (Canvas_Item (Get_Src (Link)) = It2
            and then Canvas_Item (Get_Dest (Link)) = It1)
         then
            Remove_Link (Canvas, Link);
            return False;
         end if;
         return True;
      end Remove_Internal;

      Num1 : constant Positive := Positive (Get_Value_As_Int (Start_Spin));
      Num2 : constant Positive := Positive (Get_Value_As_Int (End_Spin));
   begin
      if Num1 < Last_Item and then Num2 < Last_Item then
         It1 := Items_List (Num1);
         It2 := Items_List (Num2);
         For_Each_Link (Canvas, Remove_Internal'Unrestricted_Access);
         Refresh_Canvas (Canvas);
      end if;
   end Remove_Link;

   -------------
   -- Zoom_In --
   -------------

   procedure Zoom_In
     (Canvas : access Interactive_Canvas_Record'Class) is
   begin
      for J in Zoom_Levels'First .. Zoom_Levels'Last - 1 loop
         if Zoom_Levels (J) = Get_Zoom (Canvas) then
            Zoom (Canvas, Zoom_Levels (J + 1), 0.2);
         end if;
      end loop;
   end Zoom_In;

   --------------
   -- Zoom_Out --
   --------------

   procedure Zoom_Out
     (Canvas : access Interactive_Canvas_Record'Class) is
   begin
      for J in Zoom_Levels'First + 1 .. Zoom_Levels'Last loop
         if Zoom_Levels (J) = Get_Zoom (Canvas) then
            Zoom (Canvas, Zoom_Levels (J - 1), 0.2);
         end if;
      end loop;
   end Zoom_Out;

   -------------------
   -- Initial_Setup --
   -------------------

   procedure Initial_Setup
     (Canvas : access Interactive_Canvas_Record'Class)
   is
      Item1, Item2, Item3, Item4    : Display_Item;
      Item5 : Resizable_Item;
      Link  : Canvas_Link;
   begin
      Color_Random.Reset (Color_Gen);

      Item1 := new Display_Item_Record;
      Initialize (Item1, Canvas);
      Put (Canvas, Item1, 10, 10);

      Item2 := new Hole_Item_Record;
      Initialize (Item2, Canvas);
      Put (Canvas, Item2, 70, 240);

      Item3 := new Display_Item_Record;
      Initialize (Item3, Canvas);
      Put (Canvas, Item3, 200, 10);

      Item4 := new Hole_Item_Record;
      Initialize (Item4, Canvas);
      Put (Canvas, Item4, 280, 170);

      Item5 := new Resizable_Item_Record;
      Initialize (Item5, Canvas);
      Set_Screen_Size (Item5, 30, 30);
      Put (Canvas, Item5, 200, 170);

      Add_Canvas_Link (Canvas, Item1, Item1, "Self 1->1");
      Add_Canvas_Link (Canvas, Item3, Item1, "From3->1");
      Add_Canvas_Link (Canvas, Item1, Item4, "From1->4(1)");
      Add_Canvas_Link (Canvas, Item1, Item4, "From1->4(2)");
      Add_Canvas_Link (Canvas, Item2, Item3, "From2->3");
      Add_Canvas_Link (Canvas, Item2, Item4, "From2->4");
      Add_Canvas_Link (Canvas, Item3, Item4, "From3->4(1)");

      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item3, Item4, Start_Arrow, "From3->4(2)");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item4, Item3, End_Arrow, "From3->4(3)");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item3, Item4, Both_Arrow, "From3->4(4)");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item4, Item3, Both_Arrow, "From3->4(5)");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item3, Item4, Both_Arrow, "From3->4(6)");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item2, Item2, No_Arrow, "Self 2->2 (1)");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item2, Item2, Start_Arrow, "Self 2->2 (2)");
      Last_Link := Last_Link + 7;

      Set_Text (Num_Links_Label, Positive'Image (Last_Link - 1) & " links");
   end Initial_Setup;

   ------------------
   -- Toggle_Align --
   ------------------

   procedure Toggle_Align
     (Align : access Gtk_Widget_Record'Class;
      Canvas : Image_Canvas) is
   begin
      Align_On_Grid (Canvas, Get_Active (Gtk_Check_Button (Align)));
   end Toggle_Align;

   ----------------------
   -- Toggle_Draw_Grid --
   ----------------------

   procedure Toggle_Draw_Grid
     (Align : access Gtk_Widget_Record'Class;
      Canvas : Image_Canvas) is
   begin
      Canvas.Draw_Grid := Get_Active (Gtk_Check_Button (Align));
      Refresh_Canvas (Canvas);
   end Toggle_Draw_Grid;

   -----------------------
   -- Toggle_Orthogonal --
   -----------------------

   procedure Toggle_Orthogonal
     (Align : access Gtk_Widget_Record'Class;
      Canvas : Image_Canvas) is
   begin
      Set_Orthogonal_Links (Canvas, Get_Active (Gtk_Check_Button (Align)));
      Refresh_Canvas (Canvas);
   end Toggle_Orthogonal;

   ------------------------
   -- Background_Changed --
   ------------------------

   procedure Background_Changed
     (Bg_Draw : access Gtk_Widget_Record'Class;
      Canvas  : Image_Canvas)
   is
      Surface : Cairo_Surface;
      Style   : Gtk_Style;

   begin
      if Get_Active (Gtk_Check_Button (Bg_Draw)) then
         Surface :=
           Cairo.Png.Create_From_Png ("background.png");
         Canvas.Background :=
           Cairo.Pattern.Create_For_Surface (Surface);
         Cairo.Pattern.Set_Extend
           (Canvas.Background, Cairo_Extend_Repeat);
         Destroy (Surface);

         Style := Get_Default_Style;
         Set_Bg
           (Style, State_Normal, Gdk_Color'(Get_Black (Style)));
         Set_Fg
           (Style, State_Normal, Gdk_Color'(Get_Light (Style, State_Normal)));
         Set_Style (Canvas, Style);

      else
         if Canvas.Background /= Null_Pattern then
            Destroy (Canvas.Background);
            Canvas.Background := Null_Pattern;
         end if;

         Style := Get_Default_Style;
         Set_Bg
           (Style, State_Normal, Gdk_Color'(Get_Light (Style, State_Normal)));
         Set_Fg
           (Style, State_Normal, Gdk_Color'(Get_Black (Style)));
         Set_Style (Canvas, Style);
      end if;
      Refresh_Canvas (Canvas);
   end Background_Changed;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Canvas   : Image_Canvas;
      Box, Bbox, Bbox2, Bbox3, Spin_Box, Small : Gtk_Box;
      Button   : Gtk_Button;
      Arrow    : Gtk_Arrow;
      Scrolled : Gtk_Scrolled_Window;
      Label    : Gtk_Label;
      Adj      : Gtk_Adjustment;
      F        : Gtk_Frame;
      Align    : Gtk_Check_Button;

   begin
      Last_Item := Items_List'First;
      Last_Link := 1;

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Gtk_New_Hbox (Bbox, Homogeneous => True);
      Pack_Start (Box, Bbox, Expand => False, Fill => False);

      Gtk_New_Hbox (Bbox2, Homogeneous => True);
      Pack_Start (Box, Bbox2, Expand => False, Fill => False);

      Gtk_New_Hbox (Bbox3, Homogeneous => True);
      Pack_Start (Box, Bbox3, Expand => False, Fill => False);

      Gtk_New_Hbox (Spin_Box, Homogeneous => True);
      Pack_Start (Box, Spin_Box, Expand => False, Fill => False);

      Gtk_New (F);
      Pack_Start (Box, F);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Add (F, Scrolled);

      Canvas := new Image_Canvas_Record;
      Initialize (Canvas);
      Add (Scrolled, Canvas);
      Align_On_Grid (Canvas, False);

      Gtk_New (Button);
      Gtk_New (Arrow, Arrow_Up, Shadow_Out);
      Add (Button, Arrow);
      Pack_Start (Bbox, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked", Canvas_Cb.To_Marshaller (Zoom_In'Access), Canvas);

      Gtk_New (Button);
      Gtk_New (Arrow, Arrow_Down, Shadow_Out);
      Add (Button, Arrow);
      Pack_Start (Bbox, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked", Canvas_Cb.To_Marshaller (Zoom_Out'Access), Canvas);

      Gtk_New (Button, "Random");
      Pack_Start (Bbox, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Add_Random_Item'Access), Canvas);

      Gtk_New (Button, "Add One");
      Pack_Start (Bbox, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Add_Single_Item_No_Link'Access), Canvas);

      Gtk_New (Button, "Clear");
      Pack_Start (Bbox, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Clear'Access), Canvas);

      Gtk_New (Button, "Remove Link Start->End");
      Pack_Start (Bbox2, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Remove_Link'Access), Canvas);

      Gtk_New (Button, "Add multiple Items");
      Pack_Start (Bbox2, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Add_Items'Access), Canvas);

      Gtk_New (Button, "Add with Link->Start");
      Pack_Start (Bbox2, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Add_Single_Item_With_Link'Access), Canvas);

      Gtk_New (Align, "Align on grid");
      Set_Active (Align, Get_Align_On_Grid (Canvas));
      Pack_Start (Bbox3, Align, Expand => False, Fill => True);
      Canvas_User_Cb.Connect
        (Align, "toggled",
         Canvas_User_Cb.To_Marshaller (Toggle_Align'Access), Canvas);

      Gtk_New (Align, "Draw grid");
      Set_Active (Align, Canvas.Draw_Grid);
      Pack_Start (Bbox3, Align, Expand => False, Fill => True);
      Canvas_User_Cb.Connect
        (Align, "toggled",
         Canvas_User_Cb.To_Marshaller (Toggle_Draw_Grid'Access), Canvas);

      Gtk_New (Align, "Orthogonal links");
      Set_Active (Align, Get_Orthogonal_Links (Canvas));
      Pack_Start (Bbox3, Align, Expand => False, Fill => True);
      Canvas_User_Cb.Connect
        (Align, "toggled",
         Canvas_User_Cb.To_Marshaller (Toggle_Orthogonal'Access), Canvas);

      Gtk_New (Align, "draw background");
      Set_Active (Align, Canvas.Background /= Null_Pattern);
      Pack_Start (Bbox3, Align, Expand => True, Fill => True);
      Canvas_User_Cb.Connect
        (Align, "toggled",
         Canvas_User_Cb.To_Marshaller (Background_Changed'Access), Canvas);
      Background_Changed (Align, Canvas);

      Gtk_New (Num_Items_Label, "0 items");
      Pack_Start (Spin_Box, Num_Items_Label, Expand => False, Fill => False);

      Gtk_New (Num_Links_Label, "0 links");
      Pack_Start (Spin_Box, Num_Links_Label, Expand => False, Fill => False);

      Gtk_New_Hbox (Small, Homogeneous => False);
      Gtk_New (Label, "Add:");
      Pack_Start (Small, Label, Expand => False, Fill => False);
      Gtk_New (Adj, 10.0, 1.0, 300.0, 1.0, 30.0);
      Gtk_New (Num_Spin, Adj, 0.5, 0);
      Pack_Start (Small, Num_Spin, Expand => False, Fill => False);
      Pack_Start (Spin_Box, Small, Expand => False, Fill => False);

      Gtk_New_Hbox (Small, Homogeneous => False);
      Gtk_New (Label, "Start:");
      Pack_Start (Small, Label, Expand => False, Fill => False);
      Gtk_New (Adj, 1.0, 1.0, 300.0, 1.0, 30.0);
      Gtk_New (Start_Spin, Adj, 0.5, 0);
      Pack_Start (Small, Start_Spin, Expand => False, Fill => False);
      Pack_Start (Spin_Box, Small, Expand => False, Fill => False);

      Gtk_New_Hbox (Small, Homogeneous => False);
      Gtk_New (Label, "End:");
      Pack_Start (Small, Label, Expand => False, Fill => False);
      Gtk_New (Adj, 2.0, 1.0, 300.0, 1.0, 30.0);
      Gtk_New (End_Spin, Adj, 0.5, 0);
      Pack_Start (Small, End_Spin, Expand => False, Fill => False);
      Pack_Start (Spin_Box, Small, Expand => False, Fill => False);

      Realize (Canvas);

      --  Initialize the colors

      for J in Color_Names'Range loop
         Colors (J) := Parse (Color_Names (J).all);
         Alloc (Gtk.Widget.Get_Default_Colormap, Colors (J));
      end loop;

      Layout := Create_Pango_Layout (Frame);

      Initial_Setup (Canvas);
      Show_All (Frame);
   end Run;
end Create_Canvas;
