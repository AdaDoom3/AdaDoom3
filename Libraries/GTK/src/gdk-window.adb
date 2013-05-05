-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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

with Glib.Object; use Glib.Object;
with Gtk.Widget;  use Gtk.Widget;

package body Gdk.Window is

   ---------------
   -- Copy_Area --
   ---------------

   procedure Copy_Area
     (Window        : Gdk_Window;
      Gc            : Gdk.Gdk_GC;
      X             : Gint;
      Y             : Gint;
      Source_Window : Gdk_Window;
      Source_X      : Gint;
      Source_Y      : Gint;
      Width         : Gint;
      Height        : Gint)
   is
      procedure Internal
        (Drawable        : Gdk_Drawable;
         Gc              : Gdk.Gdk_GC;
         Source_Drawable : Gdk_Drawable;
         Source_X        : Gint;
         Source_Y        : Gint;
         X               : Gint;
         Y               : Gint;
         Width           : Gint;
         Height          : Gint);
      pragma Import (C, Internal, "gdk_draw_drawable");

   begin
      Internal
        (Window, Gc, Source_Window, Source_X, Source_Y, X, Y, Width, Height);
   end Copy_Area;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Window : in out Gdk_Window) is
      procedure Internal (Window : Gdk_Window);
      pragma Import (C, Internal, "gdk_window_destroy");
   begin
      Internal (Window);
      Window := Null_Window;
   end Destroy;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Window          : out Gdk_Window;
      Parent          : Gdk_Window;
      Attributes      : Gdk_Window_Attr;
      Attributes_Mask : Gdk_Window_Attributes_Type)
   is
      function Internal
        (Parent          : Gdk_Window;
         Attributes      : Gdk_Window_Attr;
         Attributes_Mask : Gdk_Window_Attributes_Type) return Gdk_Window;
      pragma Import (C, Internal, "gdk_window_new");

   begin
      Window := Internal (Parent, Attributes, Attributes_Mask);
   end Gdk_New;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (Window : Gdk_Window) return Gdk_Window_List.Glist is
      function Internal (Window : Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_children");
      Result : Gdk_Window_List.Glist;

   begin
      Gdk_Window_List.Set_Object (Result, Internal (Window));
      return Result;
   end Get_Children;

   --------------------
   -- Get_Decoration --
   --------------------

   procedure Get_Decorations
     (Window      : Gdk_Window;
      Decorations : out Gdk_Wm_Decoration;
      Success     : out Boolean)
   is
      function Internal
        (Window      : Gdk_Window;
         Decorations : access Gdk_Wm_Decoration) return Gboolean;
      pragma Import (C, Internal, "gdk_window_get_decorations");

      Tmp : aliased Gdk_Wm_Decoration;

   begin
      Success := Internal (Window, Tmp'Access) /= 0;

      if Success then
         Decorations := Tmp;
      end if;
   end Get_Decorations;

   ------------------------------
   -- Get_Desk_Relative_Origin --
   ------------------------------

   procedure Get_Desk_Relative_Origin
     (Window  : Gdk_Window;
      X       : out Gint;
      Y       : out Gint;
      Success : out Boolean)
   is
      function Internal
        (Window : Gdk_Window; X, Y : System.Address) return Gboolean;
      pragma Import (C, Internal, "gdk_window_get_deskrelative_origin");

      Result : Gboolean;
      X_Out, Y_Out : aliased Gint;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Result := Internal (Window, X_Out'Address, Y_Out'Address);
      X := X_Out;
      Y := Y_Out;
      Success := Result /= 0;
   end Get_Desk_Relative_Origin;

   ----------------
   -- Get_Origin --
   ----------------

   procedure Get_Origin
     (Window  : Gdk_Window;
      X       : out Gint;
      Y       : out Gint;
      Success : out Boolean)
   is
      function Internal
        (Window : Gdk_Window; X, Y : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_window_get_origin");

      X_Out, Y_Out : aliased Gint;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Success := Internal (Window, X_Out'Address, Y_Out'Address) /= 0;
      X := X_Out;
      Y := Y_Out;
   end Get_Origin;

   -----------------
   -- Get_Pointer --
   -----------------

   procedure Get_Pointer
     (Window : Gdk_Window;
      X      : out Gint;
      Y      : out Gint;
      Mask   : out Gdk.Types.Gdk_Modifier_Type;
      Result : out Gdk_Window)
   is
      function Internal
        (Window : Gdk_Window;
         X      : System.Address;
         Y      : System.Address;
         Mask   : System.Address) return Gdk_Window;
      pragma Import (C, Internal, "gdk_window_get_pointer");

      X_Out, Y_Out : aliased Gint;
      Mask_Out : aliased Gdk.Types.Gdk_Modifier_Type;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Result :=
        Internal (Window, X_Out'Address, Y_Out'Address, Mask_Out'Address);
      X := X_Out;
      Y := Y_Out;
      Mask := Mask_Out;
   end Get_Pointer;

   -------------------
   -- Get_Toplevels --
   -------------------

   function Get_Toplevels return Gdk_Window_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_window_get_toplevels");
      Result : Gdk_Window_List.Glist;

   begin
      Gdk_Window_List.Set_Object (Result, Internal);
      return Result;
   end Get_Toplevels;

   ----------------
   -- Is_Viewable --
   -----------------

   function Is_Viewable (Window : Gdk_Window) return Boolean is
      function Internal (Window : Gdk_Window) return Gboolean;
      pragma Import (C, Internal, "gdk_window_is_viewable");

   begin
      return Boolean'Val (Internal (Window));
   end Is_Viewable;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible (Window : Gdk_Window) return Boolean is
      function Internal (Window : Gdk_Window) return Gboolean;
      pragma Import (C, Internal, "gdk_window_is_visible");

   begin
      return Boolean'Val (Internal (Window));
   end Is_Visible;

   -------------------
   -- Peek_Children --
   -------------------

   function Peek_Children
     (Window : Gdk_Window) return Gdk_Window_List.Glist
   is
      function Internal (Window : Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gdk_window_peek_children");
      Result : Gdk_Window_List.Glist;

   begin
      Gdk_Window_List.Set_Object (Result, Internal (Window));
      return Result;
   end Peek_Children;

   ---------------------
   -- Process_Updates --
   ---------------------

   procedure Process_Updates
     (Window : Gdk_Window; Update_Children : Boolean := True)
   is
      procedure Internal (Window : Gdk_Window; Update_Children : Gboolean);
      pragma Import (C, Internal, "gdk_window_process_updates");

   begin
      Internal (Window, Boolean'Pos (Update_Children));
   end Process_Updates;

   ---------------------
   -- Set_Back_Pixmap --
   ---------------------

   procedure Set_Back_Pixmap
     (Window          : Gdk_Window;
      Pixmap          : Gdk.Gdk_Pixmap;
      Parent_Relative : Boolean)
   is
      procedure Internal
        (Window : Gdk_Window; Pixmap : Gdk.Gdk_Pixmap; Relative : Gint);
      pragma Import (C, Internal, "gdk_window_set_back_pixmap");

   begin
      Internal (Window, Pixmap, Boolean'Pos (Parent_Relative));
   end Set_Back_Pixmap;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Window : Gdk_Window;
      Color  : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Window : Gdk_Window; Color : System.Address);
      pragma Import (C, Internal, "gdk_window_set_background");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Window, Color_A);
   end Set_Background;

   -----------------------
   -- Set_Debug_Updates --
   -----------------------

   procedure Set_Debug_Updates (Setting : Boolean := True) is
      procedure Internal (Setting : Gboolean);
      pragma Import (C, Internal, "gdk_window_set_debug_updates");

   begin
      Internal (Boolean'Pos (Setting));
   end Set_Debug_Updates;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name (Window : Gdk_Window; Name : UTF8_String) is
      procedure Internal (Window : Gdk_Window; Name : UTF8_String);
      pragma Import (C, Internal, "gdk_window_set_icon_name");

   begin
      Internal (Window, Name & ASCII.NUL);
   end Set_Icon_Name;

   --------------------
   -- Set_Modal_Hint --
   --------------------

   procedure Set_Modal_Hint
     (Window : Gdk_Window;
      Modal  : Boolean)
   is
      procedure Internal (Window : Gdk_Window; Modal : Gboolean);
      pragma Import (C, Internal, "gdk_window_set_modal_hint");

   begin
      Internal (Window, Boolean'Pos (Modal));
   end Set_Modal_Hint;

   ---------------------------
   -- Set_Override_Redirect --
   ---------------------------

   procedure Set_Override_Redirect
     (Window            : Gdk_Window;
      Override_Redirect : Boolean := True)
   is
      procedure Internal (Window : Gdk_Window; Override_Redirect : Gboolean);
      pragma Import (C, Internal, "gdk_window_set_override_redirect");

   begin
      Internal (Window, Boolean'Pos (Override_Redirect));
   end Set_Override_Redirect;

   --------------
   -- Set_Role --
   --------------

   procedure Set_Role (Window : Gdk_Window; Role : String) is
      procedure Internal (Window : Gdk_Window; Role : String);
      pragma Import (C, Internal, "gdk_window_set_role");

   begin
      Internal (Window, Role & ASCII.NUL);
   end Set_Role;

   ------------------------
   -- Set_Static_Gravity --
   ------------------------

   function Set_Static_Gravities
     (Window     : Gdk_Window;
      Use_Static : Boolean) return Boolean
   is
      function Internal
        (Window     : Gdk_Window;
         Use_Static : Gboolean) return Gboolean;
      pragma Import (C, Internal, "gdk_window_set_static_gravities");

   begin
      return Internal (Window, Boolean'Pos (Use_Static)) /= 0;
   end Set_Static_Gravities;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Window : Gdk_Window; Title : UTF8_String) is
      procedure Internal (Window : Gdk_Window; Title : UTF8_String);
      pragma Import (C, Internal, "gdk_window_set_title");
   begin
      Internal (Window, Title & ASCII.NUL);
   end Set_Title;

   -------------------
   -- Set_User_Data --
   -------------------

   procedure Set_User_Data
     (Window : Gdk.Gdk_Window;
      Widget : access Glib.Object.GObject_Record'Class)
   is
      procedure Internal (Window : Gdk_Window; Widget : System.Address);
      pragma Import (C, Internal, "gdk_window_set_user_data");
   begin
      Internal (Window, Get_Object (Widget));
   end Set_User_Data;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Window : Gdk.Gdk_Window) return Glib.Object.GObject
   is
      procedure Internal (Window : Gdk_Window; Widget : System.Address);
      pragma Import (C, Internal, "gdk_window_get_user_data");
      Data : aliased System.Address;
      Stub : Gtk_Widget_Record;
   begin
      Internal (Window, Data'Address);
      return Get_User_Data (Data, Stub);
   end Get_User_Data;

   -----------------------
   -- Window_At_Pointer --
   -----------------------

   procedure Window_At_Pointer
     (Win_X  : out Gint;
      Win_Y  : out Gint;
      Window : out Gdk_Window)
   is
      function Internal
        (Win_X  : System.Address;
         Win_Y  : System.Address) return Gdk_Window;
      pragma Import (C, Internal, "gdk_window_at_pointer");

      X_Out, Y_Out : aliased Gint;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Window := Internal (X_Out'Address, Y_Out'Address);
      Win_X := X_Out;
      Win_Y := Y_Out;
   end Window_At_Pointer;

   ---------------------
   -- Invalidate_Rect --
   ---------------------

   procedure Invalidate_Rect
     (Window              : Gdk_Window;
      Rectangle           : Gdk.Rectangle.Gdk_Rectangle;
      Invalidate_Children : Boolean)
   is
      procedure Internal
        (Window              : Gdk_Window;
         Rectangle           : Gdk.Rectangle.Gdk_Rectangle;
         Invalidate_Children : Gboolean);
      pragma Import (C, Internal, "gdk_window_invalidate_rect");
   begin
      Internal (Window, Rectangle, Boolean'Pos (Invalidate_Children));
   end Invalidate_Rect;

   --------------------
   -- Set_Composited --
   --------------------

   procedure Set_Composited (Window : Gdk_Window; Composited : Boolean) is
      procedure Internal (Window : Gdk_Window; Composited : Gboolean);
      pragma Import (C, Internal, "gdk_window_set_composited");
   begin
      Internal (Window, Gboolean (Boolean'Pos (Composited)));
   end Set_Composited;

end Gdk.Window;
