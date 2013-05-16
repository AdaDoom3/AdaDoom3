-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
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

with System;

with Gdk.Color;              use Gdk.Color;
with Gtk.Extra.Plot;         use Gtk.Extra.Plot;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Plot_Surface is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plot_Surface_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Plot_Surface; Func : Plot3D_Function := null) is
   begin
      Widget := new Gtk_Plot_Surface_Record;
      Initialize (Widget, Func);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Plot_Surface_Record'Class;
      Func : Plot3D_Function := null)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_plot_surface_new");

      function Internal2 (Func : Plot3D_Function) return System.Address;
      pragma Import (C, Internal2, "gtk_plot_surface_new_function");
   begin
      if Func = null then
         Set_Object (Widget, Internal);
      else
         Set_Object (Widget, Internal2 (Func));
      end if;
   end Initialize;

   ----------------------
   -- Get_Grid_Visible --
   ----------------------

   function Get_Grid_Visible
     (Data : access Gtk_Plot_Surface_Record) return Boolean
   is
      function Internal (Data : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot_surface_get_grid_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Data)));
   end Get_Grid_Visible;

   ----------------------
   -- Get_Mesh_Visible --
   ----------------------

   function Get_Mesh_Visible
     (Data : access Gtk_Plot_Surface_Record) return Boolean
   is
      function Internal (Data : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot_surface_get_mesh_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Data)));
   end Get_Mesh_Visible;

   ------------
   -- Get_Nx --
   ------------

   function Get_Nx (Data : access Gtk_Plot_Surface_Record) return Gint is
      function Internal (Data   : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot_surface_get_nx");
   begin
      return Internal (Get_Object (Data));
   end Get_Nx;

   ------------
   -- Get_Ny --
   ------------

   function Get_Ny (Data : access Gtk_Plot_Surface_Record) return Gint is
      function Internal (Data   : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot_surface_get_ny");
   begin
      return Internal (Get_Object (Data));
   end Get_Ny;

   ----------------
   -- Get_Points --
   ----------------

   procedure Get_Points
     (Data : access Gtk_Plot_Surface_Record;
      X    : out Points_Array;
      Y    : out Points_Array;
      Z    : out Points_Array;
      Dx   : out Points_Array;
      Dy   : out Points_Array;
      Dz   : out Points_Array)
   is
      procedure Internal
        (Data : System.Address;
         X    : out System.Address;
         Y    : out System.Address;
         Z    : out System.Address;
         Dx   : out System.Address;
         Dy   : out System.Address;
         Dz   : out System.Address;
         Nx   : out Gint;
         Ny   : out Gint);
      pragma Import (C, Internal, "gtk_plot_surface_get_points");
      Nx, Ny : Gint;
      X1, Y1, Z1, Dx1, Dy1, Dz1 : System.Address;
   begin
      Internal (Get_Object (Data), X1, Y1, Z1, Dx1, Dy1, Dz1, Nx, Ny);
      X  := (Points => To_Double_Array (X1),  Num_Points => Nx);
      Y  := (Points => To_Double_Array (Y1),  Num_Points => Ny);
      Z  := (Points => To_Double_Array (Z1),  Num_Points => Nx * Ny);
      Dx := (Points => To_Double_Array (Dx1), Num_Points => Nx);
      Dy := (Points => To_Double_Array (Dy1), Num_Points => Ny);
      Dz := (Points => To_Double_Array (Dy1), Num_Points => Nx * Ny);
   end Get_Points;

   -----------
   -- Get_X --
   -----------

   function Get_X (Data : access Gtk_Plot_Surface_Record) return Points_Array
   is
      function Internal (Data : System.Address; Nx : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_plot_surface_get_x");
      N : aliased Gint;
      X : constant System.Address := Internal (Get_Object (Data), N'Address);

   begin
      return (To_Double_Array (X), N);
   end Get_X;

   ---------------
   -- Get_Xstep --
   ---------------

   function Get_Xstep (Data : access Gtk_Plot_Surface_Record) return Gdouble is
      function Internal (Data   : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_plot_surface_get_xstep");
   begin
      return Internal (Get_Object (Data));
   end Get_Xstep;

   -----------
   -- Get_Y --
   -----------

   function Get_Y
     (Data : access Gtk_Plot_Surface_Record) return Points_Array
   is
      function Internal
        (Data : System.Address; Ny : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_plot_surface_get_y");

      N : aliased Gint;
      Y : constant System.Address := Internal (Get_Object (Data), N'Address);

   begin
      return (To_Double_Array (Y), N);
   end Get_Y;

   ---------------
   -- Get_Ystep --
   ---------------

   function Get_Ystep (Data : access Gtk_Plot_Surface_Record) return Gdouble
   is
      function Internal (Data : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_plot_surface_get_ystep");
   begin
      return Internal (Get_Object (Data));
   end Get_Ystep;

   -----------
   -- Get_Z --
   -----------

   function Get_Z (Data : access Gtk_Plot_Surface_Record) return Points_Array
   is
      function Internal (Data : System.Address; Nz : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_plot_surface_get_z");
      N : aliased Gint;
      Z : constant System.Address := Internal (Get_Object (Data), N'Address);

   begin
      return (To_Double_Array (Z), N);
   end Get_Z;

   -----------------
   -- Set_Ambient --
   -----------------

   procedure Set_Ambient
     (Data : access Gtk_Plot_Surface_Record; Ambient : Gdouble)
   is
      procedure Internal (Data : System.Address; Ambient : Gdouble);
      pragma Import (C, Internal, "gtk_plot_surface_set_ambient");
   begin
      Internal (Get_Object (Data), Ambient);
   end Set_Ambient;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Data  : access Gtk_Plot_Surface_Record; Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Data  : System.Address; Color : System.Address);
      pragma Import (C, Internal, "gtk_plot_surface_set_color");
      C : aliased Gdk_Color := Color;
   begin
      Internal (Get_Object (Data), C'Address);
   end Set_Color;

   -------------------------
   -- Set_Grid_Background --
   -------------------------

   procedure Set_Grid_Background
     (Data       : access Gtk_Plot_Surface_Record;
      Background : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Data : System.Address; Background : System.Address);
      pragma Import (C, Internal, "gtk_plot_surface_set_grid_background");
      C : aliased Gdk_Color := Background;
   begin
      Internal (Get_Object (Data), C'Address);
   end Set_Grid_Background;

   -------------------------
   -- Set_Grid_Foreground --
   -------------------------

   procedure Set_Grid_Foreground
     (Data       : access Gtk_Plot_Surface_Record;
      Foreground : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Data : System.Address; Foreground : System.Address);
      pragma Import (C, Internal, "gtk_plot_surface_set_grid_foreground");
      C : aliased Gdk_Color := Foreground;
   begin
      Internal (Get_Object (Data), C'Address);
   end Set_Grid_Foreground;

   ----------------------
   -- Set_Grid_Visible --
   ----------------------

   procedure Set_Grid_Visible
     (Data : access Gtk_Plot_Surface_Record; Visible : Boolean)
   is
      procedure Internal (Data : System.Address; Visible : Gint);
      pragma Import (C, Internal, "gtk_plot_surface_set_grid_visible");
   begin
      Internal (Get_Object (Data), Boolean'Pos (Visible));
   end Set_Grid_Visible;

   ---------------
   -- Set_Light --
   ---------------

   procedure Set_Light
     (Data : access Gtk_Plot_Surface_Record; X, Y, Z : Gdouble)
   is
      procedure Internal (Data : System.Address; X, Y, Z : Gdouble);
      pragma Import (C, Internal, "gtk_plot_surface_set_light");
   begin
      Internal (Get_Object (Data), X, Y, Z);
   end Set_Light;

   ----------------------
   -- Set_Mesh_Visible --
   ----------------------

   procedure Set_Mesh_Visible
     (Data : access Gtk_Plot_Surface_Record; Visible : Boolean)
   is
      procedure Internal (Data    : System.Address; Visible : Gint);
      pragma Import (C, Internal, "gtk_plot_surface_set_mesh_visible");
   begin
      Internal (Get_Object (Data), Boolean'Pos (Visible));
   end Set_Mesh_Visible;

   ------------
   -- Set_Nx --
   ------------

   procedure Set_Nx (Data : access Gtk_Plot_Surface_Record; Nx : Gint) is
      procedure Internal (Data : System.Address; Nx   : Gint);
      pragma Import (C, Internal, "gtk_plot_surface_set_nx");
   begin
      Internal (Get_Object (Data), Nx);
   end Set_Nx;

   ------------
   -- Set_Ny --
   ------------

   procedure Set_Ny (Data : access Gtk_Plot_Surface_Record; Ny : Gint) is
      procedure Internal (Data : System.Address; Ny   : Gint);
      pragma Import (C, Internal, "gtk_plot_surface_set_ny");
   begin
      Internal (Get_Object (Data), Ny);
   end Set_Ny;

   ----------------
   -- Set_Points --
   ----------------

   procedure Set_Points
     (Data : access Gtk_Plot_Surface_Record;
      X    : Gdouble_Array_Access;
      Y    : Gdouble_Array_Access;
      Z    : Gdouble_Array_Access;
      Dx   : Gdouble_Array_Access;
      Dy   : Gdouble_Array_Access;
      Dz   : Gdouble_Array_Access)
   is
      procedure Internal
        (Data : System.Address;
         X    : System.Address;
         Y    : System.Address;
         Z    : System.Address;
         Dx   : System.Address;
         Dy   : System.Address;
         Dz   : System.Address;
         Nx   : Gint;
         Ny   : Gint);
      pragma Import (C, Internal, "gtk_plot_surface_set_points");
   begin
      pragma Assert (X'Length = Dx'Length);
      pragma Assert (Y'Length = Dy'Length);
      pragma Assert (Z'Length = Dz'Length);
      pragma Assert (Z'Length = X'Length * Y'Length);
      Internal (Get_Object (Data),
                X (X'First)'Address, Y (Y'First)'Address, Z (Z'First)'Address,
                Dx (Dx'First)'Address, Dy (Dy'First)'Address,
                Dz (Dz'First)'Address, X'Length, Y'Length);
   end Set_Points;

   ----------------
   -- Set_Shadow --
   ----------------

   procedure Set_Shadow
     (Data  : access Gtk_Plot_Surface_Record; Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Data  : System.Address; Color : System.Address);
      pragma Import (C, Internal, "gtk_plot_surface_set_shadow");
      C : aliased Gdk_Color := Color;
   begin
      Internal (Get_Object (Data), C'Address);
   end Set_Shadow;

   ---------------
   -- Set_Xstep --
   ---------------

   procedure Set_Xstep (Data : access Gtk_Plot_Surface_Record; Step : Gdouble)
   is
      procedure Internal (Data  : System.Address; Xstep : Gdouble);
      pragma Import (C, Internal, "gtk_plot_surface_set_xstep");
   begin
      Internal (Get_Object (Data), Step);
   end Set_Xstep;

   ---------------
   -- Set_Ystep --
   ---------------

   procedure Set_Ystep (Data : access Gtk_Plot_Surface_Record; Step : Gdouble)
   is
      procedure Internal (Data  : System.Address; Ystep : Gdouble);
      pragma Import (C, Internal, "gtk_plot_surface_set_ystep");
   begin
      Internal (Get_Object (Data), Step);
   end Set_Ystep;

   ----------------
   -- Build_Mesh --
   ----------------

   procedure Build_Mesh (Data : access Gtk_Plot_Surface_Record) is
      procedure Internal (Data : System.Address);
      pragma Import (C, Internal, "gtk_plot_surface_build_mesh");
   begin
      Internal (Get_Object (Data));
   end Build_Mesh;

   ------------------
   -- Recalc_Nodes --
   ------------------

   procedure Recalc_Nodes (Data : access Gtk_Plot_Surface_Record) is
      procedure Internal (Data : System.Address);
      pragma Import (C, Internal, "gtk_plot_surface_recalc_nodes");
   begin
      Internal (Get_Object (Data));
   end Recalc_Nodes;

   -------------------------
   -- Use_Height_Gradient --
   -------------------------

   procedure Use_Height_Gradient
     (Data : access Gtk_Plot_Surface_Record; Use_Gradient : Boolean)
   is
      procedure Internal (Data : System.Address; Gradient : Integer);
      pragma Import (C, Internal, "gtk_plot_surface_use_height_gradient");
   begin
      Internal (Get_Object (Data), Boolean'Pos (Use_Gradient));
   end Use_Height_Gradient;

   ------------------
   -- Use_Amplitud --
   ------------------

   procedure Use_Amplitud
     (Data : access Gtk_Plot_Surface_Record; Amplitud : Boolean)
   is
      procedure Internal (Data : System.Address; Amplitud : Integer);
      pragma Import (C, Internal, "gtk_plot_surface_use_amplitud");
   begin
      Internal (Get_Object (Data), Boolean'Pos (Amplitud));
   end Use_Amplitud;

   ---------------------
   -- Set_Transparent --
   ---------------------

   procedure Set_Transparent
     (Data : access Gtk_Plot_Surface_Record; Transparent : Boolean)
   is
      procedure Internal (Data : System.Address; Transparent : Integer);
      pragma Import (C, Internal, "gtk_plot_surface_set_transparent");
   begin
      Internal (Get_Object (Data), Boolean'Pos (Transparent));
   end Set_Transparent;

end Gtk.Extra.Plot_Surface;
