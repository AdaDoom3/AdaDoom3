-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
with Gtk.Extra.Plot_Data; use Gtk.Extra.Plot_Data;

package body Gtk.Extra.Plot_Canvas.Plot is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child    : out Gtk_Plot_Canvas_Plot;
      Plot     : access Gtk.Extra.Plot.Gtk_Plot_Record'Class)
   is
      function Internal (Plot : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_plot_new");
   begin
      Child := new Gtk_Plot_Canvas_Plot_Record;
      Set_Object (Child, Internal (Get_Object (Plot)));
   end Gtk_New;

   -------------
   -- Get_Pos --
   -------------

   function Get_Pos
     (Child : access Gtk_Plot_Canvas_Plot_Record) return Plot_Canvas_Plot_Pos
   is
      function Internal (Child : System.Address) return Plot_Canvas_Plot_Pos;
      pragma Import (C, Internal, "ada_gtk_plot_canvas_plot_get_pos");
   begin
      return Internal (Get_Object (Child));
   end Get_Pos;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Child : access Gtk_Plot_Canvas_Plot_Record)
      return Gtk.Extra.Plot_Data.Gtk_Plot_Data
   is
      function Internal (Child : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_plot_canvas_plot_get_data");
      Stub : Gtk_Plot_Data_Record;
   begin
      return Gtk_Plot_Data
        (Get_User_Data (Internal (Get_Object (Child)), Stub));
   end Get_Data;

   -------------------
   -- Get_Datapoint --
   -------------------

   function Get_Datapoint
     (Child : access Gtk_Plot_Canvas_Plot_Record) return Gint
   is
      function Internal (Child : System.Address) return Gint;
      pragma Import (C, Internal, "ada_gtk_plot_canvas_plot_get_datapoint");
   begin
      return Internal (Get_Object (Child));
   end Get_Datapoint;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags
     (Child : access Gtk_Plot_Canvas_Plot_Record;
      Flags : Plot_Canvas_Plot_Flags)
   is
      procedure Internal (Child : System.Address; Flags : Integer);
      pragma Import (C, Internal, "ada_gtk_plot_canvas_plot_set_flags");
   begin
      Internal (Get_Object (Child), Integer (Flags));
   end Set_Flags;

   -----------------
   -- Unset_Flags --
   -----------------

   procedure Unset_Flags
     (Child : access Gtk_Plot_Canvas_Plot_Record;
      Flags : Plot_Canvas_Plot_Flags)
   is
      procedure Internal (Child : System.Address; Flags : Integer);
      pragma Import (C, Internal, "ada_gtk_plot_canvas_plot_unset_flags");
   begin
      Internal (Get_Object (Child), Integer (Flags));
   end Unset_Flags;

end Gtk.Extra.Plot_Canvas.Plot;
