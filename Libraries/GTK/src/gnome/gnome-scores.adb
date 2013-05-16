-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                           AdaCore                                 --
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

with Gdk; use Gdk;
with System;

package body Gnome.Scores is

   use Gtk;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget   : out Gnome_Scores;
      Names    : Chars_Ptr_Array;
      Scores   : out Gfloat;
      Times    : out Time_T;
      Clear    : Guint) is
   begin
      Widget := new Gnome_Scores_Record;
      Initialize (Widget, Names, Scores, Times, Clear);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget   : access Gnome_Scores_Record'Class;
      Names    : Chars_Ptr_Array;
      Scores   : out Gfloat;
      Times    : out Time_T;
      Clear    : Guint)
   is
      function Internal
        (N_Scores : Guint;
         Names    : Chars_Ptr_Array;
         Scores   : System.Address;
         Times    : System.Address;
         Clear    : Guint)
         return System.Address;
      pragma Import (C, Internal, "gnome_scores_new");

      S : aliased Gfloat;
      T : aliased Time_T;

   begin
      Set_Object
        (Widget, Internal (Names'Length, Names, S'Address, T'Address, Clear));
      Scores := S;
      Times  := T;
   end Initialize;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Gs  : access Gnome_Scores_Record;
      Pos : Guint;
      Col : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Gs  : System.Address;
         Pos : Guint;
         Col : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_scores_set_color");
   begin
      Internal (Get_Object (Gs), Pos, Col);
   end Set_Color;

   ----------------
   -- Set_Colors --
   ----------------

   procedure Set_Colors
     (Gs  : access Gnome_Scores_Record;
      Col : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Gs  : System.Address;
         Col : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_scores_set_colors");
   begin
      Internal (Get_Object (Gs),
                Col);
   end Set_Colors;

   ------------------------
   -- Set_Current_Player --
   ------------------------

   procedure Set_Current_Player (Gs : access Gnome_Scores_Record; J : Gint) is
      procedure Internal (Gs : System.Address; J : Gint);
      pragma Import (C, Internal, "gnome_scores_set_current_player");
   begin
      Internal (Get_Object (Gs), J);
   end Set_Current_Player;

   -------------------
   -- Set_Def_Color --
   -------------------

   procedure Set_Def_Color
     (Gs  : access Gnome_Scores_Record;
      Col : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Gs : System.Address; Col : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_scores_set_def_color");
   begin
      Internal (Get_Object (Gs), Col);
   end Set_Def_Color;

   --------------------
   -- Set_Logo_Label --
   --------------------

   procedure Set_Logo_Label
     (Gs    : access Gnome_Scores_Record;
      Txt   : String;
      Font  : String;
      Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Gs    : System.Address;
         Txt   : String;
         Font  : String;
         Color : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_scores_set_logo_label");
   begin
      Internal (Get_Object (Gs), Txt & ASCII.NUL, Font & ASCII.NUL, Color);
   end Set_Logo_Label;

   --------------------------
   -- Set_Logo_Label_Title --
   --------------------------

   procedure Set_Logo_Label_Title
     (Gs  : access Gnome_Scores_Record;
      Txt : String)
   is
      procedure Internal (Gs : System.Address; Txt : String);
      pragma Import (C, Internal, "gnome_scores_set_logo_label_title");
   begin
      Internal (Get_Object (Gs), Txt & ASCII.NUL);
   end Set_Logo_Label_Title;

   ---------------------
   -- Set_Logo_Pixmap --
   ---------------------

   procedure Set_Logo_Pixmap
     (Gs   : access Gnome_Scores_Record;
      Logo : String)
   is
      procedure Internal (Gs : System.Address; Logo : String);
      pragma Import (C, Internal, "gnome_scores_set_logo_pixmap");
   begin
      Internal (Get_Object (Gs), Logo & ASCII.NUL);
   end Set_Logo_Pixmap;

   ---------------------
   -- Set_Logo_Widget --
   ---------------------

   procedure Set_Logo_Widget
     (Gs : access Gnome_Scores_Record;
      W  : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Gs : System.Address; W : System.Address);
      pragma Import (C, Internal, "gnome_scores_set_logo_widget");
   begin
      Internal (Get_Object (Gs), Get_Object (W));
   end Set_Logo_Widget;

end Gnome.Scores;
