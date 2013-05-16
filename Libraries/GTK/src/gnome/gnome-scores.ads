-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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
with Gdk.Color;
with Gnome.Dialog;
with Gtk.Widget;
with Gtkada.Types; use Gtkada.Types;

package Gnome.Scores is

   type Gnome_Scores_Record is new
     Gnome.Dialog.Gnome_Dialog_Record with private;
   type Gnome_Scores is access all Gnome_Scores_Record'Class;

   type Time_T is new Long_Integer;

   procedure Gnome_New
     (Widget   : out Gnome_Scores;
      Names    : Chars_Ptr_Array;
      Scores   : out Gfloat;
      Times    : out Time_T;
      Clear    : Guint);

   procedure Initialize
     (Widget   : access Gnome_Scores_Record'Class;
      Names    : Chars_Ptr_Array;
      Scores   : out Gfloat;
      Times    : out Time_T;
      Clear    : Guint);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Set_Color
     (Gs  : access Gnome_Scores_Record;
      Pos : Guint;
      Col : Gdk.Color.Gdk_Color);

   procedure Set_Colors
     (Gs  : access Gnome_Scores_Record;
      Col : Gdk.Color.Gdk_Color);

   procedure Set_Current_Player
     (Gs : access Gnome_Scores_Record;
      J  : Gint);

   procedure Set_Def_Color
     (Gs  : access Gnome_Scores_Record;
      Col : Gdk.Color.Gdk_Color);

   procedure Set_Logo_Label
     (Gs    : access Gnome_Scores_Record;
      Txt   : String;
      Font  : String;
      Color : Gdk.Color.Gdk_Color);

   procedure Set_Logo_Label_Title
     (Gs  : access Gnome_Scores_Record;
      Txt : String);

   procedure Set_Logo_Pixmap
     (Gs   : access Gnome_Scores_Record;
      Logo : String);

   procedure Set_Logo_Widget
     (Gs : access Gnome_Scores_Record;
      W  : access Gtk.Widget.Gtk_Widget_Record'Class);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Scores_Record is new
     Gnome.Dialog.Gnome_Dialog_Record with null record;

   pragma Import (C, Get_Type, "gnome_scores_get_type");
end Gnome.Scores;
