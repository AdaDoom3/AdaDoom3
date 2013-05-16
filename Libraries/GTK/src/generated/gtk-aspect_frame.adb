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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Aspect_Frame is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Aspect_Frame_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Aspect_Frame : out Gtk_Aspect_Frame;
       Label        : UTF8_String := "";
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean)
   is
   begin
      Aspect_Frame := new Gtk_Aspect_Frame_Record;
      Gtk.Aspect_Frame.Initialize (Aspect_Frame, Label, Xalign, Yalign, Ratio, Obey_Child);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Aspect_Frame : access Gtk_Aspect_Frame_Record'Class;
       Label        : UTF8_String := "";
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean)
   is
      function Internal
         (Label      : Interfaces.C.Strings.chars_ptr;
          Xalign     : Gfloat;
          Yalign     : Gfloat;
          Ratio      : Gfloat;
          Obey_Child : Integer) return System.Address;
      pragma Import (C, Internal, "gtk_aspect_frame_new");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr;
      Tmp_Return : System.Address;
   begin
      if Label = "" then
         Tmp_Label := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Tmp_Return := Internal (Tmp_Label, Xalign, Yalign, Ratio, Boolean'Pos (Obey_Child));
      Free (Tmp_Label);
      Set_Object (Aspect_Frame, Tmp_Return);
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
      (Aspect_Frame : access Gtk_Aspect_Frame_Record;
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean)
   is
      procedure Internal
         (Aspect_Frame : System.Address;
          Xalign       : Gfloat;
          Yalign       : Gfloat;
          Ratio        : Gfloat;
          Obey_Child   : Integer);
      pragma Import (C, Internal, "gtk_aspect_frame_set");
   begin
      Internal (Get_Object (Aspect_Frame), Xalign, Yalign, Ratio, Boolean'Pos (Obey_Child));
   end Set;

   ---------------
   -- Get_Ratio --
   ---------------

   function Get_Ratio
      (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat
   is
      function Internal (Aspect_Frame : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtkada_GtkAspectFrame_get_ratio");
   begin
      return Internal (Get_Object (Aspect_Frame));
   end Get_Ratio;

   ----------------
   -- Get_Xalign --
   ----------------

   function Get_Xalign
      (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat
   is
      function Internal (Aspect_Frame : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtkada_GtkAspectFrame_get_xalign");
   begin
      return Internal (Get_Object (Aspect_Frame));
   end Get_Xalign;

   ----------------
   -- Get_Yalign --
   ----------------

   function Get_Yalign
      (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat
   is
      function Internal (Aspect_Frame : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtkada_GtkAspectFrame_get_yalign");
   begin
      return Internal (Get_Object (Aspect_Frame));
   end Get_Yalign;

end Gtk.Aspect_Frame;
