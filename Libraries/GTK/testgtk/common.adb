-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                     Copyright (C) 2003-2006 AdaCore               --
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

with Gtk.Menu;             use Gtk.Menu;
with Gtk.Radio_Menu_Item;  use Gtk.Radio_Menu_Item;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Extra.Plot;       use Gtk.Extra.Plot;
with Gtk.Extra.Plot_Data;  use Gtk.Extra.Plot_Data;
with Gdk.Color;            use Gdk.Color;

with Ada.Strings.Fixed;

package body Common is

   -------------------------
   --  Build_Option_Menu  --
   -------------------------

   procedure Build_Option_Menu
     (Omenu   : out Gtk.Option_Menu.Gtk_Option_Menu;
      Gr      : in out Widget_SList.GSlist;
      Items   : Chars_Ptr_Array;
      History : Gint;
      Cb      : Widget_Handler.Marshallers.Void_Marshaller.Handler)

   is
      Menu      : Gtk_Menu;
      Menu_Item : Gtk_Radio_Menu_Item;

   begin
      Gtk.Option_Menu.Gtk_New (Omenu);
      Gtk_New (Menu);

      for I in Items'Range loop
         Gtk_New (Menu_Item, Gr, ICS.Value (Items (I)));
         Widget_Handler.Object_Connect (Menu_Item, "activate",
                                        Widget_Handler.To_Marshaller (Cb),
                                        Slot_Object => Menu_Item);
         Gr := Get_Group (Menu_Item);
         Append (Menu, Menu_Item);
         if Gint (I) = History then
            Set_Active (Menu_Item, True);
         end if;
         Show (Menu_Item);
      end loop;
      Gtk.Option_Menu.Set_Menu (Omenu, Menu);
      Gtk.Option_Menu.Set_History (Omenu, History);
   end Build_Option_Menu;


   --------------------
   -- Destroy_Window --
   --------------------

   procedure Destroy_Window (Win : access Gtk.Window.Gtk_Window_Record'Class;
                             Ptr : in Gtk_Window_Access) is
      pragma Warnings (Off, Win);
   begin
      Ptr.all := null;
   end Destroy_Window;

   --------------------
   -- Destroy_Dialog --
   --------------------

   procedure Destroy_Dialog (Win : access Gtk.Dialog.Gtk_Dialog_Record'Class;
                             Ptr : in Gtk_Dialog_Access) is
      pragma Warnings (Off, Win);
   begin
      Ptr.all := null;
   end Destroy_Dialog;

   --------------
   -- Image_Of --
   --------------

   function Image_Of (I : in Gint) return String is
   begin
      return Ada.Strings.Fixed.Trim (Gint'Image (I), Ada.Strings.Left);
   end Image_Of;

   ---------------------------------
   -- Set_Default_Plot_Attributes --
   ---------------------------------

   procedure Set_Default_Plot_Attributes
     (Plot : access Gtk_Plot_Record'Class) is
   begin
      Legends_Set_Attributes
        (Plot, "Helvetica", 6,
         Black (Get_Default_Colormap), White (Get_Default_Colormap));
      Axis_Set_Labels_Attributes
        (Get_Axis (Plot, Axis_Top),
         Font          => "Helvetica",
         Height        => 4,
         Angle         => Angle_0,
         Foreground    => Black (Get_Default_Colormap),
         Background    => White (Get_Default_Colormap),
         Transparent   => True,
         Justification => Justify_Left);
      Axis_Set_Labels_Attributes
        (Get_Axis (Plot, Axis_Bottom),
         Font          => "Helvetica",
         Height        => 4,
         Angle         => Angle_0,
         Foreground    => Black (Get_Default_Colormap),
         Background    => White (Get_Default_Colormap),
         Transparent   => True,
         Justification => Justify_Left);
      Axis_Set_Labels_Attributes
        (Get_Axis (Plot, Axis_Left),
         Font          => "Helvetica",
         Height        => 4,
         Angle         => Angle_90,
         Foreground    => Black (Get_Default_Colormap),
         Background    => White (Get_Default_Colormap),
         Transparent   => True,
         Justification => Justify_Left);
      Axis_Set_Labels_Attributes
        (Get_Axis (Plot, Axis_Right),
         Font          => "Helvetica",
         Height        => 4,
         Angle         => Angle_90,
         Foreground    => Black (Get_Default_Colormap),
         Background    => White (Get_Default_Colormap),
         Transparent   => True,
         Justification => Justify_Left);

      Axis_Title_Set_Attributes
        (Get_Axis (Plot, Axis_Top),
         Font          => "Helvetica",
         Height        => 6,
         Angle         => Angle_0,
         Foreground    => Black (Get_Default_Colormap),
         Background    => White (Get_Default_Colormap),
         Transparent   => True,
         Justification => Justify_Left);
      Axis_Title_Set_Attributes
        (Get_Axis (Plot, Axis_Bottom),
         Font          => "Helvetica",
         Height        => 6,
         Angle         => Angle_0,
         Foreground    => Black (Get_Default_Colormap),
         Background    => White (Get_Default_Colormap),
         Transparent   => True,
         Justification => Justify_Left);
      Axis_Title_Set_Attributes
        (Get_Axis (Plot, Axis_Left),
         Font          => "Helvetica",
         Height        => 6,
         Angle         => Angle_90,
         Foreground    => Black (Get_Default_Colormap),
         Background    => White (Get_Default_Colormap),
         Transparent   => True,
         Justification => Justify_Left);
      Axis_Title_Set_Attributes
        (Get_Axis (Plot, Axis_Right),
         Font          => "Helvetica",
         Height        => 6,
         Angle         => Angle_90,
         Foreground    => Black (Get_Default_Colormap),
         Background    => White (Get_Default_Colormap),
         Transparent   => True,
         Justification => Justify_Left);
   end Set_Default_Plot_Attributes;

end Common;
