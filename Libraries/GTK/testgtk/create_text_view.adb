-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2003                            --
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

with Glib;            use Glib;
with Gdk.Color;       use Gdk.Color;
with Gtk;             use Gtk;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Text_Tag;    use Gtk.Text_Tag;
with Gtk.Text_Iter;   use Gtk.Text_Iter;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_View;   use Gtk.Text_View;
with Gtk.Frame;       use Gtk.Frame;
with Gtk.Widget;      use Gtk.Widget;
with Pango.Font;      use Pango.Font;

package body Create_Text_View is

   procedure Insert_With_Tag
     (Buffer : access Gtk_Text_Buffer_Record'Class;
      Tag    : String;
      Text   : String);
   --  Insert some text with some special highlighting in the buffer.
   --  Note: in a real application, one would pass a Gtk_Tag instead of a tag
   --  name, to avoid a lookup in the tags table

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Text_View@B widget is a widget used to display any"
        & " text graphically. It provides support for changing fonts, colors,"
        & " background colors, as well as inserting pixmaps in the text."
        & ASCII.LF
        & "It is based on the model-view paradigm: the text itself is stored"
        & " in a non-graphical object, a @bGtk_Text_Buffer@B, which has"
        & " support for traversing the text through @bGtk_Text_Iter@B objects;"
        & " This buffer is then associated with one or many @bGtk_Text_View@B"
        & " which automatically reflect any change in the buffer."
        & ASCII.LF
        & "The text is fully editable";
   end Help;

   ---------------------
   -- Insert_With_Tag --
   ---------------------

   procedure Insert_With_Tag
     (Buffer : access Gtk_Text_Buffer_Record'Class;
      Tag    : String;
      Text   : String)
   is
      T : Gtk_Text_Tag;
      Iter, Start_Iter : Gtk_Text_Iter;
      Table : Gtk_Text_Tag_Table;
      Result : Boolean;
      pragma Warnings (Off, Result);
   begin
      Get_End_Iter (Buffer, Iter);

      if Tag = "" then
         Insert (Buffer, Iter, Text & ASCII.LF);

      else
         Table := Get_Tag_Table (Buffer);
         T := Lookup (Table, Tag);

         Insert (Buffer, Iter, Text & ASCII.LF);
         Copy (Source => Iter, Dest => Start_Iter);
         Backward_Chars (Start_Iter, Text'Length + 1, Result);
         Apply_Tag (Buffer, T, Start_Iter, Iter);
      end if;
   end Insert_With_Tag;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Buffer   : Gtk_Text_Buffer;
      Tags     : Gtk_Text_Tag_Table;
      Tag      : Gtk_Text_Tag;
      Color    : Gdk_Color;
      View     : Gtk_Text_View;
      Scrolled : Gtk_Scrolled_Window;

   begin
      Gtk_New (Tags);

      --  Create the tags that will be used to change the rendering of the
      --  text.

      Color := Parse ("red");
      Alloc (Get_Default_Colormap, Color);

      Gtk_New (Tag, "red");
      Add (Tags, Tag);
      Set_Property (Tag, Foreground_Gdk_Property, Color);

      Gtk_New (Tag, "courier");
      Add (Tags, Tag);
      Set_Property (Tag, Font_Desc_Property, From_String ("Courier bold"));

      --  Create the buffer and the views as appropriate

      Gtk_New (Buffer, Tags);

      Set_Label (Frame, "Text View");
      Gtk_New (View, Buffer);

      --  Insert some random text

      for Lien in 1 .. 50 loop
         for Count in 1 .. 10 loop
            Insert_With_Tag
              (Buffer, "", "A normal line with no special highlight");
         end loop;

         Insert_With_Tag
           (Buffer, "red", "Some text in red, notice the use of tags");
         Insert_With_Tag
           (Buffer, "courier", "The font can also be changed");
      end loop;

      --  Insert the view in the frame

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Always, Policy_Always);
      Add (Scrolled, View);

      Show_All (Scrolled);
      Add (Frame, Scrolled);
   end Run;

end Create_Text_View;
