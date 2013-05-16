-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                 Copyright (C) 2003-2013, AdaCore                  --
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

with Gtk.Box;     use Gtk.Box;
with Gtk.Enums;   use Gtk.Enums;
with Gtk.Frame;   use Gtk.Frame;
with Gtk.Label;   use Gtk.Label;
with Glib.Convert; use Glib.Convert;
with Pango.Layout; use Pango.Layout;

package body Create_Label is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Label@B is used to display any kind of text on the"
        & " screen, even on multiple line."
        & ASCII.LF
        & "This is a passive widget, in that it does not react to any event.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Vbox, Hbox : Gtk_Box;
      Label      : Gtk_Label;
      Frame2     : Gtk_Frame;
   begin
      Set_Label (Frame, "Label");
      Gtk_New_Vbox (Vbox, False, 5);
      Gtk_New_Hbox (Hbox, False, 5);
      Add (Frame, Hbox);
      Pack_Start (Hbox, Vbox, False, False, 0);

      Gtk_New (Frame2, "Normal Label");
      Gtk_New (Label, "This is a Normal Label");
      Add (Frame2, Label);
      Pack_Start (Vbox, Frame2, False, False, 0);

      Gtk_New (Frame2, "Multi-line Label");
      Gtk_New (Label, "This is a Multi-line label."
               & ASCII.LF
               & "Second Line"
               & ASCII.LF
               & "Third line");
      Add (Frame2, Label);
      Pack_Start (Vbox, Frame2, False, False, 0);

      Gtk_New (Frame2, "Left Justified Label");
      Gtk_New (Label, "This is a Left-Justified"
               & ASCII.LF & "Multi-line label." & ASCII.LF
               & "Third      line");
      Set_Justify (Label, Justify_Left);
      Add (Frame2, Label);
      Pack_Start (Vbox, Frame2, False, False, 0);

      Gtk_New (Frame2, "Right Justified Label");
      Gtk_New (Label, "This is a Right-Justified"
               & ASCII.LF & "Multi-line label." & ASCII.LF
               & "Fourth      line");
      Set_Justify (Label, Justify_Right);
      Add (Frame2, Label);
      Pack_Start (Vbox, Frame2, False, False, 0);

      Gtk_New (Frame2, "Selectable Label");
      Gtk_New (Label, "This is a selectable label "
               & ASCII.LF
               & "you can select the text with the "
               & ASCII.LF
               & "mouse and paste it.");
      Set_Selectable (Label, True);
      Add (Frame2, Label);
      Pack_Start (Vbox, Frame2, False, False, 0);

      Gtk_New_Vbox (Vbox, False, 5);
      Pack_Start (Hbox, Vbox, False, False, 0);

      Gtk_New (Frame2, "Line wrapped Label");
      Gtk_New (Label,
               "This is an example of a line-wrapped label.  It should not "
               & "be taking up the entire             "
               & "width allocated to it, but automatically wraps the words "
               & "to fit.  The time has come, for all good men, to come to "
               & "the aid of their party.  The sixth sheik's six sheep's sick."
               & ASCII.LF
               & "     It supports multiple paragraphs correctly, and  "
               & "correctly   adds many          extra  spaces. ");
      Set_Line_Wrap (Label, True);
      Add (Frame2, Label);
      Pack_Start (Vbox, Frame2, False, False, 0);

      Gtk_New (Frame2, "Character mode wrapped label");
      Gtk_New (Label,
               "This is an example of a line-wrapped label.  Like the above "
               & "example, it will not be taking up the entire width "
               & "allocated to it, but instead of wrapping words to fit, it "
               & "wraps characters to fit.  "
               & "The time has come, for all good men, to come to "
               & "the aid of their party.  The sixth sheik's six sheep's sick."
               & ASCII.LF
               & "     It supports multiple paragraphs correctly, and  "
               & "correctly   adds many          extra  spaces. ");
      Set_Line_Wrap (Label, True);
      Set_Line_Wrap_Mode (Label, Pango_Wrap_Char);
      Add (Frame2, Label);
      Pack_Start (Vbox, Frame2, False, False, 0);

      Gtk_New (Frame2, "Filled, wrapped Label");
      Gtk_New (Label,
               "This is an example of a line-wrapped label.  It should "
               & "be taking up the entire             "
               & "width allocated to it, but automatically wraps the words "
               & "to fit.  The time has come, for all good men, to come to "
               & "the aid of their party.  The sixth sheik's six sheep's sick."
               & ASCII.LF
               & "     It supports multiple paragraphs correctly, and  "
               & "correctly   adds many          extra  spaces. ");
      Set_Line_Wrap (Label, True);
      Set_Justify (Label, Justify_Fill);
      Add (Frame2, Label);
      Pack_Start (Vbox, Frame2, False, False, 0);

      Gtk_New (Frame2, "Underlined Label");
      Gtk_New
        (Label, Locale_To_UTF8
         ("This label is underlined!"
          & ASCII.LF
          & "This one is underlined in 日本語の入用quite a funky fashion"));
      Set_Justify (Label, Justify_Left);
      Set_Pattern (Label,
                   "_________________________ _ _________ _ _____ _ __"
                   & " __  ___ ____ _____");
      Add (Frame2, Label);
      Pack_Start (Vbox, Frame2, False, False, 0);

      Gtk_New (Frame2, "Markup Label");
      Gtk_New (Label);

      Set_Markup (Label,
         ("<span size=""x-large"" weight=""ultrabold"""
          & "background=""darkblue"" color=""white"">"
          & "This label has <i>markup</i>!</span>"
          & ASCII.LF
          & "You can make text <b>bold</b> <i>italic</i> "
          & "<u>underline</u> <s>striken</s> <big>big</big>. "
          & ASCII.LF
          & "<tt>You can also use mono spaced font</tt>"
          & " and write H<sub><small>2</small></sub>O "
          & "and "
          & ASCII.LF
          & "<span weight=""light"" color=""red"" size=""xx-large"">"
          & "y=裡x<sub><small>i</small></sub>"
          & "<sup><small>2</small></sup> "
          & "+ 3</span>"
          & " besides other things..."));
      Add (Frame2, Label);
      Pack_Start (Vbox, Frame2, False, False, 0);

      Show_All (Frame);
   end Run;

end Create_Label;
