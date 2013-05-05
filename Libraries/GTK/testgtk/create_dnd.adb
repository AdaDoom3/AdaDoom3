-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
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

with Glib;          use Glib;
with Gdk.Font;      use Gdk.Font;
with Gtk.Box;       use Gtk.Box;
with Gtk.Dnd;       use Gtk.Dnd;
with Gtk.Selection; use Gtk.Selection;
with Gdk.Types;     use Gdk.Types;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Button;    use Gtk.Button;
with Gtk.Table;     use Gtk.Table;
with Gtk.Label;     use Gtk.Label;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Image;     use Gtk.Image;
with Gtkada.Types;  use Gtkada.Types;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Arguments; use Gtk.Arguments;
with Interfaces.C.Strings;
with Gdk.Pixmap;    use Gdk.Pixmap;
with Gdk.Bitmap;    use Gdk.Bitmap;
with Gdk.Color;     use Gdk.Color;
with Gdk.Window;    use Gdk.Window;
with Gtk.Frame;     use Gtk.Frame;

pragma Warnings (Off); --  Gtk.Text is obsolescent
with Gtk.Text;      use Gtk.Text;
pragma Warnings (On);

with Gdk.Dnd;       use Gdk.Dnd;

package body Create_Dnd is

   function "+" (S : String) return Interfaces.C.Strings.chars_ptr
                renames Interfaces.C.Strings.New_String;

   Drag_Icon_Xpm : constant Gtkada.Types.Chars_Ptr_Array
     := "36 48 9 1"
     + "       c None"
     + ".      c #020204"
     + "+      c #8F8F90"
     + "@      c #D3D3D2"
     + "#      c #AEAEAC"
     + "$      c #ECECEC"
     + "%      c #A2A2A4"
     + "&      c #FEFEFC"
     + "*      c #BEBEBC"
     + "               ....................."
     + "              ..&&&&&&&&&&&&&&&&&&&."
     + "             ...&&&&&&&&&&&&&&&&&&&."
     + "            ..&.&&&&&&&&&&&&&&&&&&&."
     + "           ..&&.&&&&&&&&&&&&&&&&&&&."
     + "          ..&&&.&&&&&&&&&&&&&&&&&&&."
     + "         ..&&&&.&&&&&&&&&&&&&&&&&&&."
     + "        ..&&&&&.&&&@&&&&&&&&&&&&&&&."
     + "       ..&&&&&&.*$%$+$&&&&&&&&&&&&&."
     + "      ..&&&&&&&.%$%$+&&&&&&&&&&&&&&."
     + "     ..&&&&&&&&.#&#@$&&&&&&&&&&&&&&."
     + "    ..&&&&&&&&&.#$**#$&&&&&&&&&&&&&."
     + "   ..&&&&&&&&&&.&@%&%$&&&&&&&&&&&&&."
     + "  ..&&&&&&&&&&&.&&&&&&&&&&&&&&&&&&&."
     + " ..&&&&&&&&&&&&.&&&&&&&&&&&&&&&&&&&."
     + "................&$@&&&@&&&&&&&&&&&&."
     + ".&&&&&&&+&&#@%#+@#@*$%$+$&&&&&&&&&&."
     + ".&&&&&&&+&&#@#@&&@*%$%$+&&&&&&&&&&&."
     + ".&&&&&&&+&$%&#@&#@@#&#@$&&&&&&&&&&&."
     + ".&&&&&&@#@@$&*@&@#@#$**#$&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&@%&%$&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&$#@@$&&&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&+&$+&$&@&$@&&$@&&&&&&&&&&."
     + ".&&&&&&&&&+&&#@%#+@#@*$%&+$&&&&&&&&."
     + ".&&&&&&&&&+&&#@#@&&@*%$%$+&&&&&&&&&."
     + ".&&&&&&&&&+&$%&#@&#@@#&#@$&&&&&&&&&."
     + ".&&&&&&&&@#@@$&*@&@#@#$#*#$&&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&$%&%$&&&&&&&&."
     + ".&&&&&&&&&&$#@@$&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&&&+&$%&$$@&$@&&$@&&&&&&&&."
     + ".&&&&&&&&&&&+&&#@%#+@#@*$%$+$&&&&&&."
     + ".&&&&&&&&&&&+&&#@#@&&@*#$%$+&&&&&&&."
     + ".&&&&&&&&&&&+&$+&*@&#@@#&#@$&&&&&&&."
     + ".&&&&&&&&&&$%@@&&*@&@#@#$#*#&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&$%&%$&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&$#@@$&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&&+&$%&$$@&$@&&$@&&&&."
     + ".&&&&&&&&&&&&&&&+&&#@%#+@#@*$%$+$&&."
     + ".&&&&&&&&&&&&&&&+&&#@#@&&@*#$%$+&&&."
     + ".&&&&&&&&&&&&&&&+&$+&*@&#@@#&#@$&&&."
     + ".&&&&&&&&&&&&&&$%@@&&*@&@#@#$#*#&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&$%&%$&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&."
     + "....................................";

   Trashcan_Closed_Xpm : constant Gtkada.Types.Chars_Ptr_Array
     := "64 80 17 1"
     + "        c None"
     + ".       c #030304"
     + "+       c #5A5A5C"
     + "@       c #323231"
     + "#       c #888888"
     + "$       c #1E1E1F"
     + "%       c #767677"
     + "&       c #494949"
     + "* c #9E9E9C"
     + "= c #111111"
     + "- c #3C3C3D"
     + "; c #6B6B6B"
     + "> c #949494"
     + ", c #282828"
     + "' c #808080"
     + ") c #545454"
     + "! c #AEAEAC"
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                       ==......=$$...===                        "
     + "                 ..$------)+++++++++++++@$$...                  "
     + "             ..=@@-------&+++++++++++++++++++-....              "
     + "          =.$$@@@-&&)++++)-,$$$$=@@&+++++++++++++,..$           "
     + "         .$$$$@@&+++++++&$$$@@@@-&,$,-++++++++++;;;&..          "
     + "        $$$$,@--&++++++&$$)++++++++-,$&++++++;%%'%%;;$@         "
     + "       .-@@-@-&++++++++-@++++++++++++,-++++++;''%;;;%*-$        "
     + "       +------++++++++++++++++++++++++++++++;;%%%;;##*!.        "
     + "        =+----+++++++++++++++++++++++;;;;;;;;;;;;%'>>).         "
     + "         .=)&+++++++++++++++++;;;;;;;;;;;;;;%''>>#>#@.          "
     + "          =..=&++++++++++++;;;;;;;;;;;;;%###>>###+%==           "
     + "           .&....=-+++++%;;####''''''''''##'%%%)..#.            "
     + "           .+-++@....=,+%#####'%%%%%%%%%;@$-@-@*++!.            "
     + "           .+-++-+++-&-@$$=$=......$,,,@;&)+!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           =+-++-+++-+++++++++!++++!++++!+++!++!+++=            "
     + "            $.++-+++-+++++++++!++++!++++!+++!++!+.$             "
     + "              =.++++++++++++++!++++!++++!+++!++.=               "
     + "                 $..+++++++++++++++!++++++...$                  "
     + "                      $$=.............=$$                       "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                ";

   Trashcan_Open_Xpm : constant Gtkada.Types.Chars_Ptr_Array :=
     "64 80 17 1"
     + "        c None"
     + ".       c #030304"
     + "+       c #5A5A5C"
     + "@       c #323231"
     + "#       c #888888"
     + "$       c #1E1E1F"
     + "%       c #767677"
     + "&       c #494949"
     + "*       c #9E9E9C"
     + "=       c #111111"
     + "-       c #3C3C3D"
     + ";       c #6B6B6B"
     + ">       c #949494"
     + ",       c #282828"
     + "'       c #808080"
     + ")       c #545454"
     + "!       c #AEAEAC"
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                      .=.==.,@                  "
     + "                                   ==.,@-&&&)-=                 "
     + "                                 .$@,&++;;;%>*-                 "
     + "                               $,-+)+++%%;;'#+.                 "
     + "                            =---+++++;%%%;%##@.                 "
     + "                           @)++++++++;%%%%'#%$                  "
     + "                         $&++++++++++;%%;%##@=                  "
     + "                       ,-++++)+++++++;;;'#%)                    "
     + "                      @+++&&--&)++++;;%'#'-.                    "
     + "                    ,&++-@@,,,,-)++;;;'>'+,                     "
     + "                  =-++&@$@&&&&-&+;;;%##%+@                      "
     + "                =,)+)-,@@&+++++;;;;%##%&@                       "
     + "               @--&&,,@&)++++++;;;;'#)@                         "
     + "              ---&)-,@)+++++++;;;%''+,                          "
     + "            $--&)+&$-+++++++;;;%%'';-                           "
     + "           .,-&+++-$&++++++;;;%''%&=                            "
     + "          $,-&)++)-@++++++;;%''%),                              "
     + "         =,@&)++++&&+++++;%'''+$@&++++++                        "
     + "        .$@-++++++++++++;'#';,........=$@&++++                  "
     + "       =$@@&)+++++++++++'##-.................=&++               "
     + "      .$$@-&)+++++++++;%#+$.....................=)+             "
     + "      $$,@-)+++++++++;%;@=........................,+            "
     + "     .$$@@-++++++++)-)@=............................            "
     + "     $,@---)++++&)@===............................,.            "
     + "    $-@---&)))-$$=..............................=)!.            "
     + "     --&-&&,,$=,==...........................=&+++!.            "
     + "      =,=$..=$+)+++++&@$=.............=$@&+++++!++!.            "
     + "           .)-++-+++++++++++++++++++++++++++!++!++!.            "
     + "           .+-++-+++++++++++++++++++++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!+++!!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           =+-++-+++-+++++++++!++++!++++!+++!++!+++=            "
     + "            $.++-+++-+++++++++!++++!++++!+++!++!+.$             "
     + "              =.++++++++++++++!++++!++++!+++!++.=               "
     + "                 $..+++++++++++++++!++++++...$                  "
     + "                      $$==...........==$$                       "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                ";


   Have_Drag : Boolean := False;

   Log : Gtk_Text;

   Drag_Icon            : Gdk_Pixmap;
   Drag_Mask            : Gdk_Bitmap;
   Trashcan_Open        : Gdk_Pixmap;
   Trashcan_Closed      : Gdk_Pixmap;
   Trashcan_Open_Mask   : Gdk_Bitmap;
   Trashcan_Closed_Mask : Gdk_Bitmap;

   My_Target_String1  : constant Guint := 0;
   My_Target_String2  : constant Guint := 1;
   My_Target_Url      : constant Guint := 2;
   My_Target_Rootwin  : constant Guint := 3;

   package Widget_Callback is new
     Gtk.Handlers.Callback (Gtk.Widget.Gtk_Widget_Record);
   package Return_Callback is new
     Gtk.Handlers.Return_Callback (Gtk.Widget.Gtk_Widget_Record, Boolean);

   Target_Table : constant Target_Entry_Array
     := ((+"STRING",        Target_No_Constraint, My_Target_String1),
         (+"text/plain",    Target_No_Constraint, My_Target_String2),
         (+"text/uri-list", Target_No_Constraint, My_Target_Url),
         (+"application/x-rootwin-drop", Target_No_Constraint,
          My_Target_Rootwin));
   --  all the known data types in this application. Any MIME type can be used,
   --  as well a strings defined in the motif protocol, like "STRING".

   Target_Table_String : constant Target_Entry_Array
     := ((+"STRING",        Target_No_Constraint, My_Target_String1),
         (+"text/plain",    Target_No_Constraint, My_Target_String2));
   --  For a drop site that only accepts Data of type STRING or text/plain

   Target_Table_Url : constant Target_Entry_Array
     := (1 => (+"text/uri-list", Target_No_Constraint, My_Target_Url));
   --  For a drop site that only accepts Data of type url.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo demonstrates the drag-and-drop features of GtkAda."
        & " Several @bdrag-and-drop@B (dnd) protocols are supported, so that"
        & " your application can easily communicate with other external codes,"
        & " but this is mostly transparent for you."
        & ASCII.LF
        & "Although providing dnd capabilities in your application is not"
        & " difficult, it requires you to take care of several things, which"
        & " might be a little bit tricky the first time. You should look at"
        & " this demo while reading its source code, extensively documented"
        & " in testgtk/create_dnd.adb in the GtkAda distribution."
        & ASCII.LF
        & "Several things worth noting in this demo:"
        & ASCII.LF
        & ASCII.LF
        & " - Several @bdrag sources@B are available. The first only knows"
        & " how to transmitted plain text data. The second one only knows"
        & " about URLs, whereas the third one can provide any of the above."
        & ASCII.LF
        & ASCII.LF
        & " - Several @bdrop sites@B are also provided. Like the drag sources,"
        & " they all have special attributes and accept only special types"
        & " of data. Thus, some dnd operations will simply be rejected if"
        & " there is no common type between the drag source and the drop site."
        & ASCII.LF
        & ASCII.LF
        & " - In addition, all of the above have special types of @bactions@B"
        & " that they can handle. These are either @bAction_Copy@B,"
        & " @bAction_Move@B, or a combination of the two. These actions"
        & " control the default behavior of the drag and drop operation. The"
        & " action selected will be the first one common to both the drag"
        & " source and the drop site, unless you press @bshift@B at the same"
        & " time to force a move. Notice than when Action_Move is selected,"
        & " GtkAda asks the drag source to delete the data, thus the Delete"
        & " message that you see on the output."
        & ASCII.LF
        & ASCII.LF
        & " - The @btrashcan@B has a special behavior, since no default"
        & " behavior is associated to it, and everything is managed directly"
        & " by the demo, by connecting to the appropriate signals. We also"
        & " chose to change its visual aspect when the mouse is over the"
        & " widget and a drag-and-drop operation is taking place."
        & ASCII.LF
        & ASCII.LF
        & " - The @bicons@B can be freely modified. Note that the pixmap used"
        & " when dragging from the first button is different than the one used"
        & " for the other buttons. This can provide some interesting visual"
        & " clues for the user.";
   end Help;


   -------------
   -- Put_Log --
   -------------

   procedure Put_Log (Str : String) is
   begin
      Insert (Log,
              Gdk.Font.Null_Font,
              Fore => Gdk.Color.Null_Color,
              Back => Gdk.Color.Null_Color,
              Chars => Str & ASCII.LF);
   end Put_Log;

   ----------------------
   -- Target_Drag_Drop --
   ----------------------
   --  A handler called for the signal "drag_drop", ie every time the user
   --  drops something on Widget.
   --  You need to provide such a handler when you do not provide any
   --  default behavior in Dest_Set, as is the case for the pixmap in Run.
   --
   --  Note that even if there is no common target between the drag source
   --  and the drop site, this handler is called anyway. However, since
   --  Get_Targets will return a Null_List, nothing will be printed.
   --
   --  This is the general form for handlers of "drag_drop".

   function Target_Drag_Drop
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args) return Boolean
   is
      Context : Drag_Context := Drag_Context (To_C_Proxy (Args, 1));
      X       : Gint         := To_Gint (Args, 2);
      Y       : Gint         := To_Gint (Args, 3);
      Time    : Guint        := To_Guint (Args, 4);

      pragma Warnings (Off, Context);
      pragma Warnings (Off, X);
      pragma Warnings (Off, Y);
      pragma Warnings (Off, Time);

      use type Guint_List.Glist;

   begin
      Have_Drag := False;
      Put_Log ("Drop");
      Set (Gtk_Image (Widget), Trashcan_Closed, Trashcan_Closed_Mask);
      return False;
   end Target_Drag_Drop;

   -------------------------------
   -- Target_Drag_Data_Received --
   -------------------------------
   --  This function is called automatically every time some new data
   --  has been received on the trash can.
   --
   --  This is the general form of handlers for "drag_data_received".

   procedure Target_Drag_Data_Received
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : in Gtk_Args)
   is
      Context : constant Drag_Context := Drag_Context (To_C_Proxy (Args, 1));
      X    : Gint := To_Gint (Args, 2);
      Y    : Gint := To_Gint (Args, 3);
      Data : constant Selection_Data := Selection_Data (To_C_Proxy (Args, 4));
      Info : Guint := To_Guint (Args, 5); --  third item of the Target_Entry
      Time : constant Guint := To_Guint (Args, 6);

      pragma Warnings (Off, Widget);
      pragma Warnings (Off, X);
      pragma Warnings (Off, Y);
      pragma Warnings (Off, Info);
   begin
      if Get_Length (Data) >= 0
        and then Get_Format (Data) = 8
      then
         Put_Log ("Received " & Get_Data_As_String (Data) & " in trashcan");
         Finish
           (Context, Success => True, Del => False, Time => Guint32 (Time));
      else
         Finish
           (Context, Success => False, Del => False, Time => Guint32 (Time));
      end if;
   end Target_Drag_Data_Received;

   ------------------------
   -- Target_Drag_Motion --
   ------------------------
   --  This is the handler for the signal "drag_motion".
   --  It is called every time the user is doing a drag-and-drop operation, and
   --  the mouse is currently over Widget (but not released yet).
   --  This is used to change the visual aspect of Widget to provide visual
   --  clues to the user.
   --  This is the general form for handlers of "drag_motion".

   function Target_Drag_Motion  (Widget : access Gtk_Widget_Record'Class;
                                 Args   : Gtk_Args)
                                return Boolean
   is
      Context : constant Drag_Context := Drag_Context (To_C_Proxy (Args, 1));
      X       : Gint := To_Gint (Args, 2);
      Y       : Gint := To_Gint (Args, 3);
      Time    : constant Guint := To_Guint (Args, 4);

      pragma Warnings (Off, X);
      pragma Warnings (Off, Y);
      Toto : Gtk_Image;
   begin
      if not Have_Drag then
         Have_Drag := True;
         Toto := Gtk_Image (Widget);
         Set (Toto, Trashcan_Open, Trashcan_Open_Mask);
      end if;

      Drag_Status (Context, Get_Suggested_Action (Context), Guint32 (Time));

      return True;
   end Target_Drag_Motion;

   -----------------------
   -- Target_Drag_Leave --
   -----------------------
   --  A handler called whenever a drag-and-drop operation is being performed,
   --  and the mouse has just left the area covered by Widget on the screen.
   --  This is used to restore the default visual aspect of Widget.
   --
   --  Leave is also called when the drop is done on Widget.
   --
   --  This is the general form of handlers for "drag_leave".

   procedure Target_Drag_Leave (Widget : access Gtk_Widget_Record'Class;
                                Args   : Gtk_Args)
   is
      Context : Drag_Context := Drag_Context (To_C_Proxy (Args, 1));
      Time    : Guint := To_Guint (Args, 2);

      pragma Warnings (Off, Context);
      pragma Warnings (Off, Time);
   begin
      Put_Log ("Leave");
      Have_Drag := False;
      Set (Gtk_Image (Widget), Trashcan_Closed, Trashcan_Closed_Mask);
   end Target_Drag_Leave;

   ------------------------------
   -- Label_Drag_Data_Received --
   ------------------------------
   --  This function is called automatically every time some new data
   --  has been received on the label "Drop Here".
   --
   --  This is the general form of handlers for "drag_data_received".

   procedure Label_Drag_Data_Received
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : in Gtk_Args)
   is
      Context : constant Drag_Context := Drag_Context (To_C_Proxy (Args, 1));
      X    : Gint := To_Gint (Args, 2);
      Y    : Gint := To_Gint (Args, 3);
      Data : constant Selection_Data := Selection_Data (To_C_Proxy (Args, 4));
      Info : Guint := To_Guint (Args, 5); --  third item of the Target_Entry
      Time : constant Guint := To_Guint (Args, 6);

      pragma Warnings (Off, Widget);
      pragma Warnings (Off, X);
      pragma Warnings (Off, Y);
      pragma Warnings (Off, Info);
   begin
      --  Put_Log ("Selection=" & Atom_Name (Get_Selection (Data)));
      --  Put_Log ("Target="    & Atom_Name (Get_Target (Data)));
      --  Put_Log ("Type="      & Atom_Name (Get_Type (Data)));
      --  Put_Log ("Source Actions="
      --            & Drag_Action'Image (Get_Actions (Context)));
      --  Put_Log ("Suggested Action="
      --            & Drag_Action'Image (Get_Suggested_Action (Context)));
      --  Put_Log ("Action="   & Drag_Action'Image (Get_Action (Context)));

      if Get_Length (Data) >= 0
        and then Get_Format (Data) = 8
      then
         Put_Log ("Received "
                  & Get_Data_As_String (Data)
                  & " in label");
         Gtk.Dnd.Finish
           (Context, Success => True, Del => False, Time => Guint32 (Time));
      else
         Gtk.Dnd.Finish
           (Context, Success => False, Del => False, Time => Guint32 (Time));
      end if;
   end Label_Drag_Data_Received;

   --------------------------
   -- Source_Drag_Data_Get --
   --------------------------
   --  This is called when the user has dropped the item, and GtkAda needs
   --  to know what was actually dragged. The source widget should thus give
   --  some data, that will be transmitted to the drop widget through the
   --  signal "drag_data_received".
   --
   --  This is the general form of handlers for "drag_data_get".

   procedure Source_Drag_Data_Get
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : in Gtk_Args)
   is
      Context : Drag_Context := Drag_Context (To_C_Proxy (Args, 1));
      Data : constant Selection_Data := Selection_Data (To_C_Proxy (Args, 2));
      Info : constant Guint := To_Guint (Args, 3);
         --  third item of the Target_Entry
      Time    : Guint := To_Guint (Args, 4);

      pragma Warnings (Off, Widget);
      pragma Warnings (Off, Context);
      pragma Warnings (Off, Time);
   begin
      if Info = My_Target_Rootwin then
         Put_Log ("I was dropped on the root window");

      elsif Info = My_Target_Url then
         Selection_Data_Set (Data, Get_Target (Data), 8,
                             "file://www.act-europe.fr");

      else
         Selection_Data_Set (Data, Get_Target (Data), 8,
                             "I'm Data!, Info was " & Guint'Image (Info));
      end if;
   end Source_Drag_Data_Get;

   -----------------------------
   -- Source_Drag_Data_Delete --
   -----------------------------
   --  This handler is called whenever the drop site of a drag-and-drop
   --  operation has decided that the data should be deleted, or if the
   --  selected action was Action_Move.
   --
   --  This is the general handler type for "drag_data_delete".

   procedure Source_Drag_Data_Delete
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args    : Gtk_Args)
   is
      Context : Drag_Context := Drag_Context (To_C_Proxy (Args, 1));

      pragma Warnings (Off, Context);
      pragma Warnings (Off, Widget);
   begin
      Put_Log ("Delete the data!");
   end Source_Drag_Data_Delete;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Table     : Gtk_Table;
      Label     : Gtk_Label;
      Button    : Gtk_Button;
      Pixmap    : Gtk_Image;
      Box       : Gtk_Box;
   begin
      Set_Label (Frame, "Drag-and-Drop");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Gtk_New (Table, 3, 3, False);
      Pack_Start (Box, Table);

      Create_From_Xpm_D (Drag_Icon,
                         Null_Window,
                         Get_Colormap (Frame),
                         Drag_Mask,
                         Null_Color,
                         Drag_Icon_Xpm);
      Create_From_Xpm_D (Trashcan_Open,
                         Null_Window,
                         Get_Colormap (Frame),
                         Trashcan_Open_Mask,
                         Null_Color,
                         Trashcan_Open_Xpm);
      Create_From_Xpm_D (Trashcan_Closed,
                         Null_Window,
                         Get_Colormap (Frame),
                         Trashcan_Closed_Mask,
                         Null_Color,
                         Trashcan_Closed_Xpm);

      -----------------
      --  Drop sites --
      -----------------

      --  First drop site: a simple label

      Gtk_New (Label, "Drop plain text here." & ASCII.LF
               & "Action_Move only" & ASCII.LF);
      Gtk.Dnd.Dest_Set (Label,
                        Dest_Default_All,
                        Target_Table_String, -- only STRING or text/plain
                        Action_Move);
      Attach (Table, Label, 0, 1, 0, 1);

      Widget_Callback.Connect (Label, "drag_data_received",
                               Label_Drag_Data_Received'Access);

      --  Second drop site: a simple label

      Gtk_New (Label, "Drop Url here." & ASCII.LF
               & "Action_Copy or Action_Move" & ASCII.LF);
      Gtk.Dnd.Dest_Set (Label,
                        Dest_Default_All,
                        Target_Table_Url, -- only urls
                        Action_Copy + Action_Move);
      Attach (Table, Label, 1, 2, 0, 1);

      Widget_Callback.Connect (Label, "drag_data_received",
                               Label_Drag_Data_Received'Access);

      --  Third drop site: a simple label

      Gtk_New (Label, "Drop Anything" & ASCII.LF
               & "only Here" & ASCII.LF);
      Gtk.Dnd.Dest_Set (Label,
                        Dest_Default_All,
                        Target_Table, -- only urls
                        Action_Copy + Action_Move);
      Attach (Table, Label, 2, 3, 0, 1);

      Widget_Callback.Connect (Label, "drag_data_received",
                               Label_Drag_Data_Received'Access);

      -----------------
      --  Drag sites --
      -----------------

      --  First Drag site

      Gtk_New (Button, "Drag String from Here" & ASCII.LF);
      Gtk.Dnd.Source_Set (Button,
                          Button1_Mask or Button3_Mask,
                          Target_Table_String,
                          Action_Copy + Action_Move);
      Attach (Table, Button, 0, 1, 1, 2);

      Widget_Callback.Connect (Button, "drag_data_get",
                               Source_Drag_Data_Get'Access);
      Widget_Callback.Connect (Button, "drag_data_delete",
                               Source_Drag_Data_Delete'Access);

      Gtk.Dnd.Source_Set_Icon (Button,
                               Get_Colormap (Frame),
                               Drag_Icon, Drag_Mask);

      --  Second Drag site

      Gtk_New (Button, "Drag Url from Here" & ASCII.LF);
      Gtk.Dnd.Source_Set (Button,
                          Button1_Mask or Button3_Mask,
                          Target_Table_Url,
                          Action_Copy + Action_Move);
      Attach (Table, Button, 1, 2, 1, 2);

      Widget_Callback.Connect (Button, "drag_data_get",
                               Source_Drag_Data_Get'Access);
      Widget_Callback.Connect (Button, "drag_data_delete",
                               Source_Drag_Data_Delete'Access);

      --  Third Drag site

      Gtk_New (Button, "Drag String or" & ASCII.LF
               & "Url from Here" & ASCII.LF);
      Gtk.Dnd.Source_Set (Button,
                          Button1_Mask or Button3_Mask,
                          Target_Table,
                          Action_Copy + Action_Move + Action_Ask);
      Attach (Table, Button, 2, 3, 1, 2);

      Widget_Callback.Connect (Button, "drag_data_get",
                               Source_Drag_Data_Get'Access);
      Widget_Callback.Connect (Button, "drag_data_delete",
                               Source_Drag_Data_Delete'Access);

      --  Special drop site

      Gtk_New (Pixmap, Trashcan_Closed, Trashcan_Closed_Mask);
      Gtk.Dnd.Dest_Set (Pixmap);
      Attach (Table, Pixmap, 0, 1, 2, 3);

      Return_Callback.Connect (Pixmap, "drag_drop",
                               Target_Drag_Drop'Access);
      Widget_Callback.Connect (Pixmap, "drag_data_received",
                               Target_Drag_Data_Received'Access);
      Return_Callback.Connect (Pixmap, "drag_motion",
                               Target_Drag_Motion'Access);
      Widget_Callback.Connect (Pixmap, "drag_leave",
                               Target_Drag_Leave'Access);


      --  The log window
      Gtk_New (Log);
      Pack_Start (Box, Log);

      Show_All (Frame);
   end Run;

end Create_Dnd;
