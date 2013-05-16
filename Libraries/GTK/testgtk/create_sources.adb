-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                  Copyright 2006 AdaCore                           --
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

with Ada.Calendar;        use Ada.Calendar;
with Glib.Main;           use Glib, Glib.Main;
with Gtk.Box;             use Gtk.Box;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Label;           use Gtk.Label;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
with Gtk.Text_Iter;       use Gtk.Text_Iter;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Text_IO; use Ada.Text_IO;

package body Create_Sources is

   Buffer : Gtk_Text_Buffer;
   Id     : G_Source_Id := 0;

   File_Monitor : G_Source_Type := Null_Source_Type;
   --  A particular kind of G_Source that monitors changes to a file.

   type String_Access is access String;

   type Source_User_Data is record
      File_Name      : String_Access;
      Last_Check     : Time;
      File_Timestamp : OS_Time := Invalid_Time;
   end record;
   type Source_User_Data_Access is access Source_User_Data;
   --  The user data stored in our monitor.
   --  We use it to avoid checking the file system too often

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Source_User_Data_Access);

   procedure On_Destroy (Box : access Gtk_Widget_Record'Class);
   --  Called when this demo is closed.

   -------------
   -- Monitor --
   -------------
   --  This defines a new source type that monitors file events.

   procedure Create_Monitor (File_Name : String);
   --  Create a new input source that monitors changes in a file.

   function Prepare (Source : G_Source; Timeout : access Gint) return Gboolean;
   function Check   (Source : G_Source) return Gboolean;
   procedure Finalize (Source : G_Source);
   --  See the documentation in glib-main.ads for these primitive operations
   --  of G_Source

   pragma Convention (C, Prepare);
   pragma Convention (C, Check);
   pragma Convention (C, Finalize);

   --------------
   -- G_Source --
   --------------
   --  This is the implementation of a specific source of the type Monitor,
   --  which refreshes the graphical buffer to show the new file contents

   package String_Sources is new Generic_Sources (String);

   function Refresh_File (Filename : String) return Boolean;
   --  Refresh the contents of the file that the source was monitoring

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "The main even loop of gtk+ is highly configurable. It monitors"
        & " various event sources, including the windowing system, pipes,"
        & " running processes, timeouts... and will call user-defined"
        & " callbacks whenever some event happens." & ASCII.LF
        & "It is possible for you to define your own source of events, as"
        & " demonstrated here." & ASCII.LF
        & "This demo monitors a file on the disk (""sources"" in the testgtk/"
        & " directory. Open a text editor, create that file if necessary,"
        & " add some data to it, and save. You will see immediately the new"
        & " contents of the file." & ASCII.LF
        & "While it certainly isn't the most efficient way to do that (having"
        & " a timeout that checks periodically might be more appropriate),"
        & " this demo shows how you can create your own event source. On"
        & " linux systems, the kernel is able to notify users whenever some"
        & " part of the file system changes. You could connect to dbus, on"
        & " which the kernel sends this info, and use this as an event source"
        & " in your application.";
   end Help;

   ------------------
   -- Refresh_File --
   ------------------

   function Refresh_File (Filename : String) return Boolean is
      Start, Last : Gtk_Text_Iter;
      File : File_Type;
      Contents : String (1 .. 1024);
      L       : Natural;
   begin
      Get_Start_Iter (Buffer, Start);
      Get_End_Iter   (Buffer, Last);
      Delete (Buffer, Start, Last);

      Open (File, In_File, Get_Current_Dir & Filename);
      Insert_At_Cursor
        (Buffer, "File name is: " & Filename & ASCII.LF);

      loop
         Get_Line (File, Contents, L);
         exit when L = 0;

         Get_End_Iter (Buffer, Last);
         Insert (Buffer, Last, Contents (Contents'First .. L));
      end loop;

      Close (File);
      return True;

   exception
      when End_Error =>
         Close (File);
         return True;
      when Name_Error =>
         return True;
   end Refresh_File;

   -------------
   -- Prepare --
   -------------

   function Prepare (Source : G_Source; Timeout : access Gint) return Gboolean
   is
      Data : constant Source_User_Data_Access :=
        Convert (Get_User_Data (Source));
      T    : OS_Time;
   begin
      --  Note: we always set the timeout to something suitable. If we don't,
      --  its default value of -1 will be used, which means that gtk+ will wait
      --  until one even is available somewhere (most likely a graphical
      --  event), and as a result Prepare will only be called after an event
      --  has been processed, which isn't what we want

      Timeout.all := 500;

      --  The timeout above ensures that we are not called less than every
      --  500 ms. However, to spare system resources, which should ensure that
      --  we do not check the file system too often

      if Clock - Data.Last_Check > 0.4 then
         Data.Last_Check := Clock;

         --  Check whether the file has been modified
         T := File_Time_Stamp (Data.File_Name.all);
         if T /= Data.File_Timestamp then
            Data.File_Timestamp := T;
            return 1;
         else
            return 0;
         end if;

      else
         return 0;
      end if;
   end Prepare;

   -----------
   -- Check --
   -----------

   function Check (Source : G_Source) return Gboolean is
      pragma Unreferenced (Source);
   begin
      return 0;
   end Check;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Source : G_Source) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Source_User_Data, Source_User_Data_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (String, String_Access);
      Data : Source_User_Data_Access :=
        Convert (Get_User_Data (Source));
   begin
      Unchecked_Free (Data.File_Name);
      Unchecked_Free (Data);
   end Finalize;

   --------------------
   -- Create_Monitor --
   --------------------

   procedure Create_Monitor (File_Name : String) is
      Source : G_Source;
      Data   : Source_User_Data_Access;
   begin
      if File_Monitor = Null_Source_Type then
         File_Monitor := G_Source_Type_New
           (Prepare  => Prepare'Access,
            Check    => Check'Access,
            Finalize => Finalize'Access);
      end if;

      Data := new Source_User_Data'
        (Last_Check     => Clock,
         File_Name      => new String'(File_Name),
         File_Timestamp => Invalid_Time);
      Source := Source_New (File_Monitor, Data.all'Address);

      String_Sources.Set_Callback
        (Source, Refresh_File'Access, File_Name);

      --  Start executing Source
      Id := Attach (Source, null);
   end Create_Monitor;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Box : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Box);
   begin
      if Id /= 0 then
         Remove (Id);
         Id := 0;
         Buffer := null;
      end if;
   end On_Destroy;

   ---------
   -- Run --
   ---------

   procedure Run (F : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Label  : Gtk_Label;
      View   : Gtk_Text_View;
      Box    : Gtk_Box;
      Scrolled : Gtk_Scrolled_Window;
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);
      Set_Label (F, "New event source for main loop");
      Add (F, Box);

      Gtk_New
        (Label, "Edit and save the file ""sources"" in the current directory");
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Label, "and let this demo monitor its contents");
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Box, Scrolled, Expand => True, Fill => True);

      Gtk_New (Buffer);
      Gtk_New (View, Buffer);

      Add (Scrolled, View);

      Create_Monitor ("sources");
      Widget_Callback.Connect (Box, "destroy", On_Destroy'Access);

      Show_All (Box);
   end Run;

end Create_Sources;
