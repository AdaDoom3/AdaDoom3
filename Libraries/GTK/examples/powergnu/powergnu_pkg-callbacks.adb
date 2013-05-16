with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Main; use Gtk.Main;
with Gtkada.File_Selection; use Gtkada.File_Selection;
with Ada.Text_IO; use Ada.Text_IO;

package body Powergnu_Pkg.Callbacks is

   use Gtk.Arguments;

   ------------------------------
   -- On_Powergnu_Delete_Event --
   ------------------------------

   function On_Powergnu_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Object, Params);
   begin
      Main_Quit;
      return False;
   end On_Powergnu_Delete_Event;

   ---------------------------------
   -- On_Powergnu_Key_Press_Event --
   ---------------------------------

   function On_Powergnu_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1  : Gdk_Event := To_Event (Params, 1);
      Power : constant Powergnu_Access := Powergnu_Access (Object);

      procedure Show_Image;
      --  Show the current image in Power.Drawing_Area

      procedure Show_Image is
         Id : Message_Id;
      begin
         Set_Image
           (Power.Drawing_Area, Power.Images (Power.Current_Image).all);
         Pop (Power.Statusbar1, Power.Context);
         Id := Push (Power.Statusbar1,
                 Power.Context,
                 Power.Current_Image'Img & " /" &
                 Power.Num_Images'Img
                 & "  Use keyboard LEFT and RIGHT keys to navigate");
      end Show_Image;

   begin
      case Get_Key_Val (Arg1) is
         when GDK_BackSpace | GDK_Up | GDK_Left =>
            if Power.Current_Image > 1 then
               Power.Current_Image := Power.Current_Image - 1;
               Show_Image;
            end if;

         when GDK_Page_Up =>
            if Power.Current_Image > 10 then
               Power.Current_Image := Power.Current_Image - 10;
            else
               Power.Current_Image := 1;
            end if;

            Show_Image;

         when GDK_space | GDK_Down | GDK_Right =>
            if Power.Current_Image < Power.Num_Images then
               Power.Current_Image := Power.Current_Image + 1;
               Show_Image;
            end if;

         when GDK_Page_Down =>
            if Power.Current_Image <= Power.Num_Images - 10 then
               Power.Current_Image := Power.Current_Image + 10;
            else
               Power.Current_Image := Power.Num_Images;
            end if;

            Show_Image;

         when GDK_Home =>
            if Power.Current_Image /= 1 then
               Power.Current_Image := 1;
               Show_Image;
            end if;

         when GDK_End =>
            if Power.Current_Image /= Power.Num_Images then
               Power.Current_Image := Power.Num_Images;
               Show_Image;
            end if;

         when GDK_Q | GDK_LC_q =>
            Emit_Stop_By_Name (Object, "key_press_event");
            Main_Quit;

         when GDK_F3 =>
            Load_File
              (Power, File_Selection_Dialog (Must_Exist => True));

         when GDK_F4 =>
            Reparent (Power.Drawing_Area, Power.Win);
            --  Hide (Power);
            Show_All (Power.Win);

         when GDK_Escape =>
            Reparent (Power.Drawing_Area, Power.Main_Frame);
            Activate (Power.Win);
            Grab_Focus (Power.Win);
            Set_Sensitive (Power.Win);
            Hide (Power.Win);
            --  Show_All (Power);

         when others =>
            null;
      end case;

      return True;
   end On_Powergnu_Key_Press_Event;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Power : access Powergnu_Record'Class;
      Name  : String)
   is
      File  : File_Type;
      Str   : String (1 .. 1024);
      Len   : Natural;
      Id    : Message_Id;

   begin
      if Name = "" then
         return;
      end if;

      Open (File, In_File, Name);
      Power.Current_Image := 1;
      Power.Num_Images := 0;

      while not End_Of_Line (File) loop
         Get_Line (File, Str, Len);
         Power.Num_Images := Power.Num_Images + 1;
         Free (Power.Images (Power.Num_Images));
         Power.Images (Power.Num_Images) := new String'(Str (1 .. Len));
      end loop;

      Close (File);
      Set_Image (Power.Drawing_Area, Power.Images (1).all);
      Id := Push (Power.Statusbar1,
                  Power.Context,
                  Power.Current_Image'Img & " /" &
                  Power.Num_Images'Img
                  & " Use keyboard LEFT and RIGHT keys to navigate");
   end Load_File;

end Powergnu_Pkg.Callbacks;
