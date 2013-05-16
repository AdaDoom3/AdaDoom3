with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Power_GNU; use Power_GNU;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Powergnu_Pkg is

   Max_Image : constant := 1024;
   --  Maximum number of images in a presentation

   type Image_Array is array (1 .. Max_Image) of String_Access;

   type Powergnu_Record is new Gtk_Window_Record with record
      Win : Gtk_Window;
      Images : Image_Array;
      Current_Image : Natural := 0;
      Num_Images : Natural := 0;
      Context : Context_Id;
      Vbox1 : Gtk_Vbox;
      Main_Frame : Gtk_Frame;
      Drawing_Area : Image_Drawing;
      Statusbar1 : Gtk_Status_Bar;
   end record;
   type Powergnu_Access is access all Powergnu_Record'Class;

   procedure Gtk_New (Powergnu : out Powergnu_Access);
   procedure Initialize (Powergnu : access Powergnu_Record'Class);

   Powergnu : Powergnu_Access;

end Powergnu_Pkg;
