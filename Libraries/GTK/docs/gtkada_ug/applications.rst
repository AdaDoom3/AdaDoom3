.. _Starting_an_application_with_GtkAda:

***********************************
Starting an application with GtkAda
***********************************

You need to perform some initializations to start a GtkAda application::

  --  predefined units of the library
  with Gtk.Rc;
  with Gtk.Main;
  with Gtk.Enums;
  with Gtk.Window;
  ...
  --  My units
  with Callbacks;
  ...
  procedure Application is
     procedure Create_Window is ...

  begin
     --  Set the locale specific datas (e.g time and date format)
     Gtk.Main.Set_Locale;

     --  Initializes GtkAda
     Gtk.Main.Init;

     --  Load the resources. Note that this part is optional.
     Gtk.Rc.Parse ("application.rc");

     --  Create the main window
     Create_Window;

     --  Signal handling loop
     Gtk.Main.Main;
  end Application;


the `Create_Window` procedure looks like::

     procedure Create_Window is
        Main_Window : Gtk.Window.Gtk_Window;
        ...
     begin
        Gtk.Window.Gtk_New
          (Window   => Main_Window,
           The_Type => Gtk.Enums.Window_Toplevel);

        --  From Gtk.Widget:
        Gtk.Window.Set_Title (Window => Main_Window, Title  => "Editor");

        --  Construct the window and connect various callbacks

        ...
        Gtk.Window.Show_All (Main_Window);
     end Create_Window;
