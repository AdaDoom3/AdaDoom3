
with Ada.Text_IO;  use Ada.Text_IO;
with Glib;         use Glib;
with Gtk.Window;   use Gtk.Window;
with Gtk.Main;     use Gtk.Main;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Button;   use Gtk.Button;
with Gtk.Box;      use Gtk.Box;
with Gtk.Handlers; use Gtk.Handlers;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Widget;   use Gtk.Widget;

procedure Test_Handlers is

   type My_Data3 is record
      Button : Gtk_Widget;
      Object : Gtk_Widget;
      Id     : Handler_Id;
   end record;
   type My_Data3_Access is access My_Data3;

   package User_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Gtk_Widget);
   package User_Callback3 is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, My_Data3_Access);

   procedure My_Destroy (Button : access Gtk_Widget_Record'Class) is
   begin
      Put_Line ("My_Destroy");
      Destroy (Button);
   end My_Destroy;

   procedure My_Destroy2
     (Button : access Gtk_Widget_Record'Class;
      Data   : Gtk_Widget) is
   begin
      Put_Line ("My_Destroy2");
      Destroy (Data);
   end My_Destroy2;

   procedure My_Destroy3
     (Button : access Gtk_Widget_Record'Class;
      Data   : My_Data3_Access) is
   begin
      Put_Line ("My_Destroy3");
      Destroy (Data.Button);
      Disconnect (Data.Object, Data.Id);
   end My_Destroy3;

   Win              : Gtk_Window;
   Button1, Button2 : Gtk_Button;
   Vbox, Hbox       : Gtk_Box;
   Id               : Handler_Id;
   Data3            : My_Data3_Access;
begin
   Gtk.Main.Init;

   Gtk_New (Win, Window_Toplevel);

   Gtk_New_Vbox (Vbox);
   Add (Win, Vbox);

   --  Using object_connect.
   --  The callback is automatically destroyed when button2 is destroyed, so
   --  you can press button1 as many times as you want, no problem
   Gtk_New_Hbox (Hbox);
   Pack_Start (Vbox, Hbox);
   Gtk_New (Button1, "button1, object connect");
   Pack_Start (Hbox, Button1);
   Gtk_New (Button2, "button2");
   Pack_Start (Hbox, Button2);

   Widget_Callback.Object_Connect
     (Button1, "clicked",
      Widget_Callback.To_Marshaller (My_Destroy'Unrestricted_Access),
      Button2);

   --  Using user callback.
   --  The callback is not destroyed when Button2 is destroyed. As a result,
   --  the second time you press button1, you get a critical error.
   Gtk_New_Hbox (Hbox);
   Pack_Start (Vbox, Hbox);
   Gtk_New (Button1, "button1, user data (will crash)");
   Pack_Start (Hbox, Button1);
   Gtk_New (Button2, "button2");
   Pack_Start (Hbox, Button2);

   Id := User_Callback.Connect
     (Button1, "clicked",
      User_Callback.To_Marshaller (My_Destroy2'Unrestricted_Access),
      Gtk_Widget (Button2));

   --  Using user callback, with complex protection
   --  Note that memory allocated for Data3 is not freed.
   --  The callback makes sure that the callback is properly unregistered, but
   --  is heavy to put in place
   Gtk_New_Hbox (Hbox);
   Pack_Start (Vbox, Hbox);
   Gtk_New (Button1, "button1, protected user data");
   Pack_Start (Hbox, Button1);
   Gtk_New (Button2, "button2");
   Pack_Start (Hbox, Button2);

   Data3 := new My_Data3'(Object => Gtk_Widget (Button1),
                          Button => Gtk_Widget (Button2),
                          Id     => (Null_Handler_Id, null));
   Id := User_Callback3.Connect
     (Button1, "clicked",
      User_Callback3.To_Marshaller (My_Destroy3'Unrestricted_Access),
      Data3);
   Data3.Id := Id;

   --  Using user callback, simple protection.
   --  This is the same example as 2, but we automatically register the fact
   --  that when button2 is destroyed, the callback should also be destroyed.
   Gtk_New_Hbox (Hbox);
   Pack_Start (Vbox, Hbox);
   Gtk_New (Button1, "button1, watch user data");
   Pack_Start (Hbox, Button1);
   Gtk_New (Button2, "button2");
   Pack_Start (Hbox, Button2);

   Id := User_Callback.Connect
     (Button1, "clicked",
      User_Callback.To_Marshaller (My_Destroy2'Unrestricted_Access),
      Gtk_Widget (Button2));
   Add_Watch (Id, Button2);

   Show_All (Win);
   Gtk.Main.Main;
end Test_Handlers;
