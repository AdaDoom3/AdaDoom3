with Ada.Text_IO; use Ada.Text_IO;
with Glib;        use Glib;
with Glib.Error;  use Glib.Error;
with Gtk.Main;    use Gtk.Main;
with Gtk.Window;  use Gtk.Window;
with Gtk.Enums;   use Gtk.Enums;
with Gdk.Cursor;  use Gdk.Cursor;
with Gdk.Color;   use Gdk.Color;
with Gdk.Display; use Gdk.Display;
with Gdk.Window;  use Gdk.Window;
with Gdk.Pixbuf;  use Gdk.Pixbuf;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Widget;  use Gtk.Widget;

procedure Cursor is
   Win : Gtk_Window;
   Pix : Gdk_Pixbuf;
   Error : GError;

   procedure On_Realize (Win : access Gtk_Widget_Record'Class) is
      Cursor : Gdk_Cursor;
   begin
      Gdk_New_From_Pixbuf (Cursor, Pixbuf => Pix, X => 0, Y => 0);
      Set_Cursor (Get_Window (Win), Cursor);
   end On_Realize;

   W, H : Guint;
begin
   Init;
   Gtk_New (Win, Window_Toplevel);

   Gdk_New_From_File (Pix, "cursor.png", Error);
   if Error /= null then
      Put_Line ("Error after loading: " & Get_Message (Error));
   end if;

   Widget_Callback.Connect (Win, "realize", On_Realize'Unrestricted_Access);

   if not Supports_Cursor_Color (Gdk.Display.Get_Default) then
      Put_Line ("Display does not support color cursors");
   end if;
   if not Supports_Cursor_Alpha (Gdk.Display.Get_Default) then
      Put_Line ("Display does not support cursors with alpha channels");
   end if;

   Put_Line
     ("Default cursor size: "
      & Guint'Image (Get_Default_Cursor_Size (Gdk.Display.Get_Default)));

   Get_Maximal_Cursor_Size (Gdk.Display.Get_Default, W, H);
   Put_Line
     ("Maximal cursor size: " & Guint'Image (W) & "x" & Guint'Image (H));

   Show_All (Win);
   Main;
end Cursor;
