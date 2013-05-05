--  This example shows how you create contextual menus with the third mouse
--  button.

with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Menu; use Gtk.Menu;
with Gdk.Event; use Gdk.Event;
with Glib; use Glib;
with Gtk.Window; use Gtk.Window;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Main; use Gtk.Main;

procedure Contextual is

   package Menu_Cb is new Gtk.Handlers.Return_Callback
     (Widget_Type => Gtk_Menu_Record,  Return_Type => Boolean);

   function Popup_Menu_Handler
     (Menu  : access Gtk_Menu_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean is
   begin
      if Gdk.Event.Get_Event_Type (Event) = Button_Press
        and then Gdk.Event.Get_Button (Event) = 3
      then
         Popup (Menu,
                Button        => Gdk.Event.Get_Button (Event),
                Activate_Time => Gdk.Event.Get_Time (Event));
      end if;

      return False;
   end Popup_Menu_Handler;


   Menu  : Gtk_Menu;
   Win   : Gtk_Window;
   Item  : Gtk_Menu_Item;
begin
   Gtk.Main.Init;

   --  create the menu as usual
   Gtk_New (Menu);
   Gtk_New (Item, "Item1");
   Append (Menu, Item);
   Show (Item);

   --  create the widget on which you want a contextual menu
   --  Prepares it to receive button_press events
   Gtk_New (Win, Window_Toplevel);
   Set_Events (Win, Button_Press_Mask);

   --  Finally, connect both:
   Menu_Cb.Object_Connect
      (Win, "button_press_event",
       Menu_Cb.To_Marshaller (Popup_Menu_Handler'Access),
       Slot_Object => Menu);

   Show_All (Win);
   Gtk.Main.Main;
end Contextual;
