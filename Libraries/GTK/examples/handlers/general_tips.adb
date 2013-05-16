
--  See own_marshallers.ads for an explanation on this package.
--
--  This demonstrates how to use the general form of handlers

with Own_Marshaller;
with Gtk.Tips_Query;    use Gtk.Tips_Query;

procedure General_Tips is
   Tips : Gtk_Tips_Query;
begin

   --  Should create an application
   --  Should also create Tips.

   --  Connect to the general handler
   Own_Marshaller.Tips_Handlers.Connect
     (Tips, "widget_entered",
      Own_Marshaller.My_General_Tips'Access);

end General_Tips;
