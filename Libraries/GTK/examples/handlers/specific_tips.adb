
--  See own_marshallers.ads for an explanation on this package.
--
--  This demonstrates how to use the new Marshallers we created.

with Own_Marshaller;
with Gtk.Tips_Query;    use Gtk.Tips_Query;

procedure Specific_Tips is
   Tips : Gtk_Tips_Query;
begin

   --  Should create an application
   --  Should also create Tips.

   Own_Marshaller.Tips_Handlers.Connect
     (Tips, "widget_entered",
      Own_Marshaller.My_Tips_Marshaller_Pkg.To_Marshaller
      (Own_Marshaller.My_Specific_Tips'Access));

end Specific_Tips;
