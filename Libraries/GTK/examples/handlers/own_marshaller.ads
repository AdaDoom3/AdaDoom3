
--  This small examples show how you can create your own marshallers when
--  you want to create a handler for a signal that has no predefined
--  marshaller.
--
--  NOTE: you should already be quite familiar with how handlers and
--  marshallers work in GtkAda before trying to understand this example.
--
--  Most of the time, in this case, you should simply create a general
--  handler such as defined in the generic packages in Gtk.Handlers, and
--  as demonstrated in the first example below.
--  In that case, the conversion from C's System.Address types to your own
--  types is done directly in your handler.
--
--  In other cases, for instance when you reuse the same signal multiple
--  times, it might be cleaner to define a marshaller, so that your handlers
--  can have typed parameters. This is the second example demonstrated
--  below.
--
--  This example interfaces to a signal defined in the gtktipsquery.h in the
--  gtk+ distribution. The signal is called "widget_entered", and is called to
--  display a tooltip every time the pointer enteres a new widget
--  The general form for handlers, as defined in the C package is:
--
--    void  (*widget_entered) (GtkTipsQuery   *tips_query,
--                             GtkWidget      *widget,
--                             const gchar    *tip_text,
--                             const gchar    *tip_private);
--
--  There is no predefined marshaller for it in gtk-marshallers.ads

with Gtk.Arguments;
with Gtk.Handlers;
with Gtk.Marshallers;
with Gtk.Tips_Query; use Gtk.Tips_Query;
with Gtk.Widget;     use Gtk.Widget;

package Own_Marshaller is

   package Tips_Handlers is new Gtk.Handlers.Callback
     (Widget_Type => Gtk.Tips_Query.Gtk_Tips_Query_Record);

   --------------------
   -- First solution --
   --------------------

   --  First and simpler solution: use the general form for handlers, as
   --  defined in gtk-handlers.
   --  This does not require anything special, and the conversion of
   --  arguments will have to be done in the handler itself.
   --
   --  Example of use:
   --     see general_tips.adb

   procedure My_General_Tips (Tips   : access Gtk_Tips_Query_Record'Class;
                              Params : Gtk.Arguments.Gtk_Args);


   ---------------------
   -- Second solution --
   ---------------------

   --  If you want your handlers to directly have the
   --  correct number of parameters (and correctly typed), you have to
   --  provide your own marshallers. The subprogram below is the correct
   --  form for the handler, and the package implements the marshaller.
   --
   --  This solution is more work, but on the other hand is easier to reuse
   --  when the package below has been written.
   --
   --  Example of use:
   --     see specific_tips.adb

   procedure My_Specific_Tips (Tips   : access Gtk_Tips_Query_Record'Class;
                               Widget : access Gtk_Widget_Record'Class;
                               Text   : String;
                               Tips_P : String);

   package My_Tips_Marshaller_Pkg is
      type Handler is access
        procedure (Tips   : access Gtk_Tips_Query_Record'Class;
                   Widget : access Gtk_Widget_Record'Class;
                   Text   : String;
                   Tips_P : String);
      procedure My_Tips_Marshaller (Tips   : access Gtk_Tips_Query_Record'Class;
                                    Params : Gtk.Arguments.Gtk_Args;
                                    Cb     : Gtk.Marshallers.General_Handler);
      function To_Marshaller (Cb : Handler)
                             return Tips_Handlers.Marshallers.Marshaller;
   end My_Tips_Marshaller_Pkg;

end Own_Marshaller;
