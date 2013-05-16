
with Gtk.Marshallers;  use Gtk.Marshallers;
with Unchecked_Conversion;

package body Own_Marshaller is

   ---------------------
   -- My_General_Tips --
   ---------------------

   procedure My_General_Tips (Tips   : access Gtk_Tips_Query_Record'Class;
                              Params : Gtk.Arguments.Gtk_Args)
   is
      --  Have to do the conversion by hand
      Widget : Gtk_Widget := Gtk_Widget (Gtk.Arguments.To_Object (Params, 1));
      Text   : String     := Gtk.Arguments.To_String (Params, 2);
      Tips_P : String     := Gtk.Arguments.To_String (Params, 3);

   begin
      null;  --  Whatever you want here
   end My_General_Tips;


   ----------------------
   -- My_Specific_Tips --
   ----------------------

   procedure My_Specific_Tips (Tips   : access Gtk_Tips_Query_Record'Class;
                               Widget : access Gtk_Widget_Record'Class;
                               Text   : String;
                               Tips_P : String)
   is
   begin
      null;  --  Whatever you want here
   end My_Specific_Tips;

   ----------------------------
   -- My_Tips_Marshaller_Pkg --
   ----------------------------

   package body My_Tips_Marshaller_Pkg is

      function To_Handler is new Unchecked_Conversion
        (Gtk.Marshallers.General_Handler, Handler);
      function To_General_Handler is new Unchecked_Conversion
        (Handler, Gtk.Marshallers.General_Handler);

      ------------------------
      -- My_Tips_Marshaller --
      ------------------------

      procedure My_Tips_Marshaller (Tips   : access Gtk_Tips_Query_Record'Class;
                                    Params : Gtk.Arguments.Gtk_Args;
                                    Cb     : Gtk.Marshallers.General_Handler)
      is
         My_Cb : Handler := To_Handler (Cb);
      begin
         --  Basically, we do the same thing as in My_General_Tips above
         My_Cb (Tips,
                Gtk_Widget (Gtk.Arguments.To_Object (Params, 1)),
                Gtk.Arguments.To_String (Params, 2),
                Gtk.Arguments.To_String (Params, 3));
      end My_Tips_Marshaller;

      -------------------
      -- To_Marshaller --
      -------------------

      function To_Marshaller (Cb : Handler)
                             return Tips_Handlers.Marshallers.Marshaller
      is
      begin
         return (Func  => To_General_Handler (Cb),
                 Proxy => My_Tips_Marshaller'Access);
      end To_Marshaller;

   end My_Tips_Marshaller_Pkg;

end Own_Marshaller;
