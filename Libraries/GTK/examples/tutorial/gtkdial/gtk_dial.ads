with Glib; use Glib;
with Gtk.Adjustment, Gtk.Main;
with Gtk.Event_Box;
with Gtk.Enums, Gtk.Handlers;

package Gtk_Dial is

   use Gtk.Adjustment, Gtk.Main, Gtk.Enums;

   type Gtk_Dial_Record is new Gtk.Event_Box.Gtk_Event_Box_Record with private;
   type Gtk_Dial is access all Gtk_Dial_Record'Class;

   procedure Gtk_New (Dial : out Gtk_Dial; Adjustment : Gtk_Adjustment);

   procedure Initialize
     (Dial : access Gtk_Dial_Record'class;
      Adjustment : Gtk_Adjustment);

   function Get_Adjustment (Dial : access Gtk_Dial_Record)
     return Gtk_Adjustment;

   procedure Set_Update_Policy
     (Dial   : access Gtk_Dial_Record;
      Policy : Gtk_Update_Type);

   procedure Set_Adjustment
     (Dial : access Gtk_Dial_Record; Adjustment : Gtk_Adjustment);

private

   type Gtk_Dial_Record is new Gtk.Event_Box.Gtk_Event_Box_Record with record
      Policy : Gtk_Update_Type := Update_Continuous;
      --  Update Policy (Update_[Continuous/Delayed/Discontinuous])

      Button : Guint := 0;
      --  Button currently pressed or 0 if none

      Radius        : Gint := 0;
      Pointer_Width : Gint := 0;
      --  Dimensions of dial components

      Timer : Timeout_Handler_Id := 0;
      --  ID of update timer, or 0 if none

      Angle : Gdouble := 0.0;
      Last_Angle : Gdouble;
      --  Current angle

      Old_Value : Gdouble := 0.0;
      Old_Lower : Gdouble := 0.0;
      Old_Upper : Gdouble := 0.0;
      --  Old values from adjustment stored so we know when something changes

      Adjustment : Gtk_Adjustment;
      --  The adjustment object that stores the data for this dial.

      Changed_Id, Value_Changed_Id : Gtk.Handlers.Handler_Id;
      --  Handler Id of signals connected to Adjustment.
   end record;
end Gtk_Dial;
