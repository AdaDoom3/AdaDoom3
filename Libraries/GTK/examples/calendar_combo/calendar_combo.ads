
--  This file shows how a special kind of combo box can be implemented.
--  In this example, we create a combo box that contains a calendar, and
--  that allows the user to easily select a date.

with Gtk.Extra.Combo_Button; use Gtk.Extra.Combo_Button;
with Gtk.Calendar;        use Gtk.Calendar;
with Gtk.Label;           use Gtk.Label;

package Calendar_Combo is

   type Gtk_Calendar_Combo_Record is new Gtk_Combo_Button_Record with private;
   type Gtk_Calendar_Combo is access all Gtk_Calendar_Combo_Record'Class;

   procedure Gtk_New (Calendar : out Gtk_Calendar_Combo);
   procedure Initialize (Calendar : access Gtk_Calendar_Combo_Record'Class);

private
   type Gtk_Calendar_Combo_Record is new Gtk_Combo_Button_Record
     with record
        Cal   : Gtk_Calendar;
        Label : Gtk_Label;
     end record;
end Calendar_Combo;

