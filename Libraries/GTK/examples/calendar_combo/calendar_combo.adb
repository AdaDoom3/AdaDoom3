-------------------------------------------------------------------------
--    Copyright (C) 2000-2013, AdaCore                                 --
-------------------------------------------------------------------------

with Glib;        use Glib;
with Glib.Values; use Glib.Values;
with Gtk.Frame;   use Gtk.Frame;
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Button;  use Gtk.Button;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Unchecked_Conversion;
with System;
with Gtk.Handlers; use Gtk.Handlers;

package body Calendar_Combo is

   type Requisition_Access is access Gtk.Widget.Gtk_Requisition;
   function Convert is new Unchecked_Conversion
     (System.Address, Requisition_Access);

   package Cal_Cb is new Gtk.Handlers.Callback (Gtk_Calendar_Combo_Record);

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Widget      : access Gtk_Calendar_Combo_Record'Class;
      Params      : Glib.Values.GValues)
   is
      use type Gint;
      Requisition : Requisition_Access :=
        Convert (Get_Address (Nth (Params, 1)));
      Req_Arrow,
      Req_Button : Gtk_Requisition;
   begin
      Size_Request (Get_Button (Widget), Req_Button);
      Size_Request (Get_Toggle_Button (Widget), Req_Arrow);
      Requisition.Width := Req_Button.Width + Req_Arrow.Width;
      Requisition.Height := Gint'Max (Req_Button.Height, Req_Arrow.Height);

      --  Stop the signal from being propagated to the parent's default
      --  size_request function
      Emit_Stop_By_Name (Widget, "size_request");
   end Size_Request;

   -----------------
   -- Format_Date --
   -----------------

   function Format_Date (Cal : access Gtk_Calendar_Record'Class)
                        return String
   is
      Year, Month, Day : Guint;
   begin
      Get_Date (Cal, Year, Month, Day);
      return Guint'Image (Month + 1) & "/"
        & Guint'Image (Day) & "/"
        & Guint'Image (Year);
   end Format_Date;

   ------------------
   -- Day_Selected --
   ------------------

   procedure Day_Selected
     (Calendar : access Gtk_Calendar_Combo_Record'Class)
   is
   begin
      Set_Text (Calendar.Label, Format_Date (Calendar.Cal));

      --  We cannot hide the popdown window immediately here. Otherwise,
      --  gtk+ ends up calling day_selected over and over again, increasing
      --  the date every time (likely because it is trying to access a
      --  widget from the GtkCalendar, but the latter has been destroyed
      --  if we call Hide_Popdown_Window (Calendar)
   end Day_Selected;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Calendar : out Gtk_Calendar_Combo) is
   begin
      Calendar := new Gtk_Calendar_Combo_Record;
      Calendar_Combo.Initialize (Calendar);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Calendar : access Gtk_Calendar_Combo_Record'Class) is
   begin
      Gtk.Extra.Combo_Button.Initialize (Calendar);
      Gtk_New (Calendar.Cal);
      Show (Calendar.Cal);

      Gtk_New (Calendar.Label, Format_Date (Calendar.Cal));
      Add (Get_Button (Calendar), Calendar.Label);
      Show (Calendar.Label);
      Add (Get_Frame (Calendar), Calendar.Cal);

      Cal_Cb.Connect (Calendar, "size_request", Size_Request'Access);
      Cal_Cb.Object_Connect (Calendar.Cal, "day_selected",
                             Cal_Cb.To_Marshaller (Day_Selected'Access),
                             Slot_Object => Calendar);
   end Initialize;

end Calendar_Combo;
