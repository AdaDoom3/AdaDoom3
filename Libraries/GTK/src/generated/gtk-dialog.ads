-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  Dialog boxes are a convenient way to prompt the user for a small amount of
--  input, eg. to display a message, ask a question, or anything else that does
--  not require extensive effort on the user's part.
--
--  Gtkada treats a dialog as a window split horizontally. The top section is
--  a Gtk_Vbox, and is where widgets such as a Gtk_Label or a Gtk_Entry should
--  be packed. The second area is known as the action_area. This is generally
--  used for packing buttons into the dialog which may perform functions such
--  as cancel, ok, or apply. The two areas are separated by a Gtk_Hseparator.
--
--  If 'dialog' is a newly created dialog, the two primary areas of the window
--  can be accessed using Get_Vbox and Get_Action_Area as can be seen from the
--  example, below.
--
--  A 'modal' dialog (that is, one which freezes the rest of the application
--  from user input), can be created by calling Set_Modal on the dialog.
--
--  See Gtkada.Dialogs for a higher level dialog interface.
--
--  </description>
--  <screenshot>gtk-dialog</screenshot>
--  <group>Windows</group>
--  <testgtk>create_dialog.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Box;         use Gtk.Box;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Window;      use Gtk.Window;

package Gtk.Dialog is

   type Gtk_Dialog_Record is new Gtk_Window_Record with null record;
   type Gtk_Dialog is access all Gtk_Dialog_Record'Class;

   type Gtk_Dialog_Flags is mod 8;
   for Gtk_Dialog_Flags'Size use Gint'Size;
   pragma Convention (C, Gtk_Dialog_Flags);
   Modal               : constant Gtk_Dialog_Flags := 2 ** 0;
   Destroy_With_Parent : constant Gtk_Dialog_Flags := 2 ** 1;
   No_Separator        : constant Gtk_Dialog_Flags := 2 ** 2;
   --  Various flags that can be set for the dialog, with the following
   --  implications:
   --     - Modal : the dialog is modal, see Gtk.Window.Set_Modal
   --     - Destroy_With_Parent: The dialog is destroyed if its parent is
   --       destroyed. See Gtk.Window.Set_Destroy_With_Parent
   --     - No_Separator: No separator bar above the buttons.

   type Gtk_Response_Type is new Gint;
   --  Type used for Response_Id's.
   --  Positive values are totally user-interpreted.
   --  GtkAda will sometimes return Gtk_Response_None if no Response_Id is
   --  available.
   --
   --  Typical usage is:
   --    if Gtk.Dialog.Run (Dialog) = Gtk_Response_Accept then
   --       blah;
   --    end if;

   Gtk_Response_None : constant Gtk_Response_Type := -1;
   --  GtkAda returns this if a response widget has no Response_Id,
   --  or if the dialog gets programmatically hidden or destroyed.

   Gtk_Response_Reject : constant Gtk_Response_Type := -2;
   Gtk_Response_Accept : constant Gtk_Response_Type := -3;
   --  GtkAda won't return these unless you pass them in
   --  as the response for an action widget. They are
   --  for your convenience.

   Gtk_Response_Delete_Event : constant Gtk_Response_Type := -4;
   --  If the dialog is deleted through the button in the titlebar

   Gtk_Response_OK     : constant Gtk_Response_Type := -5;
   Gtk_Response_Cancel : constant Gtk_Response_Type := -6;
   Gtk_Response_Close  : constant Gtk_Response_Type := -7;
   Gtk_Response_Yes    : constant Gtk_Response_Type := -8;
   Gtk_Response_No     : constant Gtk_Response_Type := -9;
   Gtk_Response_Apply  : constant Gtk_Response_Type := -10;
   Gtk_Response_Help   : constant Gtk_Response_Type := -11;
   --  These are returned from dialogs, and you can also use them
   --  yourself if you like.

   type Response_Type_Array is array (Natural range <>) of Gtk_Response_Type;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Dialog : out Gtk_Dialog);
   procedure Initialize (Dialog : access Gtk_Dialog_Record'Class);

   procedure Gtk_New
      (Dialog : out Gtk_Dialog;
       Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags);
   procedure Initialize
      (Dialog : access Gtk_Dialog_Record'Class;
       Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags);
   --  Create a new dialog with a specific title, and specific attributes.
   --  Parent is the transient parent for the dialog (ie the one that is used
   --  for reference for the flag Destroy_With_Parent, or to compute the
   --  initial position of the dialog).
   --  Since: gtk+ GtkAda 1.0

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Action_Widget
      (Dialog      : access Gtk_Dialog_Record;
       Child       : access Gtk.Widget.Gtk_Widget_Record'Class;
       Response_Id : Gtk_Response_Type);
   --  Adds an activatable widget to the action area of a
   --  Gtk.Dialog.Gtk_Dialog, connecting a signal handler that will emit the
   --  Gtk.Dialog.Gtk_Dialog::response signal on the dialog when the widget is
   --  activated. The widget is appended to the end of the dialog's action
   --  area. If you want to add a non-activatable widget, simply pack it into
   --  the Action_Area field of the Gtk.Dialog.Gtk_Dialog struct.
   --  "child": an activatable widget
   --  "response_id": response ID for Child

   function Add_Button
      (Dialog      : access Gtk_Dialog_Record;
       Text        : UTF8_String;
       Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget;
   --  Adds a button with the given text (or a stock button, if Button_Text is
   --  a stock ID) and sets things up so that clicking the button will emit the
   --  Gtk.Dialog.Gtk_Dialog::response signal with the given Response_Id. The
   --  button is appended to the end of the dialog's action area. The button
   --  widget is returned, but usually you don't need it.
   --  "text": text of button, or stock ID
   --  "response_id": response ID for the button

   function Get_Action_Area
      (Dialog : access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box;
   --  Returns the action area of Dialog.
   --  Since: gtk+ 2.14

   function Get_Content_Area
      (Dialog : access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box;
   --  Returns the content area of Dialog.
   --  Since: gtk+ 2.14

   function Get_Has_Separator
      (Dialog : access Gtk_Dialog_Record) return Boolean;
   pragma Obsolescent (Get_Has_Separator);
   procedure Set_Has_Separator
      (Dialog  : access Gtk_Dialog_Record;
       Setting : Boolean);
   pragma Obsolescent (Set_Has_Separator);
   --  Sets whether the dialog has a separator above the buttons.
   --  Deprecated since 2.22, This function will be removed in GTK+ 3
   --  "setting": True to have a separator

   function Get_Response_For_Widget
      (Dialog : access Gtk_Dialog_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Response_Type;
   --  Gets the response id of a widget in the action area of a dialog. if
   --  Widget doesn't have a response id set.
   --  Since: gtk+ 2.8
   --  "widget": a widget in the action area of Dialog

   function Get_Widget_For_Response
      (Dialog      : access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget;
   --  Gets the widget button that uses the given response ID in the action
   --  area of a dialog.
   --  Since: gtk+ 2.20
   --  "response_id": the response ID used by the Dialog widget

   procedure Response
      (Dialog      : access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type);
   --  Emits the Gtk.Dialog.Gtk_Dialog::response signal with the given
   --  response ID. Used to indicate that the user has responded to the dialog
   --  in some way; typically either you or Gtk.Dialog.Run will be monitoring
   --  the ::response signal and take appropriate action.
   --  "response_id": response ID

   function Run (Dialog : access Gtk_Dialog_Record) return Gtk_Response_Type;
   --  Blocks in a recursive main loop until the Dialog either emits the
   --  Gtk.Dialog.Gtk_Dialog::response signal, or is destroyed. If the dialog
   --  is destroyed during the call to Gtk.Dialog.Run, Gtk.Dialog.Run returns
   --  GTK_RESPONSE_NONE. Otherwise, it returns the response ID from the
   --  ::response signal emission. Before entering the recursive main loop,
   --  Gtk.Dialog.Run calls Gtk.Widget.Show on the dialog for you. Note that
   --  you still need to show any children of the dialog yourself. During
   --  Gtk.Dialog.Run, the default behavior of
   --  Gtk.Widget.Gtk_Widget::delete-event is disabled; if the dialog receives
   --  ::delete_event, it will not be destroyed as windows usually are, and
   --  Gtk.Dialog.Run will return GTK_RESPONSE_DELETE_EVENT. Also, during
   --  Gtk.Dialog.Run the dialog will be modal. You can force Gtk.Dialog.Run to
   --  return at any time by calling Gtk.Dialog.Response to emit the ::response
   --  signal. Destroying the dialog during Gtk.Dialog.Run is a very bad idea,
   --  because your post-run code won't know whether the dialog was destroyed
   --  or not. After Gtk.Dialog.Run returns, you are responsible for hiding or
   --  destroying the dialog if you wish to do so. Typical usage of this
   --  function might be: |[ gint result = gtk_dialog_run (GTK_DIALOG
   --  (dialog)); switch (result) { case GTK_RESPONSE_ACCEPT:
   --  do_application_specific_something (); break; default:
   --  do_nothing_since_dialog_was_cancelled (); break; } gtk_widget_destroy
   --  (dialog); ]| Note that even though the recursive main loop gives the
   --  effect of a modal dialog (it prevents the user from interacting with
   --  other windows in the same window group while the dialog is run),
   --  callbacks such as timeouts, IO channel watches, DND drops, etc,
   --  <emphasis>will</emphasis> be triggered during a Gtk.Dialog.Run call.

   procedure Set_Default_Response
      (Dialog      : access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type);
   --  Sets the last widget in the dialog's action area with the given
   --  Response_Id as the default widget for the dialog. Pressing "Enter"
   --  normally activates the default widget.
   --  "response_id": a response ID

   procedure Set_Response_Sensitive
      (Dialog      : access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type;
       Setting     : Boolean);
   --  Calls <literal>gtk_widget_set_sensitive (widget, Setting)</literal> for
   --  each widget in the dialog's action area with the given Response_Id. A
   --  convenient way to sensitize/desensitize dialog buttons.
   --  "response_id": a response ID
   --  "setting": True for sensitive

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Set_Alternative_Button_Order_From_Array
     (Dialog    : access Gtk_Dialog_Record;
      New_Order : Response_Type_Array);
   --  Sets an alternative button order. If the gtk-alternative-button-order
   --  setting is set to %TRUE, the dialog buttons are reordered according to
   --  the order of the response ids passed to this function.
   --
   --  By default, GTK+ dialogs use the button order advocated by the Gnome
   --  Human Interface Guidelines with the affirmative button at the far right,
   --  and the cancel button left of it. But the builtin GTK+ dialogs and
   --  message dialogs' do provide an alternative button order, which is more
   --  suitable on some platforms, e.g. Windows.
   --
   --  Use this function after adding all the buttons to your dialog.

   function Gtk_Alternative_Dialog_Button_Order
     (Screen : Gdk.Gdk_Screen := null)  return Boolean;
   --  Returns True if dialogs are expected to use an alternative button order
   --  on the given screen (or current screen if null) . See
   --  Set_Alternative_Button_Order_From_Array for more details about
   --  alternative button order.
   --
   --  If you need to use this function, you should probably connect to the
   --  ::notify:gtk-alternative-button-order signal on the Gtk_Settings object
   --  associated to Screen, in order to be notified if the button order
   --  setting changes.
   --
   --  Returns: Whether the alternative button order should be used

   ------------
   -- Fields --
   ------------

   function Get_Vbox
      (Dialog : access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Dialog_Record, Gtk_Dialog);
   function "+"
     (Widget : access Gtk_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Dialog
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Has_Separator_Property
   --  Type: Boolean
   --  Flags: read-write
   --  When True, the dialog has a separator bar above its buttons.

   Has_Separator_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "close"
   --     procedure Handler (Self : access Gtk_Dialog_Record'Class);
   --  The ::close signal is a <link linkend="keybinding-signals">keybinding
   --  signal</link> which gets emitted when the user uses a keybinding to
   --  close the dialog. The default binding for this signal is the Escape key.
   --
   --  "response"
   --     procedure Handler
   --       (Self        : access Gtk_Dialog_Record'Class;
   --        Response_Id : Gtk_Response_Type);
   --    --  "response_id": the response ID
   --  Emitted when an action widget is clicked, the dialog receives a delete
   --  event, or the application programmer calls Gtk.Dialog.Response. On a
   --  delete event, the response ID is GTK_RESPONSE_DELETE_EVENT. Otherwise,
   --  it depends on which action widget was clicked.

   Signal_Close : constant Glib.Signal_Name := "close";
   Signal_Response : constant Glib.Signal_Name := "response";

private
   Has_Separator_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-separator");
end Gtk.Dialog;
