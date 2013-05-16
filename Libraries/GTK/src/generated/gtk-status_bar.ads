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
--  A status bar is a special widget in which you can display messages. This
--  type of widget is generally found at the bottom of application windows, and
--  is used to display help or error messages.
--
--  This widget works as a stack of messages, ie all older messages are kept
--  when a new one is inserted. It is of course possible to remove the most
--  recent message from the stack. This stack behavior is especially useful
--  when messages can be displayed from several places in your application.
--  Thus, each one subprogram that needs to print a message can simply push it
--  on the stack, and does not need to make sure that the user has had enough
--  time to read the previous message (a timeout can be set to automatically
--  remove the message after a specific delay)
--
--  Each message is associated with a specific Context_Id. Each of this
--  context can have a special name, and these context can be used to organize
--  the messages into categories (for instance one for help messages and one
--  for error messages). You can then selectively remove the most recent
--  message of each category.
--
--  </description>
--  <screenshot>gtk-status_bar</screenshot>
--  <group>Display widgets</group>
--  <testgtk>create_status.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                 use Glib;
with Glib.GSlist;          use Glib.GSlist;
with Glib.Properties;      use Glib.Properties;
with Glib.Types;           use Glib.Types;
with Gtk.Box;              use Gtk.Box;
with Gtk.Buildable;        use Gtk.Buildable;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Orientable;       use Gtk.Orientable;
with Gtk.Widget;           use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Gtk.Status_Bar is

   type Gtk_Status_Bar_Record is new Gtk_Hbox_Record with null record;
   type Gtk_Status_Bar is access all Gtk_Status_Bar_Record'Class;

   type Context_Id is new Guint;
   type Message_Id is new Guint;

   type Status_Bar_Msg is record
      Text    : Interfaces.C.Strings.chars_ptr;
      Context : Context_Id;
      Message : Message_Id;
   end record;
   --  A message from the queue. Each of this message is associated with a
   --  specific context, and has a specific number.

   --  <no_doc>
   function Convert (Msg : Status_Bar_Msg) return System.Address;
   function Convert (Msg : System.Address) return Status_Bar_Msg;
   package Messages_List is new Glib.GSlist.Generic_SList (Status_Bar_Msg);
   --  </no_doc>

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Statusbar : out Gtk_Status_Bar);
   procedure Initialize (Statusbar : access Gtk_Status_Bar_Record'Class);
   --  Creates a new Gtk.Status_Bar.Gtk_Status_Bar ready for messages.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_statusbar_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Context_Id
      (Statusbar           : access Gtk_Status_Bar_Record;
       Context_Description : UTF8_String) return Context_Id;
   --  Returns a new context identifier, given a description of the actual
   --  context. Note that the description is <emphasis>not</emphasis> shown in
   --  the UI.
   --  "context_description": textual description of what context the new
   --  message is being used in

   function Get_Has_Resize_Grip
      (Statusbar : access Gtk_Status_Bar_Record) return Boolean;
   procedure Set_Has_Resize_Grip
      (Statusbar : access Gtk_Status_Bar_Record;
       Setting   : Boolean);
   --  Sets whether the statusbar has a resize grip. True by default.
   --  "setting": True to have a resize grip

   function Get_Message_Area
      (Statusbar : access Gtk_Status_Bar_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the box containing the label widget.
   --  Since: gtk+ 2.20

   procedure Pop
      (Statusbar : access Gtk_Status_Bar_Record;
       Context   : Context_Id);
   --  Removes the first message in the GtkStatusBar's stack with the given
   --  context id. Note that this may not change the displayed message, if the
   --  message at the top of the stack has a different context id.
   --  "context": a context identifier

   function Push
      (Statusbar : access Gtk_Status_Bar_Record;
       Context   : Context_Id;
       Text      : UTF8_String) return Message_Id;
   --  Pushes a new message onto a statusbar's stack. Gtk.Status_Bar.Remove.
   --  "context": the message's context id, as returned by
   --  Gtk.Status_Bar.Get_Context_Id
   --  "text": the message to add to the statusbar

   procedure Remove
      (Statusbar : access Gtk_Status_Bar_Record;
       Context   : Context_Id;
       Message   : Message_Id);
   --  Forces the removal of a message from a statusbar's stack. The exact
   --  Context_Id and Message_Id must be specified.
   --  "context": a context identifier
   --  "Message": a message identifier, as returned by Gtk.Status_Bar.Push

   procedure Remove_All
      (Statusbar : access Gtk_Status_Bar_Record;
       Context   : Context_Id);
   --  Forces the removal of all messages from a statusbar's stack with the
   --  exact Context_Id.
   --  Since: gtk+ 2.22
   --  "context": a context identifier

   ------------
   -- Fields --
   ------------

   function Get_Messages
      (Statusbar : access Gtk_Status_Bar_Record)
       return Gtk.Status_Bar.Messages_List.GSlist;

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Status_Bar_Record) return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : access Gtk_Status_Bar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Status_Bar_Record, Gtk_Status_Bar);
   function "+"
     (Widget : access Gtk_Status_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Status_Bar
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Status_Bar_Record, Gtk_Status_Bar);
   function "+"
     (Widget : access Gtk_Status_Bar_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Status_Bar
   renames Implements_Orientable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Has_Resize_Grip_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether the statusbar has a grip for resizing the toplevel window.

   Has_Resize_Grip_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "text-popped"
   --     procedure Handler
   --       (Self    : access Gtk_Status_Bar_Record'Class;
   --        Context : Context_Id;
   --        Text    : UTF8_String);
   --    --  "context": the context id of the relevant message/statusbar.
   --    --  "text": the message that was just popped.
   --  Is emitted whenever a new message is popped off a statusbar's stack.
   --
   --  "text-pushed"
   --     procedure Handler
   --       (Self    : access Gtk_Status_Bar_Record'Class;
   --        Context : Context_Id;
   --        Text    : UTF8_String);
   --    --  "context": the context id of the relevant message/statusbar.
   --    --  "text": the message that was pushed.
   --  Is emitted whenever a new message gets pushed onto a statusbar's stack.

   Signal_Text_Popped : constant Glib.Signal_Name := "text-popped";
   Signal_Text_Pushed : constant Glib.Signal_Name := "text-pushed";

private
   Has_Resize_Grip_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-resize-grip");
end Gtk.Status_Bar;
