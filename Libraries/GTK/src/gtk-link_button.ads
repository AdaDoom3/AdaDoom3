-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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
--  A Gtk_Link_Button is like a regular button with contents that
--  appear like a web browser hyperlink.  It is associated with a
--  URI.  The URI is passed to a callback procedure that is invoked
--  when the user clicks the button.  This widget also keeps track of
--  whether it has been clicked, and changes color after it has been
--  visited.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Buttons and Toggles</group>
--  <testgtk>create_link_buttons.adb</testgtk>

with Interfaces.C.Strings;

with Glib.Properties;
with Gtk.Button;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Link_Button is

   type Gtk_Link_Button_Record is new
     Gtk.Button.Gtk_Button_Record with private;
   type Gtk_Link_Button is access all Gtk_Link_Button_Record'Class;

   function Get_Type return GType;
   --  Return the internal value associated with a Gtk_Link_Button.

   function Get_Uri
     (Link_Button : access Gtk_Link_Button_Record) return String;
   --  Retrieves the URI set using Set_Uri.
   --  Return value: a valid URI.  The returned string is owned by the
   --  link button and should not be modified or freed.

   function Get_Visited
     (Link_Button : access Gtk_Link_Button_Record) return Boolean;
   --  Retrieves the 'visited' state of the URI where the Link_Button
   --  points. The button becomes visited when it is clicked. If the URI
   --  is changed on the button, the 'visited' state is unset again.
   --
   --  The state may also be changed using Set_Visited.
   --
   --  Return value: True if the link has been visited, False otherwise

   procedure Gtk_New (Widget : out Gtk_Link_Button; Uri : String);
   --  Creates a new Gtk_Link_Button_Record with the URI as its text.

   procedure Initialize
     (Widget : access Gtk_Link_Button_Record'Class; Uri : String);
   --  Creates a new Gtk_Link_Button_Record with the URI as its text.

   procedure Gtk_New_With_Label
     (Widget : out Gtk_Link_Button;
      Uri    : String;
      Label  : String);
   --  Creates a new Gtk_Link_Button_Record containing a label.

   procedure Initialize_With_Label
     (Widget : access Gtk_Link_Button_Record'Class;
      Uri    : String;
      Label  : String);
   --  Creates a new Gtk_Link_Button containing a label.

   procedure Set_Uri
     (Link_Button : access Gtk_Link_Button_Record;
      Uri         : String);
   --  Sets Uri as the URI where the Gtk_Link_Button points. As a side-effect
   --  this unsets the 'visited' state of the button.

   type Uri_Func is access procedure
     (Button : System.Address;
      Link   : Interfaces.C.Strings.chars_ptr;
      Data   : System.Address);
   pragma Convention (C, Uri_Func);
   --  This is a low-level callback function to be used with Set_Uri_Hook.
   --  See package Generic_Uri_Hook below for a higher-level interface.
   --
   --  You could convert the parameters to Ada types with the following
   --  declarations:
   --
   --     Stub        : Gtk_Link_Button_Record;
   --     Link_Button : constant Gtk_Link_Button :=
   --                     Gtk_Link_Button (Get_User_Data (Button, Stub));
   --     Link_String : constant String := Interfaces.C.Strings.Value (Link);
   --
   --  You'd also perform an appropriate conversion on Data using
   --  Ada.Unchecked_Conversion.

   function Set_Uri_Hook
     (Func    : Uri_Func;
      Data    : System.Address;
      Destroy : G_Destroy_Notify)
      return Uri_Func;
   --  Func: a function called each time a Gtk_Link_Button is clicked, or null
   --  Data: user data to be passed to Func, or null
   --  Destroy: called when Data is no longer needed, or null
   --
   --  Sets Func as the subprogram that should be invoked every time a user
   --  clicks a Gtk_Link_Button. This function is called before every
   --  callback registered for the "clicked" signal.
   --
   --  Returns the previously set hook function.

   generic
      type Data_Type (<>) is private;
   package Generic_Uri_Hook is
      type Uri_Handler is access procedure
        (Button    : access Gtk_Link_Button_Record'Class;
         Link      : UTF8_String;
         User_Data : Data_Type);
      --  A callback that is invoked when the user presses a hyperlink.

      type Destroy_Notify is access procedure (User_Data : in out Data_Type);
      --  Destroy_Notify is called just prior to the destruction of
      --  User_Data.

      procedure Set_Uri_Hook
        (Handler   : Uri_Handler;
         User_Data : Data_Type;
         Destroy   : Destroy_Notify);
      --  Sets Handler as the subprogram that should be invoked every time
      --  a user clicks a Gtk_Link_Button. This subprogram is called before
      --  every callback registered for the "clicked" signal.
      --
      --  If no uri hook has been set, GTK+ defaults to calling gtk_show_uri().
   end Generic_Uri_Hook;

   procedure Set_Visited
     (Link_Button : access Gtk_Link_Button_Record;
      Visited     : Boolean);
   --  Sets the 'visited' state of the URI where the #GtkLinkButton
   --  points.  See Get_Visited for more details.

   Uri_Property : constant Glib.Properties.Property_String;
   Visited_Property : constant Glib.Properties.Property_Boolean;

private

   type Gtk_Link_Button_Record is new
     Gtk.Button.Gtk_Button_Record with null record;

   Uri_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("uri");
   Visited_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visited");

   pragma Import (C, Get_Type, "gtk_link_button_get_type");

end Gtk.Link_Button;
