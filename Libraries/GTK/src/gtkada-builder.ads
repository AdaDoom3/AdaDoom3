-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2011-2013, AdaCore               --
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
--
--  This package provides a high-level API for using Gtk.Builder and
--  user interface files produced with the GUI builder glade-3.
--
--  Here is how to use this package:
--
--     Step 1: create a Builder and add the XML data, just as you would a
--             standard Gtk.Builder:
--
--             declare
--                Builder : Gtkada_Builder;
--                Error   : GError;
--             begin
--                Gtk_New (Builder);
--                Error := Add_From_File (Builder, Default_Filename);
--
--     Step 2: add calls to "Register_Handler" to associate your handlers
--             with your callbacks.
--
--                Register_Handler
--                   (Builder      => Builder,
--                    Handler_Name => "my_handler_id",
--                    Handler      => My_Handler'Access);
--
--             Where:
--              - Builder is your Gtkada_Builder,
--              - "my_handler_id" is the name of the handler as specified in
--                  Glade-3, in the "Handler" column of the "Signals" tab for
--                  your object,
--              - Handler is your Ada subprogram.
--
--              You will need one call to "Register_Handler" per handler
--              declared in the Glade-3 interface. If there is one or more
--              handler declared in Glade-3 which does not have an associated
--              call to Register_Handler, an ASSERT_FAILURE will be raised by
--              the Gtk main loop.
--
--              There are multiple way to call Register_Handler, see below.
--
--     Step 3: call Do_Connect.
--
--     Step 4: when the application terminates or all Windows described through
--             your builder should be closed, call Unref to free memory
--             associated with the Builder.
--
--  </description>
--  <group>GUI Builder</group>

pragma Ada_2005;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Glib.Object; use Glib.Object;
with Gtk.Builder;

package Gtkada.Builder is

   type Gtkada_Builder_Record is new
     Gtk.Builder.Gtk_Builder_Record with private;
   type Gtkada_Builder is access all Gtkada_Builder_Record'Class;

   procedure Gtk_New (Builder : out Gtkada_Builder);
   procedure Initialize (Builder : access Gtkada_Builder_Record'Class);
   --  Create a new Gtkada_Builder.

   procedure Do_Connect (Builder : access Gtkada_Builder_Record'Class);
   --  Activate in the builder callabacks that have been connected using
   --  calls to Register_Handler functions below.

   --------------------------------------
   -- Callbacks working on the Builder --
   --------------------------------------

   --  These callbacks take as parameter the Gtkada_Builder.
   --  If a "User data" is present in the Glade-3, it will be ignored.

   type Builder_Handler is access procedure
     (Builder : access Gtkada_Builder_Record'Class);

   type Builder_Return_Handler is access function
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;

   procedure Register_Handler
     (Builder      : access Gtkada_Builder_Record'Class;
      Handler_Name : String;
      Handler      : Builder_Handler);

   procedure Register_Handler
     (Builder      : access Gtkada_Builder_Record'Class;
      Handler_Name : String;
      Handler      : Builder_Return_Handler);

   --------------------------------------------------------------
   -- Callbacks working on user data specified through Glade-3 --
   --------------------------------------------------------------

   --  Use these registry functions if your signal handler was defined in
   --  the Glade-3 interface with a "User data". The parameter User_Data
   --  passed to the handlers corresponds to the object entered in the
   --  "User data" column in Glade-3.

   type Object_Handler is access procedure
     (User_Data : access GObject_Record'Class);

   type Object_Return_Handler is access function
     (User_Data : access GObject_Record'Class) return Boolean;

   procedure Register_Handler
     (Builder      : access Gtkada_Builder_Record'Class;
      Handler_Name : String;
      Handler      : Object_Handler);

   procedure Register_Handler
     (Builder      : access Gtkada_Builder_Record'Class;
      Handler_Name : String;
      Handler      : Object_Return_Handler);

private

   type Handler_Type is (Object, Object_Return, Builder, Builder_Return);
   type Universal_Marshaller (T : Handler_Type) is record
      case T is
         when Object =>
            The_Object_Handler : Object_Handler;
         when Object_Return =>
            The_Object_Return_Handler : Object_Return_Handler;
         when Builder =>
            The_Builder_Handler : Builder_Handler;
         when Builder_Return =>
            The_Builder_Return_Handler : Builder_Return_Handler;
      end case;
   end record;

   type Universal_Marshaller_Access is access Universal_Marshaller;

   package Handlers_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Universal_Marshaller_Access,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type Gtkada_Builder_Record is new
     Gtk.Builder.Gtk_Builder_Record
   with record
      Handlers : Handlers_Map.Map;
   end record;

end Gtkada.Builder;
