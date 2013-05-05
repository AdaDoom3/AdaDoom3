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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;               use System;
with System.Assertions;    use System.Assertions;

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Glib;        use Glib;

with Gtk.Handlers;    use Gtk.Handlers;
with Gtkada.Handlers; use Gtkada.Handlers;

package body Gtkada.Builder is

   use Handlers_Map;

   package Builder_Callback is new Gtk.Handlers.Callback
     (Gtkada_Builder_Record);
   package Builder_Return_Callback is new Gtk.Handlers.Return_Callback
     (Gtkada_Builder_Record, Boolean);

   procedure Wrapper_Callback
     (C_Builder        : System.Address;
      C_Object         : System.Address;
      C_Signal_Name    : Interfaces.C.Strings.chars_ptr;
      C_Handler_Name   : Interfaces.C.Strings.chars_ptr;
      C_Connect_Object : System.Address;
      Flags            : Glib.G_Connect_Flags;
      User_Data        : System.Address);
   pragma Convention (C, Wrapper_Callback);
   --  Low-level subprogram to perform signal connections.

   procedure Connect
     (Handler_Name : String;
      Handler      : Universal_Marshaller;
      Base_Object  : GObject;
      Signal       : Glib.Signal_Name;
      After        : Boolean;
      The_Builder  : Gtkada_Builder;
      Slot_Object  : GObject);
   --  Connect object to handler

   procedure Free (Builder : access Gtkada_Builder_Record'Class);
   --  Called when the Builder is destroyed

   procedure On_Destroy
     (Data         : System.Address;
      Builder_Addr : System.Address);
   pragma Convention (C, On_Destroy);

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Handler_Name : String;
      Handler      : Universal_Marshaller;
      Base_Object  : GObject;
      Signal       : Glib.Signal_Name;
      After        : Boolean;
      The_Builder  : Gtkada_Builder;
      Slot_Object  : GObject) is
   begin
      --  Sanity checks

      case Handler.T is
         when Object | Object_Return =>
            if Slot_Object = null then
               Raise_Assert_Failure
                 ("Error when connecting handler """ & Handler_Name & """:"
                  & ASCII.LF
                  & " attempting to connect a callback of type """ &
                  Handler_Type'Image (Handler.T)
                  & """, but no User_Data was specified in glade-3");
            end if;

         when Builder | Builder_Return =>
            null;
      end case;

      --  Do the connect
      case Handler.T is
         when Object =>
            Object_Callback.Object_Connect
              (Widget      => Base_Object,
               Name        => Signal,
               Marsh       => Object_Callback.To_Marshaller
                 (Object_Callback.Marshallers.Void_Marshaller.Handler
                    (Handler.The_Object_Handler)),
               Slot_Object => Slot_Object,
               After       => After);

         when Object_Return =>
            Object_Return_Callback.Object_Connect
              (Widget      => Base_Object,
               Name        => Signal,
               Marsh       => Object_Return_Callback.To_Marshaller
                 (Object_Return_Callback.Marshallers.Void_Marshaller.Handler
                    (Handler.The_Object_Return_Handler)),
               Slot_Object => Slot_Object,
               After       => After);

         when Builder =>
            Builder_Callback.Object_Connect
              (Widget      => Base_Object,
               Name        => Signal,
               Marsh       => Builder_Callback.To_Marshaller
                 (Builder_Callback.Marshallers.Void_Marshaller.Handler
                    (Handler.The_Builder_Handler)),
               Slot_Object => The_Builder,
               After       => After);

         when Builder_Return =>
            Builder_Return_Callback.Object_Connect
              (Widget      => Base_Object,
               Name        => Signal,
               Marsh       => Builder_Return_Callback.To_Marshaller
                 (Builder_Return_Callback.Marshallers.Void_Marshaller.Handler
                    (Handler.The_Builder_Return_Handler)),
               Slot_Object => The_Builder,
               After       => After);

      end case;
   end Connect;

   ----------------------
   -- Wrapper_Callback --
   ----------------------

   procedure Wrapper_Callback
     (C_Builder        : System.Address;
      C_Object         : System.Address;
      C_Signal_Name    : Interfaces.C.Strings.chars_ptr;
      C_Handler_Name   : Interfaces.C.Strings.chars_ptr;
      C_Connect_Object : System.Address;
      Flags            : Glib.G_Connect_Flags;
      User_Data        : System.Address)
   is
      pragma Unreferenced (User_Data);
      Object      : constant GObject := Convert (C_Object);
      Signal_Name : constant String := Value (C_Signal_Name);
      After       : constant Boolean := (Flags and G_Connect_After) /= 0;
      Builder     : constant Gtkada_Builder :=
        Gtkada_Builder (Convert (C_Builder));

      The_Marshaller : Universal_Marshaller_Access;
      --  The universal marshaller

      Handler_Name  : constant String := Value (C_Handler_Name);

      C : Cursor;
   begin
      --  Find the marshaller corresponding to the handler name.

      C := Find (Builder.Handlers, To_Unbounded_String (Handler_Name));

      if C = No_Element then
         Raise_Assert_Failure
           ("Attempting to connect a callback to a handler ("""
            & Handler_Name
            & ")"" for which no callback has been registered.");
      end if;

      The_Marshaller := Element (C);

      --  Now do the actual connect

         Connect (Handler_Name => Handler_Name,
                  Handler     => The_Marshaller.all,
                  Base_Object => Object,
                  Signal      => Glib.Signal_Name (Signal_Name),
                  After       => After,
                  The_Builder => Builder,
                  Slot_Object => Convert (C_Connect_Object));

   end Wrapper_Callback;

   ----------------------
   -- Register_Handler --
   ----------------------

   procedure Register_Handler
     (Builder      : access Gtkada_Builder_Record'Class;
      Handler_Name : String;
      Handler      : Object_Handler)
   is
      Item : Universal_Marshaller_Access;
   begin
      Item := new Universal_Marshaller (Object);
      Item.The_Object_Handler := Handler;
      Insert
        (Builder.Handlers,
         Key      => To_Unbounded_String (Handler_Name),
         New_Item => Item);
   end Register_Handler;

   ----------------------
   -- Register_Handler --
   ----------------------

   procedure Register_Handler
     (Builder      : access Gtkada_Builder_Record'Class;
      Handler_Name : String;
      Handler      : Object_Return_Handler)
   is
      Item : Universal_Marshaller_Access;
   begin
      Item := new Universal_Marshaller (Object_Return);
      Item.The_Object_Return_Handler := Handler;
      Insert
        (Builder.Handlers,
         Key      => To_Unbounded_String (Handler_Name),
         New_Item => Item);
   end Register_Handler;

   ----------------------
   -- Register_Handler --
   ----------------------

   procedure Register_Handler
     (Builder      : access Gtkada_Builder_Record'Class;
      Handler_Name : String;
      Handler      : Builder_Handler)
   is
      Item : Universal_Marshaller_Access;
   begin
      Item := new Universal_Marshaller (Gtkada.Builder.Builder);
      Item.The_Builder_Handler := Handler;
      Insert
        (Builder.Handlers,
         Key      => To_Unbounded_String (Handler_Name),
         New_Item => Item);
   end Register_Handler;

   ----------------------
   -- Register_Handler --
   ----------------------

   procedure Register_Handler
     (Builder      : access Gtkada_Builder_Record'Class;
      Handler_Name : String;
      Handler      : Builder_Return_Handler)
   is
      Item : Universal_Marshaller_Access;
   begin
      Item := new Universal_Marshaller (Builder_Return);
      Item.The_Builder_Return_Handler := Handler;
      Insert
        (Builder.Handlers,
         Key      => To_Unbounded_String (Handler_Name),
         New_Item => Item);
   end Register_Handler;

   ----------------
   -- Do_Connect --
   ----------------

   procedure Do_Connect (Builder : access Gtkada_Builder_Record'Class) is
   begin
      Connect_Signals_Full
        (Builder,
         Wrapper_Callback'Access,
         User_Data => Glib.Object.Get_Object (Builder));
   end Do_Connect;

   ----------------
   -- On_Destroy --
   ----------------

   procedure Free (Builder : access Gtkada_Builder_Record'Class) is
      C : Cursor;
      E : Universal_Marshaller_Access;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Universal_Marshaller, Universal_Marshaller_Access);

   begin
      --  Free memory associated to handlers

      C := First (Builder.Handlers);

      while Has_Element (C) loop
         E := Element (C);
         Unchecked_Free (E);
         Next (C);
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Free;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Builder : out Gtkada_Builder) is
   begin
      Builder := new Gtkada_Builder_Record;
      Gtkada.Builder.Initialize (Builder);
   end Gtk_New;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Data         : System.Address;
      Builder_Addr : System.Address)
   is
      pragma Unreferenced (Data);

      Stub : Gtkada_Builder_Record;
      Builder : constant Gtkada_Builder := Gtkada_Builder
        (Get_User_Data (Builder_Addr, Stub));

   begin
      Free (Builder);
   end On_Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Builder : access Gtkada_Builder_Record'Class) is
   begin
      Gtk.Builder.Initialize (Builder);
      Weak_Ref (Builder, On_Destroy'Access);
   end Initialize;

end Gtkada.Builder;
