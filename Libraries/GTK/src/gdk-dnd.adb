-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                             AdaCore                               --
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

with System;

with Gdk;    use Gdk;
with Gtk;    use Gtk;

package body Gdk.Dnd is

   ----------------------
   -- Drag_Context_Ref --
   ----------------------

   procedure Drag_Context_Ref (Context : Drag_Context)
   is
      procedure Internal (Context : Drag_Context);
      pragma Import (C, Internal, "gdk_drag_context_ref");
   begin
      Internal (Context);
   end Drag_Context_Ref;

   ------------------------
   -- Drag_Context_Unref --
   ------------------------

   procedure Drag_Context_Unref (Context : Drag_Context)
   is
      procedure Internal (Context : Drag_Context);
      pragma Import (C, Internal, "gdk_drag_context_unref");
   begin
      Internal (Context);
   end Drag_Context_Unref;

   -----------------
   -- Drag_Status --
   -----------------

   procedure Drag_Status
     (Context : Drag_Context;
      Action  : Drag_Action;
      Time    : Guint32)
   is
      procedure Internal
        (Context : Drag_Context;
         Action  : Gint;
         Time    : Guint32);
      pragma Import (C, Internal, "gdk_drag_status");
   begin
      Internal (Context,
                Drag_Action'Pos (Action),
                Time);
   end Drag_Status;

   ----------------
   -- Drop_Reply --
   ----------------

   procedure Drop_Reply
     (Context : Drag_Context;
      Ok      : Boolean;
      Time    : Guint32)
   is
      procedure Internal
        (Context : Drag_Context;
         Ok      : Gint;
         Time    : Guint32);
      pragma Import (C, Internal, "gdk_drop_reply");
   begin
      Internal (Context,
                Boolean'Pos (Ok),
                Time);
   end Drop_Reply;

   -----------------
   -- Drop_Finish --
   -----------------

   procedure Drop_Finish
     (Context : Drag_Context;
      Success : Boolean;
      Time    : Guint32)
   is
      procedure Internal
        (Context : Drag_Context;
         Success : Gint;
         Time    : Guint32);
      pragma Import (C, Internal, "gdk_drop_finish");
   begin
      Internal (Context,
                Boolean'Pos (Success),
                Time);
   end Drop_Finish;

   ------------------------
   -- Drag_Get_Selection --
   ------------------------

   function Drag_Get_Selection (Context : Drag_Context)
                                return Gdk_Atom
   is
      function Internal (Context : Drag_Context)
                         return Gdk_Atom;
      pragma Import (C, Internal, "gdk_drag_get_selection");
   begin
      return Internal (Context);
   end Drag_Get_Selection;

   ----------------
   -- Drag_Begin --
   ----------------

   function Drag_Begin
     (Window  : Gdk.Window.Gdk_Window;
      Targets : Target_List)
      return Drag_Context
   is
      function Internal
        (Window  : Gdk_Window;
         Targets : System.Address)
         return Drag_Context;
      pragma Import (C, Internal, "gdk_drag_begin");
   begin
      return Internal (Window,
                       Targets.all'Address);
   end Drag_Begin;

   -----------------------
   -- Drag_Get_Protocol --
   -----------------------

   function Drag_Get_Protocol
     (Xid      : Guint32;
      Protocol : Drag_Protocol)
      return Guint32
   is
      function Internal
        (Xid      : Guint32;
         Protocol : Drag_Protocol)
         return Guint32;
      pragma Import (C, Internal, "gdk_drag_get_protocol");
   begin
      return Internal (Xid,
                       Protocol);
   end Drag_Get_Protocol;

   ----------------------
   -- Drag_Find_Window --
   ----------------------

   procedure Drag_Find_Window
     (Context     : Drag_Context;
      Drag_Window : Gdk.Window.Gdk_Window;
      X_Root      : Gint;
      Y_Root      : Gint;
      Dest_Window : Gdk.Window.Gdk_Window;
      Protocol    : Drag_Protocol)
   is
      procedure Internal
        (Context     : Drag_Context;
         Drag_Window : Gdk_Window;
         X_Root      : Gint;
         Y_Root      : Gint;
         Dest_Window : Gdk_Window;
         Protocol    : Drag_Protocol);
      pragma Import (C, Internal, "gdk_drag_find_window");
   begin
      Internal (Context,
                Drag_Window,
                X_Root,
                Y_Root,
                Dest_Window,
                Protocol);
   end Drag_Find_Window;

   -----------------
   -- Drag_Motion --
   -----------------

   function Drag_Motion
     (Context          : Drag_Context;
      Dest_Window      : Gdk.Window.Gdk_Window;
      Protocol         : Drag_Protocol;
      X_Root           : Gint;
      Y_Root           : Gint;
      Suggested_Action : Drag_Action;
      Possible_Actions : Drag_Action;
      Time             : Guint32)
      return Boolean
   is
      function Internal
        (Context          : Drag_Context;
         Dest_Window      : Gdk_Window;
         Protocol         : Gint;
         X_Root           : Gint;
         Y_Root           : Gint;
         Suggested_Action : Gint;
         Possible_Actions : Gint;
         Time             : Guint32)
         return Gint;
      pragma Import (C, Internal, "gdk_drag_motion");
   begin
      return Boolean'Val (Internal (Context,
                                    Dest_Window,
                                    Drag_Protocol'Pos (Protocol),
                                    X_Root,
                                    Y_Root,
                                    Drag_Action'Pos (Suggested_Action),
                                    Drag_Action'Pos (Possible_Actions),
                                    Time));
   end Drag_Motion;

   ---------------
   -- Drag_Drop --
   ---------------

   procedure Drag_Drop
     (Context : Drag_Context;
      Time    : Guint32)
   is
      procedure Internal
        (Context : Drag_Context;
         Time    : Guint32);
      pragma Import (C, Internal, "gdk_drag_drop");
   begin
      Internal (Context,
                Time);
   end Drag_Drop;

   ----------------
   -- Drag_Abort --
   ----------------

   procedure Drag_Abort
     (Context : Drag_Context;
      Time    : Guint32)
   is
      procedure Internal
        (Context : Drag_Context;
         Time    : Guint32);
      pragma Import (C, Internal, "gdk_drag_abort");
   begin
      Internal (Context,
                Time);
   end Drag_Abort;

   -----------------
   -- Get_Targets --
   -----------------

   function Get_Targets (Context : Drag_Context) return Gdk_Atom_Array is
      function Targets_Count (Context : Drag_Context) return Guint;
      pragma Import (C, Targets_Count, "ada_gtk_dnd_context_targets_count");

      procedure Internal (Context : Drag_Context; Result : Gdk_Atom_Array);
      pragma Import (C, Internal, "ada_gtk_dnd_context_get_targets");

      Length : constant Natural := Natural (Targets_Count (Context));
      Result : Gdk_Atom_Array (0 .. Length - 1);
   begin
      Internal (Context, Result);
      return Result;
   end Get_Targets;

end Gdk.Dnd;
