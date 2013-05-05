-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2013, AdaCore                 --
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

--  <group>Gdk, the low-level API</group>
with Glib; use Glib;
with Gdk.Types; use Gdk.Types;
with Gdk.Window;
with Gtk;
with Gtk.Selection; use Gtk.Selection;

package Gdk.Dnd is

   -----------------
   -- Drag_Action --
   -----------------

   type Drag_Action is mod 2 ** 32;
   --  Possible actions for a drop onto a widget, during a drag-and-drop.
   --  The drag widgets (ie the ones from which the user can start a
   --  drag-and-drop operation) should set a mask, indicating which actions
   --  it wants to do. The first action in the list below has the highest
   --  priority, the last one the lowest. The actual action chosen for the
   --  drag-and-drop will be the highest-priority one that is also accepted
   --  by the drop site.
   --
   --  Note that in the case where the drag source supports multiple actions,
   --  the user can select the one he wants. As explained above, the default
   --  one is the highest priority one. But if the user pressed Shift at the
   --  same time, Action_Move will be used if present. Ctrl-Shift selects
   --  an Action_Link, and Ctrl selects an Action_Copy.

   Action_Any : constant Drag_Action;
   --  Any of the default action is accepted.

   Action_Default : constant Drag_Action;
   --  ???

   Action_Copy    : constant Drag_Action;
   --  Copy the data to the drop site.

   Action_Move    : constant Drag_Action;
   --  Copy the data to the drop site, and delete it from the drag source.
   --  The delete command is invoked automatically by GtkAda.

   Action_Link    : constant Drag_Action;
   --  Allow the drop site to access the data, without copying it.

   Action_Private : constant Drag_Action;
   --  Any action you want to implement. No automatic behavior is provided
   --  by GtkAda.

   Action_Ask     : constant Drag_Action;
   --  ???

   -------------------
   -- Drag_Protocol --
   -------------------

   type Drag_Protocol is
     (Drag_Proto_Motif,
      Drag_Proto_Xdnd,
      Drag_Proto_Rootwin,
      Drag_Proto_None,
      Drag_Proto_Win32_Dropfiles,
      Drag_Proto_Ole2,
      Drag_Proto_Local);
   --  The various dnd protocols recognized by a window.
   --  Note that not every window recognizes every protocol, and you should
   --  be careful as to which one you use. The function Gdk.Drag_Get_Protocol
   --  returns which one is recognized by a window.

   ------------------
   -- Drag_Context --
   ------------------

   type Drag_Context is new Gdk.C_Proxy;
   --  Structure that holds information about a drag in progress.
   --  This is used on both source and destination sides.

   ------------------
   -- Drag_Context --
   ------------------

   function Get_Actions (Context : Drag_Context) return Drag_Action;
   --  Return the possible actions associated with the context.
   --  This is the list of actions defined by the source of the drag-and-drop
   --  operation, in Source_Set.
   --  (for instance, if Source_Set was used with Action_Copy + Action_Move,
   --  the result will be exactly this sum, whatever was used for Dest_Set).

   function Get_Suggested_Action (Context : Drag_Context) return Drag_Action;
   --  Return the suggested action for that context.
   --  This is the highest priority action that was set by the source of the
   --  drag-and-drop, ie the one it would rather use. The action that is
   --  actually used is the one returned by Get_Action, and depends on the
   --  mask set by the target.

   function Get_Selected_Action (Context : Drag_Context) return Drag_Action;
   --  Return the action selected for the drag-and-drop operation.
   --  This is the highest priority action common between the drag site and the
   --  drop widget (for instance, if Source_Set was used with Action_Copy +
   --  Action_Move and Dest_Set was used with only Action_Move, this will
   --  be Action_Move).

   function Get_Action (Context : Drag_Context) return Drag_Action
                        renames Get_Selected_Action;
   --  For backwards compatibility.

   type Gdk_Atom_Array is array (Natural range <>) of Gdk.Types.Gdk_Atom;
   function Get_Targets (Context : Drag_Context) return Gdk_Atom_Array;
   --  Return the list of targets supported by this context.

   procedure Drag_Context_Ref (Context : Drag_Context);

   procedure Drag_Context_Unref (Context : Drag_Context);

   procedure Drag_Status
     (Context : Drag_Context;
      Action  : Drag_Action;
      Time    : Guint32);

   procedure Drop_Reply
     (Context : Drag_Context;
      Ok      : Boolean;
      Time    : Guint32);

   procedure Drop_Finish
     (Context : Drag_Context;
      Success : Boolean;
      Time    : Guint32);
   --  Clean up from the drag, and display snapback, if necessary.

   function Drag_Get_Selection (Context : Drag_Context) return Gdk_Atom;

   function Drag_Begin
     (Window  : Gdk.Window.Gdk_Window;
      Targets : Target_List) return Drag_Context;

   function Drag_Get_Protocol
     (Xid      : Guint32;
      Protocol : Drag_Protocol) return Guint32;
   --  Return which drag protocol is recognized by a given low level window.

   procedure Drag_Find_Window
     (Context     : Drag_Context;
      Drag_Window : Gdk.Window.Gdk_Window;
      X_Root      : Gint;
      Y_Root      : Gint;
      Dest_Window : Gdk.Window.Gdk_Window;
      Protocol    : Drag_Protocol);

   function Drag_Motion
     (Context          : Drag_Context;
      Dest_Window      : Gdk.Window.Gdk_Window;
      Protocol         : Drag_Protocol;
      X_Root           : Gint;
      Y_Root           : Gint;
      Suggested_Action : Drag_Action;
      Possible_Actions : Drag_Action;
      Time             : Guint32) return Boolean;

   procedure Drag_Drop (Context : Drag_Context; Time : Guint32);

   procedure Drag_Abort (Context : Drag_Context; Time : Guint32);

private

   pragma Import (C, Get_Actions, "gdk_drag_context_get_actions");
   pragma Import
     (C, Get_Suggested_Action,
     "gdk_drag_context_get_suggested_action");
   pragma Import (C, Get_Action, "gdk_drag_context_get_selected_action");

   Action_Default : constant Drag_Action := 2 ** 0;
   Action_Copy    : constant Drag_Action := 2 ** 1;
   Action_Move    : constant Drag_Action := 2 ** 2;
   Action_Link    : constant Drag_Action := 2 ** 3;
   Action_Private : constant Drag_Action := 2 ** 4;
   Action_Ask     : constant Drag_Action := 2 ** 5;
   Action_Any     : constant Drag_Action := 2 ** 8 - 1;

end Gdk.Dnd;
