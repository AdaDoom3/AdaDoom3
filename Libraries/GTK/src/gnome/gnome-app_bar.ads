-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2002                          --
--                         ACT-Europe                                --
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

with Glib; use Glib;
with Gtk;
with Gtk.Box;

pragma Warnings (Off); --  Gtk.Progress is obsolete
with Gtk.Progress;
pragma Warnings (On);

package Gnome.App_Bar is

   type Gnome_App_Bar_Record is new Gtk.Box.Gtk_Hbox_Record with private;
   type Gnome_App_Bar is access all Gnome_App_Bar_Record'Class;

   procedure Gnome_New
     (Widget        : out Gnome_App_Bar;
      Has_Progress  : Boolean;
      Has_Status    : Boolean;
      Interactivity : Gnome_Preferences_Type);

   procedure Initialize
     (Widget        : access Gnome_App_Bar_Record'Class;
      Has_Progress  : Boolean;
      Has_Status    : Boolean;
      Interactivity : Gnome_Preferences_Type);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Appbar_Clear_Prompt (Appbar : access Gnome_App_Bar_Record);

   procedure Appbar_Clear_Stack (Appbar : access Gnome_App_Bar_Record);

   pragma Warnings (Off); --  Gtk.Progress is obsolescent
   function Appbar_Get_Progress (Appbar : access Gnome_App_Bar_Record)
                                 return Gtk.Progress.Gtk_Progress;
   pragma Warnings (On);

   function Appbar_Get_Response (Appbar : access Gnome_App_Bar_Record)
                                 return String;

   procedure Appbar_Pop (Appbar : access Gnome_App_Bar_Record);

   procedure Appbar_Push
     (Appbar : access Gnome_App_Bar_Record;
      Status : String);

   procedure Appbar_Refresh (Appbar : access Gnome_App_Bar_Record);

   procedure Appbar_Set_Default
     (Appbar         : access Gnome_App_Bar_Record;
      Default_Status : String);

   procedure Appbar_Set_Progress_Percentage
     (Appbar     : access Gnome_App_Bar_Record;
      Percentage : Gfloat);

   procedure Appbar_Set_Prompt
     (Appbar : access Gnome_App_Bar_Record;
      Prompt : String;
      Modal  : Boolean);

   procedure Appbar_Set_Status
     (Appbar : access Gnome_App_Bar_Record;
      Status : String);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "user_response"
   --    procedure Handler (Widget : access Gnome_App_Bar_Record'Class);
   --
   --  - "clear_prompt"
   --    procedure Handler (Widget : access Gnome_App_Bar_Record'Class);
   --
   --  </signals>

private
   type Gnome_App_Bar_Record is new Gtk.Box.Gtk_Hbox_Record with null record;

   pragma Import (C, Get_Type, "gnome_appbar_get_Type");
end Gnome.App_Bar;
