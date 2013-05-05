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
--  Gtk_Recent_Chooser is an interface that can be implemented by widgets
--  displaying the list of recently used files. In GTK+, the main objects that
--  implement this interface are Gtk_Recent_Chooser_Widget,
--  Gtk_Recent_Chooser_Dialog and Gtk_Recent_Chooser_Menu.
--  </description>
--  <c_version>2.16.6</c_version>

with GNAT.Strings;

with Glib.Error;
with Glib.Properties;
with Glib.Types;
with Gtk.Recent_Filter;
with Gtk.Recent_Manager;

package Gtk.Recent_Chooser is

   type Gtk_Recent_Chooser is new Glib.Types.GType_Interface;

   function Error_Quark return GQuark;

   function Get_Type return GType;

   --------------------
   -- Items and URIs --
   --------------------

   function Get_Current_Item
     (Chooser : Gtk_Recent_Chooser) return Gtk.Recent_Manager.Gtk_Recent_Info;
   --  Gets the Gtk_Recent_Info currently selected by Chooser.
   --  Use Gtk.Recent_Manager.Unref when you have finished using it.

   function Get_Current_Uri (Chooser : Gtk_Recent_Chooser) return String;
   function Set_Current_Uri
     (Chooser : Gtk_Recent_Chooser;
      Uri     : String;
      Error   : Glib.Error.GError := null)
      return Boolean;
   --  Gets/Sets Uri as the current URI for Chooser.  Set_Current_Uri returns
   --  whether the URI was found.

   function Get_Items
     (Chooser : Gtk_Recent_Chooser)
      return Gtk.Recent_Manager.Gtk_Recent_Info_List.Glist;
   --  Gets the list of recently used resources in form of Gtk_Recent_Info
   --  objects.
   --
   --  The return value of this function is affected by the "sort-type" and
   --  "limit" properties of Chooser.
   --
   --  You should Gtk.Recent_Manager.Unref on every item of the list, and then
   --  free the list itself using Gtk_Recent_Info_List.Free.

   function Get_Uris
     (Chooser : Gtk_Recent_Chooser) return GNAT.Strings.String_List;
   --  Gets the URIs of the recently used resources.
   --
   --  The return value of this function is affected by the "sort-type" and
   --  "limit" properties of Chooser.

   function Get_Limit (Chooser : Gtk_Recent_Chooser) return Gint;
   procedure Set_Limit
     (Chooser : Gtk_Recent_Chooser;
      Limit   : Gint);
   --  Gets/Sets the number of items that should be returned by
   --  Get_Items and Get_Uris.  Use -1 to specify all items.

   ---------------------
   -- Display Options --
   ---------------------

   function Get_Local_Only (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Local_Only
     (Chooser    : Gtk_Recent_Chooser;
      Local_Only : Boolean);
   --  Whether only local resources, that is resources using the file:// URI
   --  scheme, should be shown in the recently used resources selector.  If
   --  Local_Only is True (the default) then the shown resources are guaranteed
   --  to be accessible through the operating system native file system.

   function Get_Show_Icons (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Show_Icons
     (Chooser    : Gtk_Recent_Chooser;
      Show_Icons : Boolean);
   --  Whether Chooser should show an icon near the resource.

   function Get_Show_Not_Found (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Show_Not_Found
     (Chooser        : Gtk_Recent_Chooser;
      Show_Not_Found : Boolean);
   --  Whether Chooser should display the recently used resources that
   --  it didn't find.  This only applies to local resources.

   function Get_Show_Private (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Show_Private
     (Chooser      : Gtk_Recent_Chooser;
      Show_Private : Boolean);
   --  Whether to show recently used resources marked registered as private.

   function Get_Show_Tips (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Show_Tips
     (Chooser   : Gtk_Recent_Chooser;
      Show_Tips : Boolean);
   --  Whether to show a tooltip containing the full path of each
   --  recently used resource in a Gtk_Recent_Chooser widget.

   -------------
   -- Filters --
   -------------

   procedure Add_Filter
     (Chooser : Gtk_Recent_Chooser;
      Filter  : access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);
   --  Adds Filter to the list of Gtk_Recent_Filter objects held by Chooser.
   --
   --  If no previous filter objects were defined, this function will call
   --  Set_Filter.

   procedure Remove_Filter
     (Chooser : Gtk_Recent_Chooser;
      Filter  : access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);
   --  Removes Filter from the list of Gtk_Recent_Filter objects held by
   --  Chooser.

   function Get_Filter
     (Chooser : Gtk_Recent_Chooser)
      return Gtk.Recent_Filter.Gtk_Recent_Filter;
   procedure Set_Filter
     (Chooser : Gtk_Recent_Chooser;
      Filter  : access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);
   --  Gets/Sets Filter as the current Gtk_Recent_Filter object used by Chooser
   --  to affect the displayed recently used resources.

   function List_Filters
     (Chooser : Gtk_Recent_Chooser)
      return Gtk.Recent_Filter.Gtk_Recent_Filter_List.GSlist;
   --  Gets the Gtk_Recent_Filter objects held by Chooser.
   --  You should free the returned list using
   --  Gtk.Recent_Filter.Gtk_Recent_Filter_List.Free.

   ---------------
   -- Selection --
   ---------------

   function Get_Select_Multiple (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Select_Multiple
     (Chooser         : Gtk_Recent_Chooser;
      Select_Multiple : Boolean);
   --  Whether Chooser can select multiple items.

   procedure Select_All   (Chooser : Gtk_Recent_Chooser);
   procedure Unselect_All (Chooser : Gtk_Recent_Chooser);
   --  Selects/Unselects all the items inside Chooser, if the Chooser supports
   --  multiple selection.

   function Select_Uri
     (Chooser : Gtk_Recent_Chooser;
      Uri     : String;
      Error   : Glib.Error.GError := null)
      return Boolean;
   procedure Unselect_Uri
     (Chooser : Gtk_Recent_Chooser;
      Uri     : String);
   --  Selects/Unselects Uri inside Chooser.  Select_Uri returns whether the
   --  URI was found.

   -------------
   -- Sorting --
   -------------

   type Gtk_Recent_Sort_Type is
     (Gtk_Recent_Sort_None,
      Gtk_Recent_Sort_Mru,
      Gtk_Recent_Sort_Lru,
      Gtk_Recent_Sort_Custom);
   pragma Convention (C, Gtk_Recent_Sort_Type);

   function Get_Sort_Type
     (Chooser : Gtk_Recent_Chooser) return Gtk_Recent_Sort_Type;
   procedure Set_Sort_Type
     (Chooser   : Gtk_Recent_Chooser;
      Sort_Type : Gtk_Recent_Sort_Type);
   --  Changes the sorting order of the recently used resources list displayed
   --  by Chooser.

   generic
      type Data_Type is private;
   package User_Sort_Func is
      type Data_Type_Access is access Data_Type;

      type Comparison is (Before, Equal, After);

      type Gtk_Recent_Sort_Func is access function
        (Left, Right : Gtk.Recent_Manager.Gtk_Recent_Info;
         User_Data   : Data_Type_Access)
         return Comparison;
      --  Return whether the Left comes Before, After, or is Equal to,
      --  the right.

      type Destroy_Notify is access procedure (User_Data : in out Data_Type);
      --  This procedure will be called just prior to the destruction of
      --  Sort_Data.

      procedure Set_Sort_Func
        (Chooser      : Gtk_Recent_Chooser;
         Sort_Func    : Gtk_Recent_Sort_Func;
         Sort_Data    : Data_Type_Access := null;
         Data_Destroy : Destroy_Notify := null);
      --  Sets the comparison function used when sorting to be Sort_Func.  If
      --  the Chooser has the sort type set to Gtk_Recent_Sort_Custom then
      --  the chooser will sort using this function.
   end User_Sort_Func;

   -----------------
   -- Obsolescent --
   -----------------

   function Get_Show_Numbers
     (Chooser : Gtk_Recent_Chooser)
      return Boolean;
   pragma Obsolescent; --  Get_Show_Numbers
   procedure Set_Show_Numbers
     (Chooser      : Gtk_Recent_Chooser;
      Show_Numbers : Boolean);
   pragma Obsolescent; --  Set_Show_Numbers
   --  Whether to show recently used resources prepended by a unique number.
   --
   --  Deprecated: 2.12: Use Gtk.Recent_Chooser_Menu.Get_Show_Numbers and
   --  Gtk.Recent_Chooser_Menu.Set_Show_Numbers instead.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  Name:  Filter_Property
   --  Type:  Object
   --  Descr: The current filter for selecting which resources are displayed
   --
   --  Name:  Limit_Property
   --  Type:  Int
   --  Descr: The maximum number of items to be displayed
   --
   --  Name:  Local_Only_Property
   --  Type:  Boolean
   --  Descr: Whether the selected resource(s) should be limited to local
   --         file: URIs
   --
   --  Name:  Recent_Manager_Property
   --  Type:  Object
   --  Descr: The RecentManager object to use
   --
   --  Name:  Select_Multiple_Property
   --  Type:  Boolean
   --  Descr: Whether to allow multiple items to be selected
   --
   --  Name:  Show_Icons_Property
   --  Type:  Boolean
   --  Descr: Whether there should be an icon near the item
   --
   --  Name:  Show_Not_Found_Property
   --  Type:  Boolean
   --  Descr: Whether the items pointing to unavailable resources should be
   --         displayed
   --
   --  Name:  Show_Private_Property
   --  Type:  Boolean
   --  Descr: Whether the private items should be displayed
   --
   --  Name:  Show_Tips_Property
   --  Type:  Boolean
   --  Descr: Whether there should be a tooltip on the item
   --
   --  Name:  Sort_Type_Property
   --  Type:  Enum
   --  Descr: The sorting order of the items displayed
   --
   --  </properties>

   Filter_Property          : constant Glib.Properties.Property_Object;
   Limit_Property           : constant Glib.Properties.Property_Int;
   Local_Only_Property      : constant Glib.Properties.Property_Boolean;
   Recent_Manager_Property  : constant Glib.Properties.Property_Object;
   Select_Multiple_Property : constant Glib.Properties.Property_Boolean;
   Show_Icons_Property      : constant Glib.Properties.Property_Boolean;
   Show_Not_Found_Property  : constant Glib.Properties.Property_Boolean;
   Show_Private_Property    : constant Glib.Properties.Property_Boolean;
   Show_Tips_Property       : constant Glib.Properties.Property_Boolean;
   Sort_Type_Property       : constant Glib.Properties.Property_Enum;

private

   Filter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("filter");
   Limit_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("limit");
   Local_Only_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("local-only");
   Recent_Manager_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("recent-manager");
   Select_Multiple_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("select-multiple");
   Show_Icons_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-icons");
   Show_Not_Found_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-not-found");
   Show_Private_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-private");
   Show_Tips_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-tips");
   Sort_Type_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("sort-type");

   pragma Import (C, Get_Type, "gtk_recent_chooser_get_type");
   pragma Import (C, Error_Quark, "gtk_recent_chooser_error_quark");

end Gtk.Recent_Chooser;
