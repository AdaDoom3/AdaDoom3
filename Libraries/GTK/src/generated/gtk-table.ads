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
--  A Gtk_Table is a container that can contain any number of children. Each
--  of them is attached to a specific row and a specific column in widget.
--
--  Every row in the table must have the same height, and every column must
--  have the same width if the table was said as Homogeneous. But you can also
--  decide to have an heterogeneous table, where the width and height are set
--  by the children contained in the table. Check out the Gtk_Sheet widget for
--  a different kind of table that can also contain text and images in a more
--  efficient way.
--
--  </description>
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Table is

   type Gtk_Table_Record is new Gtk_Container_Record with null record;
   type Gtk_Table is access all Gtk_Table_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Table       : out Gtk_Table;
       Rows        : Guint;
       Columns     : Guint;
       Homogeneous : Boolean);
   procedure Initialize
      (Table       : access Gtk_Table_Record'Class;
       Rows        : Guint;
       Columns     : Guint;
       Homogeneous : Boolean);
   --  Create a new table. The width allocated to the table is divided into
   --  Columns columns, which all have the same width if Homogeneous is True.
   --  If Homogeneous is False, the width will be calculated with the children
   --  contained in the table. Same behavior for the rows.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_table_get_type");

   -------------
   -- Methods --
   -------------

   procedure Attach
      (Table         : access Gtk_Table_Record;
       Child         : access Gtk.Widget.Gtk_Widget_Record'Class;
       Left_Attach   : Guint;
       Right_Attach  : Guint;
       Top_Attach    : Guint;
       Bottom_Attach : Guint;
       Xoptions      : Gtk.Enums.Gtk_Attach_Options := Expand or Fill;
       Yoptions      : Gtk.Enums.Gtk_Attach_Options := Expand or Fill;
       Xpadding      : Guint := 0;
       Ypadding      : Guint := 0);
   --  Insert a new widget in the table. All the attachments are relative to
   --  the separations between columns and rows (for instance, to insert a
   --  widget spanning the first two columns in the table, you should put
   --  Left_Attach=0 and Right_Attach=2). Same behavior for the rows. Xoptions
   --  and Yoptions indicate the behavior of the child when the table is
   --  resized (whether the child can shrink or expand). See the description in
   --  Gtk.Box for more information on the possible values. Xpadding and
   --  Ypadding are the amount of space left around the child.

   procedure Attach_Defaults
      (Table         : access Gtk_Table_Record;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
       Left_Attach   : Guint;
       Right_Attach  : Guint;
       Top_Attach    : Guint;
       Bottom_Attach : Guint);
   --  Insert a new widget in the table, with default values. No padding is
   --  put around the child, and the options are set to Expand and Fill. This
   --  call is similar to Attach with default values and is only provided for
   --  compatibility.

   function Get_Col_Spacing
      (Table  : access Gtk_Table_Record;
       Column : Guint) return Guint;
   procedure Set_Col_Spacing
      (Table   : access Gtk_Table_Record;
       Column  : Guint;
       Spacing : Guint);
   --  Set the spacing in pixels between Column and the next one.

   function Get_Default_Col_Spacing
      (Table : access Gtk_Table_Record) return Guint;
   --  Gets the default column spacing for the table. This is the spacing that
   --  will be used for newly added columns. (See Gtk.Table.Set_Col_Spacings)

   function Get_Default_Row_Spacing
      (Table : access Gtk_Table_Record) return Guint;
   --  Gets the default row spacing for the table. This is the spacing that
   --  will be used for newly added rows. (See Gtk.Table.Set_Row_Spacings)

   function Get_Homogeneous (Table : access Gtk_Table_Record) return Boolean;
   procedure Set_Homogeneous
      (Table       : access Gtk_Table_Record;
       Homogeneous : Boolean);
   --  Indicate the homogeneous status of the table. If Homogeneous is True,
   --  the rows and columns of the table will all be allocated the same width
   --  or height.

   function Get_Row_Spacing
      (Table : access Gtk_Table_Record;
       Row   : Guint) return Guint;
   procedure Set_Row_Spacing
      (Table   : access Gtk_Table_Record;
       Row     : Guint;
       Spacing : Guint);

   procedure Get_Size
      (Table   : access Gtk_Table_Record;
       Rows    : out Guint;
       Columns : out Guint);
   --  Returns the number of rows and columns in the table.
   --  Since: gtk+ 2.22
   --  "rows": return location for the number of rows, or null
   --  "columns": return location for the number of columns, or null

   procedure Resize
      (Table   : access Gtk_Table_Record;
       Rows    : Guint;
       Columns : Guint);

   procedure Set_Col_Spacings
      (Table   : access Gtk_Table_Record;
       Spacing : Guint);

   procedure Set_Row_Spacings
      (Table   : access Gtk_Table_Record;
       Spacing : Guint);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Table_Record, Gtk_Table);
   function "+"
     (Widget : access Gtk_Table_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Table
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Column_Spacing_Property
   --  Type: Guint
   --  Flags: read-write
   --
   --  Name: Homogeneous_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: N_Columns_Property
   --  Type: Guint
   --  Flags: read-write
   --
   --  Name: N_Rows_Property
   --  Type: Guint
   --  Flags: read-write
   --
   --  Name: Row_Spacing_Property
   --  Type: Guint
   --  Flags: read-write

   Column_Spacing_Property : constant Glib.Properties.Property_Uint;
   Homogeneous_Property : constant Glib.Properties.Property_Boolean;
   N_Columns_Property : constant Glib.Properties.Property_Uint;
   N_Rows_Property : constant Glib.Properties.Property_Uint;
   Row_Spacing_Property : constant Glib.Properties.Property_Uint;

private
   Column_Spacing_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("column-spacing");
   Homogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("homogeneous");
   N_Columns_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-columns");
   N_Rows_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-rows");
   Row_Spacing_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("row-spacing");
end Gtk.Table;
