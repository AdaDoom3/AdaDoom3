-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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

with Gdk.Color;                use Gdk.Color;
with Gdk.Event;                use Gdk.Event;
with Gdk.Main;                 use Gdk.Main;
with Gdk.Types;                use Gdk.Types;
with Gdk.Window;               use Gdk.Window;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Types;               use Glib.Types;
with Glib.Generic_Properties;  use Glib.Generic_Properties;
with Glib.Properties;          use Glib.Properties;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glib.Values;              use Glib.Values;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Color_Button;         use Gtk.Color_Button;
with Gtk.Combo_Box;            use Gtk.Combo_Box;
with Gtk.Container;            use Gtk.Container;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Label;                use Gtk.Label;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Table;                use Gtk.Table;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Tooltips;             use Gtk.Tooltips;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.Types;             use Gtkada.Types;
with Pango.Font;               use Pango.Font;
with System;                   use System;
with System.Address_Image;

package body Gtkada.Properties is

   type Properties_Editor_Record is new Gtk_Window_Record with record
      Object      : GObject;
      Tips        : Gtk_Tooltips;
      Table       : Gtk_Table; --  Properties;
      Child_Table : Gtk_Table; --  Child properties;
      Style_Table : Gtk_Table; --  Style properties
      Type_Color  : Gdk_Color := Null_Color;
      Pick_Button : Gtk_Button;
      In_Pick     : Boolean := False;
   end record;
   type Properties_Editor is access all Properties_Editor_Record'Class;

   Global_Editor : Properties_Editor;
   --  The unique properties editor for this application.

   type Property_Type is (Property_Standard, Property_Child, Property_Style);

   procedure Editor_Delete (Editor : access GObject_Record'Class);
   --  Called when the properties editor Editor is destroyed

   procedure Gtk_New (Editor : out Properties_Editor);
   --  Create a new empty properties editor

   procedure Object_Destroyed (Editor, Object : System.Address);
   pragma Convention (C, Object_Destroyed);
   --  Called when the object that the editor is showing has been destroyed.

   procedure Remove_Children
     (Container : access Gtk_Container_Record'Class);
   --  Remove all children from a container

   procedure Pick_Widget (Editor : access Gtk_Widget_Record'Class);
   function Widget_Picked
     (Editor : access Gtk_Widget_Record'Class) return Boolean;
   --  Start picking a new object to examine. When one is clicked on,
   --  Widget_Picked is called.

   procedure Work_On
     (Editor : access Properties_Editor_Record'Class;
      Object : GObject := null);
   --  Show the properties of Object in the editor

   procedure Show_Properties_From_Type
     (Editor : access Properties_Editor_Record'Class;
      T      : GType);
   --  Show the properties associated with Editor.Object, for its specific
   --  ancestor type T.

   procedure Show_Properties
     (Editor     : access Properties_Editor_Record'Class;
      Table      : Gtk_Table;
      Prop_Type  : Property_Type;
      T          : GType;
      Properties : Param_Spec_Array);
   --  Add the description for all properties in Properties.

   function Property_Widget
     (Object        : access GObject_Record'Class;
      Property      : Param_Spec;
      Prop_Type     : Property_Type) return Gtk_Widget;
   --  Return the widget to use to edit that property

   type Pspec_Callback is access procedure
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   --  Set the value of Editor based on the contents of Value

   type Property_Modified_Data is record
      Pspec         : Param_Spec;
      Editor        : Gtk_Widget;
      Callback      : Pspec_Callback;
      Prop_Type     : Property_Type;
   end record;
   package Prop_Callback is new Gtk.Handlers.User_Callback
     (GObject_Record, Property_Modified_Data);

   type Controller_Callback is access procedure
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   --  Set the contents of Value (already initialized) to the value found in
   --  editor. This is specialized for each type of property

   type Controller_Data is record
      Object        : GObject;
      Pspec         : Param_Spec;
      Prop_Type     : Property_Type;
      Callback      : Controller_Callback;
   end record;
   package Controller_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Controller_Data);

   package Handler_Id_User_Data is new Glib.Object.User_Data (Handler_Id);

   procedure Property_Modified
     (Object : access GObject_Record'Class;
      Data   : Property_Modified_Data);
   --  Called when a property has been modified, and dispatch to the callback
   --  in Data.

   procedure Controller_Called
     (Editor : access Gtk_Widget_Record'Class;
      Data   : Controller_Data);
   --  An editor has been modified by the user, call the appropriate controller

   procedure Block_Controller
     (Editor : access Gtk_Widget_Record'Class; Block  : Boolean);
   --  Temporarily disable the controller callback on Editor, so that when the
   --  property is changed, and then the editor is changed, this doesn't
   --  return in another call to the property modified callback to prevent
   --  infinite loops.

   procedure Bool_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure String_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Int_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Uint_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Float_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Double_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Enum_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Pointer_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Object_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Color_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   --  A property has been modified

   procedure Bool_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure String_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure Int_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure Uint_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure Float_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure Double_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure Enum_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure Color_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   --  The user modified the property

   ------------------
   -- Bool_Changed --
   ------------------

   procedure Bool_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue) is
   begin
      Set_Active (Gtk_Toggle_Button (Editor), Get_Boolean (Value));
   end Bool_Changed;

   --------------------
   -- String_Changed --
   --------------------

   procedure String_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue) is
   begin
      Set_Text (Gtk_Entry (Editor), Get_String (Value));
   end String_Changed;

   -----------------
   -- Int_Changed --
   -----------------

   procedure Int_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue) is
   begin
      Set_Value (Gtk_Spin_Button (Editor), Gdouble (Get_Int (Value)));
   end Int_Changed;

   -------------------
   -- Float_Changed --
   -------------------

   procedure Float_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue) is
   begin
      Set_Value (Gtk_Spin_Button (Editor), Gdouble (Get_Float (Value)));
   end Float_Changed;

   --------------------
   -- Double_Changed --
   --------------------

   procedure Double_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue) is
   begin
      Set_Value (Gtk_Spin_Button (Editor), Get_Double (Value));
   end Double_Changed;

   -------------------
   -- Color_Changed --
   -------------------

   procedure Color_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue)
   is
   begin
      Set_Color (Gtk_Color_Button (Editor),
                 Gdk_Color'(Get_Value (Value)));
   exception
      when Unset_Value =>
         null;
   end Color_Changed;

   ---------------------
   -- Pointer_Changed --
   ---------------------

   procedure Pointer_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue)
   is
      Val   : constant System.Address := Get_Address (Value);
   begin
      if Val = System.Null_Address then
         Set_Text (Gtk_Label (Editor), "Pointer: (null)");
      else
         Set_Text
           (Gtk_Label (Editor), "Pointer: 0x" & System.Address_Image (Val));
      end if;
   end Pointer_Changed;

   --------------------
   -- Object_Changed --
   --------------------

   procedure Object_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue)
   is
      Val   : constant System.Address := Get_Address (Value);
      Stub  : GObject_Record;
      Obj   : constant GObject := Get_User_Data (Val, Stub);
   begin
      if Obj = null then
         Set_Text (Gtk_Label (Editor), "Object: (null)");
      else
         Set_Text
           (Gtk_Label (Editor),
            "Object: 0x" & System.Address_Image (Obj.all'Address));
      end if;
   end Object_Changed;

   ------------------
   -- Enum_Changed --
   ------------------

   procedure Enum_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value  : GValue) is
   begin
      Set_Active (Gtk_Combo_Box (Editor), Get_Enum (Value));
   end Enum_Changed;

   ------------------
   -- Uint_Changed --
   ------------------

   procedure Uint_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value  : GValue) is
   begin
      Set_Value (Gtk_Spin_Button (Editor), Gdouble (Get_Uint (Value)));
   end Uint_Changed;

   -------------------
   -- Bool_Modified --
   -------------------

   procedure Bool_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Boolean (Value, Get_Active (Gtk_Toggle_Button (Editor)));
   end Bool_Modified;

   --------------------
   -- Color_Modified --
   --------------------

   procedure Color_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Value (Value, Val => Get_Color (Gtk_Color_Button (Editor)));
   end Color_Modified;

   ------------------
   -- Int_Modified --
   ------------------

   procedure Int_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Int (Value, Gint (Get_Value (Gtk_Spin_Button (Editor))));
   end Int_Modified;

   --------------------
   -- Float_Modified --
   --------------------

   procedure Float_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Float (Value, Gfloat (Get_Value (Gtk_Spin_Button (Editor))));
   end Float_Modified;

   ---------------------
   -- Double_Modified --
   ---------------------

   procedure Double_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Double (Value, Get_Value (Gtk_Spin_Button (Editor)));
   end Double_Modified;

   -------------------
   -- Enum_Modified --
   -------------------

   procedure Enum_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Enum (Value, Get_Active (Gtk_Combo_Box (Editor)));
   end Enum_Modified;

   -------------------
   -- Uint_Modified --
   -------------------

   procedure Uint_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Uint (Value, Guint (Get_Value (Gtk_Spin_Button (Editor))));
   end Uint_Modified;

   ---------------------
   -- String_Modified --
   ---------------------

   procedure String_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_String (Value, Get_Text (Gtk_Entry (Editor)));
   end String_Modified;

   -----------------------
   -- Property_Modified --
   -----------------------

   procedure Property_Modified
     (Object : access GObject_Record'Class;
      Data   : Property_Modified_Data)
   is
      Value : GValue;
   begin
      Init (Value, Value_Type (Data.Pspec));
      case Data.Prop_Type is
         when Property_Child =>
            Child_Get_Property
              (Gtk_Container (Get_Parent (Gtk_Widget (Object))),
               Gtk_Widget (Object), Pspec_Name (Data.Pspec), Value);
         when Property_Standard =>
            Get_Property (Object, Pspec_Name (Data.Pspec), Value);
         when Property_Style =>
            Style_Get_Property
              (Gtk_Widget (Object), Pspec_Name (Data.Pspec), Value);
      end case;

      Block_Controller (Data.Editor, True);
      Data.Callback (Data.Editor, Value);
      Unset (Value);
      Block_Controller (Data.Editor, False);
   end Property_Modified;

   ----------------------
   -- Block_Controller --
   ----------------------

   procedure Block_Controller
     (Editor : access Gtk_Widget_Record'Class;
      Block  : Boolean)
   is
      Id : Handler_Id;
   begin
      Id := Handler_Id_User_Data.Get (Editor, "gtkada-properties-controller");
      if Block then
         Handler_Block (Editor, Id);
      else
         Handler_Unblock (Editor, Id);
      end if;

   exception
      when Gtkada.Types.Data_Error =>
         --  No such user data
         null;
   end Block_Controller;

   -----------------------
   -- Controller_Called --
   -----------------------

   procedure Controller_Called
     (Editor : access Gtk_Widget_Record'Class;
      Data   : Controller_Data)
   is
      Value : GValue;
   begin
      Init (Value, Value_Type (Data.Pspec));
      Data.Callback (Value, Editor);

      case Data.Prop_Type is
         when Property_Child =>
            Child_Set_Property
              (Gtk_Container (Get_Parent (Gtk_Widget (Data.Object))),
               Gtk_Widget (Data.Object), Pspec_Name (Data.Pspec), Value);
         when Property_Standard =>
            Set_Property (Data.Object, Pspec_Name (Data.Pspec), Value);
         when Property_Style =>
            --  Can't be set
            null;
      end case;

      Unset (Value);
   end Controller_Called;

   ---------------------
   -- Property_Widget --
   ---------------------

   function Property_Widget
     (Object    : access GObject_Record'Class;
      Property  : Param_Spec;
      Prop_Type : Property_Type) return Gtk_Widget
   is
      procedure Connect_Controller
        (Editor   : access Gtk_Widget_Record'Class;
         Signal   : Glib.Signal_Name;
         Callback : Controller_Callback);
      --  Connect a controller to Editor, ie propagate changes done to
      --  controller onto Property.

      procedure Connect_Property
        (Editor   : access Gtk_Widget_Record'Class;
         Callback : Pspec_Callback);
      --  Setup Callback so that it is called when the property is modified

      ----------------------
      -- Connect_Property --
      ----------------------

      procedure Connect_Property
        (Editor   : access Gtk_Widget_Record'Class;
         Callback : Pspec_Callback)
      is
         Id : Handler_Id;
      begin
         case Prop_Type is
            when Property_Child =>
               Id := Prop_Callback.Connect
                 (Object,
                  Signal_Child_Notify & "::"
                    & Glib.Signal_Name (Pspec_Name (Property)),
                  Prop_Callback.To_Marshaller (Property_Modified'Access),
                  User_Data   => (Pspec         => Property,
                                  Prop_Type     => Prop_Type,
                                  Editor        => Gtk_Widget (Editor),
                                  Callback      => Callback));
            when Property_Standard =>
               Id := Prop_Callback.Connect
                 (Object,
                  "notify::" & Glib.Signal_Name (Pspec_Name (Property)),
                  Prop_Callback.To_Marshaller (Property_Modified'Access),
                  User_Data   => (Pspec         => Property,
                                  Prop_Type     => Prop_Type,
                                  Editor        => Gtk_Widget (Editor),
                                  Callback      => Callback));
            when Property_Style =>
               --  We do not have a signal when it is changed, since it
               --  is in fact never changed. But we still need to
               --  initialize it anyway
               Property_Modified
                 (Object,
                  Data => (Pspec     => Property,
                           Editor    => Gtk_Widget (Editor),
                           Callback  => Callback,
                           Prop_Type => Prop_Type));
               Id.Id := Null_Handler_Id;
         end case;

         if Id.Id /= Null_Handler_Id then
            Add_Watch (Id, Editor);
         end if;
      end Connect_Property;

      ------------------------
      -- Connect_Controller --
      ------------------------

      procedure Connect_Controller
        (Editor   : access Gtk_Widget_Record'Class;
         Signal   : Glib.Signal_Name;
         Callback : Controller_Callback)
      is
         Id : Handler_Id;
      begin
         if Prop_Type /= Property_Style then
            Id := Controller_Cb.Connect
              (Editor, Signal,
               Controller_Cb.To_Marshaller (Controller_Called'Access),
               Controller_Data'
                 (Pspec         => Property,
                  Callback      => Callback,
                  Prop_Type     => Prop_Type,
                  Object        => GObject (Object)));
            Add_Watch (Id, Object);
            Handler_Id_User_Data.Set
              (Editor, Id, "gtkada-properties-controller");
         end if;
      end Connect_Controller;

      Toggle  : Gtk_Check_Button;
      Ent     : Gtk_Entry;
      Spin    : Gtk_Spin_Button;
      E_Klass : Enum_Class;
      Val     : Enum_Value;
      K       : Guint;
      Combo   : Gtk_Combo_Box;
      Label   : Gtk_Label;
      Color   : Gtk_Color_Button;

   begin
      if Value_Type (Property) = GType_Boolean then
         Gtk_New (Toggle);
         Connect_Property   (Toggle, Bool_Changed'Access);
         Connect_Controller (Toggle, "toggled", Bool_Modified'Access);
         return Gtk_Widget (Toggle);

      elsif Value_Type (Property) = GType_String then
         Gtk_New (Ent);
         Connect_Property   (Ent, String_Changed'Access);
         Connect_Controller (Ent, "changed", String_Modified'Access);
         return Gtk_Widget (Ent);

      elsif Value_Type (Property) = GType_Int then
         Gtk_New (Spin,
                  Min => Gdouble (Minimum (Param_Spec_Int (Property))),
                  Max => Gdouble (Maximum (Param_Spec_Int (Property))),
                  Step => 1.0);
         Connect_Property   (Spin, Int_Changed'Access);
         Connect_Controller (Spin, "value_changed", Int_Modified'Access);
         return Gtk_Widget (Spin);

      elsif Value_Type (Property) = GType_Uint then
         Gtk_New (Spin,
                  Min => Gdouble (Minimum (Param_Spec_Uint (Property))),
                  Max => Gdouble (Maximum (Param_Spec_Uint (Property))),
                  Step => 1.0);
         Connect_Property   (Spin, Uint_Changed'Access);
         Connect_Controller (Spin, "value_changed", Uint_Modified'Access);
         return Gtk_Widget (Spin);

      elsif Value_Type (Property) = GType_Float then
         Gtk_New (Spin,
                  Min => Gdouble (Minimum (Param_Spec_Float (Property))),
                  Max => Gdouble (Maximum (Param_Spec_Float (Property))),
                  Step => 1.0);
         Connect_Property   (Spin, Float_Changed'Access);
         Connect_Controller (Spin, "value_changed", Float_Modified'Access);
         return Gtk_Widget (Spin);

      elsif Value_Type (Property) = GType_Double then
         Gtk_New (Spin,
                  Min => Minimum (Param_Spec_Double (Property)),
                  Max => Maximum (Param_Spec_Double (Property)),
                  Step => 1.0);
         Connect_Property   (Spin, Double_Changed'Access);
         Connect_Controller (Spin, "value_changed", Double_Modified'Access);
         return Gtk_Widget (Spin);

      elsif Fundamental (Value_Type (Property)) = GType_Enum then
         Gtk_New_Text (Combo);
         E_Klass := Enumeration (Param_Spec_Enum (Property));
         K := 0;
         loop
            Val := Nth_Value (E_Klass, K);
            exit when Val = null;
            Append_Text (Combo, Nick (Val));
            K := K + 1;
         end loop;
         Connect_Property   (Combo, Enum_Changed'Access);
         Connect_Controller (Combo, "changed", Enum_Modified'Access);
         return Gtk_Widget (Combo);

      elsif Fundamental (Value_Type (Property)) = GType_Pointer then
         Gtk_New (Label);
         Set_Alignment (Label, 0.0, 0.5);
         Connect_Property (Label, Pointer_Changed'Access);
         return Gtk_Widget (Label);

      elsif Fundamental (Value_Type (Property)) = GType_Object then
         Gtk_New (Label);
         Set_Alignment (Label, 0.0, 0.5);
         Connect_Property (Label, Object_Changed'Access);
         return Gtk_Widget (Label);

      elsif Value_Type (Property) = Gdk.Color.Gdk_Color_Type then
         Gtk_New (Color);
         Connect_Property   (Color, Color_Changed'Access);
         Connect_Controller (Color, "changed", Color_Modified'Access);
         return Gtk_Widget (Color);
      end if;

      return null;
   end Property_Widget;

   ----------------------
   -- Object_Destroyed --
   ----------------------

   procedure Object_Destroyed (Editor, Object : System.Address) is
      Stub : Properties_Editor_Record;
      Ed   : constant Properties_Editor :=
        Properties_Editor (Get_User_Data (Editor, Stub));
      pragma Unreferenced (Object, Stub);
   begin
      Ed.Object := null;
      Work_On (Ed, null);
      Hide (Ed);
   end Object_Destroyed;

   -------------------
   -- Editor_Delete --
   -------------------

   procedure Editor_Delete (Editor : access GObject_Record'Class) is
      Ed : constant Properties_Editor := Properties_Editor (Editor);
   begin
      if Ed.Object /= null then
         Weak_Unref (Ed.Object, Object_Destroyed'Access, Get_Object (Ed));
      end if;

      if Global_Editor = Ed then
         Global_Editor := null;
      end if;

      Unref (Ed.Tips);
   end Editor_Delete;

   -------------------
   -- Widget_Picked --
   -------------------

   function Widget_Picked
     (Editor : access Gtk_Widget_Record'Class) return Boolean
   is
      Ed  : constant Properties_Editor := Properties_Editor (Editor);
      W   : Gtk_Widget;
   begin
      if Ed.In_Pick then
         Pointer_Ungrab (Time => 0);
         Ed.In_Pick := False;

         W := Widget_At_Pointer;
         if W /= null then
            Work_On (Ed, GObject (W));
         end if;

         return True;
      end if;
      return False;
   end Widget_Picked;

   -----------------
   -- Pick_Widget --
   -----------------

   procedure Pick_Widget (Editor : access Gtk_Widget_Record'Class) is
      Ed  : constant Properties_Editor := Properties_Editor (Editor);
      Tmp : Gdk_Grab_Status;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Pointer_Grab
        (Get_Window (Ed), False,
         Button_Release_Mask or Button_Press_Mask,
         Cursor => null, Time => 0);
      Ed.In_Pick := True;
   end Pick_Widget;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Properties_Editor) is
      Scrolled : Gtk_Scrolled_Window;
      Note     : Gtk_Notebook;
      Label    : Gtk_Label;
      Box      : Gtk_Box;
   begin
      Editor := new Properties_Editor_Record;
      Gtk.Window.Initialize (Editor, Window_Toplevel);
      Set_Title (Editor, "Properties editor");
      Set_Default_Size (Editor, 400, 400);

      Object_Callback.Connect (Editor, "destroy", Editor_Delete'Access);

      Gtk_New (Editor.Tips);
      Ref (Editor.Tips);
      Ref_Sink (Editor.Tips);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Editor, Box);

      Gtk_New (Editor.Pick_Button, "Pick");
      Pack_Start (Box, Editor.Pick_Button, Expand => False);
      Widget_Callback.Object_Connect
        (Editor.Pick_Button, "clicked", Pick_Widget'Access, Editor);
      Add_Events (Editor, Button_Release_Mask);
      Gtkada.Handlers.Return_Callback.Object_Connect
        (Editor, "button_release_event",
         Widget_Picked'Access, Editor);

      Gtk_New (Note);
      Pack_Start (Box, Note, Expand => True, Fill => True);

      --  Page 1: Properties
      Gtk_New (Label, "Properties");

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Append_Page (Note, Scrolled, Label);

      Gtk_New (Editor.Table, Rows => 0, Columns => 2, Homogeneous => False);
      Add_With_Viewport (Scrolled, Editor.Table);
      Set_Col_Spacing  (Editor.Table, 0, 5);
      Set_Row_Spacings (Editor.Table, 0);
      Modify_Font (Editor.Table, From_String ("Sans 8"));

      --  Page 2: Child properties

      Gtk_New (Label, "Child Properties");
      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Append_Page (Note, Scrolled, Label);

      Gtk_New
        (Editor.Child_Table, Rows => 0, Columns => 2, Homogeneous => False);
      Add_With_Viewport (Scrolled, Editor.Child_Table);
      Set_Col_Spacing  (Editor.Child_Table, 0, 5);
      Set_Row_Spacings (Editor.Child_Table, 0);
      Modify_Font (Editor.Child_Table, From_String ("Sans 8"));

      --  Page 3: Style properties

      Gtk_New (Label, "Style Properties");
      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Append_Page (Note, Scrolled, Label);

      Gtk_New
        (Editor.Style_Table, Rows => 0, Columns => 2, Homogeneous => False);
      Add_With_Viewport (Scrolled, Editor.Style_Table);
      Set_Col_Spacing  (Editor.Style_Table, 0, 5);
      Set_Row_Spacings (Editor.Style_Table, 0);
      Modify_Font (Editor.Style_Table, From_String ("Sans 8"));

   end Gtk_New;

   ---------------------
   -- Show_Properties --
   ---------------------

   procedure Show_Properties
     (Editor        : access Properties_Editor_Record'Class;
      Table         : Gtk_Table;
      Prop_Type     : Property_Type;
      T             : GType;
      Properties    : Param_Spec_Array)
   is
      Parent_Inserted : Boolean := False;
      Row : Guint;
      Label : Gtk_Label;
      Event : Gtk_Event_Box;
      Success : Boolean;
      W       : Gtk_Widget;
      Can_Modify : Boolean;
   begin
      for P in Properties'Range loop
         if Owner_Type (Properties (P)) = T then
            Row := Get_Property (Table, N_Rows_Property);

            if not Parent_Inserted then
               if Editor.Type_Color = Null_Color then
                  Editor.Type_Color := Parse ("red");
                  Alloc_Color (Get_Default_Colormap, Editor.Type_Color,
                               Success => Success);
               end if;

               Resize (Table, Row + 2, 2);
               Gtk_New (Label, Type_Name (T));
               Set_Alignment (Label, 0.0, 0.5);
               Modify_Fg (Label, State_Normal, Editor.Type_Color);
               Attach (Table, Label, 0, 1, Row, Row + 1,
                       Yoptions => 0);
               Row := Row + 1;
               Parent_Inserted := True;
            else
               Resize (Table, Row + 1, 2);
            end if;

            Can_Modify := Prop_Type /= Property_Style
              and then (Flags (Properties (P)) and Param_Writable) /= 0
              and then (Flags (Properties (P)) and Param_Construct_Only) = 0;

            Gtk_New (Event);
            Attach (Table, Event, 0, 1, Row, Row + 1,
                    Xpadding => 10, Yoptions => 0);
            Set_Tip (Editor.Tips, Event, Description (Properties (P)));

            Gtk_New (Label, Nick_Name (Properties (P)));
            Set_Alignment (Label, 0.0, 0.5);
            Set_Sensitive (Label, Can_Modify);
            Add (Event, Label);

            if (Flags (Properties (P)) and Param_Readable) = 0 then
               Gtk_New (Label, "<unreadable>");
               Set_Alignment (Label, 0.0, 0.5);
               W := Gtk_Widget (Label);
               Can_Modify := False;
            else
               W := Property_Widget (Editor.Object, Properties (P), Prop_Type);
               if W = null then
                  Gtk_New (Label, "uneditable type: "
                           & Type_Name (Value_Type (Properties (P))));
                  Set_Alignment (Label, 0.0, 0.5);
                  W := Gtk_Widget (Label);
                  Can_Modify := False;
               end if;
            end if;

            if W /= null then
               Attach (Table, W, 1, 2, Row, Row + 1, Yoptions => 0);
               if Prop_Type /= Property_Style then
                  Set_Sensitive (W, Can_Modify);
               end if;

               case Prop_Type is
                  when Property_Child =>
                     Child_Notify
                       (Gtk_Widget
                          (Editor.Object), Pspec_Name (Properties (P)));
                  when Property_Standard =>
                     Notify (Editor.Object, Pspec_Name (Properties (P)));
                  when Property_Style =>
                     null;
               end case;
            end if;
         end if;
      end loop;
   end Show_Properties;

   -------------------------------
   -- Show_Properties_From_Type --
   -------------------------------

   procedure Show_Properties_From_Type
     (Editor : access Properties_Editor_Record'Class;
      T      : GType) is
   begin
      if Is_Interface (T) then
         Show_Properties
           (Editor        => Editor,
            Table         => Editor.Table,
            Prop_Type     => Property_Standard,
            T             => T,
            Properties    => Interface_List_Properties
              (Default_Interface_Peek (T)));
      else
         Show_Properties
           (Editor        => Editor,
            Table         => Editor.Table,
            Prop_Type     => Property_Standard,
            T             => T,
            Properties    => Class_List_Properties
              (GObject_Class (Class_Peek (T))));
      end if;
   end Show_Properties_From_Type;

   ---------------------
   -- Remove_Children --
   ---------------------

   procedure Remove_Children
     (Container : access Gtk_Container_Record'Class)
   is
      use Widget_List;
      Children : Widget_List.Glist := Get_Children (Container);
      Tmp      : Widget_List.Glist := Children;
      N : Widget_List.Glist;
      W : Gtk_Widget;
   begin
      while Children /= Null_List loop
         N := Children;
         Children := Next (Children);
         W := Widget_List.Get_Data (N);
         Remove (Container, W);
      end loop;

      Widget_List.Free (Tmp);
   end Remove_Children;

   -------------
   -- Work_On --
   -------------

   procedure Work_On
     (Editor : access Properties_Editor_Record'Class;
      Object : GObject := null)
   is
      T : GType;
   begin
      if not In_Destruction_Is_Set (Editor) then
         if Editor.Object /= null then
            Weak_Unref
              (Editor.Object, Object_Destroyed'Access, Get_Object (Editor));
         end if;

         Remove_Children (Editor.Table);
         Resize (Editor.Table, 1, 2);
         Remove_Children (Editor.Child_Table);
         Resize (Editor.Child_Table, 1, 2);
         Remove_Children (Editor.Style_Table);
         Resize (Editor.Style_Table, 1, 2);

         Editor.Object := Object;

         if Editor.Object /= null then
            Weak_Ref
              (Editor.Object, Object_Destroyed'Access, Get_Object (Editor));

            T := Get_Type (Object);
            Set_Title (Editor, "Properties of " & Type_Name (T));

            while T /= GType_Invalid loop
               Show_Properties_From_Type (Editor, T);
               T := Parent (T);
            end loop;

            declare
               Ifaces : constant GType_Array := Interfaces (Get_Type (Object));
            begin
               for F in Ifaces'Range loop
                  Show_Properties_From_Type (Editor, Ifaces (F));
               end loop;
            end;

            --  Show the child properties owned by the parent and that apply to
            --  the current object.

            if Object.all in Gtk_Widget_Record'Class
              and then Get_Parent (Gtk_Widget (Object)) /= null
            then
               T := Get_Type (Get_Parent (Gtk_Widget (Object)));
               while T /= GType_Invalid loop
                  if Is_A (T, Gtk.Container.Get_Type) then
                     Show_Properties
                       (Editor        => Editor,
                        Table         => Editor.Child_Table,
                        Prop_Type     => Property_Child,
                        T             => T,
                        Properties    => Class_List_Child_Properties
                          (GObject_Class (Class_Peek (T))));
                  end if;
                  T := Parent (T);
               end loop;
            end if;

            --  Show the style properties

            if Object.all in Gtk_Widget_Record'Class then
               T := Get_Type (Object);
               while T /= GType_Invalid loop
                  Show_Properties
                    (Editor        => Editor,
                     Table         => Editor.Style_Table,
                     Prop_Type     => Property_Style,
                     T             => T,
                     Properties    => Class_List_Style_Properties
                       (GObject_Class (Class_Peek (T))));
                  T := Parent (T);
               end loop;
            end if;
         end if;

         Show_All (Editor);

      else
         Editor.Object := null;
      end if;
   end Work_On;

   -----------------------------
   -- Popup_Properties_Editor --
   -----------------------------

   procedure Popup_Properties_Editor
     (Object : access Glib.Object.GObject_Record'Class)
   is
   begin
      if Global_Editor = null then
         Gtk_New (Global_Editor);
      end if;

      Work_On (Global_Editor, GObject (Object));
      Present (Global_Editor);
   end Popup_Properties_Editor;

   ---------------
   -- Widget_At --
   ---------------

   function Widget_At
     (Top  : access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      use Widget_List;
      Result : Gtk_Widget;
      X0, Y0, X1, Y1, X2, Y2 : Gint;
      Tmp, Children : Widget_List.Glist;
      Child : Gtk_Widget;
   begin
      if Visible_Is_Set (Top)
        and then Get_Child_Visible (Top)
      then
         if not Is_A (Get_Type (Top), Gtk.Container.Get_Type) then
            return Gtk_Widget (Top);
         else
            if No_Window_Is_Set (Top) then
               X0 := Get_Allocation_X (Top);
               Y0 := Get_Allocation_Y (Top);
            else
               X0 := 0;
               Y0 := 0;
            end if;

            Children := Get_Children (Gtk_Container (Top));
            Tmp := Children;
            while Tmp /= Null_List loop
               Child := Get_Data (Tmp);
               Tmp := Next (Tmp);

               if Child /= null
                 and then Visible_Is_Set (Child)
                 and then Get_Child_Visible (Child)
               then
                  X1 := Get_Allocation_X (Child) - X0;
                  X2 := X1 + Get_Allocation_Width (Child);
                  Y1 := Get_Allocation_Y (Child) - Y0;
                  Y2 := Y1 + Get_Allocation_Height (Child);
                  if X1 <= X and then X <= X2
                    and then Y1 <= Y and then Y <= Y2
                  then
                     Result := Widget_At (Child, X - X1, Y - Y1);
                     exit when Result /= null;
                  end if;
               end if;

            end loop;

            if Children /= Null_List then
               Free (Children);
            end if;

            if Result = null then
               Result := Gtk_Widget (Top);
            end if;
         end if;
      end if;
      return Result;
   end Widget_At;

   -----------------------
   -- Widget_At_Pointer --
   -----------------------

   function Widget_At_Pointer return Gtk.Widget.Gtk_Widget is
      use type Gdk_Window;
      Result, Tmp : Gtk_Widget;
      Win         : Gdk_Window;
      X, Y        : Gint;
      Mask        : Gdk_Modifier_Type;
      Win_Result  : Gdk_Window;
   begin
      Window_At_Pointer (X, Y, Window => Win);
      if Win /= null then
         Tmp := Gtk_Widget (Get_User_Data (Win));
         if Tmp /= null then
            Get_Pointer (Win, X, Y, Mask, Win_Result);
            Result := Widget_At (Tmp, X, Y);
         end if;
      end if;
      return Result;
   end Widget_At_Pointer;

end Gtkada.Properties;
