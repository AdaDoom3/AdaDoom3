
with System;

package Lwobjects is

   type Lwobject is private;
   Null_Lwobject : constant Lwobject;

   function Lw_Is_Lwobject (File : String) return Boolean;
   function Lw_Object_Read (File : String) return Lwobject;
   procedure Lw_Object_Free (Object : in out Lwobject);
   procedure Lw_Object_Show (Object : Lwobject);

   function Lw_Object_Radius (Object : Lwobject) return Float;
   procedure Lw_Object_Scale (Object : Lwobject; Scale : Float);

private
   type Lwobject is new System.Address;
   Null_Lwobject : constant Lwobject := Lwobject (System.Null_Address);

   pragma Import (C, Lw_Object_Show, "lw_object_show");
   pragma Import (C, Lw_Object_Radius, "lw_object_radius");
   pragma Import (C, Lw_Object_Scale, "lw_object_scale");

end Lwobjects;
