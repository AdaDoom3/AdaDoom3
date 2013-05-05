
package body Lwobjects is

   function Lw_Is_Lwobject (File : String) return Boolean is
      function Internal (File : String) return Integer;
      pragma Import (C, Internal, "lw_is_lwobject");
   begin
      return Boolean'Val (Internal (File & ASCII.NUL));
   end Lw_Is_Lwobject;

   function Lw_Object_Read (File : String) return Lwobject is
      function Internal (File : String) return Lwobject;
      pragma Import (C, Internal, "lw_object_read");
   begin
      return Internal (File & ASCII.NUL);
   end Lw_Object_Read;

   procedure Lw_Object_Free (Object : in out Lwobject) is
      procedure Internal (Object : Lwobject);
      pragma Import (C, Internal, "lw_object_free");
   begin
      Internal (Object);
      Object := Null_Lwobject;
   end Lw_Object_Free;

end Lwobjects;
