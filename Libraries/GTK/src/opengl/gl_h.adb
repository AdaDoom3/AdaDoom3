package body gl_h is

   ---------------
   -- glLightfv --
   ---------------

   procedure glLightfv (light  : GLenum;
                        pname  : GLenum;
                        params : GLfloat_Vec_4)
   is
      procedure Internal (Light, Pname : GLenum; Params : System.Address);
      pragma Import (Stdcall, Internal, "glLightfv");
   begin
      Internal (light, pname, params'Address);
   end glLightfv;

   ------------------
   -- glMaterialfv --
   ------------------

   procedure glMaterialfv (face   : GLenum;
                           pname  : GLenum;
                           params : GLfloat_Vec_4)
   is
      procedure Internal (face, pname : GLenum; params : System.Address);
      pragma Import (Stdcall, Internal, "glMaterialfv");
   begin
      Internal (face, pname, params'Address);
   end glMaterialfv;

   -------------------
   -- glGenTextures --
   -------------------

   procedure glGenTextures (n : GLsizei; textures : in out GLuint_Vec) is
      procedure Internal (n : GLsizei; textures : System.Address);
      pragma Import (Stdcall, Internal, "glGenTextures");
   begin
      if n <= textures'Length then
         Internal (n, textures'Address);
      else
         textures := (others => 0);
      end if;
   end glGenTextures;

   -------------
   -- glFogfv --
   -------------

   procedure glFogfv (pname : GLenum; params : GLfloat_Vec_4) is
      procedure Internal (pname : GLenum; params : System.Address);
      pragma Import (Stdcall, Internal, "glFogfv");
   begin
      Internal (pname, params'Address);
   end glFogfv;

end gl_h;
