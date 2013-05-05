--  Generated from glu.h
--  Date: Wed Sep 22 12:47:09 1999
--

with Interfaces.C.Extensions;
with gl_h;

--  This file provides a very basic binding to the openGL library (they
--  were generated from the Mesa files). These are mainly provided as
--  examples, not as a full binding and are provided without support.

package glu_h is
   pragma Warnings (Off);
   --  disable warnings for this file which was generated automatically

   --  Mesa 3-D graphics library
   --  Version:  3.0
   --  Copyright (C) 1995-1998  Brian Paul
   --
   --  This library is free software; you can redistribute it and/or
   --  modify it under the terms of the GNU Library General Public
   --  License as published by the Free Software Foundation; either
   --  version 2 of the License, or (at your option) any later version.
   --
   --  This library is distributed in the hope that it will be useful,
   --  but WITHOUT ANY WARRANTY; without even the implied warranty of
   --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   --  Library General Public License for more details.
   --
   --  You should have received a copy of the GNU Library General Public
   --  License along with this library; if not, write to the Free
   --  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   --

   GLU_VERSION_1_1 : constant := 1;
   GLU_TRUE        : constant := 1;
   GLU_FALSE       : constant := 0;

   type GLUenum is new Integer;
   for GLUenum 'Size use 32;

   GLU_SMOOTH : constant GLUenum := 100000;
   GLU_FLAT : constant GLUenum := 100001;
   GLU_NONE : constant GLUenum := 100002;
   GLU_POINT : constant GLUenum := 100010;
   GLU_LINE : constant GLUenum := 100011;
   GLU_FILL : constant GLUenum := 100012;
   GLU_SILHOUETTE : constant GLUenum := 100013;
   GLU_OUTSIDE : constant GLUenum := 100020;
   GLU_INSIDE : constant GLUenum := 100021;
   GLU_BEGIN : constant GLUenum := 100100;
   GLU_VERTEX : constant GLUenum := 100101;
   GLU_END : constant GLUenum := 100102;
   GLU_ERROR : constant GLUenum := 100103;
   GLU_EDGE_FLAG : constant GLUenum := 100104;
   GLU_CW : constant GLUenum := 100120;
   GLU_CCW : constant GLUenum := 100121;
   GLU_INTERIOR : constant GLUenum := 100122;
   GLU_EXTERIOR : constant GLUenum := 100123;
   GLU_UNKNOWN : constant GLUenum := 100124;
   GLU_TESS_ERROR1 : constant GLUenum := 100151;
   GLU_TESS_ERROR2 : constant GLUenum := 100152;
   GLU_TESS_ERROR3 : constant GLUenum := 100153;
   GLU_TESS_ERROR4 : constant GLUenum := 100154;
   GLU_TESS_ERROR5 : constant GLUenum := 100155;
   GLU_TESS_ERROR6 : constant GLUenum := 100156;
   GLU_TESS_ERROR7 : constant GLUenum := 100157;
   GLU_TESS_ERROR8 : constant GLUenum := 100158;
   GLU_TESS_ERROR9 : constant GLUenum := 100159;
   GLU_AUTO_LOAD_MATRIX : constant GLUenum := 100200;
   GLU_CULLING : constant GLUenum := 100201;
   GLU_PARAMETRIC_TOLERANCE : constant GLUenum := 100202;
   GLU_SAMPLING_TOLERANCE : constant GLUenum := 100203;
   GLU_DISPLAY_MODE : constant GLUenum := 100204;
   GLU_SAMPLING_METHOD : constant GLUenum := 100205;
   GLU_U_STEP : constant GLUenum := 100206;
   GLU_V_STEP : constant GLUenum := 100207;
   GLU_PATH_LENGTH : constant GLUenum := 100215;
   GLU_PARAMETRIC_ERROR : constant GLUenum := 100216;
   GLU_DOMAIN_DISTANCE : constant GLUenum := 100217;
   GLU_MAP1_TRIM_2 : constant GLUenum := 100210;
   GLU_MAP1_TRIM_3 : constant GLUenum := 100211;
   GLU_OUTLINE_POLYGON : constant GLUenum := 100240;
   GLU_OUTLINE_PATCH : constant GLUenum := 100241;
   GLU_NURBS_ERROR1 : constant GLUenum := 100251;
   GLU_NURBS_ERROR2 : constant GLUenum := 100252;
   GLU_NURBS_ERROR3 : constant GLUenum := 100253;
   GLU_NURBS_ERROR4 : constant GLUenum := 100254;
   GLU_NURBS_ERROR5 : constant GLUenum := 100255;
   GLU_NURBS_ERROR6 : constant GLUenum := 100256;
   GLU_NURBS_ERROR7 : constant GLUenum := 100257;
   GLU_NURBS_ERROR8 : constant GLUenum := 100258;
   GLU_NURBS_ERROR9 : constant GLUenum := 100259;
   GLU_NURBS_ERROR10 : constant GLUenum := 100260;
   GLU_NURBS_ERROR11 : constant GLUenum := 100261;
   GLU_NURBS_ERROR12 : constant GLUenum := 100262;
   GLU_NURBS_ERROR13 : constant GLUenum := 100263;
   GLU_NURBS_ERROR14 : constant GLUenum := 100264;
   GLU_NURBS_ERROR15 : constant GLUenum := 100265;
   GLU_NURBS_ERROR16 : constant GLUenum := 100266;
   GLU_NURBS_ERROR17 : constant GLUenum := 100267;
   GLU_NURBS_ERROR18 : constant GLUenum := 100268;
   GLU_NURBS_ERROR19 : constant GLUenum := 100269;
   GLU_NURBS_ERROR20 : constant GLUenum := 100270;
   GLU_NURBS_ERROR21 : constant GLUenum := 100271;
   GLU_NURBS_ERROR22 : constant GLUenum := 100272;
   GLU_NURBS_ERROR23 : constant GLUenum := 100273;
   GLU_NURBS_ERROR24 : constant GLUenum := 100274;
   GLU_NURBS_ERROR25 : constant GLUenum := 100275;
   GLU_NURBS_ERROR26 : constant GLUenum := 100276;
   GLU_NURBS_ERROR27 : constant GLUenum := 100277;
   GLU_NURBS_ERROR28 : constant GLUenum := 100278;
   GLU_NURBS_ERROR29 : constant GLUenum := 100279;
   GLU_NURBS_ERROR30 : constant GLUenum := 100280;
   GLU_NURBS_ERROR31 : constant GLUenum := 100281;
   GLU_NURBS_ERROR32 : constant GLUenum := 100282;
   GLU_NURBS_ERROR33 : constant GLUenum := 100283;
   GLU_NURBS_ERROR34 : constant GLUenum := 100284;
   GLU_NURBS_ERROR35 : constant GLUenum := 100285;
   GLU_NURBS_ERROR36 : constant GLUenum := 100286;
   GLU_NURBS_ERROR37 : constant GLUenum := 100287;
   GLU_INVALID_ENUM : constant GLUenum := 100900;
   GLU_INVALID_VALUE : constant GLUenum := 100901;
   GLU_OUT_OF_MEMORY : constant GLUenum := 100902;
   GLU_INCOMPATIBLE_GL_VERSION : constant GLUenum := 100903;
   GLU_VERSION : constant GLUenum := 100800;
   GLU_EXTENSIONS : constant GLUenum := 100801;

            --   Normal vectors
            --   Quadric draw styles
            --   Quadric orientation
            --   Tesselator
            --   Contour types
            --   Tesselation errors
            --   NURBS
            --   Errors
            --   New in GLU 1.1
   --
   --  These are the GLU 1.1 typedefs.  GLU 1.2 has different ones!
   --

   type GLUquadricObj is new Interfaces.C.Extensions.opaque_structure_def;
   type GLUquadricObj_Ptr      is access GLUquadricObj;
   type GLUtriangulatorObj is new Interfaces.C.Extensions.opaque_structure_def;
   type GLUtriangulatorObj_Ptr is access GLUtriangulatorObj;
   type GLUnurbsObj is new Interfaces.C.Extensions.opaque_structure_def;
   type GLUnurbsObj_Ptr        is access GLUnurbsObj;

   --
   --  Miscellaneous functions
   --

   procedure gluLookAt (eyex    : gl_h.GLdouble;
                        eyey    : gl_h.GLdouble;
                        eyez    : gl_h.GLdouble;
                        centerx : gl_h.GLdouble;
                        centery : gl_h.GLdouble;
                        centerz : gl_h.GLdouble;
                        upx     : gl_h.GLdouble;
                        upy     : gl_h.GLdouble;
                        upz     : gl_h.GLdouble);
   procedure gluOrtho2D (left   : gl_h.GLdouble;
                         right  : gl_h.GLdouble;
                         bottom : gl_h.GLdouble;
                         top    : gl_h.GLdouble);
   procedure gluPerspective (fovy   : gl_h.GLdouble;
                             aspect : gl_h.GLdouble;
                             zNear  : gl_h.GLdouble;
                             zFar   : gl_h.GLdouble);
   procedure gluPickMatrix (x        : gl_h.GLdouble;
                            y        : gl_h.GLdouble;
                            width    : gl_h.GLdouble;
                            height   : gl_h.GLdouble;
                            viewport : gl_h.GLint_Vec_4);
   function gluProject (objx        : gl_h.GLdouble;
                        objy        : gl_h.GLdouble;
                        objz        : gl_h.GLdouble;
                        modelMatrix : gl_h.GLdouble_Vec_16;
                        projMatrix  : gl_h.GLdouble_Vec_16;
                        viewport    : gl_h.GLint_Vec_4;
                        winx        : access gl_h.GLdouble;
                        winy        : access gl_h.GLdouble;
                        winz        : access gl_h.GLdouble) return gl_h.GLint;
   function gluUnProject
       (winx        : gl_h.GLdouble;
        winy        : gl_h.GLdouble;
        winz        : gl_h.GLdouble;
        modelMatrix : gl_h.GLdouble_Vec_16;
        projMatrix  : gl_h.GLdouble_Vec_16;
        viewport    : gl_h.GLint_Vec_4;
        objx        : access gl_h.GLdouble;
        objy        : access gl_h.GLdouble;
        objz        : access gl_h.GLdouble) return gl_h.GLint;
   function gluErrorString (errorCode : gl_h.GLenum) return gl_h.GLubyte_Ptr;

   --
   --  Mipmapping and image scaling
   --

   function gluScaleImage
       (format    : gl_h.GLenum;
        widthin   : gl_h.GLint;
        heightin  : gl_h.GLint;
        typein    : gl_h.GLenum;
        datain    : Interfaces.C.Extensions.void_ptr;
        widthout  : gl_h.GLint;
        heightout : gl_h.GLint;
        typeout   : gl_h.GLenum;
        dataout   : Interfaces.C.Extensions.void_ptr) return gl_h.GLint;
   function gluBuild1DMipmaps
       (target     : gl_h.GLenum;
        components : gl_h.GLint;
        width      : gl_h.GLint;
        format     : gl_h.GLenum;
        type_Id    : gl_h.GLenum;
        data       : Interfaces.C.Extensions.void_ptr) return gl_h.GLint;
   function gluBuild2DMipmaps
       (target     : gl_h.GLenum;
        components : gl_h.GLint;
        width      : gl_h.GLint;
        height     : gl_h.GLint;
        format     : gl_h.GLenum;
        type_Id    : gl_h.GLenum;
        data       : Interfaces.C.Extensions.void_ptr) return gl_h.GLint;

   --
   --  Quadrics
   --

   function gluNewQuadric return GLUquadricObj_Ptr;
   procedure gluDeleteQuadric (state : access GLUquadricObj);
   procedure gluQuadricDrawStyle (quadObject : access GLUquadricObj;
                                  drawStyle  : gl_h.GLenum);
   procedure gluQuadricOrientation (quadObject  : access GLUquadricObj;
                                    orientation : gl_h.GLenum);
   procedure gluQuadricNormals (quadObject : access GLUquadricObj;
                                normals    : gl_h.GLenum);
   procedure gluQuadricTexture (quadObject    : access GLUquadricObj;
                                textureCoords : gl_h.GLboolean);

   type glu_h_proc_1 is access procedure;

   procedure gluQuadricCallback (qobj  : access GLUquadricObj;
                                 which : gl_h.GLenum;
                                 fn    : glu_h_proc_1);
   procedure gluCylinder (qobj       : access GLUquadricObj;
                          baseRadius : gl_h.GLdouble;
                          topRadius  : gl_h.GLdouble;
                          height     : gl_h.GLdouble;
                          slices     : gl_h.GLint;
                          stacks     : gl_h.GLint);
   procedure gluSphere (qobj   : access GLUquadricObj;
                        radius : gl_h.GLdouble;
                        slices : gl_h.GLint;
                        stacks : gl_h.GLint);
   procedure gluDisk (qobj        : access GLUquadricObj;
                      innerRadius : gl_h.GLdouble;
                      outerRadius : gl_h.GLdouble;
                      slices      : gl_h.GLint;
                      loops       : gl_h.GLint);
   procedure gluPartialDisk (qobj        : access GLUquadricObj;
                             innerRadius : gl_h.GLdouble;
                             outerRadius : gl_h.GLdouble;
                             slices      : gl_h.GLint;
                             loops       : gl_h.GLint;
                             startAngle  : gl_h.GLdouble;
                             sweepAngle  : gl_h.GLdouble);

   --
   --  Nurbs
   --

   function gluNewNurbsRenderer return GLUnurbsObj_Ptr;
   procedure gluDeleteNurbsRenderer (nobj : access GLUnurbsObj);
   procedure gluLoadSamplingMatrices (nobj        : access GLUnurbsObj;
                                      modelMatrix : gl_h.GLfloat_Vec_16;
                                      projMatrix  : gl_h.GLfloat_Vec_16;
                                      viewport    : gl_h.GLint_Vec_4);
   procedure gluNurbsProperty (nobj     : access GLUnurbsObj;
                               property : gl_h.GLenum;
                               value    : gl_h.GLfloat);
   procedure gluGetNurbsProperty (nobj     : access GLUnurbsObj;
                                  property : gl_h.GLenum;
                                  value    : access gl_h.GLfloat);
   procedure gluBeginCurve (nobj : access GLUnurbsObj);
   procedure gluEndCurve (nobj : access GLUnurbsObj);
   procedure gluNurbsCurve (nobj     : access GLUnurbsObj;
                            nknots   : gl_h.GLint;
                            knot     : access gl_h.GLfloat;
                            stride   : gl_h.GLint;
                            ctlarray : access gl_h.GLfloat;
                            order    : gl_h.GLint;
                            type_Id  : gl_h.GLenum);
   procedure gluBeginSurface (nobj : access GLUnurbsObj);
   procedure gluEndSurface (nobj : access GLUnurbsObj);
   procedure gluNurbsSurface (nobj        : access GLUnurbsObj;
                              sknot_count : gl_h.GLint;
                              sknot       : access gl_h.GLfloat;
                              tknot_count : gl_h.GLint;
                              tknot       : access gl_h.GLfloat;
                              s_stride    : gl_h.GLint;
                              t_stride    : gl_h.GLint;
                              ctlarray    : access gl_h.GLfloat;
                              sorder      : gl_h.GLint;
                              torder      : gl_h.GLint;
                              type_Id     : gl_h.GLenum);
   procedure gluBeginTrim (nobj : access GLUnurbsObj);
   procedure gluEndTrim (nobj : access GLUnurbsObj);
   procedure gluPwlCurve (nobj     : access GLUnurbsObj;
                          count    : gl_h.GLint;
                          array_Id : access gl_h.GLfloat;
                          stride   : gl_h.GLint;
                          type_Id  : gl_h.GLenum);

   type glu_h_proc_2 is access procedure;

   procedure gluNurbsCallback (nobj  : access GLUnurbsObj;
                               which : gl_h.GLenum;
                               fn    : glu_h_proc_2);

   --
   --  Polygon tesselation
   --

   function gluNewTess return GLUtriangulatorObj_Ptr;

   type glu_h_proc_3 is access procedure;

   procedure gluTessCallback (tobj  : access GLUtriangulatorObj;
                              which : gl_h.GLenum;
                              fn    : glu_h_proc_3);
   procedure gluDeleteTess (tobj : access GLUtriangulatorObj);
   procedure gluBeginPolygon (tobj : access GLUtriangulatorObj);
   procedure gluEndPolygon (tobj : access GLUtriangulatorObj);
   procedure gluNextContour (tobj    : access GLUtriangulatorObj;
                             type_Id : gl_h.GLenum);
   procedure gluTessVertex (tobj : access GLUtriangulatorObj;
                            v    : gl_h.GLdouble_Vec_3;
                            data : Interfaces.C.Extensions.void_ptr);

   --
   --  New functions in GLU 1.1
   --

   function gluGetString (name : gl_h.GLenum) return gl_h.GLubyte_Ptr;

private

   pragma Import (Stdcall, gluLookAt, "gluLookAt");
   pragma Import (Stdcall, gluOrtho2D, "gluOrtho2D");
   pragma Import (Stdcall, gluPerspective, "gluPerspective");
   pragma Import (Stdcall, gluPickMatrix, "gluPickMatrix");
   pragma Import (Stdcall, gluProject, "gluProject");
   pragma Import (Stdcall, gluUnProject, "gluUnProject");
   pragma Import (Stdcall, gluErrorString, "gluErrorString");
   pragma Import (Stdcall, gluScaleImage, "gluScaleImage");
   pragma Import (Stdcall, gluBuild1DMipmaps, "gluBuild1DMipmaps");
   pragma Import (Stdcall, gluBuild2DMipmaps, "gluBuild2DMipmaps");
   pragma Import (Stdcall, gluNewQuadric, "gluNewQuadric");
   pragma Import (Stdcall, gluDeleteQuadric, "gluDeleteQuadric");
   pragma Import (Stdcall, gluQuadricDrawStyle, "gluQuadricDrawStyle");
   pragma Import (Stdcall, gluQuadricOrientation, "gluQuadricOrientation");
   pragma Import (Stdcall, gluQuadricNormals, "gluQuadricNormals");
   pragma Import (Stdcall, gluQuadricTexture, "gluQuadricTexture");
   pragma Import (Stdcall, gluQuadricCallback, "gluQuadricCallback");
   pragma Import (Stdcall, gluCylinder, "gluCylinder");
   pragma Import (Stdcall, gluSphere, "gluSphere");
   pragma Import (Stdcall, gluDisk, "gluDisk");
   pragma Import (Stdcall, gluPartialDisk, "gluPartialDisk");
   pragma Import (Stdcall, gluNewNurbsRenderer, "gluNewNurbsRenderer");
   pragma Import (Stdcall, gluDeleteNurbsRenderer, "gluDeleteNurbsRenderer");
   pragma Import (Stdcall, gluLoadSamplingMatrices, "gluLoadSamplingMatrices");
   pragma Import (Stdcall, gluNurbsProperty, "gluNurbsProperty");
   pragma Import (Stdcall, gluGetNurbsProperty, "gluGetNurbsProperty");
   pragma Import (Stdcall, gluBeginCurve, "gluBeginCurve");
   pragma Import (Stdcall, gluEndCurve, "gluEndCurve");
   pragma Import (Stdcall, gluNurbsCurve, "gluNurbsCurve");
   pragma Import (Stdcall, gluBeginSurface, "gluBeginSurface");
   pragma Import (Stdcall, gluEndSurface, "gluEndSurface");
   pragma Import (Stdcall, gluNurbsSurface, "gluNurbsSurface");
   pragma Import (Stdcall, gluBeginTrim, "gluBeginTrim");
   pragma Import (Stdcall, gluEndTrim, "gluEndTrim");
   pragma Import (Stdcall, gluPwlCurve, "gluPwlCurve");
   pragma Import (Stdcall, gluNurbsCallback, "gluNurbsCallback");
   pragma Import (Stdcall, gluNewTess, "gluNewTess");
   pragma Import (Stdcall, gluTessCallback, "gluTessCallback");
   pragma Import (Stdcall, gluDeleteTess, "gluDeleteTess");
   pragma Import (Stdcall, gluBeginPolygon, "gluBeginPolygon");
   pragma Import (Stdcall, gluEndPolygon, "gluEndPolygon");
   pragma Import (Stdcall, gluNextContour, "gluNextContour");
   pragma Import (Stdcall, gluTessVertex, "gluTessVertex");
   pragma Import (Stdcall, gluGetString, "gluGetString");

end glu_h;

