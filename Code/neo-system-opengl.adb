--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
package body Neo.System.OpenGL
  is
  ---------------------------
  -- Operation_To_String_2 --
  ---------------------------
    function Operation_To_String_2(
      Operation : in Integer_4_Unsigned)
      return String_2
      is
      begin
        case Operation is
          when 0 =>
            return "";
          when others =>
            return "";
        end case;
      end Operation_To_String_2;
  ----------------
  -- To_Boolean --
  ----------------
    function To_Boolean(
      Item : in Integer_1_Unsigned_C)
      return Boolean
      is
        if Item = GL_TRUE then
          return True;
        else
          return False;
        end if;
      begin
      end To_Boolean;
  -----------------------------
  -- To_Integer_1_Unsigned_C --
  -----------------------------
    function To_Integer_1_Unsigned_C(
      Item : in Boolean)
      return Integer_1_Unsigned_C
      is
      begin
        if Item then
          return GL_TRUE;
        else
          return GL_FALSE;
        end if;
      end To_Integer_1_Unsigned_C;
  -----------------------------------
  -- To_Array_Integer_1_Unsigned_C --
  -----------------------------------
    function To_Array_Integer_1_Unsigned_C(
      Item : in Array_Boolean)
      return Array_Integer_1_Unsigned_C
      is
      Output : Array_Integer_1_Unsigned_C(Item'first..Item'Length);
      begin
        for I in range(Item'first..Item'Length) loop
          Output(I) := To_Integer_1_Unsigned_C(Item(I));
        end loop;
        return Output;
      end To_Array_Integer_1_Unsigned_C;
  -----------------------------------
  -- To_Array_Integer_1_Unsigned_C --
  -----------------------------------
    function To_Array_Integer_1_Unsigned_C(
      Item : in Array_Integer_1_Unsigned)
      return Array_Integer_1_Unsigned_C
      is
      Output : Array_Integer_1_Unsigned_C(Item'first .. Item'last);
      begin
        for I in range(Item'first .. Item'last) loop
          Output(I) := Integer_1_Unsigned_C(Item(I));
        end loop;
        return Output;
      end To_Array_Integer_1_Unsigned_C;
  ---------------------------------
  -- To_Array_Integer_1_Unsigned --
  ---------------------------------
    function To_Array_Integer_1_Unsigned(
      Item : in Array_Integer_1_Unsigned_C)
      return Array_Integer_1_Unsigned
      is
      Output : Array_Integer_1_Unsigned(Item'first .. Item'last);
      begin
        for I in range(Item'first .. Item'last) loop
          Output(I) := Integer_1_Unsigned(Item(I));
        end loop;
        return Output;
      end To_Array_Integer_1_Unsigned;
  ----------------------
  -- To_Array_Boolean --
  ----------------------
    function To_Array_Boolean(
      Item : in Array_Integer_1_Unsigned_C)
      return Array_Boolean
      is
      Output : Array_Boolean(Item'first..Item'last);
      begin
        for I in range(Item'first..Item'last) loop
          Output(I) := To_Boolean(Item(I));
        end loop;
        return Output;
      end To_Array_Boolean;
  ---------------------------
  -- To_Array_Float_4_Real --
  ---------------------------
    function To_Array_Float_4_Real(
      Item : in Array_Float_4_Real_C)
      return Array_Float_4_Real
      is
      Output : Array_Float_4_Real(Item'first..Item'last);
      begin
        for I in range(Item'first..Item'last) loop
          Output(I) := Float_4_Real(Item(I));
        end loop;
        return Output;
      end To_Array_Float_4_Real;
  ----------------
  -- Accumulate --
  ----------------
    procedure Accumulate(
      Operation : in Integer_4_Unsigned;
      Value     : in Float_4_Real)
      is
      begin
        if glAccum = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glAccum"                        & "( " &
          Operation_To_String_2(Operation) & ", " &
          Float_4_Real'Wide_Image(Value)   & " )" );
        glAccum.All(
          Integer_4_Unsigned_C(Operation),
          Float_4_Real_C(Value));
      end if;
  --------------------
  -- Alpha_Function --
  --------------------
    procedure Alpha_Function(
      Operation : in Integer_4_Unsigned;
      Referece  : in Float_4_Real)
      is
      begin
        if glAlphaFunc = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glAlphaFunc"          & "( " & 
          To_String_2(Operation) & ", " &
          Float_4_Real(Value)    & " )" );
        glAlphaFunc.All(
          Integer_4_Unsigned_C(Operation),
          Float_4_Real_C(Reference));
      end Alpha_Function;
  ---------------------------
  -- Are_Textures_Resident --
  ---------------------------
    function Are_Textures_Resident(
      Textures   : in Array_Integer_4_Unsigned_C
      Residences : in Access_Array_Boolean)
      return Boolean
      is
      Residences_C : Array_Integer_1_Unsigned_C(Residences'first .. Residences'last) := To_Array_Integer_1_Unsigned_C(Residences.All);
      Output       : Boolean                                                         := 0;
      begin
        if glAreTexturesResident = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glAreTexturesResident"                              & "( " &
          Natural'Wide_Image(Textures'Length)                  & ", " & 
          "'const GLuint * textures' 'GLboolean * residences'" & " )" );
        Output := To_Boolean(glAreTexturesResident.All(
          Integer_4_Unsigned_C(Textures'Length),
          Textures'address,
          Residences_C'address));
        Residences.All := To_Array_Boolean(Residences_C);
        return Output;
      end Are_Textures_Resident;
  -------------------
  -- Array_Element --
  -------------------
    procedure Array_Element(
      Index : Integer_4_Signed);
      is
      begin
        if glArrayElement = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glArrayElement"                   & "( " &
          Integer_4_Signed'Wide_Image(Index) & " )" );
        glArrayElement.All(
          Integer_4_Signed_C(Index));
      end Array_Element;
  -------------------
  -- Begin_Drawing --
  -------------------
    procedure Begin_Drawing(
      Mode : in Integer_4_Unsigned)
      is
      begin
        if glBegin = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glBegin"            & "( " & 
          Get_String.all(Mode) & " )" );
        glBegin.all(
          Integer_4_Unsigned_C(Mode));
      end Begin_Drawing;
  ------------------
  -- Bind_Texture --
  ------------------
    procedure Bind_Texture(
      Target  : in Integer_4_Unsigned
      Texture : in Integer_4_Unsigned)
      is
      begin
        if glBindTexture = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glBindTexture"               & "( " & 
          Get_String(Target)            & ", " &
          Integer_4_Unsigned_C(Texture) & " )" );
        glBindTexture.All(
          Integer_4_Unsigned_C(Target), 
          Integer_ 4_Unsigned_C(Texture);
      end Bind_Texture;
  ------------
  -- Bitmap --
  ------------
    procedure Bitmap(
      Width    : in Integer_4_Signed;
      Height   : in Integer_4_Signed;
      X_Origin : in Float_4_Real;
      Y_Origin : in Float_4_Real;
      X_Move   : in Float_4_Real;
      Y_Move   : in Float_4_Real;
      Bitmap   : in Array_Integer_1_Unsigned)
      is
      begin
        if glBitmap = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glBitmap"                          & "( " &
          Integer_4_Singed'Wide_Image(Width)  & ", " &
          Integer_4_Singed'Wide_Image(Height) & ", " &
          Float_4_Real'Wide_Image(X_Origin)   & ", " &
          Float_4_Real'Wide_Image(Y_Origin)   & ", " &
          Float_4_Real'Wide_Image(X_Move)     & ", " &
          Float_4_Real'Wide_Image(Y_Move)     & ", " &
          "const GLubyte * bitmap"            & " )" );
        glBitmap.All(
          Integer_4_Unsigned_C(Width),
          Integer_4_Unsigned_C(Height),
          Integer_4_Unsigned_C(X_Origin),
          Integer_4_Unsigned_C(Y_Origin),
          Integer_4_Unsigned_C(X_Move),
          Integer_4_Unsigned_C(Y_Move),
          Bitmap'address);
      end Bitmap;
  --------------------
  -- Blend_Function --
  --------------------
    procedure Blend_Function(
      S_Factor : in Integer_4_Unsigned;
      D_Factor : in Integer_4_Unsigned)
      is
      begin
        if glBlendFunc = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glBlendFunc"        & "( " &
          Get_String(S_Factor) & ", " &
          Get_String(D_Factor) & " )" );
        glBlendFunc.All(
          Integer_4_Unsigned_C(S_Factor),
          Integer_4_Unsigned_C(D_Factor));
      end Blend_Function;
  ---------------
  -- Call_List --
  ---------------
    procedure Call_List(
      List : in Integer_4_Unsigned)
      is
      begin
        if glCallList = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCallList" & "( " &
          Integer_4_Unsigned(List));
        glCallList.All(
          Integer_4_Unsigned_C(List));
      end Call_List;
  ----------------
  -- Call_Lists --
  ----------------
    procedure Call_Lists(
      Number   : in Integer_4_Signed;
      Grouping : in Integer_4_Unsigned := GL_UNSIGNED_BYTE;
      Lists    : in Array_Integer_1_Unsigned)
      is
      begin
        if glCallLists = null then
          raise System_Call_Failure;
        end if;
        if Grouping /= GL_UNSIGNED_BYTE or
           Grouping /= GL_2_BYTES       or
           Grouping /= GL_3_BYTES       or
           Grouping /= GL_4_BYTES       then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCallLists"                         & "( " &
          Integer_4_Signed_C'Wide_Image(Number) & ", " &
          Get_String(Grouping)                  & ", " &
          "'const GLvoid * lists'"              & " )" );
        glCallLists.All(
          Integer_4_Signed_C(Number),
          Integer_4_Unsigned_C(Pairing),
          Lists'address);
      end Call_Lists;
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Integer_2_Unsigned)
      is
      begin
        if glCallLists = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCallLists"                         & "( " &
          Integer_4_Signed_C'Wide_Image(Number) & ", " &
          Get_String(GL_UNSIGNED_SHORT)         & ", " &
          "'const GLvoid * lists'"              & " )" );
        glCallLists.All(
          Integer_4_Signed_C(Number),
          Integer_4_Unsigned_C(GL_UNSIGNED_SHORT)),
          Lists'address);
      end Call_Lists;
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Integer_4_Unsigned)
      is
      begin
        if glCallLists = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCallLists"                         & "( " &
          Integer_4_Signed_C'Wide_Image(Number) & ", " &
          Get_String(GL_UNSIGNED_INT)           & ", " &
          "'const GLvoid * lists'"              & " )" );
        glCallLists.All(
          Integer_4_Signed_C(Number),
          Integer_4_Unsigned_C(GL_UNSIGNED_INT)),
          Lists'address);
      end Call_Lists;
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Integer_1_Signed)
      is
      begin
        if glCallLists = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCallLists"                         & "( " &
          Integer_4_Signed_C'Wide_Image(Number) & ", " &
          Get_String(GL_BYTE)                   & ", " &
          "'const GLvoid * lists'"              & " )" );
        glCallLists.All(
          Integer_4_Signed_C(Number),
          Integer_4_Unsigned_C(GL_BYTE)),
          Lists'address);
      end Call_Lists;
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Integer_2_Signed)
      is
      begin
        if glCallLists = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCallLists"                         & "( " &
          Integer_4_Signed_C'Wide_Image(Number) & ", " &
          Get_String(GL_SHORT)                  & ", " &
          "'const GLvoid * lists'"              & " )" );
        glCallLists.All(
          Integer_4_Signed_C(Number),
          Integer_4_Unsigned_C(GL_SHORT)),
          Lists'address);
      end Call_Lists;
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Integer_4_Signed)
      is
      begin
        if glCallLists = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCallLists"                         & "( " &
          Integer_4_Signed_C'Wide_Image(Number) & ", " &
          Get_String(GL_INT)                    & ", " &
          "'const GLvoid * lists'"              & " )" );
        glCallLists.All(
          Integer_4_Signed_C(Number),
          Integer_4_Unsigned_C(GL_INT)),
          Lists'address);
      end Call_Lists;
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Float_4_Real)
      is
      begin
        if glCallLists = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCallLists"                         & "( " &
          Integer_4_Signed_C'Wide_Image(Number) & ", " &
          Get_String(GL_FLOAT)                  & ", " &
          "'const GLvoid * lists'"              & " )" );
        glCallLists.All(
          Integer_4_Signed_C(Number),
          Integer_4_Unsigned_C(GL_FLOAT)),
          Lists'address);
      end Call_Lists;
  -----------
  -- Clear --
  -----------
    procedure Clear(
      Mask : in Integer_4_Unsigned)
      is
      begin
        if glClear = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glClear"                           & "( " &
          Integer_4_Unsigned'Wide_Image(Mask) & " )" );
        glClear.All(
          Integer_4_Unsigned_C(Mask));
      end Clear;
  ------------------------
  -- Clear_Accumulation --
  ------------------------
    procedure Clear_Accumulation(
      Red   : in Float_4_Real;
      Green : in Float_4_Real;
      Blue  : in Float_4_Real;
      Alpha : in Float_4_Real)
      is
      begin
        if glClearAccum = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glClearAccum"                 & "( " &
          Float_4_Real'Wide_Image(Red)   & ", " &
          Float_4_Real'Wide_Image(Green) & ", " &
          Float_4_Real'Wide_Image(Blue)  & ", " &
          Float_4_Real'Wide_Image(Alpha) & " )" );
        glClearAccum.All(
          Float_4_Real_C(Red),
          Float_4_Real_C(Green),
          Float_4_Real_C(Blue),
          Float_4_Real_C(Alpha));
      end Clear_Accumulation;
  -----------------
  -- Clear_Color --
  -----------------
    procedure Clear_Color(
      Red   : in Float_4_Real;
      Green : in Float_4_Real;
      Blue  : in Float_4_Real;
      Alpha : in Float_4_Real)
      is
      begin
        if glClearColor = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glClearColor"                 & "( " &
          Float_4_Real'Wide_Image(Red)   & ", " &
          Float_4_Real'Wide_Image(Green) & ", " &
          Float_4_Real'Wide_Image(Blue)  & ", " &
          Float_4_Real'Wide_Image(Alpha) & " )" );
        glClearColor.All(
          Float_4_Real_C(Red),
          Float_4_Real_C(Green),
          Float_4_Real_C(Blue),
          Float_4_Real_C(Alpha));
      end Clear_Color;
  -----------------
  -- Clear_Depth --
  -----------------
    procedure Clear_Depth(
      Depth : in Float_8_Real)
      is
      begin
        if glClearDepth = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glClearDepth"                 & "( " &
          Float_8_Real'Wide_Image(Depth) & " )" );
        glClearDepth.All(
          Float_8_Real_C(Depth));
      end Clear_Depth;
  -----------------
  -- Clear_Index --
  -----------------
    procedure Clear_Index(
      C : in Float_4_Real)
      is
      begin
        if glClearIndex = null then
          raise System_Call_Failure;
        end if;
        Put_Line("glClearIndex" & "( " &
          Float_4_Real(C)       & " )" );
        glClearIndex.All(
          Float_4_Real_C(C));
      end Clear_Index;.
  -------------------
  -- Clear_Stencil --
  -------------------
    procedure Clear_Stencil(
      S : in Integer_4_Signed)
      is
      begin
        if glClearStencil = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glClearStencil"               & "( " &
          Integer_4_Signed'Wide_Image(S) & " )" );
        glClearStencil.All(
          Integer_4_Signed_C(S));
      end Clear_Stencil;
  ----------------
  -- Clip_Plane --
  ----------------
    procedure Clip_Plane(
      Plane    : in Integer_4_Unsigned;
      Equation : in Vector_4_Float_8_Real) --VECTOR
      is
      begin
        if glClipPlane = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glClipPlane"        & "( " &
          Get_String(Plane)    & ", " &
          "Vector"             & "( " &
            Float_8_Real'Wide_Image(V(1)) & ", " &
            Float_8_Real'Wide_Image(V(2)) & ", " &
            Float_8_Real'Wide_Image(V(3)) & ", " &
            Float_8_Real'Wide_Image(V(4)) & " )" & " )" );
        glClipPlane.All(
          Integer_4_Unsigned_C(Plane),
          Equation'address);
      end Clip_Plane;
  -----------
  -- Color --
  -----------
    procedure Color(
      Red   : in Integer_1_Unsigned;
      Green : in Integer_1_Unsigned;
      Blue  : in Integer_1_Unsigned)
      is
      begin
        if glColor3b = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3b"                          & "( " &
          Integer_1_Unsigned'Wide_Image(Red)   & ", " &
          Integer_1_Unsigned'Wide_Image(Green) & ", " &
          Integer_1_Unsigned'Wide_Image(Blue)  & " )" );
        glColor3b.All(
          Integer_1_Unsigned_C(Red),
          Integer_1_Unsigned_C(Green),
          Integer_1_Unsigned_C(Blue));
      end Color;
    procedure Color(
      V : in Vector_3_Integer_1_Signed) --VECTOR
      is
      begin
        if glColor3bv = null then
          raise System_Call_Failure;
        end if;
        Put_Line("glColor3bv"  & "( " &
          "Vector"      & "( " &
            Integer_1_Signed'Wide_Image(V(1)) & ", " &
            Integer_1_Signed'Wide_Image(V(2)) & ", " &
            Integer_1_Signed'Wide_Image(V(3)) & " )" & " )" );
        glColor3bv.All(
          V'address);
      end Color;
    procedure Color(
      Red   : in Float_8_Real;
      Green : in Float_8_Real;
      Blue  : in Float_8_Real)
      is
      begin
        if glColor3d = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3d"                    & "( " &
          Float_8_Real'Wide_Image(Red)   & ", " &
          Float_8_Real'Wide_Image(Green) & ", " &
          Float_8_Real'Wide_Image(Blue)  & " )" );
        glColor3d.All(
          Float_8_Real_C(Red),
          Float_8_Real_C(Green),
          Float_8_Real_C(Blue));
      end Color;
    procedure Color(
      V : in Vector_3_Float_8_Real) --VECTOR
      is
      begin
        if glColor3dv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3dv"  & "( " &
          "Vector"      & "( " &
            Float_8_Real'Wide_Image(V(1)) & ", " &
            Float_8_Real'Wide_Image(V(2)) & ", " &
            Float_8_Real'Wide_Image(V(3)) & " )" & " )" );
        glColor3dv.All(
          V'address);
      end Color;
    procedure Color(
      Red   : in Float_4_Real;
      Green : in Float_4_Real;
      Blue  : in Float_4_Real)
      is
      begin
        if glColor3f = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3f"                    & "( " &
          Float_4_Real'Wide_Image(Red)   & ", " & 
          Float_4_Real'Wide_Image(Green) & ", " &
          Float_4_Real'Wide_Image(Blue)  & " )" );
        glColor3f.All(
          Float_4_Real_C(Red),
          Float_4_Real_C(Green),
          Float_4_Real_C(Blue));
      end Color;
    procedure Color(
      V : in Vector_3_Float_4_Real) --VECTOR
      is
      begin
        if glColor3sfv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3fv"  & "( " &
          "Vector"      & "( " &
            Float_4_Real'Wide_Image(V(1)) & ", " &
            Float_4_Real'Wide_Image(V(2)) & ", " &
            Float_4_Real'Wide_Image(V(3)) & " )" & " )" );
        glColor3fv.All(
          V'address);
    procedure Color(
      Red   : in Integer_4_Signed;
      Green : in Integer_4_Signed;
      Blue  : in Integer_4_Signed)
      is
      begin
        if glColor3i = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3i"                        & "( " &
          Integer_4_Signed'Wide_Image(Red)   & ", " &
          Integer_4_Signed'Wide_Image(Green) & ", " &
          Integer_4_Signed'Wide_Image(Blue)  & " )" );
        glColor3i.All(
          Integer_4_Signed_C(Red),
          Integer_4_Signed_C(Green),
          Integer_4_Signed_C(Blue));
      end Color;
    procedure Color(
      V : in Vector_3_Integer_4_Signed) --VECTOR
      is
      begin
        if glColor3iv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3iv"  & "( " &
          "Vector"      & "( " &
            Integer_4_Signed'Wide_Image(V(1)) & ", " &
            Integer_4_Signed'Wide_Image(V(2)) & ", " &
            Integer_4_Signed'Wide_Image(V(3)) & " )" & " )" );
        glColor3iv.All(
          V'address);
      end Color;
    procedure Color(
      Red   : in Integer_2_Signed;
      Green : in Integer_2_Signed;
      Blue  : in Integer_2_Signed)
      is
      begin
        if glColor3s = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3s"                        & "( " &
          Integer_2_Signed'Wide_Image(Red)   & ", " &
          Integer_2_Signed'Wide_Image(Green) & ", " &
          Integer_2_Signed'Wide_Image(Blue)  & " )" );
        glColor3s.All(
          Integer_2_Singed(Red),
          Integer_2_Singed(Green),
          Integer_2_Singed(Blue));
      end Color;
    procedure Color(
      V : in Vector_3_Integer_2_Signed) --VECTOR
      is
      begin
        if glColor3sv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3sv"  & "( " &
          "Vector"      & "( " &
            Integer_2_Signed'Wide_Image(V(1)) & ", " &
            Integer_2_Signed'Wide_Image(V(2)) & ", " &
            Integer_2_Signed'Wide_Image(V(3)) & " )" & " )" );
        glColor3sv.All(
          V'address);
      end Color;
    procedure Color(
      Red   : in Integer_1_Unsigned;
      Green : in Integer_1_Unsigned;
      Blue  : in Integer_1_Unsigned)
      is
      begin
        if glColor3ub = null then
          raise System_Call_Failure;
        end if;
        Put_Line("glColor3ub"                  & "( " &
          Integer_1_Unsigned'Wide_Image(Red)   & ", " &
          Integer_1_Unsigned'Wide_Image(Green) & ", " &
          Integer_1_Unsigned'Wide_Image(Blue)  & " )" );
        glColor3ub.All(
          Integer_1_Unsigned_C(Red),
          Integer_1_Unsigned_C(Green),
          Integer_1_Unsigned_C(Blue));
      end Color;
    procedure Color(
      V : in Vector_3_Integer_1_Unsigned) --VECTOR
      is
      begin
        if glColor3ubv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3ubv" & "( " &
          "Vector"      & "( " &
            Integer_1_Unsigned'Wide_Image(V(1)) & ", " &
            Integer_1_Unsigned'Wide_Image(V(2)) & ", " &
            Integer_1_Unsigned'Wide_Image(V(3)) & " )" & " )" );
        glColor3ubv.All(
          V'address);
      end Color;
    procedure Color(
      Red   : in Integer_4_Unsigned;
      Green : in Integer_4_Unsigned;
      Blue  : in Integer_4_Unsigned)
      is
      begin
        if glColor3ui = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3ui"                         & "( " &
          Integer_4_Unsigned'Wide_Image(Red)   & ", " &
          Integer_4_Unsigned'Wide_Image(Green) & ", " &
          Integer_4_Unsigned'Wide_Image(Blue)  & " )" );
        glColor3ui.All(
          Integer_4_Unsigned_C(Red),
          Integer_4_Unsigned_C(Green),
          Integer_4_Unsigned_C(Blue));
      end Color;
    procedure Color(
      V : in Vector_3_Integer_4_Unsigned) --VECTOR
      is
      begin
        if glColor3uiv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3uiv" & "( " &
          "Vector"      & "( " &
            Integer_4_Signed'Wide_Image(V(1)) & ", " &
            Integer_4_Signed'Wide_Image(V(2)) & ", " &
            Integer_4_Signed'Wide_Image(V(3)) & " )" & " )" );
        glColor3uiv.All(
          V'address);
      end Color;
    procedure Color(
      Red   : in Integer_2_Unsigned;
      Green : in Integer_2_Unsigned;
      Blue  : in Integer_2_Unsigned)
      is
      begin
        if glColor3us = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3us"                         & "( " &
          Integer_2_Unsigned'Wide_Image(Red)   & ", " &
          Integer_2_Unsigned'Wide_Image(Green) & ", " &
          Integer_2_Unsigned'Wide_Image(Blue)  & " )" );
        glColor3us.All(
          Integer_2_Unsigned_C(Red),
          Integer_2_Unsigned_C(Green),
          Integer_2_Unsigned_C(Blue));
      end Color;
    procedure Color(
      V : in Vector_3_Integer_2_Unsigned_C) --VECTOR
      is
      begin
        if glColor4usv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor3usv" & "( " &
          "Vector"      & "( " &
            Integer_2_Unsigned'Wide_Image(V(1)) & ", " &
            Integer_2_Unsigned'Wide_Image(V(2)) & ", " &
            Integer_2_Unsigned'Wide_Image(V(3)) & " )" & " )" );
        glColor3usv.All(
          V'address);
      end Color;
    procedure Color(
      Red   : in Integer_1_Signed;
      Green : in Integer_1_Signed;
      Blue  : in Integer_1_Signed;
      Alpha : in Integer_1_Signed)
      is
      begin
        if glColor4b = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4b"                        & "( " &
          Integer_1_Signed'Wide_Image(Red)   & ", " &
          Integer_1_Signed'Wide_Image(Green) & ", " &
          Integer_1_Signed'Wide_Image(Blue)  & ", " &
          Integer_1_Signed'Wide_Image(Alpha) & " )" );
        glColor4b.All(
          Integer_1_Signed_C(Red),
          Integer_1_Signed_C(Green),
          Integer_1_Signed_C(Blue),
          Integer_1_Signed_C(Alpha));
      end Color;
    procedure Color(
      V : in Vector_4_Integer_1_Signed) --VECTOR
      is
      begin
        if glColor4bv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4bv"  & "( " &
          "Vector"      & "( " &
            Integer_1_Signed'Wide_Image(V(1)) & ", " &
            Integer_1_Signed'Wide_Image(V(2)) & ", " &
            Integer_1_Signed'Wide_Image(V(3)) & ", " &
            Integer_1_Signed'Wide_Image(V(4)) & " )" & " )" );
        glColor4bv.All(
          V'address);
      end Color;
    procedure Color(
      Red   : in Float_8_Real;
      Green : in Float_8_Real;
      Blue  : in Float_8_Real;
      Alpha : in Float_8_Real)
      is
      begin
        if glColor4d = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4d"                    & "( " &
          Float_8_Real'Wide_Image(Red)   & ", " &
          Float_8_Real'Wide_Image(Green) & ", " &
          Float_8_Real'Wide_Image(Blue)  & ", " &
          Float_8_Real'Wide_Image(Alpha  & " )" );
        glColor4d.All(
          Float_8_Real'Wide_Image(Red),
          Float_8_Real'Wide_Image(Green),
          Float_8_Real'Wide_Image(Blue),
          Float_8_Real'Wide_Image(Alpha));
      end Color;
    procedure Color(
      V : in Vector_4_Float_8_Real) --VECTOR
      is
      begin
        if glColor4dv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4dv"  & "( " &
          "Vector"      & "( " &
            Float_8_Real'Wide_Image(V(1)) & ", " &
            Float_8_Real'Wide_Image(V(2)) & ", " &
            Float_8_Real'Wide_Image(V(3)) & ", " &
            Float_8_Real'Wide_Image(V(4)) & " )" & " )" );
        glColor4dv.All(
          V'address);
      end Color;
    procedure Color(
      Red   : in Float_4_Real;
      Green : in Float_4_Real;
      Blue  : in Float_4_Real;
      Alpha : in Float_4_Real)
      is
      begin
        if glColor4f = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4f"                    & "( " &
          Float_4_Real'Wide_Image(Red)   & ", " &
          Float_4_Real'Wide_Image(Green) & ", " &
          Float_4_Real'Wide_Image(Blue)  & ", " &
          Float_4_Real'Wide_Image(Alpha) & " )" );
        glColor4f.All(
          Float_4_Real_C(Red),
          Float_4_Real_C(Green),
          Float_4_Real_C(Blue),
          Float_4_Real_C(Alpha));
      end Color;
    procedure Color(
      V : in Vector_4_Float_4_Real) --VECTOR
      is
      begin
        if glColor4fv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4fv"  & "( " &
          "Vector"      & "( " &
            Float_4_Real'Wide_Image(V(1)) & ", " &
            Float_4_Real'Wide_Image(V(2)) & ", " &
            Float_4_Real'Wide_Image(V(3)) & ", " &
            Float_4_Real'Wide_Image(V(4)) & " )" & " )" );
        glColor4fv.All(
          V'address);
      end Color;
    procedure Color(
      Red   : in Integer_4_Signed;
      Green : in Integer_4_Signed;
      Blue  : in Integer_4_Signed;
      Alpha : in Integer_4_Signed)
      is
      begin
        if glColor4i = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4i"                        & "( " &
          Integer_4_Signed'Wide_Image(Red)   & ", " &
          Integer_4_Signed'Wide_Image(Green  & ", " &
          Integer_4_Signed'Wide_Image(Blue)  & ", " &
          Integer_4_Singed'Wide_Image(Alpha) & " )" );
        glColor4i.All(
          Integer_4_Signed_C(Red),
          Integer_4_Signed_C(Green),
          Integer_4_Signed_C(Blue),
          Integer_4_Signed_C(Alpha));
      end Color;
    procedure Color(
      V : in Vector_4_Integer_4_Signed) --VECTOR
      is
      begin
        if glColor4iv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4iv"  & "( " &
          "Vector"      & "( " &
            Integer_4_Signed'Wide_Image(V(1)) & ", " &
            Integer_4_Signed'Wide_Image(V(2)) & ", " &
            Integer_4_Signed'Wide_Image(V(3)) & ", " &
            Integer_4_Signed'Wide_Image(V(4)) & " )" & " )" );
        glColor4iv(
          V'address);
      end Color;
    procedure Color(
      Red   : in Integer_2_Signed;
      Green : in Integer_2_Signed;
      Blue  : in Integer_2_Signed;
      Alpha : in Integer_2_Signed)
      is
      begin
        if glColor4s = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4s"                        & "( " &
          Integer_2_Signed'Wide_Image(Red)   & ", " &
          Integer_2_Signed'Wide_Image(Green) & ", " &
          Integer_2_Signed'Wide_Image(Blue)  & ", " &
          Integer_2_Signed'Wide_Image(Alpha  & " )" );
        glColor4s.All(
          Integer_2_Signed_C(Red),
          Integer_2_Signed_C(Green),
          Integer_2_Signed_C(Blue),
          Integer_2_Signed_C(Alpha));
      end Color;
    procedure Color(
      V : in Vector_4_Integer_2_Signed) --VECTOR
      is
      begin
        if glColor4sv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4sv"  & "( " &
          "Vector"      & "( " &
            Integer_2_Signed'Wide_Image(V(1)) & ", " &
            Integer_2_Signed'Wide_Image(V(2)) & ", " &
            Integer_2_Signed'Wide_Image(V(3)) & ", " &
            Integer_2_Signed'Wide_Image(V(4)) & " )" & " )" );
        glColor4sv(
          V'address);
      end Color;
    procedure Color(
      Red   : in Integer_1_Unsigned;
      Green : in Integer_1_Unsigned;
      Blue  : in Integer_1_Unsigned;
      Alpha : in Integer_1_Unsigned)
      is
      begin
        if glColor4ub = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4ub"                         & "( " &
          Integer_1_Unsigned'Wide_Image(Red)   & ", " &
          Integer_1_Unsigned'Wide_Image(Green) & ", " &
          Integer_1_Unsigned'Wide_Image(Blue), & ", " &
          Integer_1_Unsigned'Wide_Image(Alpha) & " )" );
        glColor4ub.All(
          Integer_1_Unsigned_C(Red),
          Integer_1_Unsigned_C(Green),
          Integer_1_Unsigned_C(Blue),
          Integer_1_Unsigned_C(Alpha));
      end Color;
    procedure Color(
      V : in Vector_4_Integer_1_Unsigned) --VECTOR
      is
      begin
        if glColor4ubv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4ubv" & "( " &
          "Vector"      & "( " &
            Integer_1_Unsigned'Wide_Image(V(1)) & ", " &
            Integer_1_Unsigned'Wide_Image(V(2)) & ", " &
            Integer_1_Unsigned'Wide_Image(V(3)) & ", " &
            Integer_1_Unsigned'Wide_Image(V(4)) & " )" & " )" );
        glColor4ubv(
          V'address);
      end Color;
    procedure Color(
      Red   : in Integer_4_Unsigned;
      Green : in Integer_4_Unsigned;
      Blue  : in Integer_4_Unsigned;
      Alpha : in Integer_4_Unsigned)
      is
      begin
        if glColor4ui = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4ui"                         & "( " &
          Integer_4_Unsigned'Wide_Image(Red)   & ", " &
          Integer_4_Unsigned'Wide_Image(Green) & ", " &
          Integer_4_Unsigned'Wide_Image(Blue)  & ", " &
          Integer_4_Unsigned'Wide_Image(Alpha) & " )" );
        glColor4ui.All(
          Integer_4_Unsigned_C(Red),
          Integer_4_Unsigned_C(Green),
          Integer_4_Unsigned_C(Blue),
          Integer_4_Unsigned_C(Alpha));
      end Color;
    procedure Color(
      V : in Vector_4_Integer_4_Unsigned) --VECTOR
      is
      begin
        if glColor4uiv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4uiv" & "( " &
          "Vector"      & "( " &
            Integer_4_Unsigned'Wide_Image(V(1)) & ", " &
            Integer_4_Unsigned'Wide_Image(V(2)) & ", " &
            Integer_4_Unsigned'Wide_Image(V(3)) & ", " &
            Integer_4_Unsigned'Wide_Image(V(4)) & " )" & " )" );
        glColor4uiv.All(
          V'address);
      end Color;
    procedure Color(
      Red   : in Integer_2_Unsigned;
      Green : in Integer_2_Unsigned;
      Blue  : in Integer_2_Unsigned;
      Alpha : in Integer_2_Unsigned)
      is
      begin
        if glColor4us = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4us"                         & "( " &
          Integer_2_Unsigned'Wide_Image(Red)   & ", " &
          Integer_2_Unsigned'Wide_Image(Green) & ", " &
          Integer_2_Unsigned'Wide_Image(Blue)  & ", " &
          Integer_2_Unsigned'Wide_Image(Alpha) & " )" );
        glColor4us.All(
          Integer_2_Unsigned(Red),
          Integer_2_Unsigned(Green),
          Integer_2_Unsigned(Blue),
          Integer_2_Unsigned(Alpha));
      end Color;
    procedure Color(
      V : in Vector_2_Integer_2_Unsigned) --VECTOR
      is
      begin
        if glColor4usv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColor4usv" & "( " &
          "Vector"      & "( " &
            Integer_2_Unsigned'Wide_Image(V(1)) & ", " &
            Integer_2_Unsigned'Wide_Image(V(2)) & ", " &
            Integer_2_Unsigned'Wide_Image(V(3)) & ", " &
            Integer_2_Unsigned'Wide_Image(V(4)) & " )" & " )" );
        glColor4usv(
          V);
      end Color;
  ----------------
  -- Color_Mask --
  ----------------
    procedure Color_Mask(
      Red   : in Integer_1_Unsigned;
      Green : in Integer_1_Unsigned;
      Blue  : in Integer_1_Unsigned;
      Alpha : in Integer_1_Unsigned)
      is
      begin
        if glColorMask = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColorMask"                        & "( " &
          Integer_1_Unsigned'Wide_Image(Red)   & ", " &
          Integer_1_Unsigned'Wide_Image(Green) & ", " &
          Integer_1_Unsigned'Wide_Image(Blue)  & ", " &
          Integer_1_Unsigned'Wide_Image(Alpha) & " )" );
        glColorMask.All(
          Integer_1_Unsigned_C(Red),
          Integer_1_Unsigned_C(Green),
          Integer_1_Unsigned_C(Blue),
          Integer_1_Unsigned_C(Alpha));
      end Color_Mask;
  --------------------
  -- Color_Material --
  --------------------
    procedure Color_Material(
      Race : in Integer_4_Unsigned;
      Mode : in Integer_4_Unsigned)
      is
      begin
        if glColorMaterial = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColorMaterial" & "( " &
          Get_String(Face)  & ", " &
          Get_String(Mode)  & " )" );
        glColorMaterial.All(
          Integer_4_Unsigned_C(Face),
          Integer_4_Unsigned_C(Mode));
      end Color_Material;
  -------------------
  -- Color_Pointer --
  -------------------
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_1_Unsigned)
      is
      begin
        if glColorPointer = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColorPointer"                    & "( " &
          Integer_4_Signed'Wide_Image(Size)   & ", " &
          Get_String(GL_UNSIGNED_BYTE)        & ", " &
          Integer_4_Signed'Wide_Image(Stride) & ", " &
          "'const GLvoid * pointer'"          & " )" &
        glColorPointer.All(
          Integer_4_Signed_C(Size),
          Integer_4_Unsigned_C(GL_UNSIGNED_BYTE),
          Integer_4_Signed_C(Stride),
          Color_Elements'address);
      end Color_Pointer;
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_2_Unsigned)
      is
      begin
        if glColorPointer = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColorPointer"                    & "( " &
          Integer_4_Signed'Wide_Image(Size)   & ", " &
          Get_String(GL_UNSIGNED_SHORT)       & ", " &
          Integer_4_Signed'Wide_Image(Stride) & ", " &
          "'const GLvoid * pointer'"          & " )" &
        glColorPointer.All(
          Integer_4_Signed_C(Size),
          Integer_4_Unsigned_C(GL_UNSIGNED_SHORT),
          Integer_4_Signed_C(Stride),
          Color_Elements'address);
      end Color_Pointer;
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_4_Unsigned)
      is
      begin
        if glColorPointer = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColorPointer"                    & "( " &
          Integer_4_Signed'Wide_Image(Size)   & ", " &
          Get_String(GL_UNSIGNED_INT)         & ", " &
          Integer_4_Signed'Wide_Image(Stride) & ", " &
          "'const GLvoid * pointer'"          & " )" &
        glColorPointer.All(
          Integer_4_Signed_C(Size),
          Integer_4_Unsigned_C(GL_UNSIGNED_INT),
          Integer_4_Signed_C(Stride),
          Color_Elements'address);
      end Color_Pointer;
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_1_Signed)
      is
      begin
        if glColorPointer = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColorPointer"                    & "( " &
          Integer_4_Signed'Wide_Image(Size)   & ", " &
          Get_String(GL_BYTE)                 & ", " &
          Integer_4_Signed'Wide_Image(Stride) & ", " &
          "'const GLvoid * pointer'"          & " )" &
        glColorPointer.All(
          Integer_4_Signed_C(Size),
          Integer_4_Unsigned_C(GL_BYTE),
          Integer_4_Signed_C(Stride),
          Color_Elements'address);
      end Color_Pointer;
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_2_Signed)
      is
      begin
        if glColorPointer = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColorPointer"                    & "( " &
          Integer_4_Signed'Wide_Image(Size)   & ", " &
          Get_String(GL_SHORT)                & ", " &
          Integer_4_Signed'Wide_Image(Stride) & ", " &
          "'const GLvoid * pointer'"          & " )" &
        glColorPointer.All(
          Integer_4_Signed_C(Size),
          Integer_4_Unsigned_C(GL_SHORT),
          Integer_4_Signed_C(Stride),
          Color_Elements'address);
      end Color_Pointer;
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_4_Signed)
      is
      begin
        if glColorPointer = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColorPointer"                    & "( " &
          Integer_4_Signed'Wide_Image(Size)   & ", " &
          Get_String(GL_INT)                  & ", " &
          Integer_4_Signed'Wide_Image(Stride) & ", " &
          "'const GLvoid * pointer'"          & " )" &
        glColorPointer.All(
          Integer_4_Signed_C(Size),
          Integer_4_Unsigned_C(GL_INT),
          Integer_4_Signed_C(Stride),
          Color_Elements'address);
      end Color_Pointer;
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Float_4_Real)
      is
      begin
        if glColorPointer = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColorPointer"                    & "( " &
          Integer_4_Signed'Wide_Image(Size)   & ", " &
          Get_String(GL_FLOAT)                & ", " &
          Integer_4_Signed'Wide_Image(Stride) & ", " &
          "'const GLvoid * pointer'"          & " )" &
        glColorPointer.All(
          Integer_4_Signed_C(Size),
          Integer_4_Unsigned_C(GL_FLOAT),
          Integer_4_Signed_C(Stride),
          Color_Elements'address);
      end Color_Pointer;
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Float_8_Real)
      is
      begin
        if glColorPointer = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glColorPointer"                    & "( " &
          Integer_4_Signed'Wide_Image(Size)   & ", " &
          Get_String(GL_DOUBLE)               & ", " &
          Integer_4_Signed'Wide_Image(Stride) & ", " &
          "'const GLvoid * pointer'"          & " )" &
        glColorPointer.All(
          Integer_4_Signed_C(Size),
          Integer_4_Unsigned_C(GL_DOUBLE),
          Integer_4_Signed_C(Stride),
          Color_Elements'address);
      end Color_Pointer;
  -----------------
  -- Copy_Pixels --
  -----------------
    procedure Copy_Pixels(
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed;
      Kind   : in Integer_4_Unsigned)
      is
      begin
        if glCopyPixels = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCopyPixels"                      & "( " &
          Integer_4_Signed'Wide_Image(X)      & ", " &
          Integer_4_Signed'Wide_Image(Y)      & ", " &
          Integer_4_Signed'Wide_Image(Width)  & ", " &
          Integer_4_Signed'Wide_Image(Height) & ", " &
          Get_String(Kind)                    & " )" );
        glCopyPixels.All(
          Integer_4_Signed_C(X),
          Integer_4_Signed_C(Y),
          Integer_4_Signed_C(Width),
          Integer_4_Signed_C(Height),
          Integer_4_Unsigned_C(Kind));
      end Copy_Pixels;
  ---------------------------
  -- Copy_Texture_Image_1D --
  ---------------------------
    procedure Copy_Texture_1D(
      Target : in Integer_4_Unsigned;
      Level  : in Integer_4_Signed;
      Format : in Integer_4_Unsigned;
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Border : in Integer_4_Signed)
      is
      begin
        if glCopyTexImage1D = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCopyTexImage1D"                  & "( " &
          Get_String(Target)                  & ", " &
          Integer_4_Signed'Wide_Image(Level)  & ", " &
          Get_String(Format)                  & ", " &
          Integer_4_Signed'Wide_Image(X)      & ", " &
          Integer_4_Signed'Wide_Image(Y)      & ", " &
          Integer_4_Signed'Wide_Image(Width)  & ", " &
          Integer_4_Signed'Wide_Image(Border) & " )" );
        glCopyTexImage1D.All(
          Integer_4_Unsigned_C(Target),
          Integer_4_Signed_C(Level),
          Integer_4_Unsigned_C(Format),
          Integer_4_Signed_C(X),
          Integer_4_Signed_C(Y),
          Integer_4_Signed_C(Width),
          Integer_4_Signed_C(Border));
      end Copy_Texture_1D;
  ---------------------------
  -- Copy_Texture_Image_2D --
  ---------------------------
    procedure Copy_Texture_Image_2D(
      Target : in Integer_4_Unsigned;
      Level  : in Integer_4_Signed;
      Format : in Integer_4_Unsigned;
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed;
      Border : in Integer_4_Signed)
      is
      begin
        if glCopyTexImage2D = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCopyTexImage2D"                  & "( " & 
          Get_String(Target)                  & ", " &
          Integer_4_Signed'Wide_Image(Level)  & ", " &
          Get_String(Format)                  & ", " &
          Integer_4_Signed'Wide_Image(X)      & ", " &
          Integer_4_Signed'Wide_Image(Y)      & ", " &
          Integer_4_Signed'Wide_Image(Width)  & ", " &
          Integer_4_Signed'Wide_Image(Height) & ", " &
          Integer_4_Signed'Wide_Image(Border) & " )" );
        glCopyTexImage2D.All(
          Integer_4_Unsigned_C(Target),
          Integer_4_Signed_C(Level),
          Integer_4_Unsigned_C(Format),
          Integer_4_Signed_C(X),
          Integer_4_Singed_C(Y),
          Integer_4_Signed_C(Width),
          Integer_4_Signed_C(Height),
          Integer_4_Signed_C(Border));
      end Copy_Texture_Image_2D;
  ------------------------------
  -- Copy_Texture_Subimage_1D --
  ------------------------------
    procedure Copy_Texture_Subimage_1D(
      Target   : in Integer_4_Unsigned;
      Level    : in Integer_4_Signed;
      X_Offset : in Integer_4_Signed;
      X        : in Integer_4_Signed;
      Y        : in Integer_4_Signed;
      Width    : in Integer_4_Signed)
      is
      begin
        if glCopyTexSubImage1D = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCopyTexSubImage1D"                 & "( " &
          Get_String(Target)                    & ", " &
          Integer_4_Signed'Wide_Image(Level)    & ", " &
          Integer_4_Signed'Wide_Image(X_Offset) & ", " &
          Integer_4_Signed'Wide_Image(X)        & ", " &
          Integer_4_Signed'Wide_Image(Y)        & ", " &
          Integer_4_Signed'Wide_Image(Width)    & " )" );
        glCopyTexSubImage1D.All(
          Integer_4_Unsigned(Target),
          Integer_4_Signed(Level),
          Integer_4_Signed(X_Offset),
          Integer_4_Signed(X),
          Integer_4_Signed(Y),
          Integer_4_Signed(Width));
      end Copy_Texture_Subimage_1D;
  ------------------------------
  -- Copy_Texture_Subimage_2D --
  ------------------------------
    procedure Copy_Texture_Subimage_2D(
      Target   : Integer_4_Unsigned;
      Level    : Integer_4_Signed;
      X_Offset : Integer_4_Signed;
      Y_Offset : Integer_4_Signed;
      X        : Integer_4_Signed;
      Y        : Integer_4_Signed;
      Width    : Integer_4_Signed;
      Height   : Integer_4_Signed) 
      is
      begin
        if glCopyTexSubImage2D = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCopyTexSubImage2D"                 & "( " &
          Get_String(Target)                    & ", " &
          Integer_4_Signed'Wide_Image(Level)    & ", " &
          Integer_4_Signed'Wide_Image(X_Offset) & ", " &
          Integer_4_Signed'Wide_Image(Y_Offset) & ", " &
          Integer_4_Signed'Wide_Image(X)        & ", " &
          Integer_4_Signed'Wide_Image(Y)        & ", " &
          Integer_4_Signed'Wide_Image(Width)    & ", " &
          Integer_4_Signed'Wide_Image(Height)   & " )" &;
        glCopyTexSubImage2D.All(
          Integer_4_Unsigned_C(Target),
          Integer_4_Signed_C(Level),
          Integer_4_Signed_C(X_Offset),
          Integer_4_Signed_C(Y_Offset),
          Integer_4_Signed_C(X),
          Integer_4_Signed_C(Y),
          Integer_4_Signed_C(Width),
          Integer_4_Signed_C(Height));
      end Copy_Texture_Subimage_2D;
  ---------------
  -- Cull_Face --
  ---------------
    procedure Cull_Face(
      Mode : in Integer_4_Unsigned)
      is
      begin
        if glCullFace = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glCullFace"     & "( " &
          Get_String(Mode) & " )" );
        glCullFace.All(
          Integer_4_Unsigned_C(Mode);
      end Cull_Face;
  ------------------
  -- Delete_Lists --
  ------------------
    procedure Delete_Lists(
      List   : in Integer_4_Unsigned;
      Bounds : in Integer_4_Signed)
      is
      begin
        if glDeleteLists = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDeleteLists" & "( " &
          Integer_4_Unsigned'Wide_Image(List) & ", " &
          Integer_4_Signed'Wide_Image(Bounds) & " )" );
        glDeleteLists.All(
          Integer_4_Unsigned_C(List),
          Integer_4_Signed_C(Bounds));
      end Delete_Lists;
  ---------------------
  -- Delete_Textures --
  ---------------------
    procedure Delete_Textures(
      N        : in     Integer_4_Signed_C,
      Textures : in out Array_Integer_1_Unsigned_C)
      is
      begin
        if glDeleteTextures = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDeleteTextures"               & "( " &
          Integer_4_Signed_C'Wide_Image(N) & ", " &
          "'const GLuint * textures'"      & " )" );
        glDeleteTextures.All(
          Integer_4_Signed_C(N),
          Textures'address);
      end Delete_Textures;
  --------------------
  -- Depth_Function --
  --------------------
    procedure Depth_Function(
      Operation : in Integer_4_Unsigned)
      is
      begin
        if glDepthFunc = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDepthFunc"         & "( " &
          Get_String(Operation) & " )" );
        glDepthFunc.All(
          Integer_4_Unsigned_C(Operation));
      end Depth_Function;
  ----------------
  -- Depth_Mask --
  ----------------
    procedure Depth_Mask(
      Flag : in Integer_1_Unsigned)
      is
      begin
        if glDepthMask = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDepthMask"            & "( " &
          Integer_1_Unsigned(Flag) & " )" );
        glDepthMask.All(
          Integer_1_Unsigned(Flag));
      end Depth_Mask;
  -----------------
  -- Depth_Range --
  -----------------
    procedure Depth_Range(
      Z_Near : in Float_8_Real;
      Z_Far  : in Float_8_Real)
      is
      begin
        if glDepthRange = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDepthRange"                  & "( " &
          Float_8_Real'Wide_Image(Z_Near) & ", " &
          Float_8_Real'Wide_Image(Z_Far)  & " )" );
        glDepthRange.All(
          Float_9_Real_C(Z_Near),
          Float_9_Real_C(Z_Far);
      end Depth_Range;
  -------------
  -- Disable --
  -------------
    procedure Disable(
      Cap : in Integer_4_Unsigned)
      is
      begin
        if glDisable = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDisable"     & "( " &
          Get_String(Cap) & " )" );
        glDisable.All(
          Integer_4_Unsigned_C(Cap));
      end Disable;
  --------------------------
  -- Disable_Client_State --
  --------------------------
    procedure Disable_Client_State(
      Cap : in Integer_4_Unsigned)
      is
      begin
        if glDisableClientState = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDisableClientState" & "( " &
          Get_String(Cap)        & " )" );
        glDisableClientState.All(
          Cap);
      end Disable_Cliend_State;
  -----------------
  -- Draw_Arrays --
  -----------------
    procedure Draw_Arrays(
      Mode  : Integer_4_Unsigned;
      First : Integer_4_Signed;
      Count : Integer_4_Signed)
      is
      begin
        if glDrawArrays = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDrawArrays"                     & "( " &
          Get_String(Mode)                   & ", " &
          Integer_4_Signed'Wide_Image(First) & ", " &
          Integer_4_Signed'Wide_Image(Count) & " )" );
        glDrawArrays.All(
          Integer_4_Unsigned_C(Mode),
          Integer_4_Signed_C(First),
          Integer_4_Signed_C(Count));
      end Draw_Arrays;
  -----------------
  -- Draw_Buffer --
  -----------------
    procedure Draw_Buffer(
      Mode : in Integer_4_Unsigned)
      is
      begin
        if glDrawBuffer = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDrawBuffer"   & "( " &
          Get_String(Mode) & " )" );
        glDrawBuffer.All(
          Integer_4_Unsigned_C(Mode));
      end Draw_Buffer;
  -------------------
  -- Draw_Elements --
  -------------------
    procedure Draw_Elements(
      Mode    : in Integer_4_Unsigned;
      Indices : in Array_Integer_1_Unsigned)
      is
      begin
        if glDrawElements = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDrawElements"                     & "( " &
          Get_String(Mode)                     & ", " &
          Positive'Wide_Image(Indicies'Length) & ", " &
          Get_String(GL_UNSIGNED_BYTE)         & ", " &
          "'const GLvoid * indices'"           & " )" );
        glDrawElements.All(
          Integer_4_Unsigned_C(Mode),
          Integer_4_Unsigned_C(Indices'Length),
          Integer_4_Unsigned_C(GL_UNSIGNED_BYTE),
          Indices'address);
      end Draw_Elements;
    procedure Draw_Elements(
      Mode    : in Integer_4_Unsigned;
      Indices : in Array_Integer_2_Unsigned)
      is
      begin
        if glDrawElements = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDrawElements"                     & "( " &
          Get_String(Mode)                     & ", " &
          Positive'Wide_Image(Indicies'Length) & ", " &
          Get_String(GL_UNSIGNED_SHORT)        & ", " &
          "'const GLvoid * indices'"           & " )" );
        glDrawElements.All(
          Integer_4_Unsigned_C(Mode),
          Integer_4_Unsigned_C(Indices'Length),
          Integer_4_Unsigned_C(GL_UNSIGNED_SHORT),
          Indices'address);
      end Draw_Elements;
    procedure Draw_Elements(
      Mode    : in Integer_4_Unsigned;
      Indices : in Array_Integer_4_Unsigned)
      is
      begin
        if glDrawElements = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDrawElements"                     & "( " &
          Get_String(Mode)                     & ", " &
          Positive'Wide_Image(Indicies'Length) & ", " &
          Get_String(GL_UNSIGNED_INT)          & ", " &
          "'const GLvoid * indices'"           & " )" );
        glDrawElements.All(
          Integer_4_Unsigned_C(Mode),
          Integer_4_Unsigned_C(Indices'Length),
          Integer_4_Unsigned_C(GL_UNSIGNED_INT),
          Indices'address);
      end Draw_Elements;
  -----------------
  -- Draw_Pixels --
  -----------------
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_1_Unsigned) --MATRIX
      is
      begin
        if glDrawPixels = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDrawPixels"                                   & "( " &
          Positive'Wide_Image(Pixels'Length)               & ", " &
          Positive'Wide_Image(Pixels(Pixels'first)'Length) & ", " &
          Get_String(Format)                               & ", " &
          Get_String(GL_UNSIGNED_BYTE)                     & ", " &
          "'const GLvoid * pixels'"                        & " )" );
        glDrawPixel.All(
          Integer_4_Signed_C(Pixels'Length),
          Integer_4_Signed_C(Pixels(Pixels'first)'Length),
          Integer_4_Unsigned_C(Format),
          Integer_4_Unsigned_C(GL_UNSIGNED_BYTE),
          Pixels'address);
      end Draw_Pixels;
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_1_Signed) --MATRIX
      is
      begin
        if glDrawPixels = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDrawPixels"                                   & "( " &
          Positive'Wide_Image(Pixels'Length)               & ", " &
          Positive'Wide_Image(Pixels(Pixels'first)'Length) & ", " &
          Get_String(Format)                               & ", " &
          Get_String(GL__BYTE)                             & ", " &
          "'const GLvoid * pixels'"                        & " )" );
        glDrawPixel.All(
          Integer_4_Signed_C(Pixels'Length),
          Integer_4_Signed_C(Pixels(Pixels'first)'Length),
          Integer_4_Unsigned_C(Format),
          Integer_4_Unsigned_C(GL_BYTE),
          Pixels'address);
      end Draw_Pixels;
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_2_Unsigned) --MATRIX
      is
      begin
        if glDrawPixels = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDrawPixels"                                   & "( " &
          Positive'Wide_Image(Pixels'Length)               & ", " &
          Positive'Wide_Image(Pixels(Pixels'first)'Length) & ", " &
          Get_String(Format)                               & ", " &
          Get_String(GL_UNSIGNED_SHORT)                    & ", " &
          "'const GLvoid * pixels'"                        & " )" );
        glDrawPixel.All(
          Integer_4_Signed_C(Pixels'Length),
          Integer_4_Signed_C(Pixels(Pixels'first)'Length),
          Integer_4_Unsigned_C(Format),
          Integer_4_Unsigned_C(GL_UNSIGNED_SHORT),
          Pixels'address);
      end Draw_Pixels;
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_2_Signed) --MATRIX
      is
      begin
        if glDrawPixels = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDrawPixels"                                   & "( " &
          Positive'Wide_Image(Pixels'Length)               & ", " &
          Positive'Wide_Image(Pixels(Pixels'first)'Length) & ", " &
          Get_String(Format)                               & ", " &
          Get_String(GL_SHORT)                             & ", " &
          "'const GLvoid * pixels'"                        & " )" );
        glDrawPixel.All(
          Integer_4_Signed_C(Pixels'Length),
          Integer_4_Signed_C(Pixels(Pixels'first)'Length),
          Integer_4_Unsigned_C(Format),
          Integer_4_Unsigned_C(GL_SHORT),
          Pixels'address);
      end Draw_Pixels;
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_4_Unsigned) --MATRIX
      is
      begin
        if glDrawPixels = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDrawPixels"                                   & "( " &
          Positive'Wide_Image(Pixels'Length)               & ", " &
          Positive'Wide_Image(Pixels(Pixels'first)'Length) & ", " &
          Get_String(Format)                               & ", " &
          Get_String(GL_UNSIGNED_INT)                      & ", " &
          "'const GLvoid * pixels'"                        & " )" );
        glDrawPixel.All(
          Integer_4_Signed_C(Pixels'Length),
          Integer_4_Signed_C(Pixels(Pixels'first)'Length),
          Integer_4_Unsigned_C(Format),
          Integer_4_Unsigned_C(GL_UNSIGNED_INT),
          Pixels'address);
      end Draw_Pixels;
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_4_Signed) --MATRIX
      is
      begin
        if glDrawPixels = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glDrawPixels"                                   & "( " &
          Positive'Wide_Image(Pixels'Length)               & ", " &
          Positive'Wide_Image(Pixels(Pixels'first)'Length) & ", " &
          Get_String(Format)                               & ", " &
          Get_String(GL_INT)                               & ", " &
          "'const GLvoid * pixels'"                        & " )" );
        glDrawPixel.All(
          Integer_4_Signed_C(Pixels'Length),
          Integer_4_Signed_C(Pixels(Pixels'first)'Length),
          Integer_4_Unsigned_C(Format),
          Integer_4_Unsigned_C(GL_INT),
          Pixels'address);
      end Draw_Pixels;
  ---------------
  -- Edge_Flag --
  ---------------
    procedure Edge_Flag(
      Flag : in Integer_1_Unsigned)
      is
      begin
        if glEdgeFlag = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEdgeFlag"                        & "( " &
          Integer_1_Unsigned'Wide_Image(Flag) & " )" );
        glEdgeFlag.All(
          Integer_1_Unsigned_C(Flag));
      end Edge_Flag;
    -- procedure Edge_Flag(
    --   Offset  : Integer_4_Signed_C,
    --   Pointer : GLvoid *) --UNKNOWN
    --   is
    --   begin
    --     if glEdgeFlagPointer = null then
    --       raise System_Call_Failure;
    --     end if;
    --     Put_Line(
    --       "glEdgeFlagPointer"                   & "( " &
    --       Integer_4_Signed_C'Wide_Image(Offset) & ", " &
    --       "'const GLvoid * pointer'"            & " )" &);
    --     glEdgeFlagPointer.All(
    --       Integer_4_Signed_C(Offset),
    --       Pointer'address);
    --   end Edge_Flag;
    procedure Edge_Flag(
      Flag : Integer_1_Unsigned)
      is
      begin
        if glEdgeFlag = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEdgeFlag"                        & "( " &
          Integer_1_Unsigned'Wide_Image(Flag) & " )" );
        glEdgeFlag.All(
          Integer_1_Unsigned_C(Flag);
      end Edge_Flag;
  ------------
  -- Enable --
  ------------
    procedure Enable(
      Cap : in Integer_4_Unsigned)
      is
      begin
        if glEnable = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEnable"      & "( " &
          Get_String(Cap) & " )" );
        glEnable.All(C
          Integer_4_Unsigned_C(Cap));
      end Enable;
  -------------------------
  -- Enable_Client_State --
  -------------------------
    procedure Enable_Client_State(
      State : Integer_4_Unsigned)
      is
      begin
        if glEnableClientState = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEnableClientState" & "( " &
          Get_String(State)     & " )" );
        glEnableClientState.All(
          Integer_4_Unsigned_C(State));
      end Enable_Cliend_State;
  ----------------
  -- End_OpenGL --
  ----------------
    procedure End_Drawing
      is
      begin
        if glEnd = null then
          raise System_Call_Failure;
        end if;
        Put_Line("glEnd");
        glEnd.All;
      end End_Drawing;
  --------------
  -- End_List --
  --------------
    procedure End_List
      is
      begin
        if glEndList = null then
          raise System_Call_Failure;
        end if;
        Put_Line("glEndList");
        glEndList.All; 
      end End_List;
  -------------------------
  -- Evaluate_Coordinate --
  -------------------------
    procedure Evaluate_Coordinate(
      U : in Float_8_Real)
      is
      begin
        if glEvalCoord1d = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEvalCoord1d"            & "( " &
          Float_8_Real'Wide_Image(U) & " )" );
        glEvalCoord1d.All(
          Float_8_Real_C(U));
      end Evaluate_Coordinate;
    procedure Evaluate_Coordinate(
      U : in Array_Vector_3_Float_8_Real) --VECTOR
      is
      begin
        if glEvalCoord1dv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEvalCoord1dv"       & "( " &
          "'const GLdouble * u'" & " )" );
        glEvalCoord1dv.All(
          U'address);
      end Evaluate_Coordinate;
    procedure Evaluate_Coordinate(
      U : in Float_4_Real)
      is
      begin
        if glEvalCoord1f = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEvalCoord1f"
          Float_4_Real'Wide_Image(U));
        glEvalCoord1f.All(
          Float_4_Real(U));
      end Evaluate_Coordinate;
    procedure Evaluate_Coordinate(
      U : in Array_Vector_3_Float_4_Real) --VECTOR
      is
      begin
        if glEvalCoord1fv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEvalCoord1fv"      & "( " &
          "'const GLfloat * u'" & " )" );
        glEvalCoord1fv.All(
          U'address);
      end Evaluate_Coordinate;
    procedure Evaluate_Coordinate(
      U : in Float_8_Real;
      V : in Float_8_Real)
      is
      begin
        if glEvalCoord2d = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEvalCoord2d"            & "( " &
          Float_8_Real'Wide_Image(U) & ", " &
          Float_8_Real'Wide_Image(V) & " )" );
        glEvalCoord2d.All(
          Float_8_Real_C(U),
          Float_8_Real_C(V));
      end Evaluate_Coordinate;
    procedure Evaluate_Coordinate(
      U : in Array_Vector_3_Float_8_Real) --VECTOR
      is
      begin
        if glEvalCoord2dv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEvalCoord2dv"       & "( " &
          "'const GLdouble * u'" & " )" );
        glEvalCoord2dv.All(
          U'address);
      end Evaluate_Coordinate;
    procedure Evaluate_Coordinate(
      U : in Float_4_Real;
      V : in Float_4_Real)
      is
      begin
        if glEvalCoord2f = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEvalCoord2f"            & "( " &
          Float_4_Real'Wide_Image(U) & ", " &
          Float_4_Real'Wide_Image(V) & " )" );
        glEvalCoord2f.All(
          Float_4_Real_C(U),
          Float_4_Real_C(V));
      end Evaluate_Coordinate;
    procedure Evaluate_Coordinate(
      U : in Access_Constant_Float_4_Real) --VECTOR
      is
      begin
        if glEvalCoord2fv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEvalCoord2fv"
          "'const GLfloat * u'");
        glEvalCoord2fv.All(
          U'address);
      end Evaluate_Coordinate;
  -------------------
  -- Evaluate_Mesh --
  -------------------
    procedure Evaluate_Mesh(
      Mode : in Integer_4_Unsigned;
      I_1  : in Integer_4_Signed;
      J_2  : in Integer_4_Signed)
      is
      begin
        if glEvalMesh1 = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEvalMesh1"         & "( " &
          Get_String(Mode)      & ", " &
          Integer_4_Signed(I_1) & ", " &
          Integer_4_Signed(I_2) & " )" );
        glEvalMesh1.All(
          Integer_4_Unsigned_C(Mode),
          Integer_4_Signed(I_1),
          Integer_4_Signed(I_2));
      end Evaluate_Mesh;
    procedure Evaluate_Mesh(
      Mode : in Integer_4_Unsigned;
      I_1  : in Integer_4_Signed;
      I_2  : in Integer_4_Signed;
      J_1  : in Integer_4_Signed;
      J_2  : in Integer_4_Signed)
      is
      begin
        if glEvalMesh2 = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEvalMesh2"         & "( " &
          Get_String(Mode)      & ", " &
          Integer_4_Signed(I_1) & ", " &
          Integer_4_Signed(I_2) & ", " &
          Integer_4_Signed(J_1) & ", " &
          Integer_4_Signed(J_2) & " )" );
        EvalMesh2.All(
          Integer_4_Unsigned_C(Mode),
          Integer_4_Signed_C(I_1),
          Integer_4_Signed_C(I_2),
          Integer_4_Signed_C(J_1),
          Integer_4_Signed_C(J_2));
      end Evaluate_Mesh;
  --------------------
  -- Evaluate_Point --
  --------------------
    procedure Evaluate_Point(
      I : in Integer_4_Signed)
      is
      begin
        Put_Line(
          "glEvalPoint1"                 & "( " &
          Integer_4_Signed'Wide_Image(I) & " )" );
        Eval_Point_1.All(
          Integer_4_Signed_C(I));
      end Evaluate_Point;
    procedure Evaluate_Point(
      I : in Integer_4_Signed;
      J : in Integer_4_Signed)
      is
      begin
        if glEvalPoint2 = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glEvalPoint2"                 & "( " &
          Integer_4_Signed'Wide_Image(I) & ", " &
          Integer_4_Signed'Wide_Image(J) & " )" );
        glEvalPoint2(
          Integer_4_Signed_C(I),
          Integer_4_Signed_C(J));
      end Evaluate_Point;
  ---------------------
  -- Feedback_Buffer --
  ---------------------
    Function Get_Feedback_Buffer(
      Size   : in     Integer_4_Signed;
      Kind   : in     Integer_4_Unsigned);
      return Array_Float_4_Real
      is
      Output := new Array_Float_4_Real_C(1..Size) := (others => 0.0);
      begin
        if glFeedbackBuffer = null then
          raise System_Call_Failure;
        end if;
        Put_Line("glFeedbackBuffer"         & "( " &
          Integer_4_Signed'Wide_Image(Size) & ", " &
          Get_String(Kind)                  & ", " &
          "'GLfloat * buffer'"              & " )" );
        glFeedbackBuffer.All(
          Integer_4_Signed_C(Size),
          Integer_4_Unsigned_C(Kind),
          Output'address);
        return To_Array_Float_4_Real(Output);
      end Get_Feedback_Buffer;
  ------------
  -- Finish --
  ------------
    procedure Finish
      is
      begin
        if glFinish = null then
          raise System_Call_Failure;
        end if;
        Put_Line("glFinish");
        glFinish.All; 
      end Finish;
  -----------
  -- Flush --
  -----------
    procedure Flush
      is
      begin
        if glFlush = null then
          raise System_Call_Failure;
        end if;
        Put_Line("glFlush");
        glFlush.All; 
      end Flush;
  ----------
  -- Fog --
  ---------
    procedure Fog(
      Name      : in Integer_4_Unsigned;
      Parameter : in Float_4_Real)
      is
      begin
        if glFogf = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glFogf"                            & "( " &
          Get_String(Name)                    & ", " &
          Float_4_Real'Wide_Image(Parameter) & " )" );
        glFogf.All(
          Integer_4_Unsigned_C(Name),
          Float_4_Real_C(Parameter));
      end Fog;
    procedure Fog(
      Name      : in Integer_4_Unsigned;
      Parameter : in Access_Constant_Float_4_Real) --VECTOR
      is
      begin
        if glFogfv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glFogfv"                  & "( " &
          Get_String(Name)           & ", " &
          "'const GLfloat * params'" & " )" );
        glFogfv.All(
          Integer_4_Unsigned_C(Name),
          Parameter'address);
      end Fog;
    procedure Fog(
      Name      : in Integer_4_Unsigned;
      Parameter : in Integer_4_Signed)
      is
      begin
        if glFogi = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glFogi"                               & "( " &
          Get_String(Name)                       & ", " &
          Integer_4_Signed'Wide_Image(Parameter) & " )" );
        glFogi.All(
          Integer_4_Unsigned_C(Name),
          Integer_4_Signed_C(Parameter));
      end Fog;
    procedure Fog(
      Name      : in Integer_4_Unsigned;
      Parameter : in Access_Constant_Integer_4_Signed) --VECTOR
      is
      begin
        if glFogiv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glFogiv"                & "( " &
          Get_String(Name)         & ", " &
          "'const GLint * params'" & " )" );
        glFogiv.All(
          Integer_4_Unsigned_C(Name),
          Parameter'address);
      end Fog;
  ----------------
  -- Front_Face --
  ----------------
    procedure Front_Face(
      Mode : in Integer_4_Unsigned)
      is
      begin
        if glFrontFace = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glFrontFace"    & "( " &
          Get_String(Mode) & " )" );
        glFrontFace.All(
          Integer_4_Unsigned_C(Mode));
      end Front_Face;
  -------------
  -- Frustum --
  -------------
    procedure Frustum(
      Left   : in Float_8_Real;
      Right  : in Float_8_Real;
      Bottom : in Float_8_Real;
      Top    : in Float_8_Real;
      Z_Near : in Float_8_Real;
      Z_Far  : in Float_8_Real)
      is
      begin
        if glFrustum = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glFrustum"                     & "( " &
          Float_8_Real'Wide_Image(Left)   & ", " &
          Float_8_Real'Wide_Image(Right)  & ", " &
          Float_8_Real'Wide_Image(Bottom) & ", " &
          Float_8_Real'Wide_Image(Top)    & ", " &
          Float_8_Real'Wide_Image(Z_Near) & ", " &
          Float_8_Real'Wide_Image(Z_Far)  & " )" );
        glFrustum.All(
          Float_8_Real_C(Left),
          Float_8_Real_C(Right),
          Float_8_Real_C(Bottom),
          Float_8_Real_C(Top),
          Float_8_Real_C(Z_Near),
          Float_8_Real_C(Z_Far));
      end Frustum;
  --------------------
  -- Generate_Lists --
  --------------------
    function Generate_Lists(
      Bounds : in Integer_4_Signed)
      return Integer_4_Unsigned
      is
      begin
        if glGenLists = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glGenLists"             & "( " &
          Integer_4_Signed(Bounds) & " )" );
        return Integer_4_Unsigned(
          glGenLists.All(
            Integer_4_Unsigned_C(Bounds)));
      end Generate_Lists;
  -----------------------
  -- Generate_Textures --
  -----------------------
    function Generate_Textures(
      Size : in Integer_4_Positive)
      return Array_Integer_4_Unsigned
      is
      Output : Array_Integer_4_Unsigned_C(1..Size) := (others => 0);
      begin
        if glGenTextures = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glGenTextures"                     & "( " &
          Integer_4_Positive'Wide_Image(Size) & " )" );
        if(Size > Integer_4_Signed_C'last){
          Put_Line("Warning in function glGenTextures: Parameter 'size' is greater than Integer_4_Signed'last.");
          Size := Integer_4_Positive(Integer_4_Signed_C'last);
        }
        glGenTextures.All(
          Integer_4_Signed_C(Size),
          Output'address);
        return To_Array_Integer_4_Unsigned(Output);
      end Generate_Textures;
  ---------------
  -- Get_Bytes --
  ---------------
    function Get_Bytes(
      Name : in Integer_4_Unsigned)
      return Array_Integer_1_Unsigned
      is
      Output : Access_Array_Integer_1_Unsigned_C := new Array_Integer_1_Unsigned_C;
      begin
        if glGetBooleanv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glGetBooleanv"        & "( " &
          Get_String(Name)       & ", " &
          "'GLboolean * params'" & " )" );
        glGetBooleanv.All(
          Integer_4_Unsigned_C(Name),
          Output'address);
        return To_Array_Integer_1_Unsigned(Address_To_Access_Unbounded_Array(Output'address));
      end Get_Bytes;
  --------------------
  -- Get_Clip_Plane --
  --------------------
    function Get_Clip_Plane(
      Plane    : in Integer_4_Unsigned)
      return Vector_4_Float_8_Real       --VECTOR
      is
      Equation : Vector_4_Float_8_Real_C;
      begin
        if glGetClipPlane = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glGetClipPlane"        & "( " & 
          Get_String(Plane)       & ", " &
          "'GLdouble * equation'" & " )" );
        glGetClipPlane.All(
          Integer_4_Unsigned_C(Plane),
          Equation'address);
        return To_Vector_4_Float_8_Real_C(Equation);
      end Get_Clip_Plane;
  ----------------
  -- Get_Double --
  ----------------
    function Get_Double(
      Name : in Integer_4_Unsigned)
      return Array_Float_8_Real
      is
      Output : Float_8_Real_C := 0.0;
      begin
        if glGetDoublev = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glGetDoublev"        & "( " &
          Get_String(Name)      & ", " &
          "'GLdouble * params'" & " )" );
        glGetDoublev.All(
          Integer_4_Unsigned_C(Name),
          Output'address);
        return
          To_Array_Float_8_Real(
            Address_To_Access_Unbounded_Array(
              Output'access));
      end Get_Double;
  ---------------
  -- Get_Error --
  ---------------
    function Get_Error
      return Integer_4_Unsigned
      is
      begin
        if glGetError = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glGetError");
        return Integer_4_Unsigned(glGetError.All);
      end Get_Error;
  ---------------
  -- Get_Float --
  ---------------
    function Get_Float(
      Name : in Integer_4_Unsigned)
      return Array_Float_4_Real
      is
      Output : Float_4_Real_C := 0.0;
      begin
        if glGetFloatv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glGetFloatv"        & "( " &
          Get_String(Name)     & ", " &
          "'GLfloat * params'" & " )" );
        glGetFloatv.All(
          Integer_4_Unsigned(Name),
          Output'address);
        return
          To_Array_Float_4_Real( 
            Address_To_Access_Unbounded_Array(
              Output'access));
      end Get_Float;
  -----------------
  -- Get_Integer --
  -----------------
    function Get_Integer(
      Name : in Integer_4_Unsigned)
      return Array_Integer_4_Unsigned
      is
      Output : Integer_4_Signed_C
      begin
        if glGetIntegerv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glGetIntegerv"    & "( " &
          Get_String(Name)   & ", " &
          "'GLint * params'" & " )" );
        glGetIntegerv.All(Name, params);
      end Get_Integer;
  ---------------
  -- Get_Light --
  ---------------
    procedure Get_Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned)
      return Access_Array_Float_4_Real
      is
      Output : Access_Array_Float
      begin
        if glGetLightfv = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
          "glGetLightfv"       & "( " &
          Get_String(Light)    & ", " &
          Get_String(Name)     & ", " &
          "'GLfloat * params'" & " )" );
        GetLightfv(
          Integer_4_Unsigned_C(Light),
          Integer_4_Unsigned_C(Name),
          params);
      end Get_Light;
    procedure Get_Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Integer_4_Signed)
      is
      begin
        -- unknown type: "GLint *" name: "params"
        Put_Line("glGetLightiv %s %s 'GLint * params'", EnumString(light), EnumString(pname));
        GetLightiv(light, pname, params);
      end Get_Light;
  -------------
  -- Get_Map --
  -------------
    procedure Get_Map(
      Target : in Integer_4_Unsigned;
      Query  : in Integer_4_Unsigned;
      V      : in Access_Float_8_Real)
      is
      begin
        -- unknown type: "GLdouble *" name: "v"
        Put_Line("glGetMapdv %s %s 'GLdouble * v'", EnumString(target), EnumString(query));
        GetMapdv(target, query, v);
      end Get_Map;
    procedure Get_Map(
      Target : in Integer_4_Unsigned;
      Query  : in Integer_4_Unsigned;
      V      : in Access_Float_4_Real)
      is
      begin
        -- unknown type: "GLfloat *" name: "v"
        Put_Line("glGetMapfv %s %s 'GLfloat * v'", EnumString(target), EnumString(query));
        GetMapfv(target, query, v);
      end Get_Map;
    procedure Get_Map(
      Target : in Integer_4_Unsigned;
      Query  : in Integer_4_Unsigned;
      V      : in Access_Integer_4_Signed)
      is
      begin
        -- unknown type: "GLint *" name: "v"
        Put_Line("glGetMapiv %s %s 'GLint * v'", EnumString(target), EnumString(query));
        GetMapiv(target, query, v);
      end Get_Map;
  ------------------
  -- Get_Material --
  ------------------
    procedure Get_Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Float_4_Real)
      is
      begin
        -- unknown type: "GLfloat *" name: "params"
        Put_Line("glGetMaterialfv %s %s 'GLfloat * params'", EnumString(face), EnumString(pname));
        GetMaterialfv(face, pname, params);
      end Get_Material;
    procedure Get_Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Integer_4_Signed)
      is
      begin
        -- unknown type: "GLint *" name: "params"
        Put_Line("glGetMaterialiv %s %s 'GLint * params'", EnumString(face), EnumString(pname));
        GetMaterialiv(face, pname, params);
      end Get_Material;
  -------------------
  -- Get_Pixel_Map --
  -------------------
    procedure Get_Pixel_Map(
      Map    : in Integer_4_Unsigned;
      Values : in Access_Float_4_Real)
      is
      begin
        -- unknown type: "GLfloat *" name: "values"
        Put_Line("glGetPixelMapfv %s 'GLfloat * values'", EnumString(map));
        GetPixelMapfv(map, values);
      end Get_Pixel_Map;
    procedure Get_Pixel_Map(
      Map    : in Integer_4_Unsigned;
      Values : in Access_Integer_4_Unsigned)
      is
      begin
        -- unknown type: "GLuint *" name: "values"
        Put_Line("glGetPixelMapuiv %s 'GLuint * values'", EnumString(map));
        GetPixelMapuiv(map, values);
      end Get_Pixel_Map;
    procedure Get_Pixel_Map(
      Map    : in Integer_4_Unsigned;
      Values : in Win32.PWSTR)
      is
      begin
        -- unknown type: "GLushort *" name: "values"
        Put_Line("glGetPixelMapusv %s 'GLushort * values'", EnumString(map));
        GetPixelMapusv(map, values);
      end Get_Pixel_Map;
  -----------------
  -- Get_Pointer --
  -----------------
    procedure Get_Pointer(
      GLenum pname,
      GLvoid* *params)
      is
      begin
        -- unknown type: "GLvoid* *" name: "params"
        Put_Line("glGetPointerv %s 'GLvoid* * params'", EnumString(pname));
        GetPointerv(pname, params);
      end Get_Pointer;
  -------------------------
  -- Get_Polygon_Stipple --
  -------------------------
    procedure Get_Polygon_Stipple(                              
      Mask : in Access_Integer_1_Unsigned);  
      is
      begin
        -- unknown type: "GLubyte *" name: "mask"
        Put_Line("glGetPolygonStipple 'GLubyte * mask'");
        GetPolygonStipple(mask);
      end Get_Polygon_Stipple;
  ----------------
  -- Get_String --
  ----------------
    function Get_String(                                       
      Name : in Integer_4_Unsigned)                                   
      return String_2_C
      is
      begin
        Put_Line(
          "glGetString"                  & "( " &
          Integer_4_Unsigned'Wide_Image(Name) & " )" );
        return To_Wide_String(To_Ada(Get_String(
          Name)));
      end Get_String;
  -----------------------------
  -- Get_Texture_Environment --
  -----------------------------
    procedure Get_Texture_Environment(                                    
      Target     : in Integer_4_Unsigned;                                 
      Name       : in Integer_4_Unsigned;                                 
      Parameters : in Access_Float_4_Real)
      is
      begin
        -- unknown type: "GLfloat *" name: "params"
        Put_Line("glGetTexEnvfv %s %s 'GLfloat * params'", EnumString(target), EnumString(pname));
        GetTexEnvfv(target, pname, params);
      end Get_Texture_Environment;
    procedure Get_Texture_Environment(                                    
      Target     : in Integer_4_Unsigned;                                 
      Name       : in Integer_4_Unsigned;                                 
      Parameters : in Access_Integer_4_Signed)
      is
      begin
        -- unknown type: "GLint *" name: "params"
        Put_Line("glGetTexEnviv %s %s 'GLint * params'", EnumString(target), EnumString(pname));
        GetTexEnviv(target, pname, params);
      end Get_Texture_Environment;
  ----------------------------
  -- Get_Texture_Generation --
  ----------------------------
    procedure Get_Texture_Generation(                                    
      Coordinate : in Integer_4_Unsigned;                                 
      Name       : in Integer_4_Unsigned;                                 
      Parameters : in Access_Float_8_Real)
      is
      begin
        -- unknown type: "GLdouble *" name: "params"
        Put_Line("glGetTexGendv %s %s 'GLdouble * params'", EnumString(coord), EnumString(pname));
        GetTexGendv(coord, pname, params);
      end Get_Texture_Generation;
    procedure Get_Texture_Generation(                                    
      Coordinate : in Integer_4_Unsigned;                                 
      Name       : in Integer_4_Unsigned;                                 
      Parameters : in Access_Float_4_Real)
      is
      begin
        -- unknown type: "GLfloat *" name: "params"
        Put_Line("glGetTexGenfv %s %s 'GLfloat * params'", EnumString(coord), EnumString(pname));
        GetTexGenfv(coord, pname, params);
      end Get_Texture_Generation;
    procedure Get_Texture_Generation(                                    
      Coordinate : in Integer_4_Unsigned;                                 
      Name       : in Integer_4_Unsigned;                                 
      Parameters : in Access_Integer_4_Signed)
      is
      begin
        -- unknown type: "GLint *" name: "params"
        Put_Line("glGetTexGeniv %s %s 'GLint * params'", EnumString(coord), EnumString(pname));
        GetTexGeniv(coord, pname, params);
      end Get_Texture_Generation;
  -----------------------
  -- Get_Texture_Image --
  -----------------------
    procedure Get_Texture_Image(
      Target : in Integer_4_Unsigned;
      Level  : in Integer_4_Signed;
      Format : in Integer_4_Unsigned;
      Kind   : in Integer_4_Unsigned;
      Pixels : in PGLvoid)
      is
      begin
        -- unknown type: "GLvoid *" name: "pixels"
        Put_Line("glGetTexImage %s %d %s %s 'GLvoid * pixels'", EnumString(target), level, EnumString(format), EnumString(type));
        GetTexImage(target, level, format, type, pixels);
      end Get_Texture_Image;
  ---------------------------------
  -- Get_Texture_Level_Parameter --
  ---------------------------------
    procedure Get_Texture_Level_Parameter(
      Target     : in Integer_4_Unsigned;
      Level      : in Integer_4_Signed;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Float_4_Real)
      is
      begin
        -- unknown type: "GLfloat *" name: "params"
        Put_Line("glGetTexLevelParameterfv %s %d %s 'GLfloat * params'", EnumString(target), level, EnumString(pname));
        GetTexLevelParameterfv(target, level, pname, params);
      end Get_Texture_Level_Parameter;
    procedure Get_Texture_Level_Parameter(                         
      Target : in Integer_4_Unsigned;                                 
      Level  : in Integer_4_Signed;                                  
      Name   : in Integer_4_Unsigned;                                 
      Parameters : in Access_Integer_4_Signed)
      is
      begin
        -- unknown type: "GLint *" name: "params"
        Put_Line("glGetTexLevelParameteriv %s %d %s 'GLint * params'", EnumString(target), level, EnumString(pname));
        GetTexLevelParameteriv(target, level, pname, params);
      end Get_Texture_Level_Parameter;
  ---------------------------
  -- Get_Texture_Parameter --
  ---------------------------
    procedure Get_Texture_Parameter(                              
      Target     : in Integer_4_Unsigned;                                 
      Name       : in Integer_4_Unsigned;                                 
      Parameters : in Access_Float_4_Real)
      is
      begin
        -- unknown type: "GLfloat *" name: "params"
        Put_Line("glGetTexParameterfv %s %s 'GLfloat * params'", EnumString(target), EnumString(pname));
        GetTexParameterfv(target, pname, params);
      end Get_Texture_Parameter;
    procedure Get_Texture_Parameter(                              
      Target     : in Integer_4_Unsigned;                                 
      Name       : in Integer_4_Unsigned;                                 
      Parameters : in Access_Integer_4_Signed)
      is
      begin
        -- unknown type: "GLint *" name: "params"
        Put_Line("glGetTexParameteriv %s %s 'GLint * params'", EnumString(target), EnumString(pname));
        GetTexParameteriv(target, pname, params);
      end Get_Texture_Parameter;
  ----------
  -- Hint --
  ----------
    procedure Hint(
      Target : in Integer_4_Unsigned;
      Mode   : in Integer_4_Unsigned)
      is
      begin
        if glHint = null then
          raise System_Call_Failure;
        end if;
        Put_Line(
         "glHint"            & "( " &
          Get_String(Target) & ", " &
          Get_String(Mode)   & " )" );
        glHint.All(
          Integer_4_Unsigned_C(Target),
          Integer_4_Unsigned_C(Mode));
      end Hint;
  ----------------
  -- Index_Mask --
  ----------------
    procedure Index_Mask(
      Mask : in Integer_4_Unsigned)
      is
      begin
        Put_Line(
          "glIndexMask"                       & "( " &
          Integer_4_Unsigned'Wide_Image(Mask) & " )" );
        IndexMask(
          Integer_4_Unsigned_C(Mask));
      end Index_Mask;
  -----------
  -- Index --
  -----------
    procedure Index(
      Kind    : Integer_4_Unsigned;
      Stride  : Integer_4_Signed;
      Pointer : const GLvoid *)
      is
      begin
        Put_Line("glIndexPointer %s %d 'const GLvoid * pointer'", EnumString(type), stride);
        IndexPointer(type, stride, pointer);
      end Index;
    procedure Index(
      C : in Float_8_Real)
      is
      begin
        Put_Line("glIndexd %g", c);
        Indexd(c);
      end Index;
    procedure Index(
      C : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "c"
        Put_Line("glIndexdv 'const GLdouble * c'");
        Indexdv(c);
      end Index;
    procedure Index(
      C : in Float_4_Real)
      is
      begin
        Put_Line("glIndexf %g", c);
        Indexf(c);
      end Index;
    procedure Index(
      C : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "c"
        Put_Line("glIndexfv 'const GLfloat * c'");
        Indexfv(c);
      end Index;
    procedure Index(
      C : in Integer_4_Signed)
      is
      begin
        Put_Line("glIndexi %d", c);
        Indexi(c);
      end Index;
    procedure Index(
      C : in Access_Constant_Integer_4_Unsigned)
      is
      begin
        -- unknown type: "const GLint *" name: "c"
        Put_Line("glIndexiv 'const GLint * c'");
        Indexiv(c);
      end Index;
    procedure Index(
      C : in Integer_2_Signed)
      is
      begin
        Put_Line("glIndexs %d", c);
        Indexs(c);
      end Index;
    procedure Index(
      C : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "c"
        Put_Line("glIndexsv 'const GLshort * c'");
        Indexsv(c);
      end Index;
    procedure Index(
      GLubyte c)
      is
      begin
        Put_Line("glIndexub %d", c);
        Indexub(c);
      end Index;
    procedure Index(
      const GLubyte *c)
      is
      begin
        -- unknown type: "const GLubyte *" name: "c"
        Put_Line("glIndexubv 'const GLubyte * c'");
        Indexubv(c);
      end Index;
  ----------------------
  -- Initialize_Names --
  ----------------------
    procedure Initialize_Names
      is
      begin
        Put_Line("glInitNames");
        InitNames; 
      end Initialize_Names;
  ------------------------
  -- Interleaved_Arrays --
  ------------------------
    procedure Interleaved_Arrays(
      GLenum format,
      GLsizei stride,
      const GLvoid *pointer)
      is
      begin
        -- unknown type: "const GLvoid *" name: "pointer"
        Put_Line("glInterleavedArrays %s %d 'const GLvoid * pointer'", EnumString(format), stride);
        InterleavedArrays(format, stride, pointer);
      end Interleaved_Arrays;
  ----------------
  -- Is_Enabled --
  ----------------
    function Is_Enabled(
      Cap : in Integer_4_Unsigned)
      return Boolean
      is
      begin
        Put_Line("glIsEnabled %s", EnumString(cap));
        return dllIsEnabled(cap);
      end Is_Enabled;
  -------------
  -- Is_List --
  -------------
    function Is_List(
      List : in Integer_4_Unsigned)
      return Boolean
      is
      begin
        Put_Line("glIsList %d", list);
        return dllIsList(list);
      end Is_List;
  ----------------
  -- Is_Texture --
  ----------------
    function Is_Texture(
      GLuint texture)
      return Boolean
      is
      begin
        Put_Line("glIsTexture %d", texture);
        return dllIsTexture(texture);
      end Is_Texture;
  -----------------
  -- Light_Model --
  -----------------
    procedure Light_Model(
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real)
      is
      begin
        Put_Line("glLightModelf %s %g", EnumString(pname), param);
        LightModelf(pname, param);
      end Light_Model;
    procedure Light_Model(
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "params"
        Put_Line("glLightModelfv %s 'const GLfloat * params'", EnumString(pname));
        LightModelfv(pname, params);
      end Light_Model;
    procedure Light_Model(
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed)
      is
      begin
        Put_Line("glLightModeli %s %d", EnumString(pname), param);
        LightModeli(pname, param);
      end Light_Model;
    procedure Light_Model(
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Unsigned)
      is
      begin
        -- unknown type: "const GLint *" name: "params"
        Put_Line("glLightModeliv %s 'const GLint * params'", EnumString(pname));
        LightModeliv(pname, params);
      end Light_Model;
  -----------
  -- Light --
  -----------
    procedure Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real)
      is
      begin
        Put_Line("glLightf %s %s %g", EnumString(light), EnumString(pname), param);
        Lightf(light, pname, param);
      end Light;
    procedure Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "params"
        Put_Line("glLightfv %s %s 'const GLfloat * params'", EnumString(light), EnumString(pname));
        Lightfv(light, pname, params);
      end Light;
    procedure Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed)
      is
      begin
        Put_Line("glLighti %s %s %d", EnumString(light), EnumString(pname), param);
        Lighti(light, pname, param);
      end Light;
    procedure Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Unsigned)
      is
      begin
        -- unknown type: "const GLint *" name: "params"
        Put_Line("glLightiv %s %s 'const GLint * params'", EnumString(light), EnumString(pname));
        Lightiv(light, pname, params);
      end Light;
  ------------------
  -- Line_Stipple --
  ------------------
    procedure Line_Stipple(
      Factor  : in Integer_4_Signed;
      Pattern : in Integer_2_Unsigned)
      is
      begin
        Put_Line("glLineStipple %d %d", factor, pattern);
        LineStipple(factor, pattern);
      end Line_Stipple;
  ----------------
  -- Line_Width --
  ----------------
    procedure Line_Width(
      Width : in Float_4_Real)
      is
      begin
        Put_Line("glLineWidth %g", width);
        LineWidth(width);
      end Line_Width;
  ---------------
  -- List_Base --
  ---------------
    procedure List_Base(
      Base : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glListBase %d", base);
        ListBase(base);
      end List_Base;
  -------------------
  -- Load_Identity --
  -------------------
    procedure Load_Identity
      is
      begin
        Put_Line("glLoadIdentity");
        LoadIdentity; 
      end Load_Identity;
  -----------------
  -- Load_Matrix --
  -----------------
    procedure Load_Matrix(
      M : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "m"
        Put_Line("glLoadMatrixd 'const GLdouble * m'");
        LoadMatrixd(m);
      end Load_Matrix;
    procedure Load_Matrix(
      M : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "m"
        Put_Line("glLoadMatrixf 'const GLfloat * m'");
        LoadMatrixf(m);
      end Load_Matrix;
  ---------------
  -- Load_Name --
  ---------------
    procedure Load_Name(
      Name : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glLoadName %d", name);
        LoadName(name);
      end Load_Name;
  ---------------------
  -- Logic_Operation --
  ---------------------
    procedure Logic_Operation(
      Operation_Code : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glLogicOp %s", EnumString(opcode));
        LogicOp(opcode);
      end Logic_Operation;
  ---------
  -- Map --
  ---------
    procedure Map(
      Target : in Integer_4_Unsigned;
      U_1    : in Float_8_Real;
      U_2    : in Float_8_Real;
      Stride : in Integer_4_Signed;
      Order  : in Integer_4_Signed;
      Points : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "points"
        Put_Line("glMap1d %s %g %g %d %d 'const GLdouble * points'", EnumString(target), u1, u2, stride, order);
        Map1d(target, u1, u2, stride, order, points);
      end Map;
    procedure Map(
      Target : in Integer_4_Unsigned;
      U_1     : in Float_4_Real;
      U_2     : in Float_4_Real;
      Stride : in Integer_4_Signed;
      Order  : in Integer_4_Signed;
      Points : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "points"
        Put_Line("glMap1f %s %g %g %d %d 'const GLfloat * points'", EnumString(target), u1, u2, stride, order);
        Map1f(target, u1, u2, stride, order, points);
      end Map;
    procedure Map(
      Target   : in Integer_4_Unsigned;
      U_1      : in Float_8_Real;
      U_2      : in Float_8_Real;
      U_Stride : in Integer_4_Signed;
      U_Order  : in Integer_4_Signed;
      V_1      : in Float_8_Real;
      V_2      : in Float_8_Real;
      V_Stride : in Integer_4_Signed;
      V_Order  : in Integer_4_Signed;
      Points   : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "points"
        Put_Line("glMap2d %s %g %g %d %d %g %g %d %d 'const GLdouble * points'", EnumString(target), u1, u2, ustride, uorder, v1, v2, vstride, vorder);
        Map2d(target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points);
      end Map;
    procedure Map(
      Target   : in Integer_4_Unsigned;
      U_1      : in Float_4_Real;
      U_2      : in Float_4_Real;
      U_Stride : in Integer_4_Signed;
      U_Order  : in Integer_4_Signed;
      V_1      : in Float_4_Real;
      V_2      : in Float_4_Real;
      V_Stride : in Integer_4_Signed;
      V_Order  : in Integer_4_Signed;
      Points   : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "points"
        Put_Line("glMap2f %s %g %g %d %d %g %g %d %d 'const GLfloat * points'", EnumString(target), u1, u2, ustride, uorder, v1, v2, vstride, vorder);
        Map2f(target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points);
      end Map;
  --------------
  -- Map_Grid --
  --------------
    procedure Map_Grid(
      U_N : in Integer_4_Signed;
      U_1 : in Float_8_Real;
      U_2 : in Float_8_Real)
      is
      begin
        Put_Line("glMapGrid1d %d %g %g", un, u1, u2);
        MapGrid1d(un, u1, u2);
      end Map_Grid;
    procedure Map_Grid(
      U_N : in Integer_4_Signed;
      U_1 : in Float_4_Real;
      U_2 : in Float_4_Real)
      is
      begin
        Put_Line("glMapGrid1f %d %g %g", un, u1, u2);
        MapGrid1f(un, u1, u2);
      end Map_Grid;
    procedure Map_Grid(
      U_N : in Integer_4_Signed;
      U_1 : in Float_8_Real;
      U_2 : in Float_8_Real;
      V_N : in Integer_4_Signed;
      V_1 : in Float_8_Real;
      V_2 : in Float_8_Real)
      is
      begin
        Put_Line("glMapGrid2d %d %g %g %d %g %g", un, u1, u2, vn, v1, v2);
        MapGrid2d(un, u1, u2, vn, v1, v2);
      end Map_Grid;
    procedure Map_Grid(
      U_N : in Integer_4_Signed;
      U_1 : in Float_4_Real;
      U_2 : in Float_4_Real;
      V_N : in Integer_4_Signed;
      V_1 : in Float_4_Real;
      V_2 : in Float_4_Real)
      is
      begin
        Put_Line("glMapGrid2f %d %g %g %d %g %g", un, u1, u2, vn, v1, v2);
        MapGrid2f(un, u1, u2, vn, v1, v2);
      end Map_Grid;
  --------------
  -- Material --
  --------------
    procedure Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real)
      is
      begin
        Put_Line("glMaterialf %s %s %g", EnumString(face), EnumString(pname), param);
        Materialf(face, pname, param);
      end Material;
    procedure Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "params"
        Put_Line("glMaterialfv %s %s 'const GLfloat * params'", EnumString(face), EnumString(pname));
        Materialfv(face, pname, params);
      end Material;
    procedure Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed)
      is
      begin
        Put_Line("glMateriali %s %s %d", EnumString(face), EnumString(pname), param);
        Materiali(face, pname, param);
      end Material;
    procedure Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "params"
        Put_Line("glMaterialiv %s %s 'const GLint * params'", EnumString(face), EnumString(pname));
        Materialiv(face, pname, params);
      end Material;
  -----------------
  -- Matrix_Mode --
  -----------------
    procedure Matrix_Mode(
      Mode : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glMatrixMode %s", EnumString(mode));
        MatrixMode(mode);
      end Matrix_Mode;
  ---------------------
  -- Multiply_Matrix --
  ---------------------
    procedure Multiply_Matrix(
      M : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "m"
        Put_Line("glMultMatrixd 'const GLdouble * m'");
        MultMatrixd(m);
      end Multiply_Matrix;
    procedure Multiply_Matrix(
      M : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "m"
        Put_Line("glMultMatrixf 'const GLfloat * m'");
        MultMatrixf(m);
      end Multiply_Matrix;
  --------------
  -- New_List --
  --------------
    procedure New_List(
      List : in Integer_4_Unsigned;
      Mode : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glNewList %d %s", list, EnumString(mode));
        NewList(list, mode);
      end New_List;
  ------------
  -- Normal --
  ------------
    procedure Normal(
      N_X : in Integer_1_Unsigned;
      N_Y : in Integer_1_Unsigned;
      N_Z : in Integer_1_Unsigned)
      is
      begin
        Put_Line("glNormal3b %d %d %d", nx, ny, nz);
        Normal3b(nx, ny, nz);
      end Normal;
    procedure Normal(
      V : in Win32.PCSTR)
      is
      begin
        -- unknown type: "const GLbyte *" name: "v"
        Put_Line("glNormal3bv 'const GLbyte * v'");
        Normal3bv(v);
      end Normal;
    procedure Normal(
      N_X : in Float_8_Real;
      N_Y : in Float_8_Real;
      N_Z : in Float_8_Real)
      is
      begin
        Put_Line("glNormal3d %g %g %g", nx, ny, nz);
        Normal3d(nx, ny, nz);
      end Normal;
    procedure Normal(
      V : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v"
        Put_Line("glNormal3dv 'const GLdouble * v'");
        Normal3dv(v);
      end Normal;
    procedure Normal(
      N_X : in Float_4_Real;
      N_Y : in Float_4_Real;
      N_Z : in Float_4_Real)
      is
      begin
        Put_Line("glNormal3f %g %g %g", nx, ny, nz);
        Normal3f(nx, ny, nz);
      end Normal;
    procedure Normal(
      V : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v"
        Put_Line("glNormal3fv 'const GLfloat * v'");
        Normal3fv(v);
      end Normal;
    procedure Normal(
      N_X : in Integer_4_Signed;
      N_Y : in Integer_4_Signed;
      N_Z : in Integer_4_Signed)
      is
      begin
        Put_Line("glNormal3i %d %d %d", nx, ny, nz);
        Normal3i(nx, ny, nz);
      end Normal;
    procedure Normal(
      V : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v"
        Put_Line("glNormal3iv 'const GLint * v'");
        Normal3iv(v);
      end Normal;
    procedure Normal(
      N_X : in Integer_2_Signed;
      N_Y : in Integer_2_Signed;
      N_Z : in Integer_2_Signed)
      is
      begin
        Put_Line("glNormal3s %d %d %d", nx, ny, nz);
        Normal3s(nx, ny, nz);
      end Normal;
    procedure Normal(
      V : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "v"
        Put_Line("glNormal3sv 'const GLshort * v'");
        Normal3sv(v);
      end Normal;
    procedure Normal(
      GLenum type,
      GLsizei stride,
      const GLvoid *pointer)
      is
      begin
        -- unknown type: "const GLvoid *" name: "pointer"
        Put_Line("glNormalPointer %s %d 'const GLvoid * pointer'", EnumString(type), stride);
        NormalPointer(type, stride, pointer);
      end Normal;
  ------------------
  -- Orthographic --
  ------------------
    procedure Orthographic(
      Left   : in Float_8_Real;
      Right  : in Float_8_Real;
      Bottom : in Float_8_Real;
      Top    : in Float_8_Real;
      Z_Near : in Float_8_Real;
      Z_Far  : in Float_8_Real)
      is
      begin
        Put_Line("glOrtho %g %g %g %g %g %g", left, right, bottom, top, zNear, zFar);
        Ortho(left, right, bottom, top, zNear, zFar);
      end Orthographic;
  ------------------
  -- Pass_Through --
  ------------------
    procedure Pass_Through(
      Token : in Float_4_Real)
      is
      begin
        Put_Line("glPassThrough %g", token);
        PassThrough(token);
      end Pass_Through;
  ---------------
  -- Pixel_Map --
  ---------------
    procedure Pixel_Map(
      Map      : in Integer_4_Unsigned;
      Map_Size : in Integer_4_Signed;
      Values   : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "values"
        Put_Line("glPixelMapfv %s %d 'const GLfloat * values'", EnumString(map), mapsize);
        PixelMapfv(map, mapsize, values);
      end Pixel_Map;
    procedure Pixel_Map(
      Map      : in Integer_4_Unsigned;
      Map_Size : in Integer_4_Signed;
      Values   : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLuint *" name: "values"
        Put_Line("glPixelMapuiv %s %d 'const GLuint * values'", EnumString(map), mapsize);
        PixelMapuiv(map, mapsize, values);
      end Pixel_Map;
    procedure Pixel_Map(
      Map      : in Integer_4_Unsigned;
      Map_Size : in Integer_4_Signed;
      Values   : in Win32.PCWSTR)
      is
      begin
        -- unknown type: "const GLushort *" name: "values"
        Put_Line("glPixelMapusv %s %d 'const GLushort * values'", EnumString(map), mapsize);
        PixelMapusv(map, mapsize, values);
      end Pixel_Map;
  -----------------
  -- Pixel_Store --
  -----------------
    procedure Pixel_Store(
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real)
      is
      begin
        Put_Line("glPixelStoref %s %g", EnumString(pname), param);
        PixelStoref(pname, param);
      end Pixel_Store;
    procedure Pixel_Store(
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed)
      is
      begin
        Put_Line("glPixelStorei %s %d", EnumString(pname), param);
        PixelStorei(pname, param);
      end Pixel_Store;
  --------------------
  -- Pixel_Transfer --
  --------------------
    procedure Pixel_Transfer(
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real)
      is
      begin
        Put_Line("glPixelTransferf %s %g", EnumString(pname), param);
        PixelTransferf(pname, param);
      end Pixel_Transfer;
    procedure Pixel_Transfer(
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed)
      is
      begin
        Put_Line("glPixelTransferi %s %d", EnumString(pname), param);
        PixelTransferi(pname, param);
      end Pixel_Transfer;
  ----------------
  -- Pixel_Zoom --
  ----------------
    procedure Pixel_Zoom(
      X_Factor : in Float_4_Real;
      Y_Factor : in Float_4_Real)
      is
      begin
        Put_Line("glPixelZoom %g %g", xfactor, yfactor);
        PixelZoom(xfactor, yfactor);
      end Pixel_Zoom;
  ----------------
  -- Point_Size --
  ----------------
    procedure Point_Size(
      Size : in Float_4_Real)
      is
      begin
        Put_Line("glPointSize %g", size);
        PointSize(size);
      end Point_Size;
  ------------------
  -- Polygon_Mode --
  ------------------
    procedure Polygon_Mode(
      Face : in Integer_4_Unsigned;
      Mode : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glPolygonMode %s %s", EnumString(face), EnumString(mode));
        PolygonMode(face, mode);
      end Polygon_Mode;
  --------------------
  -- Polygon_Offset --
  --------------------
    procedure Polygon_Offset(
      GLfloat factor,
      GLfloat units)
      is
      begin
        Put_Line("glPolygonOffset %g %g", factor, units);
        PolygonOffset(factor, units);
      end Polygon_Offset;
  ---------------------
  -- Polygon_Stipple --
  ---------------------
    procedure Polygon_Stipple(
      Mask : in Access_Integer_1_Unsigned_C)
      is
      begin
        -- unknown type: "const GLubyte *" name: "mask"
        Put_Line("glPolygonStipple 'const GLubyte * mask'");
        PolygonStipple(mask);
      end Polygon_Stipple;
  -------------------
  -- Pop_Attribute --
  -------------------
    procedure Pop_Attribute
      is
      begin
        Put_Line("glPopAttrib");
        PopAttrib; 
      end Pop_Attribute;
  --------------------------
  -- Pop_Client_Attribute --
  --------------------------
    procedure Pop_Client_Attribute
      is
      begin
        Put_Line("glPopClientAttrib");
        PopClientAttrib; 
      end Pop_Client_Attribute;
  ----------------
  -- Pop_Matrix --
  ----------------
    procedure Pop_Matrix
      is
      begin
        Put_Line("glPopMatrix");
        PopMatrix; 
      end Pop_Matrix;
  --------------
  -- Pop_Name --
  --------------
    procedure Pop_Name
      is
      begin
        Put_Line("glPopName");
        PopName; 
      end Pop_Name;
  -------------------------
  -- Prioritize_Textures --
  -------------------------
    procedure Prioritize_Textures(
      GLsizei n,
      const GLuint *textures,
      const GLclampf *priorities)
      is
      begin
        -- unknown type: "const GLuint *" name: "textures"
        -- unknown type: "const GLclampf *" name: "priorities"
        Put_Line("glPrioritizeTextures %d 'const GLuint * textures' 'const GLclampf * priorities'", n);
        PrioritizeTextures(n, textures, priorities);
      end Prioritize_Textures;
  --------------------
  -- Push_Attribute --
  --------------------
    procedure Push_Attribute(
      Mask : in Integer_4_Unsigned)
      is
      begin
        -- unknown type: "GLbitfield" name: "mask"
        Put_Line("glPushAttrib 'GLbitfield mask'");
        PushAttrib(mask);
      end Push_Attribute;
  ---------------------------
  -- Push_Client_Attribute --
  ---------------------------
    procedure Push_Client_Attribute(
      GLbitfield mask)
      is
      begin
        -- unknown type: "GLbitfield" name: "mask"
        Put_Line("glPushClientAttrib 'GLbitfield mask'");
        PushClientAttrib(mask);
      end Push_Client_Attribute;
  -----------------
  -- Push_Matrix --
  -----------------
    procedure Push_Matrix
      is
      begin
        Put_Line("glPushMatrix");
        PushMatrix; 
      end Push_Matrix;
  ---------------
  -- Push_Name --
  ---------------
    procedure Push_Name(
      Name : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glPushName %d", name);
        PushName(name);
      end Push_Name;
  ---------------------
  -- Raster_Position --
  ---------------------
    procedure Raster_Position(
      X : in Float_8_Real;
      Y : in Float_8_Real)
      is
      begin
        Put_Line("glRasterPos2d %g %g", x, y);
        RasterPos2d(x, y);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v"
        Put_Line("glRasterPos2dv 'const GLdouble * v'");
        RasterPos2dv(v);
      end Raster_Position;
    procedure Raster_Position(
      X : in Float_4_Real;
      Y : in Float_4_Real)
      is
      begin
        Put_Line("glRasterPos2f %g %g", x, y);
        RasterPos2f(x, y);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v"
        Put_Line("glRasterPos2fv 'const GLfloat * v'");
        RasterPos2fv(v);
      end Raster_Position;
    procedure Raster_Position(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed)
      is
      begin
        Put_Line("glRasterPos2i %d %d", x, y);
        RasterPos2i(x, y);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v"
        Put_Line("glRasterPos2iv 'const GLint * v'");
        RasterPos2iv(v);
      end Raster_Position;
    procedure Raster_Position(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed)
      is
      begin
        Put_Line("glRasterPos2s %d %d", x, y);
        RasterPos2s(x, y);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "v"
        Put_Line("glRasterPos2sv 'const GLshort * v'");
        RasterPos2sv(v);
      end Raster_Position;
    procedure Raster_Position(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real)
      is
      begin
        Put_Line("glRasterPos3d %g %g %g", x, y, z);
        RasterPos3d(x, y, z);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v"
        Put_Line("glRasterPos3dv 'const GLdouble * v'");
        RasterPos3dv(v);
      end Raster_Position;
    procedure Raster_Position(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real)
      is
      begin
        Put_Line("glRasterPos3f %g %g %g", x, y, z);
        RasterPos3f(x, y, z);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v"
        Put_Line("glRasterPos3fv 'const GLfloat * v'");
        RasterPos3fv(v);
      end Raster_Position;
    procedure Raster_Position(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed;
      Z : in Integer_4_Signed)
      is
      begin
        Put_Line("glRasterPos3i %d %d %d", x, y, z);
        RasterPos3i(x, y, z);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v"
        Put_Line("glRasterPos3iv 'const GLint * v'");
        RasterPos3iv(v);
      end Raster_Position;
    procedure Raster_Position(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed;
      Z : in Integer_2_Signed)
      is
      begin
        Put_Line("glRasterPos3s %d %d %d", x, y, z);
        RasterPos3s(x, y, z);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "v"
        Put_Line("glRasterPos3sv 'const GLshort * v'");
        RasterPos3sv(v);
      end Raster_Position;
    procedure Raster_Position(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real;
      W : in Float_8_Real)
      is
      begin
        Put_Line("glRasterPos4d %g %g %g %g", x, y, z, w);
        RasterPos4d(x, y, z, w);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v"
        Put_Line("glRasterPos4dv 'const GLdouble * v'");
        RasterPos4dv(v);
      end Raster_Position;
    procedure Raster_Position(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real;
      W : in Float_4_Real)
      is
      begin
        Put_Line("glRasterPos4f %g %g %g %g", x, y, z, w);
        RasterPos4f(x, y, z, w);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v"
        Put_Line("glRasterPos4fv 'const GLfloat * v'");
        RasterPos4fv(v);
      end Raster_Position;
    procedure Raster_Position(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed;
      Z : in Integer_4_Signed;
      W : in Integer_4_Signed)
      is
      begin
        Put_Line("glRasterPos4i %d %d %d %d", x, y, z, w);
        RasterPos4i(x, y, z, w);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v"
        Put_Line("glRasterPos4iv 'const GLint * v'");
        RasterPos4iv(v);
      end Raster_Position;
    procedure Raster_Position(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed;
      Z : in Integer_2_Signed;
      W : in Integer_2_Signed)
      is
      begin
        Put_Line("glRasterPos4s %d %d %d %d", x, y, z, w);
        RasterPos4s(x, y, z, w);
      end Raster_Position;
    procedure Raster_Position(
      V : in Access_Constant_Integer_2_Signed
      is
      begin
        -- unknown type: "const GLshort *" name: "v"
        Put_Line("glRasterPos4sv 'const GLshort * v'");
        RasterPos4sv(v);
      end Raster_Position;
  -----------------
  -- Read_Buffer --
  -----------------
    procedure Read_Buffer(
      Mode : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glReadBuffer %s", EnumString(mode));
        ReadBuffer(mode);
      end Read_Buffer;
  -----------------
  -- Read_Pixels --
  -----------------
    procedure Read_Pixels(
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed;
      Format : in Integer_4_Unsigned;
      Kind   : in Integer_4_Unsigned;
      Pixels : in PGLvoid)
      is
      begin
        -- unknown type: "GLvoid *" name: "pixels"
        Put_Line("glReadPixels %d %d %d %d %s %s 'GLvoid * pixels'", x, y, width, height, EnumString(format), EnumString(type));
        ReadPixels(x, y, width, height, format, type, pixels);
      end Read_Pixels;
  ---------------
  -- Rectangle --
  ---------------
    procedure Rectangle(
      X_1 : in Float_8_Real;
      Y_1 : in Float_8_Real;
      X_2 : in Float_8_Real;
      Y_2 : in Float_8_Real)
      is
      begin
        Put_Line("glRectd %g %g %g %g", x1, y1, x2, y2);
        Rectd(x1, y1, x2, y2);
      end Rectangle;
    procedure Rectangle(
      V_1 : in Access_Constant_Float_8_Real;
      V_2 : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v1"
        -- unknown type: "const GLdouble *" name: "v2"
        Put_Line("glRectdv 'const GLdouble * v1' 'const GLdouble * v2'");
        Rectdv(v1, v2);
      end Rectangle;
    procedure Rectangle(
      X_1 : in Float_4_Real;
      Y_1 : in Float_4_Real;
      X_2 : in Float_4_Real;
      Y_2 : in Float_4_Real)
      is
      begin
        Put_Line("glRectf %g %g %g %g", x1, y1, x2, y2);
        Rectf(x1, y1, x2, y2);
      end Rectangle;
    procedure Rectangle(
      V_1 : in Access_Constant_Float_4_Real;
      V_2 : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v1"
        -- unknown type: "const GLfloat *" name: "v2"
        Put_Line("glRectfv 'const GLfloat * v1' 'const GLfloat * v2'");
        Rectfv(v1, v2);
      end Rectangle;
    procedure Rectangle(
      X_1 : in Integer_4_Signed;
      Y_1 : in Integer_4_Signed;
      X_2 : in Integer_4_Signed;
      Y_2 : in Integer_4_Signed)
      is
      begin
        Put_Line("glRecti %d %d %d %d", x1, y1, x2, y2);
        Recti(x1, y1, x2, y2);
      end Rectangle;
    procedure Rectangle(
      V_1 : in Access_Constant_Integer_4_Signed;
      V_2 : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v1"
        -- unknown type: "const GLint *" name: "v2"
        Put_Line("glRectiv 'const GLint * v1' 'const GLint * v2'");
        Rectiv(v1, v2);
      end Rectangle;
    procedure Rectangle(
      X_1 : in Integer_2_Signed;
      Y_1 : in Integer_2_Signed;
      X_2 : in Integer_2_Signed;
      Y_2 : in Integer_2_Signed)
      is
      begin
        Put_Line("glRects %d %d %d %d", x1, y1, x2, y2);
        Rects(x1, y1, x2, y2);
      end Rectangle;
    procedure Rectangle(
      V_1 : in Access_Constant_Integer_2_Signed;
      V_2 : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "v1"
        -- unknown type: "const GLshort *" name: "v2"
        Put_Line("glRectsv 'const GLshort * v1' 'const GLshort * v2'");
        Rectsv(v1, v2);
      end Rectangle;
  -----------------
  -- Render_Mode --
  -----------------
    function Render_Mode(
      Mode : in Integer_4_Unsigned)
      return Integer_4_Signed
      is
      begin
        Put_Line("glRenderMode %s", EnumString(mode));
        return dllRenderMode(mode);
      end Render_Mode;
  ------------
  -- Rotate --
  ------------
    procedure Rotate(
      Angle : in Float_8_Real;
      X     : in Float_8_Real;
      Y     : in Float_8_Real;
      Z     : in Float_8_Real)
      is
      begin
        Put_Line("glRotated %g %g %g %g", angle, x, y, z);
        Rotated(angle, x, y, z);
      end Rotate;
    procedure Rotate(
      Angle : in Float_4_Real;
      X     : in Float_4_Real;
      Y     : in Float_4_Real;
      Z     : in Float_4_Real)
      is
      begin
        Put_Line("glRotatef %g %g %g %g", angle, x, y, z);
        Rotatef(angle, x, y, z);
      end Rotate;
  -----------
  -- Scale --
  -----------
    procedure Scale(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real)
      is
      begin
        Put_Line("glScaled %g %g %g", x, y, z);
        Scaled(x, y, z);
      end Scale;
    procedure Scale(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real)
      is
      begin
        Put_Line("glScalef %g %g %g", x, y, z);
        Scalef(x, y, z);
      end Scale;
  -------------
  -- Scissor --
  -------------
    procedure Scissor(
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed)
      is
      begin
        Put_Line("glScissor %d %d %d %d", x, y, width, height);
        Scissor(x, y, width, height);
      end Scissor;
  -------------------
  -- Select_Buffer --
  -------------------
    procedure Select_Buffer(
      Size   : in Integer_4_Signed;
      Buffer : in Access_Integer_4_Unsigned)
      is
      begin
        if ---
          raise System_Call_Failure;
        end if;
        -- unknown type: "GLuint *" name: "buffer"
        Put_Line("glSelectBuffer %d 'GLuint * buffer'", size);
        SelectBuffer(size, buffer);
      end Select_Buffer;
  -----------------
  -- Shade_Model --
  -----------------
    procedure Shade_Model(
      Mode : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glShadeModel %s", EnumString(mode));
        ShadeModel(mode);
      end Shade_Model;
  ----------------------
  -- Stencil_Function --
  ----------------------
    procedure Stencil_Function(
      Function  : in Integer_4_Unsigned;
      Reference : in Integer_4_Signed;
      Mask      : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glStencilFunc %s %d %d", EnumString(func), ref, mask);
        StencilFunc(func, ref, mask);
      end Stencil_Function;
  ------------------
  -- Stencil_Mask --
  ------------------
    procedure Stencil_Mask(
      Mask : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glStencilMask %d", mask);
        StencilMask(mask);
      end Stencil_Mask;
  -----------------------
  -- Stencil_Operation --
  -----------------------
    procedure Stencil_Operation(
      Fail   : in Integer_4_Unsigned;
      Z_Fail : in Integer_4_Unsigned;
      Z_Pass : in Integer_4_Unsigned)
      is
      begin
        Put_Line("glStencilOp %s %s %s", EnumString(fail), EnumString(zfail), EnumString(zpass));
        StencilOp(fail, zfail, zpass);
      end Stencil_Operation;
  ------------------------
  -- Texture_Coordinate --
  ------------------------
    procedure Texture_Coordinate(
      S : in Float_8_Real)
      is
      begin
        Put_Line("glTexCoord1d %g", s);
        TexCoord1d(s);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v"
        Put_Line("glTexCoord1dv 'const GLdouble * v'");
        TexCoord1dv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Float_4_Real)
      is
      begin
        Put_Line("glTexCoord1f %g", s);
        TexCoord1f(s);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v"
        Put_Line("glTexCoord1fv 'const GLfloat * v'");
        TexCoord1fv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Integer_4_Signed)
        Put_Line("glTexCoord1i %d", s);
        TexCoord1i(s);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v"
        Put_Line("glTexCoord1iv 'const GLint * v'");
        TexCoord1iv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Integer_2_Signed)
      is
      begin
        Put_Line("glTexCoord1s %d", s);
        TexCoord1s(s);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "v"
        Put_Line("glTexCoord1sv 'const GLshort * v'");
        TexCoord1sv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Float_8_Real;
      T : in Float_8_Real)
      is
      begin
        Put_Line("glTexCoord2d %g %g", s, t);
        TexCoord2d(s, t);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v"
        Put_Line("glTexCoord2dv 'const GLdouble * v'");
        TexCoord2dv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Float_4_Real;
      T : in Float_4_Real)
      is
      begin
        Put_Line("glTexCoord2f %g %g", s, t);
        TexCoord2f(s, t);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v"
        Put_Line("glTexCoord2fv 'const GLfloat * v'");
        TexCoord2fv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Integer_4_Signed;
      T : in Integer_4_Signed)
      is
      begin
        Put_Line("glTexCoord2i %d %d", s, t);
        TexCoord2i(s, t);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v"
        Put_Line("glTexCoord2iv 'const GLint * v'");
        TexCoord2iv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Integer_2_Signed;
      T : in Integer_2_Signed)
      is
      begin
        Put_Line("glTexCoord2s %d %d", s, t);
        TexCoord2s(s, t);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "v"
        Put_Line("glTexCoord2sv 'const GLshort * v'");
        TexCoord2sv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Float_8_Real;
      T : in Float_8_Real;
      R : in Float_8_Real)
      is
      begin
        Put_Line("glTexCoord3d %g %g %g", s, t, r);
        TexCoord3d(s, t, r);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v"
        Put_Line("glTexCoord3dv 'const GLdouble * v'");
        TexCoord3dv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Float_4_Real;
      T : in Float_4_Real;
      R : in Float_4_Real)
      is
      begin
        Put_Line("glTexCoord3f %g %g %g", s, t, r);
        TexCoord3f(s, t, r);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v"
        Put_Line("glTexCoord3fv 'const GLfloat * v'");
        TexCoord3fv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Integer_4_Signed;
      T : in Integer_4_Signed;
      R : in Integer_4_Signed)
      is
      begin
        Put_Line("glTexCoord3i %d %d %d", s, t, r);
        TexCoord3i(s, t, r);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v"
        Put_Line("glTexCoord3iv 'const GLint * v'");
        TexCoord3iv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Integer_2_Signed;
      T : in Integer_2_Signed;
      R : in Integer_2_Signed)
      is
      begin
        Put_Line("glTexCoord3s %d %d %d", s, t, r);
        TexCoord3s(s, t, r);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "v"
        Put_Line("glTexCoord3sv 'const GLshort * v'");
        TexCoord3sv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Float_8_Real;
      T : in Float_8_Real;
      R : in Float_8_Real;
      Q : in Float_8_Real)
      is
      begin
        Put_Line("glTexCoord4d %g %g %g %g", s, t, r, q);
        TexCoord4d(s, t, r, q);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v"
        Put_Line("glTexCoord4dv 'const GLdouble * v'");
        TexCoord4dv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Float_4_Real;
      T : in Float_4_Real;
      R : in Float_4_Real;
      Q : in Float_4_Real)
      is
      begin
        Put_Line("glTexCoord4f %g %g %g %g", s, t, r, q);
        TexCoord4f(s, t, r, q);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v"
        Put_Line("glTexCoord4fv 'const GLfloat * v'");
        TexCoord4fv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Integer_4_Signed;
      T : in Integer_4_Signed;
      R : in Integer_4_Signed;
      Q : in Integer_4_Signed)
      is
      begin
        Put_Line("glTexCoord4i %d %d %d %d", s, t, r, q);
        TexCoord4i(s, t, r, q);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v"
        Put_Line("glTexCoord4iv 'const GLint * v'");
        TexCoord4iv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      S : in Integer_2_Signed;
      T : in Integer_2_Signed;
      R : in Integer_2_Signed;
      Q : in Integer_2_Signed)
      is
      begin
        Put_Line("glTexCoord4s %d %d %d %d", s, t, r, q);
        TexCoord4s(s, t, r, q);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "v"
        Put_Line("glTexCoord4sv 'const GLshort * v'");
        TexCoord4sv(v);
      end Texture_Coordinate;
    procedure Texture_Coordinate(
      GLint size,
      GLenum type,
      GLsizei stride,
      const GLvoid *pointer)
      is
      begin
        -- unknown type: "const GLvoid *" name: "pointer"
        Put_Line("glTexCoordPointer %d %s %d 'const GLvoid * pointer'", size, EnumString(type), stride);
        TexCoordPointer(size, type, stride, pointer);
      end Texture_Coordinate;
  -------------------------
  -- Texture_Environment --
  -------------------------
    procedure Texture_Environment(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real)
      is
      begin
        Put_Line("glTexEnvf %s %s %g", EnumString(target), EnumString(pname), param);
        TexEnvf(target, pname, param);
      end Texture_Environment;
    procedure Texture_Environment(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "params"
        Put_Line("glTexEnvfv %s %s 'const GLfloat * params'", EnumString(target), EnumString(pname));
        TexEnvfv(target, pname, params);
      end Texture_Environment;
    procedure Texture_Environment(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed)
      is
      begin
        Put_Line("glTexEnvi %s %s %d", EnumString(target), EnumString(pname), param);
        TexEnvi(target, pname, param);
      end Texture_Environment;
    procedure Texture_Environment(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "params"
        Put_Line("glTexEnviv %s %s 'const GLint * params'", EnumString(target), EnumString(pname));
        TexEnviv(target, pname, params);
      end Texture_Environment;
  ----------------------
  -- Texture_Generate --
  ----------------------
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_8_Real)
      is
      begin
        Put_Line("glTexGend %s %s %g", EnumString(coord), EnumString(pname), param);
        TexGend(coord, pname, param);
      end Texture_Generate;
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "params"
        Put_Line("glTexGendv %s %s 'const GLdouble * params'", EnumString(coord), EnumString(pname));
        TexGendv(coord, pname, params);
      end Texture_Generate;
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real)
      is
      begin
        Put_Line("glTexGenf %s %s %g", EnumString(coord), EnumString(pname), param);
        TexGenf(coord, pname, param);
      end Texture_Generate;
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "params"
        Put_Line("glTexGenfv %s %s 'const GLfloat * params'", EnumString(coord), EnumString(pname));
        TexGenfv(coord, pname, params);
      end Texture_Generate;
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed)
      is
      begin
        Put_Line("glTexGeni %s %s %d", EnumString(coord), EnumString(pname), param);
        TexGeni(coord, pname, param);
      end Texture_Generate;
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "params"
        Put_Line("glTexGeniv %s %s 'const GLint * params'", EnumString(coord), EnumString(pname));
        TexGeniv(coord, pname, params);
      end Texture_Generate;
  ----------------------
  -- Texture_Image_1D --
  ----------------------
    procedure Texture_Image_1D(
      Target     : in Integer_4_Unsigned;
      level      : in Integer_4_Signed;
      components : in Integer_4_Signed;
      Width      : in Integer_4_Signed;
      Border     : in Integer_4_Signed;
      Format     : in Integer_4_Unsigned;
      Kind       : in Integer_4_Unsigned;
      Pixels     : in Win32.PCVOID)
      is
      begin
        -- unknown type: "const GLvoid *" name: "pixels"
        Put_Line("glTexImage1D %s %d %d %d %d %s %s 'const GLvoid * pixels'", EnumString(target), level, internalformat, width, border, EnumString(format), EnumString(type));
        TexImage1D(target, level, internalformat, width, border, format, type, pixels);
      end Texture_Image_1D;
  ----------------------
  -- Texture_Image_2D --
  ----------------------
    procedure Texture_Image_2D(
      Target     : in Integer_4_Unsigned;
      level      : in Integer_4_Signed;
      components : in Integer_4_Signed;
      Width      : in Integer_4_Signed;
      Height     : in Integer_4_Signed;
      Border     : in Integer_4_Signed;
      Format     : in Integer_4_Unsigned;
      Kind       : in Integer_4_Unsigned;
      Pixels     : in Win32.PCVOID)
      is
      begin
        -- unknown type: "const GLvoid *" name: "pixels"
        Put_Line("glTexImage2D %s %d %d %d %d %d %s %s 'const GLvoid * pixels'", EnumString(target), level, internalformat, width, height, border, EnumString(format), EnumString(type));
        TexImage2D(target, level, internalformat, width, height, border, format, type, pixels);
      end Texture_Image_2D;
  -----------------------
  -- Texture_Parameter --
  -----------------------
    procedure Texture_Parameter(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real)
      is
      begin
        Put_Line("glTexParameterf %s %s %g", EnumString(target), EnumString(pname), param);
        TexParameterf(target, pname, param);
      end Texture_Parameter;
    procedure Texture_Parameter(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "params"
        Put_Line("glTexParameterfv %s %s 'const GLfloat * params'", EnumString(target), EnumString(pname));
        TexParameterfv(target, pname, params);
      end Texture_Parameter;
    procedure Texture_Parameter(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed)
      is
      begin
        Put_Line("glTexParameteri %s %s %d", EnumString(target), EnumString(pname), param);
        TexParameteri(target, pname, param);
      end Texture_Parameter;
    procedure Texture_Parameter(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "params"
        Put_Line("glTexParameteriv %s %s 'const GLint * params'", EnumString(target), EnumString(pname));
        TexParameteriv(target, pname, params);
      end Texture_Parameter;
  -------------------------
  -- Texture_Subimage_1D --
  -------------------------
    procedure Texture_Subimage_1D(
      GLenum target,
      GLint level,
      GLint xoffset,
      GLsizei width,
      GLenum format,
      GLenum type,
      const GLvoid *pixels)
      is
      begin
        -- unknown type: "const GLvoid *" name: "pixels"
        Put_Line("glTexSubImage1D %s %d %d %d %s %s 'const GLvoid * pixels'", EnumString(target), level, xoffset, width, EnumString(format), EnumString(type));
        TexSubImage1D(target, level, xoffset, width, format, type, pixels);
      end Texture_Subimage_1D;
  -------------------------
  -- Texture_Subimage_2D --
  -------------------------
    procedure Texture_Subimage_2D(
      GLenum target,
      GLint level,
      GLint xoffset,
      GLint yoffset,
      GLsizei width,
      GLsizei height,
      GLenum format,
      GLenum type,
      const GLvoid *pixels)
      is
      begin
        -- unknown type: "const GLvoid *" name: "pixels"
        Put_Line("glTexSubImage2D %s %d %d %d %d %d %s %s 'const GLvoid * pixels'", EnumString(target), level, xoffset, yoffset, width, height, EnumString(format), EnumString(type));
        TexSubImage2D(target, level, xoffset, yoffset, width, height, format, type, pixels);
      end Texture_Subimage_2D;
  ---------------
  -- Translate --
  ---------------
    procedure Translate(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real)
      is
      begin
        Put_Line("glTranslated %g %g %g", x, y, z);
        Translated(x, y, z);
      end Translate;
    procedure Translate(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real)
      is
      begin
        Put_Line("glTranslatef %g %g %g", x, y, z);
        Translatef(x, y, z);
      end Translate;
  ------------
  -- Vertex --
  ------------
    procedure Vertex(
      X : in Float_8_Real;
      Y : in Float_8_Real)
      is
      begin
        Put_Line("glVertex2d %g %g", x, y);
        Vertex2d(x, y);
      end ;
    procedure Vertex(
      V : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v"
        Put_Line("glVertex2dv 'const GLdouble * v'");
        Vertex2dv(v);
      end ;
    procedure Vertex(
      X : in Float_4_Real;
      Y : in Float_4_Real)
      is
      begin
        Put_Line("glVertex2f %g %g", x, y);
        Vertex2f(x, y);
      end ;
    procedure Vertex(
      V : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v"
        Put_Line("glVertex2fv 'const GLfloat * v'");
        Vertex2fv(v);
      end Vertex;
    procedure Vertex(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed)
      is
      begin
        Put_Line("glVertex2i %d %d", x, y);
        Vertex2i(x, y);
      end Vertex;
    procedure Vertex(
      V : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v"
        Put_Line("glVertex2iv 'const GLint * v'");
        Vertex2iv(v);
      end Vertex;
    procedure Vertex(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed)
      is
      begin
        Put_Line("glVertex2s %d %d", x, y);
        Vertex2s(x, y);
      end Vertex;
    procedure Vertex(
      V : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "v"
        Put_Line("glVertex2sv 'const GLshort * v'");
        Vertex2sv(v);
      end Vertex;
    procedure Vertex(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real)
      is
      begin
        Put_Line("glVertex3d %g %g %g", x, y, z);
        Vertex3d(x, y, z);
      end Vertex;
    procedure Vertex(
      V : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v"
        Put_Line("glVertex3dv 'const GLdouble * v'");
        Vertex3dv(v);
      end Vertex;
    procedure Vertex(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real)
      is
      begin
        Put_Line("glVertex3f %g %g %g", x, y, z);
        Vertex3f(x, y, z);
      end Vertex;
    procedure Vertex(
      V : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v"
        Put_Line("glVertex3fv 'const GLfloat * v'");
        Vertex3fv(v);
      end Vertex;
    procedure Vertex(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed;
      Z : in Integer_4_Signed)
      is
      begin
        Put_Line("glVertex3i %d %d %d", x, y, z);
        Vertex3i(x, y, z);
      end Vertex;
    procedure Vertex(
      V : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v"
        Put_Line("glVertex3iv 'const GLint * v'");
        Vertex3iv(v);
      end Vertex;
    procedure Vertex(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed;
      Z : in Integer_2_Signed)
      is
      begin
        Put_Line("glVertex3s %d %d %d", x, y, z);
        Vertex3s(x, y, z);
      end Vertex;
    procedure Vertex(
      V : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "v"
        Put_Line("glVertex3sv 'const GLshort * v'");
        Vertex3sv(v);
      end Vertex;
    procedure Vertex(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real;
      W : in Float_8_Real)
      is
      begin
        Put_Line("glVertex4d %g %g %g %g", x, y, z, w);
        Vertex4d(x, y, z, w);
      end Vertex;
    procedure Vertex(
      V : in Access_Constant_Float_8_Real)
      is
      begin
        -- unknown type: "const GLdouble *" name: "v"
        Put_Line("glVertex4dv 'const GLdouble * v'");
        Vertex4dv(v);
      end Vertex;
    procedure Vertex(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real;
      W : in Float_4_Real)
      is
      begin
        Put_Line("glVertex4f %g %g %g %g", x, y, z, w);
        Vertex4f(x, y, z, w);
      end Vertex;
    procedure Vertex(
      V : in Access_Constant_Float_4_Real)
      is
      begin
        -- unknown type: "const GLfloat *" name: "v"
        Put_Line("glVertex4fv 'const GLfloat * v'");
        Vertex4fv(v);
      end Vertex;
    procedure Vertex(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed;
      Z : in Integer_4_Signed;
      W : in Integer_4_Signed)
      is
      begin
        Put_Line("glVertex4i %d %d %d %d", x, y, z, w);
        Vertex4i(x, y, z, w);
      end Vertex;
    procedure Vertex(
      V : in Access_Constant_Integer_4_Signed)
      is
      begin
        -- unknown type: "const GLint *" name: "v"
        Put_Line("glVertex4iv 'const GLint * v'");
        Vertex4iv(v);
      end Vertex;
    procedure Vertex(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed;
      Z : in Integer_2_Signed;
      W : in Integer_2_Signed)
      is
      begin
        Put_Line("glVertex4s %d %d %d %d", x, y, z, w);
        Vertex4s(x, y, z, w);
      end Vertex;
    procedure Vertex(
      V : in Access_Constant_Integer_2_Signed)
      is
      begin
        -- unknown type: "const GLshort *" name: "v"
        Put_Line("glVertex4sv 'const GLshort * v'");
        Vertex4sv(v);
      end Vertex;
    procedure Vertex(
      GLint size,
      GLenum type,
      GLsizei stride,
      const GLvoid *pointer)
      is
      begin
        -- unknown type: "const GLvoid *" name: "pointer"
        Put_Line("glVertexPointer %d %s %d 'const GLvoid * pointer'", size, EnumString(type), stride);
        VertexPointer(size, type, stride, pointer);
      end Vertex;
  --------------
  -- Viewport --
  --------------
    procedure Viewport(
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed)
      is
      begin
        Put_Line("glViewport %d %d %d %d", x, y, width, height);
        Viewport(x, y, width, height);
      end Viewport; 
  end Neo.System.OpenGL;