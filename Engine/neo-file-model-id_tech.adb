separate(Neo.File.Model) package body Id_Tech is
  type Record_Internal_Joint is record
      Name     : String_2_Unbounded;
      Parent   : Integer_4_Signed;
      Position : Record_Coordinate_3D := (others => <>);
      Rotation : Record_Quaternion := (others => <>);
    end record;
  package Vector_Record_Internal_Joint is new Vectors(Record_Internal_Joint);
  use Vector_Record_Internal_Joint.Unprotected;
  use Tree_Record_Animation_Joint.Unprotected;
  function Build_Skeleton(Joints : in out Vector_Record_Internal_Joint.Unprotected.Vector) return Tree_Record_Animation_Joint.Unprotected.Tree is
    Skeleton : Tree_Record_Animation_Joint.Unprotected.Tree;
    begin
      for Joint of Joints loop
        null;--if Joint.Parent = 0 then Append_Child(Skeleton, Skeleton.Root, Joint.Data, Joint.Index);
        --else Skeleton.Append_Child(Joints.Element(Joint.Parent).Index, Joint.Data, Joint.Index); end if;
      end loop;
      return Skeleton;
    end Build_Skeleton;
  function Load(Name : in String_2) return Record_Mesh is
    type Record_Internal_Vertex is record 
        Texture      : Record_Coordinate_3D := (others => <>);
        Index        : Integer_4_Natural  := 0;
        Weight_Start : Integer_4_Natural  := 0;
        Weight_Count : Integer_4_Natural  := 0;
      end record;
    package Vector_Record_Internal_Vertex is new Vectors(Record_Internal_Vertex);
    type Record_Internal_Weight is record
        Joint    : Integer_4_Natural := 0;
        Amount   : Integer_4_Natural := 0;
        Position : Record_Coordinate_3D := (others => <>);
      end record;
    package Vector_Record_Internal_Weight is new Vectors(Record_Internal_Weight);
    type Record_Internal_Surface is record
        Shader    : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
        Weights   : Vector_Record_Internal_Weight.Unprotected.Vector;
        Vertices  : Vector_Record_Internal_Vertex.Unprotected.Vector;
        Triangles : Vector_Record_Coordinate_3D.Unprotected.Vector;
      end record;
    package Vector_Record_Internal_Surface is new Vectors(Record_Internal_Surface);
    Surfaces : Vector_Record_Internal_Surface.Unprotected.Vector;
    Joints   : Vector_Record_Internal_Joint.Unprotected.Vector;
    Weights  : Vector_Record_Internal_Weight.Unprotected.Vector;
    Sufaces  : Vector_Record_Mesh_Surface.Unprotected.Vector;
    Vertex   : Record_Internal_Vertex   := (others => <>);
    Surface  : Record_Internal_Surface  := (others => <>);
    Weight   : Record_Internal_Weight   := (others => <>);
    Joint    : Record_Internal_Joint    := (others => <>);
    Shape    : Record_Mesh_Vertex       := (True, others => <>);
    Mesh     : Record_Mesh              := (True, others => <>);
    package Mesh_Parser is new Parser(Name, "//");
    use Mesh_Parser;
    begin
      Assert("MD5Version");  Skip;
      Assert("commandline"); Skip_Set("""", """");
      Assert("numJoints");   Skip;
      Assert("numMeshes");   Skip;
      Assert("joints");      Assert("{");
      while Peek /= "}" loop
        Joint.Name   := Next_Set("""", """");
        Joint.Parent := Integer_4_Signed(Next_Number);
        Assert("("); Joint.Position := (Next_Number, Next_Number, Next_Number);                Assert(")");
        Assert("("); Joint.Rotation := To_Quaternion((Next_Number, Next_Number, Next_Number)); Assert(")");
        Joints.Append(Joint);
      end loop;
      Skip;
      while not At_End loop
        Assert("mesh");     Assert("{");
        Assert("shader");   Surface.Shader := Next_Set("""", """");
        Assert("numverts"); Skip;
        while Peek = "vert" loop
          Skip;
          Vertex.Index        := Integer_4_Natural(Next_Number); delay 5.0;
          Assert("("); Vertex.Texture := (Next_Number, Next_Number, Next_Number); Assert(")");
          Vertex.Weight_Count := Integer_4_Natural(Next_Number);
          Vertex.Weight_Start := Integer_4_Natural(Next_Number);
          Surface.Vertices.Replace_Element(Vertex.Index, Vertex);
        end loop; 
        Assert("numtris"); Skip;
        while Peek = "tris" loop
          Skip(2);
          Surface.Triangles.Append((Next_Number, Next_Number, Next_Number));
        end loop; 
        Assert("numweights"); Skip;
        while Peek = "weight" loop
          Skip(2);
          Weight.Joint  := Integer_4_Natural(Next_Number);
          Weight.Amount := Integer_4_Natural(Next_Number);
          Assert("("); Weight.Position := (Next_Number, Next_Number, Next_Number); Assert(")");
          Surface.Weights.Append(Weight);
        end loop;
        Assert("}");
        Surfaces.Append(Surface);
        Surface.Vertices.Clear;
        Surface.Triangles.Clear;
        Surface.Weights.Clear;
      end loop;
      for Surface of Surfaces loop
        for Triangle of Surface.Triangles loop
 --           for J in Surface.Vertices.Element(I).Weight_Start..Surface.Vertices.Element(I).Weight_Start + Surface.Vertices.Element(I).Weight_Count loop
 --             Weights.Append((Surface.Weights.Element(J).Amount, Surface.Weights.Element(J).Position, Joints.Element(Surface.Weights.Element(J).Joint).Name));
 --           end loop;
 --           Shape.Insert(I, (True, Surface.Vertices.Element(I).Texture, Weights)); 
            Weights.Clear;
 --         Sufaces.Append(Shape);
        end loop;
 --       Mesh.Surfaces.Append((Surface.Material, Sufaces));
        Sufaces.Clear;
      end loop;
      Mesh.Skeleton := Build_Skeleton(Joints);
      --Mesh.Bounds   := Build_Bounds(Weigh_Surfaces(Mesh.Skeleton, Mesh.Surfaces));
      return Mesh;
    end Load;
  procedure Save(Name : in String_2; Item : in Record_Mesh) is
    begin 
      null;
    end Save;
  -- function return Record_Animation is
  --   type Record_Internal_Frame is record
  --       Start_Index    : Integer_4_Positive := 
  --       Joint          : Integer_4_Signed := 1;
  --       Has_Position_X : Boolean          := False;
  --       Has_Position_Y : Boolean          := False;
  --       Has_Position_Z : Boolean          := False;
  --       Has_Rotation_X : Boolean          := False;
  --       Has_Rotation_Y : Boolean          := False;
  --       Has_Rotation_Z : Boolean          := False;
  --     end record;
  --   package Vector_Internal_Frame is new Vectors(Record_Internal_Frame);
  --   Frames      :
  --   Joints      :
  --   Joints_Base : 
  --   Animation   :
  --   begin
  --     Assert("MD5Version");            Skip;
  --     Assert("commandline");           Skip_Set("""", """");
  --     Assert("numFrames");             Skip;
  --     Assert("numJoints");             Skip;
  --     Assert("frameRate");             Animation.Frame_Rate := Next;
  --     Assert("numAnimatedComponents"); Skip;
  --     Assert("hierarchy");             Assert("{");
  --     while Peek /= "}" loop
  --       Joints.Append(
  --         Name   => Next_Set("""", """"),
  --         Parent => Next,
  --         others => <>);
  --       I     := I + 1;
  --       Flags := Next;
  --       if Flags > 0 then
  --         Frames.Append(
  --           Start          => Next,
  --           Joint          => I,
  --           Has_Position_X => (Flags and 16#0000_#) > 0,
  --           Has_Position_Y => (Flags and 16#0000_#) > 0,
  --           Has_Position_Z => (Flags and 16#0000_#) > 0,
  --           Has_Rotation_X => (Flags and 16#0000_#) > 0,
  --           Has_Rotation_Y => (Flags and 16#0000_#) > 0,
  --           Has_Rotation_Z => (Flags and 16#0000_#) > 0);
  --       else Skip; end if;
  --     end loop; Skip;
  --     Assert("bounds", "{");
  --     while Peek /= "}" loop
        
  --     end loop; Skip;
  --     Assert("baseframe", "{");
  --     while Peek /= "}" loop
        
  --     end loop; Skip;
  --     while not At_End loop
  --       Assert("frame");
  --       Joints := Joints_Base;
  --       while Peek /= "}" loop Frame_Data.Append(Next); end loop; Skip;
  --       for Frame of Frames loop
  --         Joint := Joints.Element
  --         if Frame.Has_Position_X then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
  --         if Frame.Has_Position_Y then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
  --         if Frame.Has_Position_Z then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
  --         if Frame.Has_Rotation_X then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
  --         if Frame.Has_Rotation_Z then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
  --         if Frame.Has_Rotation_Y then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
  --         Joints.Replace_Element(Frame.Joint, Joint);
  --       end loop;
  --       Animation.Frames.Append(Build_Skeleton(Joints));
  --     end loop;
  --     return Animation;
  --   end ;
end Id_Tech;
  
