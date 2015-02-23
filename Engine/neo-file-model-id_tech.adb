package Id_Tech is
  type Record_Internal_Joint is record
      Name :
      Parent :
      Position :
      Rotation :
    end record;
  package Vector_Record_Internal_Joint is new Vectors(Record_Internal_Joint);
  function Build_Skeleton(Joints : in out Vector_Record_Internal_Joint.Unsafe) return Tree_Record_Animation_Joint is
    Skeleton : Tree_Record_Animation_Joint;
    begin
      for Joint of Joints loop
        if Joint.Parent = 0 then Skeleton.Append_Child(Skeleton.Root, Joint.Data, Joint.Index);
        else Skeleton.Append_Child(Joints.Element(Joint.Parent).Index, Joint.Data, Joint.Index); end if;
      end loop;
      return Skeleton;
    end Build_Skeleton;
  function return Record_Animation is
    type Record_Internal_Frame is record
        Start_Index    : Integer_4_Positive := 
        Joint          : Integer_4_Signed := 1;
        Has_Position_X : Boolean          := False;
        Has_Position_Y : Boolean          := False;
        Has_Position_Z : Boolean          := False;
        Has_Rotation_X : Boolean          := False;
        Has_Rotation_Y : Boolean          := False;
        Has_Rotation_Z : Boolean          := False;
      end record;
    package Vector_Internal_Frame is new Vectors(Record_Internal_Frame);
    Frames      :
    Joints      :
    Joints_Base : 
    Animation   :
    begin
      Parser.Assert("MD5Version");            Parser.Skip;
      Parser.Assert("commandline");           Parser.Skip_Set("""", """");
      Parser.Assert("numFrames");             Parser.Skip;
      Parser.Assert("numJoints");             Parser.Skip;
      Parser.Assert("frameRate");             Animation.Frame_Rate := Parser.Next;
      Parser.Assert("numAnimatedComponents"); Parser.Skip;
      Parser.Assert("hierarchy");             Parser.Assert("{");
      while Parser.Peek /= "}" loop
        Joints.Append(
          Name   => Parser.Next_Set("""", """"),
          Parent => Parser.Next,
          others => <>);
        I     := I + 1;
        Flags := Parser.Next;
        if Flags > 0 then
          Frames.Append(
            Start          => Parser.Next,
            Joint          => I,
            Has_Position_X => (Flags and 16#0000_#) > 0,
            Has_Position_Y => (Flags and 16#0000_#) > 0,
            Has_Position_Z => (Flags and 16#0000_#) > 0,
            Has_Rotation_X => (Flags and 16#0000_#) > 0,
            Has_Rotation_Y => (Flags and 16#0000_#) > 0,
            Has_Rotation_Z => (Flags and 16#0000_#) > 0);
        else Parser.Skip; end if;
      end loop; Parser.Skip;
      Parser.Assert("bounds", "{");
      while Parser.Peek /= "}" loop
        
      end loop; Parser.Skip;
      Parser.Assert("baseframe", "{");
      while Parser.Peek /= "}" loop
        
      end loop; Parser.Skip;
      while not Parser.At_End loop
        Parser.Assert("frame");
        Joints := Joints_Base;
        while Parser.Peek /= "}" loop Frame_Data.Append(Parser.Next); end loop; Parser.Skip;
        for Frame of Frames loop
          Joint := Joints.Element
          if Frame.Has_Position_X then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
          if Frame.Has_Position_Y then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
          if Frame.Has_Position_Z then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
          if Frame.Has_Rotation_X then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
          if Frame.Has_Rotation_Z then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
          if Frame.Has_Rotation_Y then Joint.Position(I) := Frame_Data.Element(Frame.Index); Frame.Index := Frame.Index + 1; end if;
          Joints.Replace_Element(Frame.Joint, Joint);
        end loop;
        Animation.Frames.Append(Build_Skeleton(Joints));
      end loop;
      return Animation;
    end ;
  function return Record_Mesh is
    package Vector_Array_3_By_1_Integer_4_Natural is new Vectors(Array_)
    type Record_Internal_Vertex is record 
        Texture :
        Weight_Start :
        Weight_Count :
      end record;
    package Vector_Record_Internal_Vertex is new Vectors(Record_Internal_Vertex);
    type Record_Internal_Weight is record
        Joint    :
        Amount   :
        Position :  
      end record;
    package Vector_Record_Internal_Weight is new Vectors(Record_Internal_Weight);
    type Record_Internal_Surface is record
        Shader    : 
        Vertices  : Vector_Record_Internal_Vertex.Unsafe;
        Triangles : 
        Weights   : Vector_Record_Internal_Weight.Unsafe;
      end record;
    package Vector_Record_Internal_Surface is new (Record_Internal_Surface);
    Surfaces : Vector_Record_Internal_Surface.Unsafe;
    Surface  : Record_Internal_Surface := (others => <>);
    Joints   : Vector_Record_Internal_Joint.Unsafe;
    Weights  : Vector
    Shapes   :
    Shape    :
    Mesh     : Record_Mesh(Is_Animated => True) := (others => <>);
    begin
      Parser.Assert_And_Skip("MD5Version");
      Parser.Assert_And_Skip_Set("commandline", ("""", """"));
      Parser.Assert_And_Skip("numJoints");
      Parser.Assert_And_Skip("numMeshes");
      Parser.Assert("joints", "{");
      while Parser.Peek /= "}" loop
        Joints.Append(
          Name     => Parser.Next_Set("""", """"),
          Parent   => Parser.Next,
          Position => Parser.Next_Set("(", ")"),
          Rotation => To_Rotation(To_Quaternion(Parser.Next_Set("(", ")"))));
      end loop;
      Parser.Skip;
      while not Parser.At_End loop
        Parser.Assert("mesh", "{");
        Parser.Assert("shader");
        Surface.Shader := Parser.Next;
        Parser.Assert_And_Skip("numverts");
        while Parser.Peek = "vert" loop Parser.Skip;
          Surface.Vertices.Replace_Element(
            Index    => Parser.Next,
            New_Item =>(
              Texture      => Parser.Next_Set("(", ")"),
              Weight_Count => Parser.Next,
              Weight_Start => Parser.Next));
        end loop; 
        Parser.Assert_And_Skip("numtris"); 
        while Parser.Peek = "tris" loop Parser.Skip(2);
          Surface.Triangles.Append(Parser.Next);
        end loop; 
        Parser.Assert_And_Skip("numweights");
        while Parser.Peek = "weight" loop Parser.Skip(2);
          Surface.Weights.Append((
            Joint    => Parser.Next,
            Amount   => Parser.Next,
            Position => Parser.Next_Set("(", ")")))+;
        end loop;
        Parser.Assert("}");
        Surfaces.Append(Surface); Surface.Vertices.Clear; Surface.Triangles.Clear; Surface.Weights.Clear;
      end loop;
      for Surface of Surfaces loop
        for Triangle of Surface.Triangles loop
          for I in Triangle loop
            for J in Surface.Vertices.Element(I).Weight_Start..Surface.Vertices.Element(I).Weight_Start + Surface.Vertices.Element(I).Weight_Count loop
              Weights.Append((Surface.Weights.Element(J).Amount, Surface.Weights.Element(J).Position, Joints.Element(Surface.Weights.Element(J).Joint).Name));
            end loop;
            Shape.Insert(I, (True, Surface.Vertices.Element(I).Texture, Weights)); Weights.Clear;
          end loop;
          Shapes.Append(Shape); Shape.Clear;
        end loop;
        Mesh.Surfaces.Append((Surface.Material, Shapes)); Shapes.Clear;
      end loop;
      Mesh.Skeleton := Build_Skeleton(Joints);
      Mesh.Bounds   := Build_Bounds(Weigh_Surfaces(Mesh.Skeleton, Mesh.Surfaces));
      return Mesh;
    end ;
end Id_Tech;
  
