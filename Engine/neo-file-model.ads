with Neo.System.Processor.Geometry; use Neo.System.Processor.Geometry;
package Neo.File.Model is
  Multiple_Records_In_File : Exception;
  type Enumerated_Format is(
    Wavefront_Format,
    -- 1988 Wavefront Technologies
    --
    Studio_Max_Format,
    -- 1990 Autodesk
    --
    Id_Technology_Format,
    -- 1991 Id Software
    -- http://web.archive.org/web/20121018035741/http://www.modwiki.net/wiki/Modelling
    Light_Wave_3D_Format,
    -- 1994 NewTek
    --
    Blender_Format,
    -- 1995 Neo Geo, Not a Number Technologies
    --
    Half_Life_Format,
    -- 1996 Valve Software
    -- http://web.archive.org/web/20120308145129/http://www.bagthorpe.org/bob/cofrdrbob/bspformat.html
    Maya_Format);
    -- 1997 Alias Technologies
    --
  type Record_Animation_Joint is record
      Name        : String_2_Unbounded       := NULL_STRING_2_UNBOUNDED;
      Coordinate  : Record_Coordinate_3D     := (others => <>);
      Orientation : Vector_Record_Quaternion := (others => <>);
    end record;
  package Tree_Record_Animation_Joint is new Trees(Record_Animation_Joint);
  type Record_Animation_Frame is record
      Skeleton : Tree_Record_Animation_Joint.Unsafe;
      Bounds   : Record_Bounds
    end record;
  package Vector_Tree_Record_Animation_Joint_Unsafe is new Vectors(Tree_Record_Animation_Joint.Unsafe);
  type Record_Animation is record
      Frame_Rate : Integer_4_Positive := 1;
      Frames     : Vector_Tree_Record_Animation_Joint_Unsafe.Unsafe;
    end record;
  package Hashed_Map_Record_Animation is new Hashed_Maps(Record_Animation);
  type Record_Mesh_Weight is record
      Joint     : String_2_Unbounded := NULL_SRING_2_UNBOUNDED;
      Position  : Integer_4_Natural  := 0; 
      Amount    : Float_4_Percent    := 0;
    end record;
  package Vector_Record_Mesh_Weight is new Vectors(Record_Mesh_Weight);
  type Record_Mesh_Vertex(Is_Animated : Boolean := False) is record
      Texture : Record_Coordinate_2D := (others => <>);
      case Is_Animated is
        when True  => Weights : Vector_Record_Mesh_Weight.Unsafe;
        when False => Position : 
      end case;
    end record;
  package Ordered_Map_Record_Mesh_Vertex is new Ordered_Map(Integer_4_Natural, Record_Mesh_Vertex);
  package Vector_Ordered_Map_Record_Mesh_Vertex_Unsafe is new Vectors(Ordered_Map_Record_Mesh_Vertex.Unsafe);
  type Record_Mesh_Surface is record
      Material : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Shapes   : Vector_Ordered_Map_Record_Mesh_Vertex_Unsafe.Unsafe;
    end record;
  package Vector_Record_Mesh_Surface is new Vectors(Record_Surface);
  type Record_Mesh(Is_Animated : Boolean := False) is record
      Bounds   : Record_Bounds := (others => <>);
      Surfaces : Vector_Record_Mesh_Surface.Unsafe;
      case Is_Animated is
        when True =>
          Skeleton   : Tree_Record_Animation_Joint.Unsafe;
          Animations : Vector_String_2_Unbounded;
        when False => null;
      end case;
    end record;
  package Hashed_Map_Record_Mesh is new Hashed_Maps(Record_Mesh);
  type Record_Level_Edge is record
      Start : Integer_4_Natural := 0;
      End   : Integer_4_Natural := 0;
    end record;
  package Vector_Record_Level_Edge is new Vectors(Record_Level_Edge);
  type Record_Side is record
      Clip : Enumerated_Level_Clip;
      Plane : Record_Plane;
      No_Clip,
      Player_Clip,
      Opaque_Clip,
      Water_Clip,
      Solid_Clip,
      Monster_Clip,
      Moveable_Clip,
      Bot_Clip,
      Blood_Clip,
      Trigger_Clip,
      Body_Clip,
      Flashlight_Clip,
      Corpse_Clip,
      Animation_Clip,
      Obstacle_Clip);\
    end record;
  package Vector_Record_Side
  type Record_Level_Brush is record
      Bounds : Record_Bounds := (others => <>);
      Sides  : Vetor_Record_Side.Unsafe;
    end record;
  package Vector_Record_Level_Brush is Vectors(Record_Level_Brush);
  package Vector_Vector_Record_Level_Brush_Unsafe is Vectors(Vector_Record_Level_Brush.Unsafe);
  type Record_Level_Surface is record
      Material : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Bounds   : Record_Bounds      := (others => <>);
      Plane    : Record_Plane       := (others => <>);
      Edges    : Vector_Integer_4_Natural.Unsafe;
    end record;
  package Vector_Record_Level_Surface is new Vectors(Record_Level_Surface);
  package Vector_Vector_Record_Level_Surface_Unsafe is new Vectors(Vector_Record_Level_Surface.Unsafe);
  type Record_Level_Patch is record
      Edge_Verticies : Record_Level_Edge   := (others => <>);
      Edge_Triangles : Record_Level_Edge   := (others => <>);
      Surface        : Record_Mesh_Surface := (others => <>);
      Subdivisions_X : Integer_4_Natural   := 0;
      Subdivisions_Y : Integer_4_Natural   := 0;
      Width          : Integer_4_Natural   := 0;
      Height         : Integer_4_Natural   := 0;
      Maximum_Width  : Integer_4_Natural   := 0;
      Maximum_Height : Integer_4_Natural   := 0;
    end record;
  package Vector_Record_Level_Patch is new Vectors(Record_Level_Patch);
  type Record_Level_Brush is record
      Material : String_2_Unbounded           := NULL_STRING_2_UNBOUNDED;
      Texture  : Array_2_Record_Coordinate_3D := (others => (others => <>));
      Origin   : Record_Coordinate_3D         := (others => <>);
      Plane    : Record_Plane                 := (others => <>);
    end record;
  package Vector_Record_Level_Brush is new Vectors(Record_Level_Brush);
  package Vector_Vector_Record_Level_Brush_Unsafe is new Vectors(Vector_Record_Level_Brush.Unsafe);
  type Enumerated_Level_Partition is (X_Partition, Y_Partition, Z_Partition);
  type Record_Level_Collision_Section is record
      Partition : Enumerated_Level_Partition := Enumerated_Level_Partition'first;
      Distance  : Integer_4_Natural          := 0;
    end record;
  package Tree_Record_Level_Collision_Section is new Trees(Record_Level_Collision_Section);
  package Tree_Record_Plane is new Trees(Record_Plane);
  type Record_Level is record
      Geometry_CRC       : Integer_4_Unsigned := 0;
      Edge_Verticies     : Vector_Record_Level_Edge.Unsafe;
      Patches            : Vector_Record_Level_Patch.Unsafe;
      Vertices           : Vector_Record_Coordinate_3D.Unsafe;
      Brushes            : Vector_Vector_Record_Level_Brush_Unsafe.Unsafe;
      Entities           : Vector_Hashed_Map_String_2_Unbounded_Unsafe.Unsafe;
      Brush_Blocks       : Vector_Vector_Record_Level_Brush_Unsafe.Unsafe;
      Surface_Blocks     : Vector_Vector_Record_Level_Surface_Unsafe.Unsafe;
      Surface_Sections   : Tree_Record_Plane.Unsafe;
      Collision_Sections : Tree_Record_Level_Collision_Section.Unsafe;
    end record;
  package Hashed_Map_Record_Level is new Hashed_Maps(Record_Level);
  type Record_Camera_Frame is record
      Coordinate    : Record_Coordinate_3D     := (others => <>);
      Orientation   : Vector_Record_Quaternion := (others => <>);
      Field_Of_View : Float_4_Real             := 0.0;
    end record;
  package Vector_Record_Camera_Frame is new Vectors(Record_Camera_Frame);
  type Record_Camera is record
      Frame_Rate : Integer_4_Positive := 1;
      Frames     : Vector_Record_Camera_Frame.Unsafe;
      Cuts       : Vector_Integer_4_Positive.Unsafe;
    end record;
  package Hashed_Map_Record_Camera is new Hashed_Maps(Record_Camera);
  function Load(Path : in String_2) return Record_Animation;
  function Load(Path : in String_2) return Record_Camera;
  function Load(Path : in String_2) return Record_Mesh;
  function Load(Path : in String_2) return Record_Level;
  procedure Save(Path : in String_2; Record_Animation);
  procedure Save(Path : in String_2l Record_Camera);
  procedure Save(Path : in String_2; Record_Mesh);
  procedure Save(Path : in String_2; Record_Level);
private
end Neo.File.Model;

















