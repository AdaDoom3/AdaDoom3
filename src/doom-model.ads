with Neo.System.Processor.Geometry; use Neo.System.Processor.Geometry;
package Neo.File.Model is
  type Enumerated_Format is(
    --Wavefront_Format,
    -- 1988 Wavefront Technologies
    --
    --Studio_Max_Format,
    -- 1990 Autodesk
    --
    Id_Technology_Format--,
    -- 1991 Id Software
    -- http://web.archive.org/web/20121018035741/http://www.modwiki.net/wiki/Modelling
    --Light_Wave_3D_Format,
    -- 1994 NewTek
    -- Documentation link here
    -- Blender_Format,
    -- 1995 Neo Geo, Not a Number Technologies
    --
    --Valve_Format,
    -- 1996 Valve Software
    -- http://web.archive.org/web/20120308145129/http://www.bagthorpe.org/bob/cofrdrbob/bspformat.html
    );--Maya_Format);
    -- 1997 Alias Technologies
    --
  package Vector_String_2_Unbounded is new Vectors(String_2_Unbounded);
  subtype Array_Bounds is Array_Record_Coordinate_3D(1..2);
  type Record_Animation_Joint is record
      Name        : String_2_Unbounded   := NULL_STRING_2_UNBOUNDED;
      Coordinate  : Record_Coordinate_3D := (others => <>);
      Orientation : Vector_Record_Quaternion.Unprotected.Vector;
    end record;
  package Tree_Record_Animation_Joint is new Trees(Record_Animation_Joint);
  type Record_Animation_Frame is record
      Skeleton : Tree_Record_Animation_Joint.Unprotected.Tree;
      Bounds   : Array_Bounds := (others => (others => <>));
    end record;
  package Vector_Tree_Record_Animation_Join_Unprotected is new Vectors(Tree_Record_Animation_Joint.Unprotected.Tree);
  type Record_Animation is record
      Frame_Rate : Integer_4_Positive := 1;
      Frames     : Vector_Tree_Record_Animation_Join_Unprotected.Unprotected.Vector;
    end record;
  package Hashed_Map_Record_Animation is new Hashed_Maps(Record_Animation);
  type Record_Mesh_Weight is record
      Joint     : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Position  : Integer_4_Natural  := 0; 
      Amount    : Float_4_Percent    := 0.0;
    end record;
  package Vector_Record_Mesh_Weight is new Vectors(Record_Mesh_Weight);
  type Record_Mesh_Vertex(Is_Animated : Boolean := False) is record
      Texture : Record_Coordinate_2D := (others => <>);
      --case Is_Animated is
        --when True  =>
        Weights : Vector_Record_Mesh_Weight.Unprotected.Vector;
        --when False => Position : 
      --end case;
    end record;
  package Vector_Record_Mesh_Vertex is new Vectors(Record_Mesh_Vertex);
  package Ordered_Map_Record_Mesh_Vertex is new Ordered_Maps(Integer_4_Natural, Vector_Record_Mesh_Vertex.Unprotected.Vector);
  package Vector_Ordered_Map_Record_Mesh_Vertex_Unprotected is new Vectors(Ordered_Map_Record_Mesh_Vertex.Unprotected.Map);
  type Record_Mesh_Surface is record
      Material : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Shapes   : Vector_Ordered_Map_Record_Mesh_Vertex_Unprotected.Unprotected.Vector;
    end record;
  package Vector_Record_Mesh_Surface is new Vectors(Record_Mesh_Surface);
  type Record_Mesh(Is_Animated : Boolean := False) is record
      Bounds   : Array_Bounds := (others => (others => <>));
      Surfaces : Vector_Record_Mesh_Surface.Unprotected.Vector;
      case Is_Animated is
        when True =>
          Skeleton   : Tree_Record_Animation_Joint.Unprotected.Tree;
          Animations : Vector_String_2_Unbounded.Unprotected.Vector;
        when False => null;
      end case;
    end record;
  package Hashed_Map_Record_Mesh is new Hashed_Maps(Record_Mesh);
  -- type Record_Level_Edge is record
  --     Start : Integer_4_Natural := 0;
  --     End   : Integer_4_Natural := 0;
  --   end record;
  -- package Vector_Record_Level_Edge is new Vectors(Record_Level_Edge);
  -- type Record_Side is record
  --     Clip : Enumerated_Level_Clip;
  --     Plane : Record_Plane;
  --     No_Clip,
  --     Player_Clip,
  --     Opaque_Clip,
  --     Water_Clip,
  --     Solid_Clip,
  --     Monster_Clip,
  --     Moveable_Clip,
  --     Bot_Clip,
  --     Blood_Clip,
  --     Trigger_Clip,
  --     Body_Clip,
  --     Flashlight_Clip,
  --     Corpse_Clip,
  --     Animation_Clip,
  --     Obstacle_Clip);
  --   end record;
  -- package Vector_Record_Side
--  type Record_Level_Brush is record
--      Bounds : Record_Bounds := (others => <>);
--      Sides  : Vetor_Record_Side.Unprotected.Vector;
--    end record;
  -- package Vector_Record_Level_Brush is Vectors(Record_Level_Brush);
  -- package Vector_Vector_Record_Level_Brush_Unprotected is Vectors(Vector_Record_Level_Brush.Unprotected.Vector);
--  type Record_Level_Surface is record
--      Material : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
--      Bounds   : Record_Bounds      := (others => <>);
--      Plane    : Record_Plane       := (others => <>);
--      Edges    : Vector_Integer_4_Natural.Unprotected.Vector;
--    end record;
  -- package Vector_Record_Level_Surface is new Vectors(Record_Level_Surface);
  -- package Vector_Vector_Record_Level_Surface_Unprotected is new Vectors(Vector_Record_Level_Surface.Unprotected.Vector);
  -- type Record_Level_Patch is record
  --     Edge_Verticies : Record_Level_Edge   := (others => <>);
  --     Edge_Triangles : Record_Level_Edge   := (others => <>);
  --     Surface        : Record_Mesh_Surface := (others => <>);
  --     Subdivisions_X : Integer_4_Natural   := 0;
  --     Subdivisions_Y : Integer_4_Natural   := 0;
  --     Width          : Integer_4_Natural   := 0;
  --     Height         : Integer_4_Natural   := 0;
  --     Maximum_Width  : Integer_4_Natural   := 0;
  --     Maximum_Height : Integer_4_Natural   := 0;
  --   end record;
  -- package Vector_Record_Level_Patch is new Vectors(Record_Level_Patch);
  -- type Record_Level_Brush is record
  --     Material : String_2_Unbounded           := NULL_STRING_2_UNBOUNDED;
  --     Texture  : Array_2_Record_Coordinate_3D := (others => (others => <>));
  --     Origin   : Record_Coordinate_3D         := (others => <>);
  --     Plane    : Record_Plane                 := (others => <>);
  --   end record;
  -- package Vector_Record_Level_Brush is new Vectors(Record_Level_Brush);
  -- package Vector_Vector_Record_Level_Brush_Unprotected is new Vectors(Vector_Record_Level_Brush.Unprotected.Vector);
  -- type Enumerated_Level_Partition is (X_Partition, Y_Partition, Z_Partition);
  -- type Record_Level_Collision_Section is record
  --     Partition : Enumerated_Level_Partition := Enumerated_Level_Partition'first;
  --     Distance  : Integer_4_Natural          := 0;
  --   end record;
  -- package Tree_Record_Level_Collision_Section is new Trees(Record_Level_Collision_Section);
  -- package Tree_Record_Plane is new Trees(Record_Plane);
  -- type Record_Level is record
  --     Geometry_CRC       : Integer_4_Unsigned := 0;
  --     Edge_Verticies     : Vector_Record_Level_Edge.Unprotected.Vector;
  --     Patches            : Vector_Record_Level_Patch.Unprotected.Vector;
  --     Vertices           : Vector_Record_Coordinate_3D.Unprotected.Vector;
  --     Brushes            : Vector_Vector_Record_Level_Brush_Unprotected.Unprotected.Vector;
  --     Entities           : Vector_Hashed_Map_String_2_Unbounded_Unprotected.Unprotected.Vector;
  --     Brush_Blocks       : Vector_Vector_Record_Level_Brush_Unprotected.Unprotected.Vector;
  --     Surface_Blocks     : Vector_Vector_Record_Level_Surface_Unprotected.Unprotected.Vector;
  --     Surface_Sections   : Tree_Record_Plane.Unprotected.Vector;
  --     Collision_Sections : Tree_Record_Level_Collision_Section.Unprotected.Vector;
  --   end record;
  -- package Hashed_Map_Record_Level is new Hashed_Maps(Record_Level);
  -- type Record_Camera_Frame is record
  --     Coordinate    : Record_Coordinate_3D     := (others => <>);
  --     Orientation   : Vector_Record_Quaternion := (others => <>);
  --     Field_Of_View : Float_4_Real             := 0.0;
  --   end record;
  -- package Vector_Record_Camera_Frame is new Vectors(Record_Camera_Frame);
  -- type Record_Camera is record
  --     Frame_Rate : Integer_4_Positive := 1;
  --     Frames     : Vector_Record_Camera_Frame.Unprotected.Vector;
  --     Cuts       : Vector_Integer_4_Positive.Unprotected.Vector;
  --   end record;
  -- package Hashed_Map_Record_Camera is new Hashed_Maps(Record_Camera);
  --function Load  (Name : in String_2) return Record_Animation;
  --function Load  (Name : in String_2) return Record_Camera;
  function Load  (Name : in String_2) return Record_Mesh;
  --function Load  (Name : in String_2) Item Record_Map;
  --procedure Save (Name : in String_2; Item : in Record_Animation);
  --procedure Save (Name : in String_2; Item : in Record_Camera);
  procedure Save (Name : in String_2; Item : in Record_Mesh);
  --procedure Save (Name : in String_2; Map  : in Record_Map);
private
  --package Wavefront is
  --    function Load  (Name : in String_2) return Record_Mesh;
  --    procedure Save (Name : in String_2; Mesh : in Record_Mesh);
  --  end Wavefront;
  --package Studio_Max is
  --    function Load  (Name : in String_2) return Record_Animation;
  --    function Load  (Name : in String_2) return Record_Camera;
  --    function Load  (Name : in String_2) return Record_Mesh;
  --    procedure Save (Name : in String_2; Animation : in Record_Animation);
  --    procedure Save (Name : in String_2; Camera    : in Record_Camera);
  --    procedure Save (Name : in String_2; Mesh      : in Record_Mesh);
  --  end Studio_Max;
  package Id_Tech is
  --    function Load  (Name : in String_2) return Record_Animation;
  --    function Load  (Name : in String_2) return Record_Camera;
      function Load  (Name : in String_2) return Record_Mesh;
  --    function Load  (Name : in String_2) return Record_Map; 
  --    procedure Save (Name : in String_2; Item : in Record_Animation);
  --    procedure Save (Name : in String_2; Item : in Record_Camera);
      procedure Save (Name : in String_2; Item : in Record_Mesh);
  --    procedure Save (Name : in String_2; Item : in Record_Map);
    end Id_Tech;
  --package Light_Wave is
  --    function Load  (Name : in String_2) return Record_Mesh;
  --    procedure Save (Name : in String_2; Mesh : in Record_Mesh);
  --  end Light_Wave;
  --package Blender is
  --    function Load  (Name : in String_2) return Record_Animation;
  --    function Load  (Name : in String_2) return Record_Camera;
  --    function Load  (Name : in String_2) return Record_Mesh;
  --    procedure Save (Name : in String_2; Animation : in Record_Animation);
  --    procedure Save (Name : in String_2; Camera    : in Record_Camera);
  --    procedure Save (Name : in String_2; Mesh      : in Record_Mesh);
  --  end Blender;
  --package Valve is
  --    function Load  (Name : in String_2) return Record_Animation;
  --    function Load  (Name : in String_2) return Record_Camera;
  --    function Load  (Name : in String_2) return Record_Mesh;
  --    function Load  (Name : in String_2) return Record_Map; 
  --    procedure Save (Name : in String_2; Animation : in Record_Animation);
  --    procedure Save (Name : in String_2; Camera    : in Record_Camera);
  --    procedure Save (Name : in String_2; Mesh      : in Record_Mesh);
  --    procedure Save (Name : in String_2; Map       : in Record_Map);
  --  end Valve;
  --package Maya is
  --    function Load  (Name : in String_2) return Record_Animation;
  --    function Load  (Name : in String_2) return Record_Camera;
  --    function Load  (Name : in String_2) return Record_Mesh;
  --    procedure Save (Name : in String_2; Animation : in Record_Animation);
  --    procedure Save (Name : in String_2; Camera    : in Record_Camera);
  --    procedure Save (Name : in String_2; Mesh      : in Record_Mesh);
  --  end Maya;
  --package Map       is new Handler(Enumerated_Format, Record_Map);
  package Mesh      is new Handler(Enumerated_Format, Record_Mesh);
  --package Camera    is new Handler(Enumerated_Format, Record_Camera);
  --package Animation is new Handler(Enumerated_Format, Record_Animation);
  --package Wavefront_Mesh       is new Mesh.Format      (Wavefront_Format,     Wavefront.Save'access,  Wavefront.Load'access,  "obj");
  --package Studio_Max_Mesh      is new Mesh.Format      (Studio_Max_Format,    Studio_Max.Save'access, Studio_Max.Load'access, "3dmax");
  --package Studio_Max_Camera    is new Camera.Format    (Studio_Max_Format,    Studio_Max.Save'access, Studio_Max.Load'access, "3dmax");
  --package Studio_Max_Animation is new Animation.Format (Studio_Max_Format,    Studio_Max.Save'access, Studio_Max.Load'access, "3dmax");
  --package Id_Tech_Map          is new Map.Format       (Id_Technology_Format, Id_Tech.Save'access,    Id_Tech.Load'access,    "md5map");
  package Id_Tech_Mesh         is new Mesh.Format      (Id_Technology_Format, Id_Tech.Save'access,    Id_Tech.Load'access,    "md5mesh");
  --package Id_Tech_Camera       is new Camera.Format    (Id_Technology_Format, Id_Tech.Save'access,    Id_Tech.Load'access,    "md5camera");
  --package Id_Tech_Animation    is new Animation.Format (Id_Technology_Format, Id_Tech.Save'access,    Id_Tech.Load'access,    "md5anim");
  --package Light_Wave_Mesh      is new Mesh.Format      (Light_Wave_3D_Format, Light_Wave.Save'access, Light_Wave.Load'access, "lwo");
  --package Blender_Mesh         is new Mesh.Format      (Blender_Format,       Blender.Save'access,    Blender.Load'access,    "blend");
  --package Blender_Camera       is new Camera.Format    (Blender_Format,       Blender.Save'access,    Blender.Load'access,    "blend");
  --package Blender_Animation    is new Animation.Format (Blender_Format,       Blender.Save'access,    Blender.Load'access,    "blend");
  --package Valve_Map            is new Map.Format       (Valve_Format,         Valve.Save'access,      Valve.Load'access,      "map");
  --package Valve_Mesh           is new Mesh.Format      (Valve_Format,         Valve.Save'access,      Valve.Load'access,      "mdl");
  --package Valve_Camera         is new Camera.Format    (Valve_Format,         Valve.Save'access,      Valve.Load'access,      "mdl");
  --package Valve_Animation      is new Animation.Format (Valve_Format,         Valve.Save'access,      Valve.Load'access,      "mdl");
  --package Maya_Mesh            is new Mesh.Format      (Maya_Format,          Maya.Save'access,       Maya.Load'access,       "mb");
  --package Maya_Camera          is new Camera.Format    (Maya_Format,          Maya.Save'access,       Maya.Load'access,       "mb");
  --package Maya_Animation       is new Animation.Format (Maya_Format,          Maya.Save'access,       Maya.Load'access,       "mb");
end Neo.File.Model;
