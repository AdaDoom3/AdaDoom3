
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

package body Neo.File.Model is
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
  --package body Wavefront  is separate;
  --package body Studio_Max is separate;
  package body Id_Tech    is separate;
  --package body Light_Wave is separate;
  --package body Blender    is separate;
  --package body Valve      is separate;
  --package body Maya       is separate;
  --function Load  (Name : in String_2) return Record_Animation     renames Animataion.Load;
  --function Load  (Name : in String_2) return Record_Camera        renames Camera.Load;
  function Load  (Name : in String_2) return Record_Mesh          renames Mesh.Load;
  --function Load  (Name : in String_2) return Record_Map           renames Map.Load;
  --procedure Save (Name : in String_2; Item : in Record_Animation) renames Animation.Save;
  --procedure Save (Name : in String_2; Item : in Record_Camera)    renames Camera.Save;
  procedure Save (Name : in String_2; Item : in Record_Mesh)      renames Mesh.Save;
  --procedure Save (Name : in String_2; Item : in Record_Map)       renames Map.Save;
end;
