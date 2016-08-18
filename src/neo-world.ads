package World is
  type Record_Object;
  type Record_Object (Kind : Enumerated_Object) is record
    case Kind is
      when Bone_Object =>
      when Camera_Object =>
      when Light_Object =>
      when Geometry_Object =>
      when Morph_Object =>
      when Material_Object =>
      when Prop_Object =>
      when Animation_Object =>