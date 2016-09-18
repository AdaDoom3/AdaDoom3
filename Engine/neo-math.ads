package Math is
  type Record_Triangle is record
      A, B, C : Integer_8_Unsigned;
    end record;
  type Record_Edge is record
      Vertex, Face : Array_Integer_8_Unsigned(1..2);
    end record;
  type Record_Vector_2D is record
      X, Y : Float_8_Real;
    end record;
  type Record_Vector_3D is record
      X, Y, Z : Float_8_Real;
    end record;
  type Record_Vector_4D is record
      X, Y, Z, W : Float_8_Real;
    end record;
  function "-" (Right : Record_Vector_2D)       return Record_Vector_2D is return (-Right.X, -Right.Y);
  function "-" (Right : Record_Vector_3D)       return Record_Vector_3D is return (-Right.X, -Right.Y, -Right.Z);
  function "-" (Right : Record_Vector_4D)       return Record_Vector_4D is return (-Right.X, -Right.Y, -Right.Z, -Right.W);
  function "-" (Left, Right : Record_Vector_2D) return Record_Vector_2D is return (Left.X - Right.X, Left.X - Right.Y);
  function "-" (Left, Right : Record_Vector_3D) return Record_Vector_3D is return (Left.X - Right.X, Left.X - Right.Y, Left.Z - Right.Z);
  function "-" (Left, Right : Record_Vector_4D) return Record_Vector_4D is return (Left.X - Right.X, Left.X - Right.Y, Left.Z - Right.Z, Left.W - Right.W);
  function "+" (Left, Right : Record_Vector_2D) return Record_Vector_2D is return (Left.X + Right.X, Left.X + Right.Y);
  function "+" (Left, Right : Record_Vector_3D) return Record_Vector_3D is return (Left.X + Right.X, Left.Y + Right.Y, Left.Z + Right.Z);
  function "+" (Left, Right : Record_Vector_4D) return Record_Vector_4D is return (Left.X + Right.X, Left.Y + Right.Y, Left.Z + Right.Z, Left.W + Right.W);


end;