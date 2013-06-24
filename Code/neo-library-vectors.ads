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
with
  Ada.Finalization,
  Neo.Library.Mathmatics,
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Neo.Library.Mathmatics,
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.Library.Vectors
  is
  -------------
  -- Records --
  -------------
    type Record_Vector
      is private;
    type Record_Vector_Polar
      is record
        ρ : Float_4_Positive := 1.0;
        θ : Float_4_Real     := 0.0;
        ϕ : Float_4_Real     := 0.0;
      end record;
    type Record_Vector_2
      is record
        X : Float_4_Real := 0.0;
        Y : Float_4_Real := 0.0;
      end record;
    type Record_Vector_3
      is record
        X : Float_4_Real := 0.0;
        Y : Float_4_Real := 0.0;
        Z : Float_4_Real := 0.0;
      end record;
    type Record_Vector_4
      is record
        X : Float_4_Real := 0.0;
        Y : Float_4_Real := 0.0;
        Z : Float_4_Real := 0.0;
        W : Float_4_Real := 0.0;
      end record;
    type Record_Vector_5
      is record
        X : Float_4_Real := 0.0;
        Y : Float_4_Real := 0.0;
        Z : Float_4_Real := 0.0;
        S : Float_4_Real := 0.0;
        T : Float_4_Real := 0.0;
      end record;
    type Record_Vector_6
      is record
        X : Float_4_Real := 0.0;
        Y : Float_4_Real := 0.0;
        Z : Float_4_Real := 0.0;
        S : Float_4_Real := 0.0;
        T : Float_4_Real := 0.0;
        U : Float_4_Real := 0.0;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Put(
      Item : in Record_Vector_2);
    procedure Put(
      Item : in Record_Vector_3);
    procedure Put(
      Item : in Record_Vector_4);
    procedure Put(
      Item : in Record_Vector_5);
    procedure Put(
      Item : in Record_Vector_6);
    function Get_Length(
      Item : in Record_Vector_2)
      return Float_4_Real;
    function Get_Length(
      Item : in Record_Vector_3)
      return Float_4_Real;
    function Get_Length(
      Item : in Record_Vector_4)
      return Float_4_Real;
    function Get_Length(
      Item : in Record_Vector_6)
      return Float_4_Real;
    function Get_Length_Quickly(
      Item : in Record_Vector_2)
      return Float_4_Real;
    function Get_Length_Quickly(
      Item : in Record_Vector_3)
      return Float_4_Real;
    function Get_Length_Quickly(
      Item : in Record_Vector_4)
      return Float_4_Real;
    function Get_Length_Quickly(
      Item : in Record_Vector_6)
      return Float_4_Real;
    function Get_Length_Squared(
      Item : in Record_Vector_2)
      return Float_4_Real;
    function Get_Length_Squared(
      Item : in Record_Vector_3)
      return Float_4_Real;
    function Get_Length_Squared(
      Item : in Record_Vector_4)
      return Float_4_Real;
    function Get_Length_Squared(
      Item : in Record_Vector_6)
      return Float_4_Real;
    function Normalize(
      Item : in Record_Vector_2)
      return Float_4_Real;
    function Normalize(
      Item : in Record_Vector_3)
      return Float_4_Real;
    function Normalize(
      Item : in Record_Vector_4)
      return Float_4_Real;
    function Normalize(
      Item : in Record_Vector_6)
      return Float_4_Real;
    function Normalize_Quickly(
      Item : in Record_Vector_2)
      return Float_4_Real;
    function Normalize_Quickly(
      Item : in Record_Vector_3)
      return Float_4_Real;
    function Normalize_Quickly(
      Item : in Record_Vector_4)
      return Float_4_Real;
    function Normalize_Quickly(
      Item : in Record_Vector_6)
      return Float_4_Real;
    function Are_Equal(
      Item_A : in Record_Vector_2;
      Item_B : in Record_Vector_2;
      ε      : in Float_4_Real)
      return Boolean;
    function Are_Equal(
      Item_A : in Record_Vector_3;
      Item_B : in Record_Vector_3;
      ε      : in Float_4_Real)
      return Boolean;
    function Are_Equal(
      Item_A : in Record_Vector_4;
      Item_B : in Record_Vector_4;
      ε      : in Float_4_Real)
      return Boolean;
    function Are_Equal(
      Item_A : in Record_Vector_6;
      Item_B : in Record_Vector_6;
      ε      : in Float_4_Real)
      return Boolean;
    function "-"(
      Item : in Record_Vector_2)
      return Record_Vector_2;
    function "-"(
      Item : in Record_Vector_3)
      return Record_Vector_3;
    function "-"(
      Item : in Record_Vector_4)
      return Record_Vector_4;
    function "-"(
      Item : in Record_Vector_6)
      return Record_Vector_6;
    function "-"(
      Left  : in Record_Vector_2;
      Right : in Record_Vector_2)
      return Record_Vector_2;
    function "-"(
      Left  : in Record_Vector_3;
      Right : in Record_Vector_3)
      return Record_Vector_3;
    function "-"(
      Left  : in Record_Vector_4;
      Right : in Record_Vector_4)
      return Record_Vector_4;
    function "-"(
      Left  : in Record_Vector_6;
      Right : in Record_Vector_6)
      return Record_Vector_6;
    function "+"(
      Item : in Record_Vector_2)
      return Record_Vector_2;
    function "+"(
      Item : in Record_Vector_3)
      return Record_Vector_3;
    function "+"(
      Item : in Record_Vector_4)
      return Record_Vector_4;
    function "+"(
      Item : in Record_Vector_6)
      return Record_Vector_6;
    function "+"(
      Left  : in Record_Vector_2;
      Right : in Record_Vector_2)
      return Record_Vector_2;
    function "+"(
      Left  : in Record_Vector_3;
      Right : in Record_Vector_3)
      return Record_Vector_3;
    function "+"(
      Left  : in Record_Vector_4;
      Right : in Record_Vector_4)
      return Record_Vector_4;
    function "+"(
      Left  : in Record_Vector_6;
      Right : in Record_Vector_6)
      return Record_Vector_6;
    function "*"(
      Left  : in Record_Vector_2;
      Right : in Record_Vector_2);
      return Float_4_Real;
    function "*"(
      Left  : in Record_Vector_2;
      Right : in Float_4_Real);
      return Record_Vector_2;
    function "*"(
      Left  : in Float_4_Real;
      Right : in Record_Vector_2);
      return Record_Vector_2;
    function "*"(
      Left  : in Record_Vector_3;
      Right : in Float_4_Real);
      return Record_Vector_3;
    function "*"(
      Left  : in Float_4_Real;
      Right : in Record_Vector_3);
      return Record_Vector_3;
    function "*"(
      Left  : in Float_4_Real;
      Right : in Float_4_Real);
      return Record_Vector_3;
    function "*"(
      Left  : in Record_Vector_4;
      Right : in Float_4_Real);
      return Record_Vector_4;
    function "*"(
      Left  : in Float_4_Real;
      Right : in Record_Vector_4);
      return Record_Vector_4;
    function "*"(
      Left  : in Float_4_Real;
      Right : in Float_4_Real);
      return Record_Vector_4;
    function "*"(
      Left  : in Record_Vector_6;
      Right : in Float_4_Real);
      return Record_Vector_6;
    function "*"(
      Left  : in Float_4_Real;
      Right : in Record_Vector_6);
      return Record_Vector_6;
    function "*"(
      Left  : in Float_4_Real;
      Right : in Float_4_Real);
      return Record_Vector_6;
    function "/"(
      Left  : in Record_Vector_2;
      Right : in Float_4_Real)
      return Record_Vector_2;
    function "/"(
      Left  : in Float_4_Real;
      Right : in Record_Vector_2)
      return Record_Vector_2;
    function "/"(
      Left  : in Record_Vector_3;
      Right : in Float_4_Real)
      return Record_Vector_3;
    function "/"(
      Left  : in Float_4_Real;
      Right : in Record_Vector_3)
      return Record_Vector_3;
    function "/"(
      Left  : in Record_Vector_4;
      Right : in Float_4_Real)
      return Record_Vector_4;
    function "/"(
      Left  : in Float_4_Real;
      Right : in Record_Vector_4)
      return Record_Vector_4;
    function "/"(
      Left  : in Record_Vector_6;
      Right : in Float_4_Real)
      return Record_Vector_6;
    function "/"(
      Left  : in Float_4_Real;
      Right : in Record_Vector_6)
      return Record_Vector_6;
    function "="(
      Left  : in Record_Vector_2;
      Right : in Record_Vector_2)
      return Boolean;
    function "="(
      Left  : in Record_Vector_3;
      Right : in Record_Vector_3)
      return Boolean;
    function "="(
      Left  : in Record_Vector_4;
      Right : in Record_Vector_4)
      return Boolean;
    function "="(
      Left  : in Record_Vector_6;
      Right : in Record_Vector_6)
      return Boolean;




    function Scale(
      Item_A : in Record_Vector_2;
      Item_B : in Record_Vector_)
      return Record_Vector_2;

    function Truncate(
      Item   : in Record_Vector_;
      Length : in Float_4_Real)
      return Record_Vector_2;

    procedure Snap(
      Item     : in out Record_Vector_;
      Do_Floor : in     Boolean := False);
    function Snap(
      Item     : in Record_Vector_
      Do_Floor : in Boolean := False)
      return Record_Vector_;

    function Clamp(
      Item    : in Record_Vector_
      Minimum : in Record_Vector_2;
      Maximum : in Record_Vector_2)
      return Record_Vector_;
    procedure Clamp(
      Item    : in out Record_Vector_
      Minimum : in     Record_Vector_2;
      Maximum : in     Record_Vector_2);

  -- idVec2      Scale( const idVec2 &a ) const;
  -- idVec2      Truncate( float length ) const; -- cap length
  -- void      Clamp( const idVec2 &min, const idVec2 &max );

  -- void      Lerp( const idVec2 &v1, const idVec2 &v2, const float l );
  -- bool      FixDegenerateNormal();  -- fix degenerate axial cases
  -- bool      FixDenormals();     -- change tiny numbers to zero
  -- idVec3      Cross( const idVec3 &a ) const;
  -- idVec3 &    Cross( const idVec3 &a, const idVec3 &b );
  -- float     Normalize();        -- returns length
  -- float     NormalizeFast();      -- returns length
  -- idVec3      Truncate( float length ) const;   -- cap length
  -- void      Clamp( const idVec3 &min, const idVec3 &max );
  -- void      Snap();         -- snap to closest integer value
  -- void      SnapInt();        -- snap towards integer (floor)
  -- int       GetDimension() const;
  -- const char *  ToString( int precision = 2 ) const;
  -- void      NormalVectors( idVec3 &left, idVec3 &down ) const;  -- vector should be normalized
  -- void      OrthogonalBasis( idVec3 &left, idVec3 &up ) const;
  -- void      ProjectOntoPlane( const idVec3 &normal, const float overBounce = 1.0f );
  -- bool      ProjectAlongPlane( const idVec3 &normal, const float epsilon, const float overBounce = 1.0f );
  -- void      ProjectSelfOntoSphere( const float radius );
  -- void      Lerp( const idVec3 &v1, const idVec3 &v2, const float l );
  -- void      SLerp( const idVec3 &v1, const idVec3 &v2, const float l );
  -- void      Lerp( const idVec4 &v1, const idVec4 &v2, const float l );
  -- void      Lerp( const idVec5 &v1, const idVec5 &v2, const float l );
  -- const idVec3 &  SubVec3( int index ) const;
  -- idVec3 &    SubVec3( int index );
  -- ID_INLINE         idVecX();
  -- ID_INLINE         explicit idVecX( int length );
  -- ID_INLINE         explicit idVecX( int length, float *data );
  -- ID_INLINE         ~idVecX();
  -- ID_INLINE float     Get( int index ) const;
  -- ID_INLINE float &     Get( int index );
  -- ID_INLINE float     operator[]( const int index ) const;
  -- ID_INLINE float &     operator[]( const int index );
  -- ID_INLINE idVecX      operator-() const;
  -- ID_INLINE idVecX &    operator=( const idVecX &a );
  -- ID_INLINE idVecX      operator*( const float a ) const;
  -- ID_INLINE idVecX      operator/( const float a ) const;
  -- ID_INLINE float     operator*( const idVecX &a ) const;
  -- ID_INLINE idVecX      operator-( const idVecX &a ) const;
  -- ID_INLINE idVecX      operator+( const idVecX &a ) const;
  -- ID_INLINE idVecX &    operator*=( const float a );
  -- ID_INLINE idVecX &    operator/=( const float a );
  -- ID_INLINE idVecX &    operator+=( const idVecX &a );
  -- ID_INLINE idVecX &    operator-=( const idVecX &a );
  -- friend ID_INLINE  idVecX  operator*( const float a, const idVecX &b );
  -- ID_INLINE bool      Compare( const idVecX &a ) const;             -- exact compare, no epsilon
  -- ID_INLINE bool      Compare( const idVecX &a, const float epsilon ) const;    -- compare with epsilon
  -- ID_INLINE bool      operator==( const idVecX &a ) const;            -- exact compare, no epsilon
  -- ID_INLINE bool      operator!=( const idVecX &a ) const;            -- exact compare, no epsilon
  -- ID_INLINE void      SetSize( int size );
  -- ID_INLINE void      ChangeSize( int size, bool makeZero = false );
  -- ID_INLINE int       GetSize() const { return size; }
  -- ID_INLINE void      SetData( int length, float *data );
  -- ID_INLINE void      Zero();
  -- ID_INLINE void      Random( int seed, float l = 0.0f, float u = 1.0f );
  -- ID_INLINE void      Random( int length, int seed, float l = 0.0f, float u = 1.0f );
  -- ID_INLINE void      Negate();
  -- ID_INLINE void      Clamp( float min, float max );
  -- ID_INLINE idVecX &    SwapElements( int e1, int e2 );
  -- ID_INLINE float     Length() const;
  -- ID_INLINE float     LengthSqr() const;
  -- ID_INLINE idVecX      Normalize() const;
  -- ID_INLINE float     NormalizeSelf();
  -- ID_INLINE int       GetDimension() const;
  -- ID_INLINE void      AddScaleAdd( const float scale, const idVecX & v0, const idVecX & v1 );
  -- ID_INLINE const idVec3 &  SubVec3( int index ) const;
  -- ID_INLINE idVec3 &    SubVec3( int index );
  -- ID_INLINE const idVec6 &  SubVec6( int index = 0 ) const;
  -- ID_INLINE idVec6 &    SubVec6( int index = 0 );
  -- ID_INLINE const float * ToFloatPtr() const;
  -- ID_INLINE float *     ToFloatPtr();
  -- const char *  ToString( int precision = 2 ) const;
-------
private
-------
  -------------
  -- Records --
  -------------
    type Record_Vector
      is new Ada.Finalization.Controlled
      with record
        int       size;         -- size of the vector
        int       alloced;        -- if -1 p points to data set with SetData
        float *     p;            -- memory the vector is stored
        static float  temp[VECX_MAX_TEMP+4];  -- used to store intermediate results
        static float *  tempPtr;        -- pointer to 16 byte aligned temporary memory
        static int    tempIndex;        -- index into memory pool, wraps around
      end record;
  -----------------
  -- Subprograms --
  -----------------
    ID_INLINE void  SetTempSize( int size );
  end Neo.Library.Vectors;
