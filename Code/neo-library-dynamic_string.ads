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
  Ada.Strings,
  Ada.Finalization,
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types;
use
  Ada.Strings,
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types;
package Neo.Library.Dynamic_String
  is
  -----------
  -- Types --
  -----------
    type Record_Dynamic_String
      is private;
  ------------------
  -- Enumerations --
  ------------------
    type Enuemrated_String_Measure
      is(
      Size_String_Measure,
      Bandwidth_String_Measure);
  -----------------
  -- Subprograms --
  -----------------
    procedure Set_Length(
      Source : in out Record_Dynamic_String
      Length : in     Integer_4_Natural);
    function Get(
      Source : in Record_Dynamic_String;
      Index  : in Integer_4_Positive)
      return Character_1;
    function Get(
      Source : in Record_Dynamic_String;
      Index  : in Integer_4_Positive)
      return Character_2;
    function Get_Length(
      Item : in Record_Dynamic_String)
      return Integer_4_Natural;
    function Get_Number_Of_Bits_Allocated
      Item : in Record_Dynamic_String)
      return Integer_4_Natural;
    procedure To_Lower_Case(
      Item : in out Record_Dynamic_String);
    procedure To_Upper_Case(
      Item : in out Record_Dynamic_String);
    function To_Lower_Case(
      Item : in Record_Dynamic_String)
      return Record_Dynamic_String;
    function To_Upper_Case(
      Item : in Record_Dynamic_String)
      return Record_Dynamic_String;
    function To_Record_Dynamic_String(
      Item : in Character_1)
      return Record_Dynamic_String;
    function To_Record_Dynamic_String(
      Item : in Character_2)
      return Record_Dynamic_String;
    function To_Record_Dynamic_String(
      Item : in String_1)
      return Record_Dynamic_String;
    function To_Record_Dynamic_String(
      Item : in String_2)
      return Record_Dynamic_String;
    function "&"(
      Left  : in Record_Dynamic_String;
      Right : in Record_Dynamic_String)
      return Record_Dynamic_String;
    function "&"(
      Left  : in Character_1;
      Right : in Record_Dynamic_String)
      return Record_Dynamic_String;
    function "&"(
      Left  : in Record_Dynamic_String;
      Right : in Character_1)
      return Record_Dynamic_String;
    function "&"(
      Left  : in Character_2;
      Right : in Record_Dynamic_String)
      return Record_Dynamic_String;
    function "&"(
      Left  : in Record_Dynamic_String;
      Right : in Character_2)
      return Record_Dynamic_String;
    function "&"(
      Left  : in String_1;
      Right : in Record_Dynamic_String)
      return Record_Dynamic_String;
    function "&"(
      Left  : in Record_Dynamic_String;
      Right : in String_1)
      return Record_Dynamic_String;
    function "&"(
      Left  : in String_2;
      Right : in Record_Dynamic_String)
      return Record_Dynamic_String;
    function "&"(
      Left  : in Record_Dynamic_String;
      Right : in String_2)
      return Record_Dynamic_String;
    function "="(
      Left  : in Record_Dynamic_String;
      Right : in Record_Dynamic_String)
      return Boolean;
    function "="(
      Left  : in Character_1;
      Right : in Record_Dynamic_String)
      return Boolean;
    function "="(
      Left  : in Record_Dynamic_String;
      Right : in Character_1)
      return Boolean;
    function "="(
      Left  : in Character_2;
      Right : in Record_Dynamic_String)
      return Boolean;
    function "="(
      Left  : in Record_Dynamic_String;
      Right : in Character_2)
      return Boolean;
    function "="(
      Left  : in String_1;
      Right : in Record_Dynamic_String)
      return Boolean;
    function "="(
      Left  : in Record_Dynamic_String;
      Right : in String_1)
      return Boolean;
    function "="(
      Left  : in String_2;
      Right : in Record_Dynamic_String)
      return Boolean;
    function "="(
      Left  : in Record_Dynamic_String;
      Right : in String_2)
      return Boolean;
    function Are_Insensitive_Equal(
      Item_A : in Record_Dynamic_String;
      Item_B : in Record_Dynamic_String)
      return Boolean;
    function Are_Insensitive_Equal(
      Item_A : in Character_1;
      Item_B : in Record_Dynamic_String)
      return Boolean;
    function Are_Insensitive_Equal(
      Item_A : in Record_Dynamic_String;
      Item_B : in Character_1)
      return Boolean;
    function Are_Insensitive_Equal(
      Item_A : in Character_2;
      Item_B : in Record_Dynamic_String)
      return Boolean;
    function Are_Insensitive_Equal(
      Item_A : in Record_Dynamic_String;
      Item_B : in Character_2)
      return Boolean;
    function Are_Insensitive_Equal(
      Item_A : in String_1;
      Item_B : in Record_Dynamic_String)
      return Boolean;
    function Are_Insensitive_Equal(
      Item_A : in Record_Dynamic_String;
      Item_B : in String_1)
      return Boolean;
    function Are_Insensitive_Equal(
      Item_A : in String_2;
      Item_B : in Record_Dynamic_String)
      return Boolean;
    function Are_Insensitive_Equal(
      Item_A : in Record_Dynamic_String;
      Item_B : in String_2)
      return Boolean;
    function Are_Equal_Paths(
      Source : in Record_Dynamic_String;
      Path   : in Record_Dynamic_String)
      return Boolean;
    function Are_Equal_Paths(
      Source : in Record_Dynamic_String;
      Path   : in String_1)
      return Boolean;
    function Are_Equal_Paths(
      Source : in Record_Dynamic_String;
      Path   : in String_2)
      return Boolean;
    function Are_Equal_Path_Prefixes(
      Source : in Record_Dynamic_String;
      Path   : in Record_Dynamic_String)
      return Boolean;
    function Are_Equal_Path_Prefixes(
      Source : in Record_Dynamic_String;
      Path   : in String_1)
      return Boolean;
    function Are_Equal_Path_Prefixes(
      Source : in Record_Dynamic_String;
      Path   : in String_2)
      return Boolean;
    function Does_Contain_Lower_Case(
      Item : in Record_Dynamic_String)
      return Boolean;
    function Does_Contain_Upper_Case(
      Item : in Record_Dynamic_String)
      return Boolean;
    function Is_Numeric(
      Item : in Record_Dynamic_String)
      return Boolean;
    function Is_Empty(
      Item : in Record_Dynamic_String)
      return Boolean;
    procedure Empty(
      Item : in out Record_Dynamic_String);
    function Empty(
      Item : in Record_Dynamic_String)
      return Record_Dynamic_String;
    procedure Clear(
      Item : in out Record_Dynamic_String);
    procedure Fill_With_Character(
      Source      : in out Record_Dynamic_String;
      Item        : in     Character_1;
      Up_To_Index : in     Integer_4_Positive);

    -- ???
    procedure Insert_After(
      Source   : in out Record_Dynamic_String;
      Addition : in     Record_Dynamic_String;
      Index    : in     Integer_4_Positive);
    procedure Insert_After(
      Source   : in out Record_Dynamic_String;
      Addition : in     Character_1;
      Index    : in     Integer_4_Positive);
    procedure Insert_After(
      Source   : in out Record_Dynamic_String;
      Addition : in     Character_2;
      Index    : in     Integer_4_Positive);
    procedure Insert_After(
      Source   : in out Record_Dynamic_String;
      Addition : in     String_1;
      Index    : in     Integer_4_Positive);
    procedure Insert_After(
      Source   : in out Record_Dynamic_String;
      Addition : in     String_2;
      Index    : in     Integer_4_Positive);
    function Index(
      Source   : in Record_Dynamic_String;
      Pattern  : in String;
      Going    : in Direction := Forward;
      Mapping  : in Maps.Character_Mapping := Maps.Identity)
      return Integer_4_Natural;
    function Index(
      Source   : in Record_Dynamic_String;
      Pattern  : in String;
      Going    : in Direction := Forward;
      Mapping  : in Maps.Character_Mapping_Function)
      return Integer_4_Natural;
    function Index(
      Source : in Record_Dynamic_String;
      Set    : in Maps.Character_Set;
      Test   : in Membership := Inside;
      Going  : in Direction  := Forward)
      return Integer_4_Natural;
    function Index_Non_Blank(
      Source : in Record_Dynamic_String;
      Going  : in Direction := Forward)
      return Integer_4_Natural;
    function Count(
      Source  : in Record_Dynamic_String;
      Pattern : in String;
      Mapping : in Maps.Character_Mapping := Maps.Identity)
      return Integer_4_Natural;
    function Count(
      Source   : in Record_Dynamic_String;
      Pattern  : in String;
      Mapping  : in Maps.Character_Mapping_Function)
      return Integer_4_Natural;
    function Count(
      Source : in Record_Dynamic_String;
      Set    : in Maps.Character_Set)
      return Integer_4_Natural;
    procedure Find_Token(
      Source : in     Record_Dynamic_String;
      Set    : in     Maps.Character_Set;
      Test   : in     Membership;
      First  : in out Integer_4_Positive;
      Last   : in out Integer_4_Natural);
    function Replace_Slice(
      Source : in Record_Dynamic_String;
      Low    : in Integer_4_Positive;
      High   : in Integer_4_Natural;
      By     : in String)
      return Record_Dynamic_String;
    procedure Replace_Slice(
      Source : in out Record_Dynamic_String;
      Low    : in     Integer_4_Positive;
      High   : in     Integer_4_Natural;
      By     : in     String);
    function Insert(
      Source   : in Record_Dynamic_String;
      Before   : in Integer_4_Positive;
      New_Item : in String)
      return Record_Dynamic_String;
    procedure Insert(
      Source   : in out Record_Dynamic_String;
      Before   : in     Integer_4_Positive;
      New_Item : in     String);
    function Overwrite(
      Source   : in Record_Dynamic_String;
      Position : in Integer_4_Positive;
      New_Item : in String)
      return Record_Dynamic_String;
    procedure Overwrite(
      Source   : in out Record_Dynamic_String;
      Position : in     Integer_4_Positive;
      New_Item : in     String);
    function Delete(
      Source  : in Record_Dynamic_String;
      From    : in Integer_4_Positive;
      Through : in Integer_4_Natural)
      return Record_Dynamic_String;
    procedure Delete(
      Source  : in out Record_Dynamic_String;
      From    : in     Integer_4_Positive;
      Through : in     Integer_4_Natural);
    function Trim(
      Source : in Record_Dynamic_String;
      Side   : in Trim_End)
      return Record_Dynamic_String;
    procedure Trim(
      Source : in out Record_Dynamic_String;
      Side   : in     Trim_End);
    function Trim(
      Source : in Record_Dynamic_String;
      Left   : in Maps.Character_Set;
      Right  : in Maps.Character_Set)
      return Record_Dynamic_String;
    procedure Trim(
      Source : in out Record_Dynamic_String;
      Left   : in     Maps.Character_Set;
      Right  : in     Maps.Character_Set);
    function Head(
      Source : in Record_Dynamic_String;
      Count  : in Integer_4_Natural;
      Pad    : in Character := Space)
      return Record_Dynamic_String;
    procedure Head(
      Source : in out Record_Dynamic_String;
      Count  : in     Integer_4_Natural;
      Pad    : in     Character := Space);
    function Tail(
      Source : in Record_Dynamic_String;
      Count  : in Integer_4_Natural;
      Pad    : in Character := Space)
      return Record_Dynamic_String;
    procedure Tail(
      Source : in out Record_Dynamic_String;
      Count  : in     Integer_4_Natural;
      Pad    : in     Character := Space);


  static bool       IsValidUTF8( const uint8 * s, const int maxLen, utf8Encoding_t & encoding );
  static ID_INLINE bool IsValidUTF8( const char * s, const int maxLen, utf8Encoding_t & encoding ) { return IsValidUTF8( ( const uint8* )s, maxLen, encoding ); }
  static ID_INLINE bool IsValidUTF8( const uint8 * s, const int maxLen );
  static ID_INLINE bool IsValidUTF8( const char * s, const int maxLen ) { return IsValidUTF8( ( const uint8* )s, maxLen ); }


  int         Find( const char c, int start = 0, int end = -1 ) const;
  int         Find( const char *text, bool casesensitive = true, int start = 0, int end = -1 ) const;
  bool        Filter( const char *filter, bool casesensitive ) const;
  int         Last( const char c ) const;           -- return the index to the last occurance of 'c', returns -1 if not found
  void        Format( VERIFY_FORMAT_STRING const char *fmt, ... );          -- perform a threadsafe sprintf to the Source
  static idStr    FormatInt( const int num, bool isCash = false );      -- formats an integer as a value with commas
  static idStr    FormatCash( const int num ) { return FormatInt( num, true ); }
  void        StripTrailingWhitespace();        -- strip trailing white space characters
  idStr &       StripQuotes();              -- strip quotes around Source
  bool        Replace( const char *old, const char *nw );
  bool        ReplaceChar( const char old, const char nw );
  ID_INLINE void    CopyRange( const char * text, int start, int end );
  -- file name methods
  int         FileNameHash() const;           -- hash key for the filename (skips extension)
  idStr &       BackSlashesToSlashes();         -- convert slashes
  idStr &       SlashesToBackSlashes();         -- convert slashes
  idStr &       SetFileExtension( const char *extension );    -- set the given file extension
  idStr &       StripFileExtension();           -- remove any file extension
  idStr &       StripAbsoluteFileExtension();       -- remove any file extension looking from front (useful if there are multiple .'s)
  idStr &       DefaultFileExtension( const char *extension );  -- if there's no file extension use the default
  idStr &       DefaultPath( const char *basepath );      -- if there's no path use the default
  void        AppendPath( const char *text );         -- append a partial path
  idStr &       StripFilename();              -- remove the filename from a path
  idStr &       StripPath();                -- remove the path from the filename
  void        ExtractFilePath( idStr &dest ) const;     -- copy the file path to another Source
  void        ExtractFileName( idStr &dest ) const;     -- copy the filename to another Source
  void        ExtractFileBase( idStr &dest ) const;     -- copy the filename minus the extension to another Source
  void        ExtractFileExtension( idStr &dest ) const;    -- copy the file extension to another Source
  bool        CheckExtension( const char *ext );
  -- hash keys
  static int      Hash( const char *Source );
  static int      Hash( const char *Source, int length );
  static int      IHash( const char *Source );          -- case insensitive
  static int      IHash( const char *Source, int length );    -- case insensitive
  friend int      sprintf( idStr &dest, const char *fmt, ... );
  friend int      vsprintf( idStr &dest, const char *fmt, va_list ap );
  void        ReAllocate( int amount, bool keepold );       -- reallocate Source data buffer
  void        FreeData();                 -- free allocated Source memory
            -- format value in the given measurement with the best unit, returns the best unit
  int         BestUnit( const char *format, float value, Measure_t measure );
            -- format value in the requested unit and measurement
  void        SetUnit( const char *format, float value, int unit, Measure_t measure );
  static void     InitMemory();
  static void     ShutdownMemory();
  static void     PurgeMemory();
  static void     ShowMemoryUsage_f( const idCmdArgs &args );

  int         DynamicMemoryUsed() const;
  static idStr    FormatNumber( int number );
  ASSERT_ENUM_STRING( Source, index )   ( 1 / (int)!( Source - index ) ) ? #Source : ""

-------
private
-------
  ---------------
  -- Constants --
  ---------------
    FILE_HASH_SIZE                     : constant Integer_4_Positive := 1024;
    MINIMUM_STRING_ALLOCATION_IN_BYTES : constant Integer_4_Positive := 20;
    MAXIMUM_STRING_ALLOCATION_IN_BYTES : constant Integer_4_Positive := 32;
    static const uint32 STATIC_BIT  = 31;
    static const uint32 STATIC_MASK = 1u << STATIC_BIT;
    static const uint32 ALLOCED_MASK = STATIC_MASK - 1;
  ------------
  -- Arrays --
  ------------
    type Array_Base_Buffer
      is array 1..MINIMUM_STRING_ALLOCATION_IN_BYTES
      of Integer_1_Unsigned;
  -------------
  -- Records --
  -------------
    type Record_Dynamic_String
      is record
        Length                    : Integer_4_Natural;
        Data                      : Access_Array_Integer_1_Unsigned;
        Is_Data_Static            : Boolean;
        Number_Of_Bytes_Allocated : Integer_4_Natural;
        Buffer                    : Array_Base_Buffer;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    procedure Enusure_Buffer_Is_Large_Enough(
      Amount      : in Integer_4_Positive;
      Do_Keep_Old : in Boolean := True);
    procedure Set_Static_Buffer(
      Buffer : in
  void        EnsureAlloced( int amount, bool keepold = true ); -- ensure Source data buffer is large anough

  -- sets the data point to the specified buffer... note that this ignores makes the passed buffer empty and ignores
  -- anything currently in the idStr's dynamic buffer.  This method is intended to be called only from a derived class's constructor.
  ID_INLINE void    SetStaticBuffer( char * buffer, const int bufferLength );
  ID_INLINE void    Construct();




  ID_INLINE int   GetAlloced() const { return allocedAndFlag & ALLOCED_MASK; }
  ID_INLINE void    SetAlloced( const int a ) { allocedAndFlag = ( allocedAndFlag & STATIC_MASK ) | ( a & ALLOCED_MASK); }

  ID_INLINE bool    IsStatic() const { return ( allocedAndFlag & STATIC_MASK ) != 0; }
  ID_INLINE void    SetStatic( const bool isStatic ) { allocedAndFlag = ( allocedAndFlag & ALLOCED_MASK ) | ( isStatic << STATIC_BIT ); }

  end Neo.Library.Dynamic_String;
