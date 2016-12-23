
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

with Ada.Finalization; use Ada.Finalization;
with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Neo.Core.Arrays;  use Neo.Core.Arrays;
with Neo.Core.Vectors; use Neo.Core;
with Neo.Core.Ordered;

-- Separator for the "Data" layer consisting of packages for loading and understanding common file formats
package Neo.Data is

  -------------
  -- Parsing --
  -------------
  --
  -- Parsing complex data encoded in a text document can be difficult without a set of basic functionality - so it is provided here with
  -- the generic package Parser. The package is instantiated with the path to the file the user desires to parse along with various
  -- settings like comment prefixes for pre-processing.
  --
  -- Also, an internal data structure (such as an image file) may involve many kinds of accectable extensions (png, tga, etc.) - each
  -- with their own Load function. Although it would be possible to write such a function to dispatch each one accordingly, it would not
  -- be The Right Thing™ because loaders for other internal data structures (audio, mesh, or video) will follow the same pattern -
  -- causing code duplication. The Handler package exists to encapsulate this pattern.
  --
  -- Ex.
  --   package Neo.Data.Image is
  --       type Format_Kind is (JPG_Format, PNG_Format);
  --       type Image_State is record ... end record;
  --       function Load (Path : Str) return Image_State;
  --     end;
  --   ...
  --   package body Neo.Data.Image is
  -- 
  --       -- Separate packages (one for each Image.Format_Kind)
  --       package PNG is function Load (Path : Str) return Image_State; end; package PNG is separate;
  --       package JPG is function Load (Path : Str) return Image_State; end; package JPG is separate;
  --
  --       -- Create the loader
  --       package Handle_Image is new Handler (Format_Kind, Image_State);
  --
  --       -- Register the formats in the loader
  --       package PNG_Image is new Handle_Image.Format (PNG_Format, PNG.Load, "png");
  --       package JPG_Image is new Handle_Image.Format (JPG_Format, JPG.Load, "jpg,jpeg");
  --
  --       -- Redirect internal handlers to the public load function
  --       function Load (Path : Str) return Image_State renames Handle_Image.Load;
  --     end;
  --   ...
  --   separate (Neo.Image) package body PNG is
  --       function Load (Path : Str) return Image_State is
  --
  --         -- Lets pretend that PNG is a text based format (it isn't really)
  --         package Parse_PNG is new Parser (Path, "//", "/*", "*/"); use Parse_PNG;
  --         ...
  --         begin
  --            Data := Next;
  --            Assert ("This string must exist", "This one must also exist but it may have whitespace separating it");
  --            Skip (5);
  --            Other_Data := Next_Set ("""", """");
  --            if At_EOL then ...
  --         end;
  --       end;
  --

  -- String splitting
  function Split (Item : Str; On : Str := " ") return Vector_Str_16_Unbound.Unsafe.Vector;
  function Split (Item : Str; On : Str := " ") return Array_Str_Unbound;

  -- Package for catagorizing sets of parsers that load a single data file (e.g. an image format)
  generic
    type Format_T is (<>);
    type T is private;
  package Handler is

      -- Dispatching load function
      function Load (Path : Str) return T;

      -- Package for registering a load function based on file extensions
      generic
        Kind : Format_T;
        with function Load (Path : Str) return T;
        Extensions : Str; -- Separated by commas: "tga,png,tga"
      package Format is end;
    end;

  -- Text file parser for loading data
  generic
    Path            : Str;
    Comment         : Str     := "//"; 
    Comment_Start   : Str     := "/*";
    Comment_End     : Str     := "*/";
    Separator       : Char_16 := ' ';
    Tab_Replacement : Char_16 := ' ';
  package Parser is

      -- Test for the position of the parser currently in the file
      function At_EOL return Bool;
      function At_EOF return Bool;

      -- Fetch the next item in the file
      function Next return Str_Unbound;
      function Next return Str is (To_Str (Next));
      function Next return Real;
      function Next return Real_64;
      function Next return Byte;
      function Next return Int;
      function Next return Int_32_Unsigned;
      function Next return Int_64;
      function Next return Int_64_Unsigned;

      -- Check ahead without advancing
      function Peek_U return Str_Unbound; -- Wish this could be overloaded...
      function Peek return Str     is (To_Str             (Peek_U));
      function Peek return Real    is (Real'Wide_Value    (Peek));
      function Peek return Real_64 is (Real_64'Wide_Value (Peek));
      function Peek return Int     is (Int'Wide_Value     (Peek));
      function Peek return Int_64  is (Int_64'Wide_Value  (Peek));

      -- Parse the entire next line 
      function Next_Line return Str_Unbound;
      function Next_Line return Str is (To_Str (Next_Line));

      -- A "set" is a bounded item like a string surrounded by quotes
      function Next_Set           (Ending : Str) return Str_Unbound;
      function Next_Set           (Ending : Str) return Str is (To_Str (Next_Set (Ending)));
      function Next_Set (Starting, Ending : Str) return Str_Unbound;
      function Next_Set (Starting, Ending : Str) return Str is (To_Str (Next_Set (Starting, Ending)));

      -- Procedures for ignoring data or skiping useless information
      procedure Skip_Line;
      procedure Skip_Set           (Ending : Str);
      procedure Skip_Set (Starting, Ending : Str);
      procedure Skip     (Amount : Positive := 1);

      -- Skip_Until is a mix between peek and skip, it will skip strings until the target gets peeked
      procedure Skip_Until (T    : Array_Str_Unbound; Fail_On_EOF : Bool := False); -- Wish this could have Text instead of T...
      procedure Skip_Until (Text               : Str; Fail_On_EOF : Bool := False);
      procedure Skip_Until (T1, T2             : Str; Fail_On_EOF : Bool := False);
      procedure Skip_Until (T1, T2, T3         : Str; Fail_On_EOF : Bool := False);
      procedure Skip_Until (T1, T2, T3, T4     : Str; Fail_On_EOF : Bool := False);
      procedure Skip_Until (T1, T2, T3, T4, T5 : Str; Fail_On_EOF : Bool := False);

      -- Assert that the next item must match the text argument
      procedure Assert (Text               : Str);
      procedure Assert (T1, T2             : Str);
      procedure Assert (T1, T2, T3         : Str);
      procedure Assert (T1, T2, T3, T4     : Str);
      procedure Assert (T1, T2, T3, T4, T5 : Str);
    end;
end;