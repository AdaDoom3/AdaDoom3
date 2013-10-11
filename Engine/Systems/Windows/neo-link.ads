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
package Neo.Link
  is
  ---------------
  -- Constants -- http://web.archive.org/web/20130120140636/http://helgeklein.com/blog/2008/04/windows-x64-all-the-same-yet-very-different-part-7/
  ---------------
    EXTENSION           : constant String_1 := ".dll";
    DIRECTORY_LIBRARIES : constant String_1 := "/Windows/System32/"; -- %SystemRoot%\System32\
    DIRECTORY_STEAM     : constant String_1 := "/Program Files" & (if WORD_SIZE = 64 then " (x86)" else "") & "/Steam/"; -- %PROGRAMFILES(X86)%\Steam\ or %PROGRAMFILES%\Steam\
  ----------------
  -- Directives --
  ----------------
    --pragma Linker_Options("libopus32"                           & EXTENSION);
    --pragma Linker_Options("libogg"                              & EXTENSION);
    --pragma Linker_Options("libtheora"         & WORD_SIZE_IMAGE & EXTENSION);
    --pragma Linker_Options("libvorbis"                           & EXTENSION);
    --pragma Linker_Options("libvorbisfile"                       & EXTENSION);
    --pragma Linker_Options(DIRECTORY_STEAM     & "Steam"         & EXTENSION);
    --pragma Linker_Options("libsdl"            & WORD_SIZE_IMAGE & EXTENSION);
    pragma Linker_Options(DIRECTORY_LIBRARIES & "gdi32"         & EXTENSION);
    pragma Linker_Options(DIRECTORY_LIBRARIES & "hid"           & EXTENSION);
    pragma Linker_Options(DIRECTORY_LIBRARIES & "setupapi"      & EXTENSION);
    pragma Linker_Options(DIRECTORY_LIBRARIES & "opengl32"      & EXTENSION);
    pragma Linker_Options(DIRECTORY_LIBRARIES & "XInput9_1_0"   & EXTENSION);
    pragma Linker_Options(DIRECTORY_LIBRARIES & "D3DX9_42"      & EXTENSION);
  end Neo.Link;
