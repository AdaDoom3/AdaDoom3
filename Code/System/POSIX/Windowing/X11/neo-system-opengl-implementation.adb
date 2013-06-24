--
--
--
--
--
--
--
-- https://github.com/AdaDoom3/AdaDoom3/blob/testgenprotectremoval/Libraries/SDL/src/video/x11/SDL_x11opengl.c
-- https://github.com/id-Software/DOOM-3/blob/master/neo/sys/linux/glimp.cpp
--
--
--
--
--
--
--
with
  Neo.Linux;
use
  Neo.Linux;
separate(Neo.System.OpenGL)
package Implementation
  is
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize(
      Driver : in String_2)
      is
      begin
        raise System_Call_Failure;
      end Initialize;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is
      begin
        raise System_Call_Failure;
      end Finalize;
  end Implementation;
