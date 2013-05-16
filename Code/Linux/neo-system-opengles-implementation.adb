--
--
--
--
--
--
--
-- https://bitbucket.org/slouken/sdl/src/e8916fe9cfc8/src/video/x11/SDL_x11opengles.c?at=default
--
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
separate(Neo.System.OpenGLES)
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
