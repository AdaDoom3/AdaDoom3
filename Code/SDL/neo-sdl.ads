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
WITH
  System,
  Interfaces,
  Interfaces.C,
  Ada.Command_Line,
  Ada.Unchecked_Conversion,
  Neo.Foundation.Data_Types;
USE
  System,
  Interfaces,
  Interfaces.C,
  Ada.Command_Line,
  Neo.Foundation.Data_Types;
PACKAGE Neo.SDL
  IS
  ---------------
  -- CONSTANTS --
  ---------------
    FLAG_VISIBLE_INITIALLY : CONSTANT Integer_4_Unsigned_C := 16#0000_0010#;
    FLAG_FULLSCREEN        : CONSTANT Integer_4_Unsigned_C := 16#0000_0001#;
  ---------------
  -- ACCESSORS --
  ---------------
    TYPE Access_Window
      IS NEW Address;
  -----------------
  -- SUBPROGRAMS --
  -----------------
    PROCEDURE Set_Window_Title(
      Window : IN Access_Window;
      Title  : IN const char *title); -- UTF 8
    PROCEDURE Set_Window_Icon(
      Window  : IN Access_Window;
      Surface : IN Access_Surfaces);
extern DECLSPEC void SDLCALL SDL_SetWindowSize(SDL_Window * window, int w,
                                               int h);
extern DECLSPEC void SDLCALL SDL_SetWindowPosition(SDL_Window * window,
                                                   int x, int y);
extern DECLSPEC void SDLCALL SDL_GetWindowPosition(SDL_Window * window,
                                                   int *x, int *y);
extern DECLSPEC void SDLCALL SDL_MinimizeWindow(SDL_Window * window);
extern DECLSPEC int SDLCALL SDL_SetWindowFullscreen(SDL_Window * window,
                                                    SDL_bool fullscreen);
extern DECLSPEC void SDLCALL SDL_SetWindowGrab(SDL_Window * window,
                                               SDL_bool grabbed);
extern DECLSPEC void SDLCALL SDL_DestroyWindow(SDL_Window * window);
extern DECLSPEC void SDLCALL SDL_EnableScreenSaver(void);
extern DECLSPEC void SDLCALL SDL_DisableScreenSaver(void);
extern DECLSPEC int SDLCALL SDL_VideoInit(const char *driver_name);
    FUNCTION Create_Window(
      Title  : IN const char *title; -- UTF 8
      X      : IN Integer_4_Signed_C;
      Y      : IN Integer_4_Signed_C;
      Width  : IN Integer_4_Signed_C;
      Height : IN Integer_4_Signed_C;
      Flags  : IN Integer_4_Unsigned_C)
      RETURN Access_Window;
-------
PRIVATE
-------
  ----------------
  -- DIRECTIVES --
  ----------------
    PRAGMA Import(Stdcall, Set_Window_Title, "SDL_SetWindowTitle");
    PRAGMA Import(Stdcall, Create_Window,    "SDL_CreateWindow");
extern DECLSPEC void SDLCALL SDL_SetWindowSize(SDL_Window * window, int w,
                                               int h);
extern DECLSPEC void SDLCALL SDL_SetWindowPosition(SDL_Window * window,
                                                   int x, int y);
extern DECLSPEC void SDLCALL SDL_GetWindowPosition(SDL_Window * window,
                                                   int *x, int *y);
extern DECLSPEC void SDLCALL SDL_MinimizeWindow(SDL_Window * window);
extern DECLSPEC int SDLCALL SDL_SetWindowFullscreen(SDL_Window * window,
                                                    SDL_bool fullscreen);
extern DECLSPEC void SDLCALL SDL_SetWindowGrab(SDL_Window * window,
                                               SDL_bool grabbed);
extern DECLSPEC void SDLCALL SDL_DestroyWindow(SDL_Window * window);
extern DECLSPEC void SDLCALL SDL_EnableScreenSaver(void);
extern DECLSPEC void SDLCALL SDL_DisableScreenSaver(void);
extern DECLSPEC int SDLCALL SDL_VideoInit(const char *driver_name);
