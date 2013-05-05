################################
# Configuration
################################

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(CheckCSourceCompiles)
include(CheckObjCSourceCompiles)
include(CheckFunctionExists)
include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckTypeSize)

if (ANDROID)
    set(SDL_LOADSO_DLOPEN "" CACHE INTERNAL "Enable loadable libs with dlopen")
    set(HAVE_ALLOCA_H 1 CACHE INTERNAL "")
    set(HAVE_SYS_TYPES_H 1 CACHE INTERNAL "")
    set(HAVE_STDIO_H 1 CACHE INTERNAL "")
    set(STDC_HEADERS 1 CACHE INTERNAL "")
    set(HAVE_STRING_H 1 CACHE INTERNAL "")
    set(HAVE_INTTYPES_H 1 CACHE INTERNAL "")
    set(HAVE_STDINT_H 1 CACHE INTERNAL "")
    set(HAVE_CTYPE_H 1 CACHE INTERNAL "")
    set(HAVE_MATH_H 1 CACHE INTERNAL "")
    set(HAVE_SIGNAL_H 1 CACHE INTERNAL "")
    set(HAVE_MALLOC 1 CACHE INTERNAL "")
    set(HAVE_CALLOC 1 CACHE INTERNAL "")
    set(HAVE_REALLOC 1 CACHE INTERNAL "")
    set(HAVE_FREE 1 CACHE INTERNAL "")
    set(HAVE_ALLOCA 1 CACHE INTERNAL "")
    set(HAVE_GETENV 1 CACHE INTERNAL "")
    set(HAVE_SETENV 1 CACHE INTERNAL "")
    set(HAVE_PUTENV 1 CACHE INTERNAL "")
    set(HAVE_SETENV 1 CACHE INTERNAL "")
    set(HAVE_UNSETENV 1 CACHE INTERNAL "")
    set(HAVE_QSORT 1 CACHE INTERNAL "")
    set(HAVE_ABS 1 CACHE INTERNAL "")
    set(HAVE_BCOPY 1 CACHE INTERNAL "")
    set(HAVE_MEMSET 1 CACHE INTERNAL "")
    set(HAVE_MEMCPY 1 CACHE INTERNAL "")
    set(HAVE_MEMMOVE 1 CACHE INTERNAL "")
    set(HAVE_MEMCMP 1 CACHE INTERNAL "")
    set(HAVE_STRLEN 1 CACHE INTERNAL "")
    set(HAVE_STRLCPY 1 CACHE INTERNAL "")
    set(HAVE_STRLCAT 1 CACHE INTERNAL "")
    set(HAVE_STRDUP 1 CACHE INTERNAL "")
    set(HAVE_STRCHR 1 CACHE INTERNAL "")
    set(HAVE_STRRCHR 1 CACHE INTERNAL "")
    set(HAVE_STRSTR 1 CACHE INTERNAL "")
    set(HAVE_STRTOL 1 CACHE INTERNAL "")
    set(HAVE_STRTOUL 1 CACHE INTERNAL "")
    set(HAVE_STRTOLL 1 CACHE INTERNAL "")
    set(HAVE_STRTOULL 1 CACHE INTERNAL "")
    set(HAVE_STRTOD 1 CACHE INTERNAL "")
    set(HAVE_ATOI 1 CACHE INTERNAL "")
    set(HAVE_ATOF 1 CACHE INTERNAL "")
    set(HAVE_STRCMP 1 CACHE INTERNAL "")
    set(HAVE_STRNCMP 1 CACHE INTERNAL "")
    set(HAVE_STRCASECMP 1 CACHE INTERNAL "")
    set(HAVE_STRNCASECMP 1 CACHE INTERNAL "")
    set(HAVE_SSCANF 1 CACHE INTERNAL "")
    set(HAVE_SNPRINTF 1 CACHE INTERNAL "")
    set(HAVE_VSNPRINTF 1 CACHE INTERNAL "")
    set(HAVE_M_PI 1 CACHE INTERNAL "")
    set(HAVE_ATAN 1 CACHE INTERNAL "")
    set(HAVE_ATAN2 1 CACHE INTERNAL "")
    set(HAVE_CEIL 1 CACHE INTERNAL "")
    set(HAVE_COPYSIGN 1 CACHE INTERNAL "")
    set(HAVE_COS 1 CACHE INTERNAL "")
    set(HAVE_COSF 1 CACHE INTERNAL "")
    set(HAVE_FABS 1 CACHE INTERNAL "")
    set(HAVE_FLOOR 1 CACHE INTERNAL "")
    set(HAVE_LOG 1 CACHE INTERNAL "")
    set(HAVE_POW 1 CACHE INTERNAL "")
    set(HAVE_SCALBN 1 CACHE INTERNAL "")
    set(HAVE_SIN 1 CACHE INTERNAL "")
    set(HAVE_SINF 1 CACHE INTERNAL "")
    set(HAVE_SQRT 1 CACHE INTERNAL "")
    set(HAVE_SIGACTION 1 CACHE INTERNAL "")
    set(HAVE_SETJMP 1 CACHE INTERNAL "")
    set(HAVE_NANOSLEEP 1 CACHE INTERNAL "")
    set(HAVE_SYSCONF 1 CACHE INTERNAL "")
    set(SIZEOF_VOIDP 4 CACHE INTERNAL "")

    set(SDL_VIDEO_OPENGL_ES 1 CACHE INTERNAL "Enable OpenGL ES video")
    set(SDL_VIDEO_RENDER_OGL_ES 1 CACHE INTERNAL "Enable OpenGL ES video render")
    #set(SDL_VIDEO_RENDER_OGL_ES2 1 CACHE INTERNAL "Enable OpenGL ES2 video render")
    set(SDL_AUDIO_DRIVER_ANDROID 1 CACHE INTERNAL "Enable Android audio driver")
    set(SDL_HAPTIC_DUMMY 1 CACHE INTERNAL "Enable dummy haptic driver")
    set(SDL_JOYSTICK_ANDROID 1 CACHE INTERNAL "Enable Android joystick support")
    set(SDL_VIDEO_DRIVER_ANDROID 1 CACHE INTERNAL "Enable Android video driver")
    set(SDL_LOADSO_DLOPEN 1 CACHE INTERNAL "Enable loadable libs with dlopen")
else (ANDROID)
    CHECK_INCLUDE_FILES("stdlib.h;stdarg.h;string.h;float.h" STDC_HEADERS)
    CHECK_INCLUDE_FILES(sys/types.h HAVE_SYS_TYPES_H)
    CHECK_INCLUDE_FILES(alloca.h HAVE_ALLOCA_H)
    CHECK_INCLUDE_FILES(stdio.h HAVE_STDIO_H)
    CHECK_INCLUDE_FILES(stdlib.h HAVE_STDLIB_H)
    CHECK_INCLUDE_FILES(stddef.h HAVE_STDDEF_H)
    CHECK_INCLUDE_FILES(stdarg.h HAVE_STDARG_H)
    CHECK_INCLUDE_FILES(malloc.h HAVE_MALLOC_H)
    CHECK_INCLUDE_FILES(memory.h HAVE_MEMORY_H)
    CHECK_INCLUDE_FILES(string.h HAVE_STRING_H)
    CHECK_INCLUDE_FILES(strings.h HAVE_STRINGS_H)
    CHECK_INCLUDE_FILES(inttypes.h HAVE_INTTYPES_H)
    CHECK_INCLUDE_FILES(stdint.h HAVE_STDINT_H)
    CHECK_INCLUDE_FILES(ctype.h HAVE_CTYPE_H)
    CHECK_INCLUDE_FILES(math.h HAVE_MATH_H)
    CHECK_INCLUDE_FILES(iconv.h HAVE_ICONV_H)
    CHECK_INCLUDE_FILES(signal.h HAVE_SIGNAL_H)
    CHECK_INCLUDE_FILES(altivec.h HAVE_ALTIVEC_H)
    CHECK_INCLUDE_FILES(pthread_np.h HAVE_PTHREAD_NP_H)
    CHECK_INCLUDE_FILES(dlfcn.h HAVE_DLFCN_H)
    CHECK_TYPE_SIZE("void *" SIZEOF_VOIDP)

    CHECK_LIBRARY_EXISTS(c dlopen "" HAVE_DLOPEN_IN_C)
    if (HAVE_DLOPEN_IN_C)
        set(SDL_LOADSO_DLOPEN 1 CACHE INTERNAL "Enable loadable libs with dlopen")
    else (HAVE_DLOPEN_IN_C)
        CHECK_LIBRARY_EXISTS(dl dlopen "" HAVE_DLOPEN_IN_DL)
        if (HAVE_DLOPEN_IN_DL)
            set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} dl)
            set(SDL_LOADSO_DLOPEN 1 CACHE INTERNAL "Enable loadable libs with dlopen")
            set(CMAKE_REQUIRED_LIBRARIES ${CMAKECMAKE_REQUIRED_LIBRARIES} dl)
        else (HAVE_DLOPEN_IN_DL)
            CHECK_LIBRARY_EXISTS(ltdl dlopen "" HAVE_DLOPEN_IN_LTDL)
            if (HAVE_DLOPEN_IN_LTDL)
                set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} ltdl)
                set(SDL_LOADSO_DLOPEN 1 CACHE INTERNAL "Enable loadable libs with dlopen")
                set(CMAKE_REQUIRED_LIBRARIES ${CMAKECMAKE_REQUIRED_LIBRARIES} ltdl)
            else (HAVE_DLOPEN_IN_LTDL)
                set(SDL_LOADSO_DLOPEN "" CACHE INTERNAL "Enable loadable libs with dlopen")
            endif (HAVE_DLOPEN_IN_LTDL)
        endif (HAVE_DLOPEN_IN_DL)
    endif (HAVE_DLOPEN_IN_C)

    CHECK_LIBRARY_EXISTS(m pow "" HAVE_LIBM)
    if (HAVE_LIBM)
        set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} m)
        set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} m)
    endif (HAVE_LIBM)

    CHECK_FUNCTION_EXISTS(malloc HAVE_MALLOC)
    CHECK_FUNCTION_EXISTS(calloc HAVE_CALLOC)
    CHECK_FUNCTION_EXISTS(realloc HAVE_REALLOC)
    CHECK_FUNCTION_EXISTS(free HAVE_FREE)
    CHECK_FUNCTION_EXISTS(alloca HAVE_ALLOCA)
    CHECK_FUNCTION_EXISTS(getenv HAVE_GETENV)
    CHECK_FUNCTION_EXISTS(setenv HAVE_SETENV)
    CHECK_FUNCTION_EXISTS(putenv HAVE_PUTENV)
    CHECK_FUNCTION_EXISTS(unsetenv HAVE_UNSETENV)
    CHECK_FUNCTION_EXISTS(qsort HAVE_QSORT)
    CHECK_FUNCTION_EXISTS(abs HAVE_ABS)
    CHECK_FUNCTION_EXISTS(bcopy HAVE_BCOPY)
    CHECK_FUNCTION_EXISTS(memset HAVE_MEMSET)
    CHECK_FUNCTION_EXISTS(memcpy HAVE_MEMCPY)
    CHECK_FUNCTION_EXISTS(memmove HAVE_MEMMOVE)
    CHECK_FUNCTION_EXISTS(memcmp HAVE_MEMCMP)
    CHECK_FUNCTION_EXISTS(strlen HAVE_STRLEN)
    CHECK_FUNCTION_EXISTS(strlcpy HAVE_STRLCPY)
    CHECK_FUNCTION_EXISTS(strlcat HAVE_STRLCAT)
    CHECK_FUNCTION_EXISTS(strdup HAVE_STRDUP)
    CHECK_FUNCTION_EXISTS(_strrev HAVE__STRREV)
    CHECK_FUNCTION_EXISTS(_strupr HAVE__STRUPR)
    CHECK_FUNCTION_EXISTS(_strlwr HAVE__STRLWR)
    CHECK_FUNCTION_EXISTS(index HAVE_INDEX)
    CHECK_FUNCTION_EXISTS(rindex HAVE_RINDEX)
    CHECK_FUNCTION_EXISTS(strchr HAVE_STRCHR)
    CHECK_FUNCTION_EXISTS(strrchr HAVE_STRRCHR)
    CHECK_FUNCTION_EXISTS(strstr HAVE_STRSTR)
    CHECK_FUNCTION_EXISTS(itoa HAVE_ITOA)
    CHECK_FUNCTION_EXISTS(_ltoa HAVE__LTOA)
    CHECK_FUNCTION_EXISTS(_uitoa HAVE__UITOA)
    CHECK_FUNCTION_EXISTS(_ultoa HAVE__ULTOA)
    CHECK_FUNCTION_EXISTS(strtol HAVE_STRTOL)
    CHECK_FUNCTION_EXISTS(strtoul HAVE_STRTOUL)
    CHECK_FUNCTION_EXISTS(_i64toa HAVE__I64TOA)
    CHECK_FUNCTION_EXISTS(_ui64toa HAVE__UI64TOA)
    CHECK_FUNCTION_EXISTS(strtoll HAVE_STRTOLL)
    CHECK_FUNCTION_EXISTS(strtoull HAVE_STRTOULL)
    CHECK_FUNCTION_EXISTS(strtod HAVE_STRTOD)
    CHECK_FUNCTION_EXISTS(atoi HAVE_ATOI)
    CHECK_FUNCTION_EXISTS(atof HAVE_ATOF)
    CHECK_FUNCTION_EXISTS(strcmp HAVE_STRCMP)
    CHECK_FUNCTION_EXISTS(strncmp HAVE_STRNCMP)
    CHECK_FUNCTION_EXISTS(_stricmp HAVE__STRICMP)
    CHECK_FUNCTION_EXISTS(strcasecmp HAVE_STRCASECMP)
    CHECK_FUNCTION_EXISTS(_strnicmp HAVE__STRNICMP)
    CHECK_FUNCTION_EXISTS(strncasecmp HAVE_STRNCASECMP)
    CHECK_FUNCTION_EXISTS(sscanf HAVE_SSCANF)
    CHECK_FUNCTION_EXISTS(snprintf HAVE_SNPRINTF)
    CHECK_FUNCTION_EXISTS(vsnprintf HAVE_VSNPRINTF)
    CHECK_FUNCTION_EXISTS(atan HAVE_ATAN)
    CHECK_FUNCTION_EXISTS(atan2 HAVE_ATAN2)
    CHECK_FUNCTION_EXISTS(ceil HAVE_CEIL)
    CHECK_FUNCTION_EXISTS(copysign HAVE_COPYSIGN)
    CHECK_FUNCTION_EXISTS(cos HAVE_COS)
    CHECK_FUNCTION_EXISTS(cosf HAVE_COSF)
    CHECK_FUNCTION_EXISTS(fabs HAVE_FABS)
    CHECK_FUNCTION_EXISTS(floor HAVE_FLOOR)
    CHECK_FUNCTION_EXISTS(log HAVE_LOG)
    CHECK_FUNCTION_EXISTS(pow HAVE_POW)
    CHECK_FUNCTION_EXISTS(scalbn HAVE_SCALBN)
    CHECK_FUNCTION_EXISTS(sin HAVE_SIN)
    CHECK_FUNCTION_EXISTS(sinf HAVE_SINF)
    CHECK_FUNCTION_EXISTS(sqrt HAVE_SQRT)
    CHECK_FUNCTION_EXISTS(sigaction HAVE_SIGACTION)
    CHECK_FUNCTION_EXISTS(setjmp HAVE_SETJMP)
    CHECK_FUNCTION_EXISTS(nanosleep HAVE_NANOSLEEP)
    CHECK_FUNCTION_EXISTS(sysconf HAVE_SYSCONF)
    CHECK_FUNCTION_EXISTS(sysctlbyname HAVE_SYSCTLBYNAME)
    CHECK_FUNCTION_EXISTS(clock_gettime HAVE_CLOCK_GETTIME)
    CHECK_FUNCTION_EXISTS(getpagesize HAVE_GETPAGESIZE)
    CHECK_FUNCTION_EXISTS(mprotect HAVE_MPROTECT)
    CHECK_FUNCTION_EXISTS(iconv HAVE_ICONV)
    CHECK_FUNCTION_EXISTS(pthread_setname_np HAVE_PTHREAD_SETNAME_NP)
    CHECK_FUNCTION_EXISTS(pthread_set_name_np HAVE_PTHREAD_SET_NAME_NP)

    CHECK_SYMBOL_EXISTS(M_PI math.h HAVE_M_PI)
    CHECK_SYMBOL_EXISTS(sa_sigaction signal.h HAVE_SA_SIGACTION)
endif (ANDROID)

CHECK_C_SOURCE_COMPILES("
    #include <pthread.h>
    #include <semaphore.h>
    int main() {
        sem_timedwait(NULL, NULL);
        return 0;
    }
    "
    HAVE_SEM_TIMEDWAIT)

CHECK_C_SOURCE_COMPILES("
    int main() {
        int a;
        void *x, *y, *z;
        __sync_lock_test_and_set(&a, 4);
        __sync_lock_test_and_set(&x, y);
        __sync_fetch_and_add(&a, 1);
        __sync_bool_compare_and_swap(&a, 5, 10);
        __sync_bool_compare_and_swap(&x, y, z);
        return 0;
    }
    "
    HAVE_GCC_ATOMICS)

CHECK_C_SOURCE_COMPILES("
    int main() {
        int a;
        __sync_lock_test_and_set(&a, 1);
        __sync_lock_release(&a);
        return 0;
    }
    "
    HAVE_GCC_SYNC_LOCK_TEST_AND_SET)


find_package(ALSA)
if (ALSA_FOUND)
    set(SDL_AUDIO_DRIVER_ALSA 1 CACHE INTERNAL "Enable ALSA audio driver")
    get_filename_component(ALSA_LIBRARY_FILE "${ALSA_LIBRARY}" NAME)
    set(SDL_AUDIO_DRIVER_ALSA_DYNAMIC "${ALSA_LIBRARY_FILE}" CACHE INTERNAL "ALSA dynamic library")
else (ALSA_FOUND)
    set(SDL_AUDIO_DRIVER_ALSA "" CACHE INTERNAL "Enable ALSA audio driver")
    set(SDL_AUDIO_DRIVER_ALSA_DYNAMIC "" CACHE INTERNAL "ALSA dynamic library")
endif (ALSA_FOUND)

find_package(PulseAudio)
if (PULSEAUDIO_FOUND)
    set(SDL_AUDIO_DRIVER_PULSEAUDIO 1 CACHE INTERNAL "Enable PulseAudio audio driver")
    set(SDL_AUDIO_DRIVER_PULSEAUDIO_DYNAMIC "${PULSEAUDIO_LIBRARY}" CACHE INTERNAL "PulseAudio dynamic library")
else (PULSEAUDIO_FOUND)
    set(SDL_AUDIO_DRIVER_PULSEAUDIO "" CACHE INTERNAL "Enable PulseAudio audio driver")
    set(SDL_AUDIO_DRIVER_PULSEAUDIO_DYNAMIC "" CACHE INTERNAL "PulseAudio dynamic library")
endif (PULSEAUDIO_FOUND)


CHECK_C_SOURCE_COMPILES("
    #include <linux/input.h>
    int main() {
        #ifndef EVIOCGNAME
        #error EVIOCGNAME() ioctl not available
        #endif
        return 0;
    }
    "
    SDL_INPUT_LINUXEV)

if (SDL_INPUT_LINUXEV AND NOT A320)
    set(SDL_JOYSTICK_LINUX 1 CACHE INTERNAL "Enable Linux joystick support")
    set(SDL_HAPTIC_LINUX 1 CACHE INTERNAL "Enable Linux haptick support")
endif (SDL_INPUT_LINUXEV AND NOT A320)

find_package(Threads)
if ((CMAKE_USE_PTHREADS_INIT AND NOT MINGW) OR ANDROID)
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} ${CMAKE_THREAD_LIBS_INIT})
    set(SDL_THREAD_PTHREAD 1 CACHE INTERNAL "Enable pthreads support")
    set(SDL_THREAD_PTHREAD_RECURSIVE_MUTEX 1 CACHE INTERNAL "Enable pthreads recursive mutex")

CHECK_C_SOURCE_COMPILES("
    #include <pthread.h>
    int main() {
        pthread_mutexattr_t attr;
        pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
        return 0;
    }
    "
    SDL_THREAD_PTHREAD_RECURSIVE_MUTEX)
endif((CMAKE_USE_PTHREADS_INIT AND NOT MINGW) OR ANDROID)

find_package(X11)
if (X11_FOUND)
    set(SDL_VIDEO_DRIVER_X11 1 CACHE INTERNAL "Enable X11 support")
    get_filename_component(X11_X11_LIB_FILE "${X11_X11_LIB}" NAME)
    set(SDL_VIDEO_DRIVER_X11_DYNAMIC "${X11_X11_LIB_FILE}" CACHE INTERNAL "X11 library file")
    get_filename_component(X11_Xext_LIB_FILE "${X11_Xext_LIB}" NAME)
    set(SDL_VIDEO_DRIVER_X11_DYNAMIC_XEXT "${X11_Xext_LIB_FILE}" CACHE INTERNAL "X11 extension library file")
    if (X11_Xcursor_FOUND)
        get_filename_component(X11_Xcursor_LIB_FILE "${X11_Xcursor_LIB}" NAME)
        set(SDL_VIDEO_DRIVER_X11_XCURSOR 1 CACHE INTERNAL "Enable X11 cursor")
        set(SDL_VIDEO_DRIVER_X11_DYNAMIC_XCURSOR "${X11_Xcursor_LIB_FILE}" CACHE INTERNAL "X11 cursor library file")
    endif (X11_Xcursor_FOUND)
    if (X11_Xinerama_FOUND)
        get_filename_component(X11_Xinerama_LIB_FILE "${X11_Xinerama_LIB}" NAME)
        set(SDL_VIDEO_DRIVER_X11_XINERAMA 1 CACHE INTERNAL "Enable X11 Xinerama")
        set(SDL_VIDEO_DRIVER_X11_DYNAMIC_XINERAMA "${X11_Xinerama_LIB_FILE}" CACHE INTERNAL "X11 Xinerama library file")
    endif (X11_Xinerama_FOUND)
    if (X11_Xinput_FOUND)
        get_filename_component(X11_Xinput_LIB_FILE "${X11_Xinput_LIB}" NAME)
        set(SDL_VIDEO_DRIVER_X11_XINPUT 1 CACHE INTERNAL "Enable X11 input")
        set(SDL_VIDEO_DRIVER_X11_DYNAMIC_XINPUT "${X11_Xinput_LIB_FILE}" CACHE INTERNAL "X11 input library file")
    endif (X11_Xinput_FOUND)
    if (X11_Xrandr_FOUND)
        get_filename_component(X11_Xrandr_LIB_FILE "${X11_Xrandr_LIB}" NAME)
        set(SDL_VIDEO_DRIVER_X11_XRANDR 1 CACHE INTERNAL "Enable X11 randr")
        set(SDL_VIDEO_DRIVER_X11_DYNAMIC_XRANDR "${X11_Xrandr_LIB_FILE}" CACHE INTERNAL "X11 randr library file")
    endif (X11_Xrandr_FOUND)
    if (X11_Xscreensaver_FOUND)
        get_filename_component(X11_Xscreensaver_LIB_FILE "${X11_Xscreensaver_LIB}" NAME)
        set(SDL_VIDEO_DRIVER_X11_XSCRNSAVER 1 CACHE INTERNAL "Enable X11 screensaver")
        set(SDL_VIDEO_DRIVER_X11_DYNAMIC_XSS "${X11_Xscreensaver_LIB_FILE}" CACHE INTERNAL "X11 library file")
    endif (X11_Xscreensaver_FOUND)
    get_filename_component(X11_Xext_LIB_FILE "${X11_Xext_LIB}" NAME)
    set(SDL_VIDEO_DRIVER_X11_DYNAMIC_XEXT "${X11_Xext_LIB_FILE}" CACHE INTERNAL "X11 library file")
    if (X11_Xshape_FOUND)
        set(SDL_VIDEO_DRIVER_X11_XSHAPE 1 CACHE INTERNAL "Enable X11 shape")
    endif (X11_Xshape_FOUND)

    include_directories("/usr/X11/include")
    set (CMAKE_REQUIRED_INCLUDES "/usr/X11/include")
    CHECK_C_SOURCE_COMPILES("
        #include <GL/gl.h>
        #include <GL/glx.h>
        int main() {
            return 0;
        }
        "
        HAVE_X11_OPENGL)
    if (HAVE_X11_OPENGL)
        set(SDL_VIDEO_OPENGL 1 CACHE INTERNAL "Enable OpenGL video driver")
        set(SDL_VIDEO_OPENGL_GLX 1 CACHE INTERNAL "Enable OpenGL GLX video driver")
        set(SDL_VIDEO_RENDER_OGL 1 CACHE INTERNAL "Enable OpenGL renderer")
    endif (HAVE_X11_OPENGL)
endif(X11_FOUND)

if (ANDROID)
endif (ANDROID)

if (A320)
    set (SDL_AUDIO_DRIVER_OSS 1 CACHE INTERNAL "Enable OSS audio driver")
    set (SDL_CPUINFO_DISABLED 1 CACHE INTERNAL "Disable CPU-info feature")
    set (SDL_HAPTIC_DISABLED 1 CACHE INTERNAL "Disable Haptic support")
    set (SDL_JOYSTICK_DUMMY 1 CACHE INTERNAL "Enable dummy joystick")
    set (SDL_VIDEO_DRIVER_DIRECTFB 1 CACHE INTERNAL "Enable DirectFB video driver")
    set (SDL_VIDEO_DRIVER_DIRECTFB_DYNAMIC "/usr/local/lib/libdirectfb-1.4.so.0" CACHE INTERNAL "Enable dynamic DirectFB video driver")
    set (SDL_VIDEO_RENDER_DIRECTFB 1 CACHE INTERNAL "Enable DirectFB render")
endif (A320)

if (APPLE)
    set(SDL_AUDIO_DRIVER_COREAUDIO 1 CACHE INTERNAL "Enable CoreAudio")
    set(SDL_VIDEO_DRIVER_DUMMY 1 CACHE INTERNAL "Enable dummy video driver")
    if (IOS)
        set(SDL_HAPTIC_DISABLED 1 CACHE INTERNAL "Disable Haptic support")
        set(SDL_LOADSO_DISABLED 1 CACHE INTERNAL "Disable dynamic loading support")
        set(SDL_THREAD_PTHREAD 1 CACHE INTERNAL "Enable pthread support")
        set(SDL_THREAD_PTHREAD_RECURSIVE_MUTEX 1 CACHE INTERNAL "Enable recursive mutex in pthread")
        set(SDL_VIDEO_DRIVER_UIKIT 1 CACHE INTERNAL "Enable UIKit video driver")
        set(SDL_VIDEO_OPENGL_ES 1 CACHE INTERNAL "Enable OpenGL ES video")
        set(SDL_VIDEO_RENDER_OGL_ES 1 CACHE INTERNAL "Enable OpenGL ES video render")
        #set(SDL_VIDEO_RENDER_OGL_ES2 1 CACHE INTERNAL "Enable OpenGL ES2 video render")
        set(SDL_POWER_UIKIT 1 CACHE INTERNAL "Enable UIKit power")
        set(SDL_IPHONE_KEYBOARD 1 CACHE INTERNAL "Enable iPhone keyboard support")
        set(SDL_IPHONE_MAX_GFORCE "5.0" CACHE INTERNAL "iPhone max recognized G-force from accelerometer")
    else(IOS)
        set(SDL_JOYSTICK_IOKIT 1 CACHE INTERNAL "Enable IOKit Joystick")
        set(SDL_HAPTIC_IOKIT 1 CACHE INTERNAL "Enable IOKit Haptic")
        set(SDL_POWER_MACOSX 1 CACHE INTERNAL "Enable Mac OS X power")
        set(SDL_VIDEO_OPENGL 1 CACHE INTERNAL "Enable OpenGL video driver")
        set(SDL_VIDEO_OPENGL_CGL 1 CACHE INTERNAL "Enable OpenGL CGL video driver")
        set(SDL_VIDEO_RENDER_OGL 1 CACHE INTERNAL "Enable OpenGL renderer")

        CHECK_OBJC_SOURCE_COMPILES("
        #import <Cocoa/Cocoa.h>
        int main() {
        return 0;
        }
        "
        HAVE_COCOA)
        if (HAVE_COCOA)
            set (SDL_VIDEO_DRIVER_COCOA 1 "Enable Cocoa video driver")
        endif (HAVE_COCOA)
    endif(IOS)
endif (APPLE)

if (UNIX)
    set(SDL_TIMER_UNIX "1" CACHE INTERNAL "Enable UNIX timer support")
endif (UNIX)

if (CMAKE_SYSTEM MATCHES Linux)
    set(SDL_POWER_LINUX "1" CACHE INTERNAL "Enable Linux power support")
endif (CMAKE_SYSTEM MATCHES Linux)

if (WIN32)
    set(SDL_POWER_WINDOWS "1" CACHE INTERNAL "Enable Windows power support")
    set(SDL_AUDIO_DRIVER_WINMM "1" CACHE INTERNAL "Enable WinMM audio support")
    set(SDL_LOADSO_WINDOWS "1" CACHE INTERNAL "Enable loadso for Windows")
    set(SDL_THREAD_WINDOWS "1" CACHE INTERNAL "Enable Windows threads")
    set(SDL_TIMER_WINDOWS "1" CACHE INTERNAL "Enable Windows timer")
    set(SDL_VIDEO_DRIVER_WINDOWS "1" CACHE INTERNAL "Enable Windows video")
    set(SDL_VIDEO_OPENGL "1" CACHE INTERNAL "Enable OpenGL video")
    set(SDL_VIDEO_OPENGL_WGL "1" CACHE INTERNAL "Enable Windows OpenGL video")
    set(SDL_VIDEO_RENDER_OGL "1" CACHE INTERNAL "Enable OpenGL render")

    CHECK_INCLUDE_FILES(d3d9.h HAVE_D3D9_H)
    CHECK_INCLUDE_FILES(dinput.h HAVE_DINPUT_H)
    CHECK_INCLUDE_FILES(dsound.h HAVE_DSOUND_H)

    if (HAVE_D3D9_H)
#        set (SDL_VIDEO_RENDER_D3D "1" CACHE INTERNAL "Enable Direc3D render")
    endif (HAVE_D3D9_H)

    if (HAVE_DINPUT_H)
        set (SDL_JOYSTICK_DINPUT "1" CACHE INTERNAL "Enable DirectInpt joystick support")
        set (SDL_HAPTIC_DINPUT "1" CACHE INTERNAL "Enable DirectInpt haptic support")
    else (HAVE_DINPUT_H)
        set (SDL_JOYSTICK_WINMM "1" CACHE INTERNAL "Enable WinMM joystick support")
        set(SDL_HAPTIC_DISABLED "1" CACHE INTERNAL "Disable haptick support")
    endif (HAVE_DINPUT_H)

    if (HAVE_DSOUND_H)
        set(SDL_AUDIO_DRIVER_DSOUND "1" CACHE INTERNAL "Enable DirectSound audio support")
    endif (HAVE_DSOUND_H)
endif (WIN32)

set(SDL_ASSEMBLY_ROUTINES "1" CACHE INTERNAL "Enable assembly routines support")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/include/SDL_config.h.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/SDL_config_cmake.h)
include_directories(${CMAKE_CURRENT_BINARY_DIR})


################################
# Source
################################
set(SDL2_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/src/SDL_assert.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/SDL.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/SDL_error.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/SDL_fatal.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/SDL_hints.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/SDL_log.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/atomic/SDL_atomic.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/atomic/SDL_spinlock.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/SDL_audio.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/SDL_audiocvt.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/SDL_audiodev.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/SDL_audiotypecvt.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/SDL_mixer.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/SDL_wave.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/dummy/SDL_dummyaudio.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpuinfo/SDL_cpuinfo.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/events/SDL_clipboardevents.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/events/SDL_dropevents.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/events/SDL_events.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/events/SDL_gesture.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/events/SDL_keyboard.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/events/SDL_mouse.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/events/SDL_quit.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/events/SDL_touch.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/events/SDL_windowevents.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/file/SDL_rwops.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/haptic/SDL_haptic.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/joystick/SDL_joystick.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/e_atan2.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/e_log.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/e_pow.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/e_rem_pio2.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/e_sqrt.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/k_cos.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/k_rem_pio2.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/k_sin.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/s_atan.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/s_copysign.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/s_cos.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/s_fabs.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/s_floor.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/s_scalbn.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libm/s_sin.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/power/SDL_power.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/render/SDL_render.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/render/SDL_yuv_mmx.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/render/SDL_yuv_sw.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/render/software/SDL_blendfillrect.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/render/software/SDL_blendline.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/render/software/SDL_blendpoint.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/render/software/SDL_drawline.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/render/software/SDL_drawpoint.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/render/software/SDL_render_sw.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/stdlib/SDL_getenv.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/stdlib/SDL_iconv.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/stdlib/SDL_malloc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/stdlib/SDL_qsort.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/stdlib/SDL_stdlib.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/stdlib/SDL_string.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/SDL_thread.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/timer/SDL_timer.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_blit_0.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_blit_1.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_blit_A.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_blit_auto.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_blit.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_blit_copy.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_blit_N.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_blit_slow.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_bmp.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_clipboard.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_fillrect.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_pixels.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_rect.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_RLEaccel.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_shape.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_stretch.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_surface.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/SDL_video.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/dummy/SDL_nullevents.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/dummy/SDL_nullframebuffer.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/video/dummy/SDL_nullvideo.c
)

if (ANDROID)
    add_definitions(-DGL_GLEXT_PROTOTYPES)
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} dl log)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/android/SDL_androidaudio.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/joystick/android/SDL_sysjoystick.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/android/SDL_androidevents.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/android/SDL_androidgl.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/android/SDL_androidkeyboard.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/android/SDL_androidtouch.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/android/SDL_androidvideo.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/android/SDL_androidwindow.c
    )
endif (ANDROID)

if (APPLE)
    if (IOS)
        set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework Foundation")
        set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework OpenGLES")
        list(APPEND SDL2_SOURCES
            ${CMAKE_CURRENT_SOURCE_DIR}/src/joystick/iphoneos/SDL_sysjoystick.m
            ${CMAKE_CURRENT_SOURCE_DIR}/src/joystick/iphoneos/SDLUIAccelerationDelegate.m
        )
    else (IOS)
        set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework Carbon")
        set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework OpenGL")
    endif(IOS)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/disk/SDL_diskaudio.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/file/cocoa/SDL_rwopsbundlesupport.m
    )
endif (APPLE)

if (CMAKE_SYSTEM MATCHES Linux AND NOT ANDROID)
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} GL)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/main/dummy/SDL_dummy_main.c
        )
endif (CMAKE_SYSTEM MATCHES Linux AND NOT ANDROID)

if (WIN32)
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} winmm OpenGL32)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/core/windows/SDL_windows.c
        )
endif (WIN32)

if (SDL_AUDIO_DRIVER_ALSA)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/alsa/SDL_alsa_audio.c
        )
endif (SDL_AUDIO_DRIVER_ALSA)

if (SDL_AUDIO_DRIVER_COREAUDIO)
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework AudioToolbox")
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework CoreAudio")
    if (NOT IOS)
        set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework AudioUnit")
    endif (NOT IOS)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/coreaudio/SDL_coreaudio.c
        )
endif (SDL_AUDIO_DRIVER_COREAUDIO)

if (SDL_AUDIO_DRIVER_DSOUND)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/directsound/SDL_directsound.c
        )
endif (SDL_AUDIO_DRIVER_DSOUND)

if (SDL_AUDIO_DRIVER_OSS)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/dsp/SDL_dspaudio.c
        )
endif (SDL_AUDIO_DRIVER_OSS)

if (SDL_AUDIO_DRIVER_PULSEAUDIO)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/pulseaudio/SDL_pulseaudio.c
        )
endif (SDL_AUDIO_DRIVER_PULSEAUDIO)

if (SDL_AUDIO_DRIVER_WINMM)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/audio/winmm/SDL_winmm.c
        )
endif (SDL_AUDIO_DRIVER_WINMM)

if (SDL_HAPTIC_DINPUT)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/haptick/windows/SDL_syshaptic.c
        )
endif (SDL_HAPTIC_DINPUT)

if (SDL_HAPTIC_DUMMY)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/haptic/dummy/SDL_syshaptic.c
        )
endif (SDL_HAPTIC_DUMMY)

if (SDL_HAPTIC_IOKIT)
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework ForceFeedback")
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/haptic/darwin/SDL_syshaptic.c
        )
endif (SDL_HAPTIC_IOKIT)

if (SDL_HAPTIC_LINUX)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/haptic/linux/SDL_syshaptic.c
        )
endif (SDL_HAPTIC_LINUX)

if (SDL_JOYSTICK_DINPUT)
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} -ldinput8 -ldxguid -ldxerr8)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/joystick/windows/SDL_dxjoystick.c
        )
endif (SDL_JOYSTICK_DINPUT)

if (SDL_JOYSTICK_IOKIT)
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework IOKit")
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/joystick/darwin/SDL_sysjoystick.c
        )
endif (SDL_JOYSTICK_IOKIT)

if (SDL_JOYSTICK_DUMMY)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/joystick/dummy/SDL_sysjoystick.c
        )
endif (SDL_JOYSTICK_DUMMY)

if (SDL_JOYSTICK_LINUX)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/joystick/linux/SDL_sysjoystick.c
        )
endif (SDL_JOYSTICK_LINUX)

if (SDL_JOYSTICK_WINMM)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/joystick/windows/SDL_mmjoystick.c
        )
endif (SDL_JOYSTICK_WINMM)

if (SDL_LOADSO_DLOPEN)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/loadso/dlopen/SDL_sysloadso.c
        )
endif (SDL_LOADSO_DLOPEN)

if (SDL_LOADSO_WINDOWS)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/loadso/windows/SDL_sysloadso.c
        )
endif (SDL_LOADSO_WINDOWS)

if (SDL_POWER_MACOSX)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/power/macosx/SDL_syspower.c
        )
endif (SDL_POWER_MACOSX)

if (SDL_POWER_LINUX)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/power/linux/SDL_syspower.c
        )
endif (SDL_POWER_LINUX)

if(SDL_POWER_UIKIT)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/power/uikit/SDL_syspower.m
        )
endif(SDL_POWER_UIKIT)

if (SDL_POWER_WINDOWS)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/power/windows/SDL_syspower.c
        )
endif (SDL_POWER_WINDOWS)

if (SDL_THREAD_PTHREAD)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/pthread/SDL_syscond.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/pthread/SDL_sysmutex.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/pthread/SDL_syssem.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/pthread/SDL_systhread.c
        )
elseif (SDL_THREAD_WINDOWS)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/generic/SDL_syscond.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/windows/SDL_sysmutex.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/windows/SDL_syssem.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/windows/SDL_systhread.c
        )
else (SDL_THREAD_PTHREAD)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/generic/SDL_syscond.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/generic/SDL_sysmutex.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/generic/SDL_syssem.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/thread/generic/SDL_systhread.c
        )
endif (SDL_THREAD_PTHREAD)

if (SDL_TIMER_UNIX)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/timer/unix/SDL_systimer.c
        )
endif (SDL_TIMER_UNIX)

if (SDL_TIMER_WINDOWS)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/timer/windows/SDL_systimer.c
        )
endif (SDL_TIMER_WINDOWS)

if (SDL_VIDEO_DRIVER_COCOA)
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework Cocoa")
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/cocoa/SDL_cocoaclipboard.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/cocoa/SDL_cocoamodes.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/cocoa/SDL_cocoashape.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/cocoa/SDL_cocoaevents.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/cocoa/SDL_cocoamouse.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/cocoa/SDL_cocoavideo.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/cocoa/SDL_cocoakeyboard.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/cocoa/SDL_cocoaopengl.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/cocoa/SDL_cocoawindow.m
        )
endif (SDL_VIDEO_DRIVER_COCOA)

if (SDL_VIDEO_DRIVER_DIRECTFB)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/directfb/SDL_DirectFB_WM.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/directfb/SDL_DirectFB_dyn.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/directfb/SDL_DirectFB_events.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/directfb/SDL_DirectFB_modes.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/directfb/SDL_DirectFB_mouse.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/directfb/SDL_DirectFB_opengl.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/directfb/SDL_DirectFB_render.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/directfb/SDL_DirectFB_shape.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/directfb/SDL_DirectFB_video.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/directfb/SDL_DirectFB_window.c
        )
endif (SDL_VIDEO_DRIVER_DIRECTFB)

if (SDL_VIDEO_DRIVER_WINDOWS)
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} version imm32)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/windows/SDL_windowsclipboard.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/windows/SDL_windowsevents.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/windows/SDL_windowsframebuffer.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/windows/SDL_windowskeyboard.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/windows/SDL_windowsmodes.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/windows/SDL_windowsmouse.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/windows/SDL_windowsopengl.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/windows/SDL_windowsshape.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/windows/SDL_windowsvideo.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/windows/SDL_windowswindow.c
        )
endif (SDL_VIDEO_DRIVER_WINDOWS)

if (SDL_VIDEO_DRIVER_UIKIT)
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework QuartzCore")
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework CoreGraphics")
    set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} "-framework UIKit")
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/uikit/jumphack.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/uikit/SDL_uikitappdelegate.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/uikit/SDL_uikitevents.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/uikit/SDL_uikitopengles.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/uikit/SDL_uikitopenglview.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/uikit/SDL_uikitvideo.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/uikit/SDL_uikitview.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/uikit/SDL_uikitviewcontroller.m
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/uikit/SDL_uikitwindow.m
    )
endif (SDL_VIDEO_DRIVER_UIKIT)

if (SDL_VIDEO_DRIVER_X11)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/imKStoUCS.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11clipboard.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11dyn.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11events.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11framebuffer.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11keyboard.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11modes.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11mouse.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11opengl.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11opengles.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11shape.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11touch.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11video.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/video/x11/SDL_x11window.c
        )
endif (SDL_VIDEO_DRIVER_X11)

if (SDL_VIDEO_RENDER_D3D)
    list (APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/render/direct3d/SDL_render_d3d.c
        )
endif (SDL_VIDEO_RENDER_D3D)

if (SDL_VIDEO_RENDER_OGL)
    list (APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/render/opengl/SDL_render_gl.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/render/opengl/SDL_shaders_gl.c
        )
endif (SDL_VIDEO_RENDER_OGL)

if (SDL_VIDEO_RENDER_OGL_ES)
    if (ANDROID)
        set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} GLESv1_CM)
    endif (ANDROID)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/render/opengles/SDL_render_gles.c
    )
endif (SDL_VIDEO_RENDER_OGL_ES)

if (SDL_VIDEO_RENDER_OGL_ES2)
    if (ANDROID)
        set(SDL2_DEPENDENCIES ${SDL2_DEPENDENCIES} GLESv2)
    endif (ANDROID)
    list(APPEND SDL2_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/render/opengles2/SDL_render_gles2.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/render/opengles2/SDL_shaders_gles2.c
    )
endif (SDL_VIDEO_RENDER_OGL_ES2)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

add_library(SDL2 STATIC ${SDL2_SOURCES})
target_link_libraries(SDL2 ${SDL2_DEPENDENCIES})

if (ANDROID)
    add_library(SDL2_jni SHARED EXCLUDE_FROM_ALL
        "${CMAKE_CURRENT_SOURCE_DIR}/src/core/android/SDL_android.cpp"
        )
    target_link_libraries(SDL2_jni SDL2)
endif (ANDROID)
