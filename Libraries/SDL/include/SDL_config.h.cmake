/*
  Simple DirectMedia Layer
  Copyright (C) 1997-2011 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*/

#ifndef _SDL_config_cmake_h
#define _SDL_config_cmake_h

/**
 *  \file SDL_config.h.cmake
 *
 *  This is a set of defines to configure the SDL features
 */

/* General platform specific identifiers */
#include "SDL_platform.h"

/* C language features */
#undef const
#undef inline
#undef volatile

/* C datatypes */
#cmakedefine SIZEOF_VOIDP @SIZEOF_VOIDP@
#cmakedefine HAVE_GCC_ATOMICS 1
#cmakedefine HAVE_GCC_SYNC_LOCK_TEST_AND_SET 1
#undef HAVE_PTHREAD_SPINLOCK

/* Useful headers */
#cmakedefine HAVE_ALLOCA_H 1
#cmakedefine HAVE_SYS_TYPES_H 1
#cmakedefine HAVE_STDIO_H 1
#cmakedefine STDC_HEADERS 1
#cmakedefine HAVE_STDLIB_H 1
#cmakedefine HAVE_STDARG_H 1
#cmakedefine HAVE_MALLOC_H 1
#cmakedefine HAVE_MEMORY_H 1
#cmakedefine HAVE_STRING_H 1
#cmakedefine HAVE_STRINGS_H 1
#cmakedefine HAVE_INTTYPES_H 1
#cmakedefine HAVE_STDINT_H 1
#cmakedefine HAVE_CTYPE_H 1
#cmakedefine HAVE_MATH_H 1
#cmakedefine HAVE_ICONV_H 1
#cmakedefine HAVE_SIGNAL_H 1
#cmakedefine HAVE_ALTIVEC_H 1
#cmakedefine HAVE_PTHREAD_NP_H 1

/* C library functions */
#cmakedefine HAVE_MALLOC 1
#cmakedefine HAVE_CALLOC 1
#cmakedefine HAVE_REALLOC 1
#cmakedefine HAVE_FREE 1
#cmakedefine HAVE_ALLOCA 1
#ifndef __WIN32__ /* Don't use C runtime versions of these on Windows */
#cmakedefine HAVE_GETENV 1
#cmakedefine HAVE_SETENV 1
#cmakedefine HAVE_PUTENV 1
#cmakedefine HAVE_UNSETENV 1
#endif
#cmakedefine HAVE_QSORT 1
#cmakedefine HAVE_ABS 1
#cmakedefine HAVE_BCOPY 1
#cmakedefine HAVE_MEMSET 1
#cmakedefine HAVE_MEMCPY 1
#cmakedefine HAVE_MEMMOVE 1
#cmakedefine HAVE_MEMCMP 1
#cmakedefine HAVE_STRLEN 1
#cmakedefine HAVE_STRLCPY 1
#cmakedefine HAVE_STRLCAT 1
#cmakedefine HAVE_STRDUP 1
#cmakedefine HAVE__STRREV 1
#cmakedefine HAVE__STRUPR 1
#cmakedefine HAVE__STRLWR 1
#cmakedefine HAVE_INDEX 1
#cmakedefine HAVE_RINDEX 1
#cmakedefine HAVE_STRCHR 1
#cmakedefine HAVE_STRRCHR 1
#cmakedefine HAVE_STRSTR 1
#cmakedefine HAVE_ITOA 1
#cmakedefine HAVE__LTOA 1
#cmakedefine HAVE__UITOA 1
#cmakedefine HAVE__ULTOA 1
#cmakedefine HAVE_STRTOL 1
#cmakedefine HAVE_STRTOUL 1
#cmakedefine HAVE__I64TOA 1
#cmakedefine HAVE__UI64TOA 1
#cmakedefine HAVE_STRTOLL 1
#cmakedefine HAVE_STRTOULL 1
#cmakedefine HAVE_STRTOD 1
#cmakedefine HAVE_ATOI 1
#cmakedefine HAVE_ATOF 1
#cmakedefine HAVE_STRCMP 1
#cmakedefine HAVE_STRNCMP 1
#cmakedefine HAVE__STRICMP 1
#cmakedefine HAVE_STRCASECMP 1
#cmakedefine HAVE__STRNICMP 1
#cmakedefine HAVE_STRNCASECMP 1
#cmakedefine HAVE_SSCANF 1
#cmakedefine HAVE_SNPRINTF 1
#cmakedefine HAVE_VSNPRINTF 1
#cmakedefine HAVE_M_PI 1
#cmakedefine HAVE_ATAN 1
#cmakedefine HAVE_ATAN2 1
#cmakedefine HAVE_CEIL 1
#cmakedefine HAVE_COPYSIGN 1
#cmakedefine HAVE_COS 1
#cmakedefine HAVE_COSF 1
#cmakedefine HAVE_FABS 1
#cmakedefine HAVE_FLOOR 1
#cmakedefine HAVE_LOG 1
#cmakedefine HAVE_POW 1
#cmakedefine HAVE_SCALBN 1
#cmakedefine HAVE_SIN 1
#cmakedefine HAVE_SINF 1
#cmakedefine HAVE_SQRT 1
#cmakedefine HAVE_SIGACTION 1
#cmakedefine HAVE_SA_SIGACTION 1
#cmakedefine HAVE_SETJMP 1
#cmakedefine HAVE_NANOSLEEP 1
#cmakedefine HAVE_SYSCONF 1
#cmakedefine HAVE_SYSCTLBYNAME 1
#cmakedefine HAVE_CLOCK_GETTIME 1
#cmakedefine HAVE_GETPAGESIZE 1
#cmakedefine HAVE_MPROTECT 1
#cmakedefine HAVE_ICONV 1
#cmakedefine HAVE_PTHREAD_SETNAME_NP 1
#cmakedefine HAVE_PTHREAD_SET_NAME_NP 1
#cmakedefine HAVE_SEM_TIMEDWAIT

/* SDL internal assertion support */
#undef SDL_DEFAULT_ASSERT_LEVEL

/* Allow disabling of core subsystems */
#undef SDL_ATOMIC_DISABLED
#undef SDL_AUDIO_DISABLED
#cmakedefine SDL_CPUINFO_DISABLED 1
#undef SDL_EVENTS_DISABLED
#undef SDL_FILE_DISABLED
#undef SDL_JOYSTICK_DISABLED
#cmakedefine SDL_HAPTIC_DISABLED 1
#undef SDL_LOADSO_DISABLED
#undef SDL_RENDER_DISABLED
#undef SDL_THREADS_DISABLED
#undef SDL_TIMERS_DISABLED
#undef SDL_VIDEO_DISABLED
#undef SDL_POWER_DISABLED

/* Enable various audio drivers */
#cmakedefine SDL_AUDIO_DRIVER_ANDROID 1
#cmakedefine SDL_AUDIO_DRIVER_ALSA 1
#cmakedefine SDL_AUDIO_DRIVER_ALSA_DYNAMIC "@SDL_AUDIO_DRIVER_ALSA_DYNAMIC@"
#undef SDL_AUDIO_DRIVER_ARTS
#undef SDL_AUDIO_DRIVER_ARTS_DYNAMIC
#cmakedefine SDL_AUDIO_DRIVER_PULSEAUDIO 1
#cmakedefine SDL_AUDIO_DRIVER_PULSEAUDIO_DYNAMIC "@SDL_AUDIO_DRIVER_PULSEAUDIO_DYNAMIC@"
#undef SDL_AUDIO_DRIVER_BEOSAUDIO
#undef SDL_AUDIO_DRIVER_BSD
#cmakedefine SDL_AUDIO_DRIVER_COREAUDIO 1
#undef SDL_AUDIO_DRIVER_DISK
#undef SDL_AUDIO_DRIVER_DUMMY
#undef SDL_AUDIO_DRIVER_XAUDIO2
#cmakedefine SDL_AUDIO_DRIVER_DSOUND 1
#undef SDL_AUDIO_DRIVER_ESD
#undef SDL_AUDIO_DRIVER_ESD_DYNAMIC
#undef SDL_AUDIO_DRIVER_NAS
#undef SDL_AUDIO_DRIVER_NAS_DYNAMIC
#undef SDL_AUDIO_DRIVER_NDS
#cmakedefine SDL_AUDIO_DRIVER_OSS 1
#undef SDL_AUDIO_DRIVER_OSS_SOUNDCARD_H
#undef SDL_AUDIO_DRIVER_PAUDIO
#undef SDL_AUDIO_DRIVER_QSA
#undef SDL_AUDIO_DRIVER_SUNAUDIO
#cmakedefine SDL_AUDIO_DRIVER_WINMM 1
#undef SDL_AUDIO_DRIVER_FUSIONSOUND
#undef SDL_AUDIO_DRIVER_FUSIONSOUND_DYNAMIC

/* Enable various input drivers */
#cmakedefine SDL_INPUT_LINUXEV 1
#undef SDL_INPUT_TSLIB
#cmakedefine SDL_JOYSTICK_ANDROID 1
#undef SDL_JOYSTICK_BEOS
#cmakedefine SDL_JOYSTICK_DINPUT 1
#cmakedefine SDL_JOYSTICK_DUMMY 1
#cmakedefine SDL_JOYSTICK_IOKIT 1
#cmakedefine SDL_JOYSTICK_LINUX 1
#undef SDL_JOYSTICK_NDS
#cmakedefine SDL_JOYSTICK_WINMM 1
#undef SDL_JOYSTICK_USBHID
#undef SDL_JOYSTICK_USBHID_MACHINE_JOYSTICK_H
#cmakedefine SDL_HAPTIC_DUMMY 1
#cmakedefine SDL_HAPTIC_LINUX 1
#cmakedefine SDL_HAPTIC_IOKIT 1
#cmakedefine SDL_HAPTIC_DINPUT 1

/* Enable various shared object loading systems */
#undef SDL_LOADSO_BEOS
#cmakedefine SDL_LOADSO_DLOPEN 1
#undef SDL_LOADSO_DUMMY
#undef SDL_LOADSO_LDG
#cmakedefine SDL_LOADSO_WINDOWS 1

/* Enable various threading systems */
#undef SDL_THREAD_BEOS
#undef SDL_THREAD_NDS
#cmakedefine SDL_THREAD_PTHREAD 1
#cmakedefine SDL_THREAD_PTHREAD_RECURSIVE_MUTEX 1
#undef SDL_THREAD_PTHREAD_RECURSIVE_MUTEX_NP
#cmakedefine SDL_THREAD_WINDOWS 1

/* Enable various timer systems */
#undef SDL_TIMER_BEOS
#undef SDL_TIMER_DUMMY
#undef SDL_TIMER_NDS
#cmakedefine SDL_TIMER_UNIX 1
#cmakedefine SDL_TIMER_WINDOWS 1
#undef SDL_TIMER_WINCE

/* Enable various video drivers */
#cmakedefine SDL_VIDEO_DRIVER_ANDROID 1
#undef SDL_VIDEO_DRIVER_BWINDOW
#cmakedefine SDL_VIDEO_DRIVER_COCOA 1
#cmakedefine SDL_VIDEO_DRIVER_DIRECTFB 1
#cmakedefine SDL_VIDEO_DRIVER_DIRECTFB_DYNAMIC "@SDL_VIDEO_DRIVER_DIRECTFB_DYNAMIC@"
#cmakedefine SDL_VIDEO_DRIVER_DUMMY 1
#undef SDL_VIDEO_DRIVER_NDS
#cmakedefine SDL_VIDEO_DRIVER_UIKIT 1
#cmakedefine SDL_VIDEO_DRIVER_WINDOWS 1
#cmakedefine SDL_VIDEO_DRIVER_X11 1
#cmakedefine SDL_VIDEO_DRIVER_X11_DYNAMIC "@SDL_VIDEO_DRIVER_X11_DYNAMIC@"
#cmakedefine SDL_VIDEO_DRIVER_X11_DYNAMIC_XEXT "@SDL_VIDEO_DRIVER_X11_DYNAMIC_XEXT@"
#cmakedefine SDL_VIDEO_DRIVER_X11_DYNAMIC_XCURSOR "@SDL_VIDEO_DRIVER_X11_DYNAMIC_XCURSOR@"
#cmakedefine SDL_VIDEO_DRIVER_X11_DYNAMIC_XINERAMA "@SDL_VIDEO_DRIVER_X11_DYNAMIC_XINERAMA@"
#cmakedefine SDL_VIDEO_DRIVER_X11_DYNAMIC_XINPUT "@SDL_VIDEO_DRIVER_X11_DYNAMIC_XINPUT@"
#cmakedefine SDL_VIDEO_DRIVER_X11_DYNAMIC_XRANDR "@SDL_VIDEO_DRIVER_X11_DYNAMIC_XRANDR@"
#cmakedefine SDL_VIDEO_DRIVER_X11_DYNAMIC_XSS "@SDL_VIDEO_DRIVER_X11_DYNAMIC_XSS@"
#cmakedefine SDL_VIDEO_DRIVER_X11_DYNAMIC_XVIDMODE "@SDL_VIDEO_DRIVER_X11_DYNAMIC_XVIDMODE@"
#cmakedefine SDL_VIDEO_DRIVER_X11_XCURSOR 1
#cmakedefine SDL_VIDEO_DRIVER_X11_XINERAMA 1
#cmakedefine SDL_VIDEO_DRIVER_X11_XINPUT 1
#cmakedefine SDL_VIDEO_DRIVER_X11_XRANDR 1
#cmakedefine SDL_VIDEO_DRIVER_X11_XSCRNSAVER 1
#cmakedefine SDL_VIDEO_DRIVER_X11_XSHAPE 1
#cmakedefine SDL_VIDEO_DRIVER_X11_XVIDMODE 1

#cmakedefine SDL_VIDEO_RENDER_D3D 1
#cmakedefine SDL_VIDEO_RENDER_OGL 1
#cmakedefine SDL_VIDEO_RENDER_OGL_ES 1
#cmakedefine SDL_VIDEO_RENDER_OGL_ES2  1
#cmakedefine SDL_VIDEO_RENDER_DIRECTFB 1

/* Enable OpenGL support */
#cmakedefine SDL_VIDEO_OPENGL 1
#cmakedefine SDL_VIDEO_OPENGL_ES 1
#undef SDL_VIDEO_OPENGL_BGL
#cmakedefine SDL_VIDEO_OPENGL_CGL 1
#cmakedefine SDL_VIDEO_OPENGL_GLX 1
#cmakedefine SDL_VIDEO_OPENGL_WGL 1
#undef SDL_VIDEO_OPENGL_OSMESA
#undef SDL_VIDEO_OPENGL_OSMESA_DYNAMIC

/* Enable system power support */
#cmakedefine SDL_POWER_LINUX 1
#cmakedefine SDL_POWER_WINDOWS 1
#cmakedefine SDL_POWER_MACOSX 1
#undef SDL_POWER_BEOS
#undef SDL_POWER_NINTENDODS
#undef SDL_POWER_HARDWIRED
#cmakedefine SDL_POWER_UIKIT 1

/* iPhone settings */
#cmakedefine SDL_IPHONE_KEYBOARD 1
#cmakedefine SDL_IPHONE_MAX_GFORCE @SDL_IPHONE_MAX_GFORCE@

/* Enable assembly routines */
#cmakedefine SDL_ASSEMBLY_ROUTINES 1
#undef SDL_ALTIVEC_BLITTERS

#endif /* _SDL_config_cmake_h */
