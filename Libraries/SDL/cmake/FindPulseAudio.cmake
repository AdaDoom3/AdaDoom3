# Try to find the PulseAudio library
#
# Once done this will define:
#
#  PULSEAUDIO_FOUND - system has the PulseAudio library
#  PULSEAUDIO_INCLUDE_DIR - the PulseAudio include directory
#  PULSEAUDIO_LIBRARY - the libraries needed to use PulseAudio
#  PULSEAUDIO_MAINLOOP_LIBRARY - the libraries needed to use PulsAudio Mainloop
#
# The minimum required version of PulseAudio can be specified using the
# standard syntax, e.g. find_package(PulseAudio 1.0)
#
# Copyright (c) 2008, Matthias Kretz, <kretz@kde.org>
# Copyright (c) 2009, Marcus Hufgard, <Marcus.Hufgard@hufgard.de>
# Copyright (c) 2011, Colin Guthrie, <colin@mageia.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

# Support PULSEAUDIO_MINIMUM_VERSION for compatibility:
if(NOT PulseAudio_FIND_VERSION)
  set(PulseAudio_FIND_VERSION "${PULSEAUDIO_MINIMUM_VERSION}")
endif(NOT PulseAudio_FIND_VERSION)

# the minimum version of PulseAudio we require
if(NOT PulseAudio_FIND_VERSION)
  set(PulseAudio_FIND_VERSION "0.9.9")
endif(NOT PulseAudio_FIND_VERSION)

if (NOT WIN32)
   include(FindPkgConfig)
   pkg_check_modules(PC_PULSEAUDIO QUIET libpulse>=${PulseAudio_FIND_VERSION})
   pkg_check_modules(PC_PULSEAUDIO_MAINLOOP QUIET libpulse-mainloop-glib)
endif (NOT WIN32)

find_path(PULSEAUDIO_INCLUDE_DIR pulse/pulseaudio.h
   HINTS
   ${PC_PULSEAUDIO_INCLUDEDIR}
   ${PC_PULSEAUDIO_INCLUDE_DIRS}
   )

find_library(PULSEAUDIO_LIBRARY NAMES pulse-simple libpulse-simple pulse libpulse
   HINTS
   ${PC_PULSEAUDIO_LIBDIR}
   ${PC_PULSEAUDIO_LIBRARY_DIRS}
   )

find_library(PULSEAUDIO_MAINLOOP_LIBRARY NAMES pulse-mainloop pulse-mainloop-glib libpulse-mainloop-glib
   HINTS
   ${PC_PULSEAUDIO_LIBDIR}
   ${PC_PULSEAUDIO_LIBRARY_DIRS}
   )

if (PULSEAUDIO_VERSION)
   # Already in cache, be silent
   set(PULSEAUDIO_FIND_QUIETLY TRUE)
endif (PULSEAUDIO_VERSION)

if (PULSEAUDIO_INCLUDE_DIR AND NOT PULSEAUDIO_VERSION)
   # Use the separate major, minor, micro defines as these are more reliable than the combined string
   # which may change if/when the minor or micro bits are dropped...
   file(STRINGS "${PULSEAUDIO_INCLUDE_DIR}/pulse/version.h" pulse_version_h REGEX ".*define.+PA_MAJOR.+")
   string(REGEX REPLACE ".*define.+PA_M[A-Z]+[^0-9]+([0-9]+).*" "\\1" PULSEAUDIO_MAJOR "${pulse_version_h}")

   file(STRINGS "${PULSEAUDIO_INCLUDE_DIR}/pulse/version.h" pulse_version_h REGEX ".*define.+PA_MINOR.+")
   string(REGEX REPLACE ".*define.+PA_M[A-Z]+[^0-9]+([0-9]+).*" "\\1" PULSEAUDIO_MINOR "${pulse_version_h}")

   file(STRINGS "${PULSEAUDIO_INCLUDE_DIR}/pulse/version.h" pulse_version_h REGEX ".*define.+PA_MICRO.+")
   string(REGEX REPLACE ".*define.+PA_M[A-Z]+[^0-9]+([0-9]+).*" "\\1" PULSEAUDIO_MICRO "${pulse_version_h}")

   set(PULSEAUDIO_VERSION "${PULSEAUDIO_MAJOR}.${PULSEAUDIO_MINOR}.${PULSEAUDIO_MICRO}" CACHE STRING "Version number of PulseAudio" FORCE)
endif (PULSEAUDIO_INCLUDE_DIR AND NOT PULSEAUDIO_VERSION)

# NB find_package_handle_standard_args seems to fail horribly and not work

#include(FindPackageHandleStandardArgs)
#find_package_handle_standard_args(PulseAudio REQUIRED_VARS PULSEAUDIO_LIBRARY PULSEAUDIO_INCLUDE_DIR
#                                             VERSION_VAR PULSEAUDIO_VERSION )

if (PULSEAUDIO_INCLUDE_DIR AND PULSEAUDIO_LIBRARY)
  include(MacroEnsureVersion)
  macro_ensure_version("${PulseAudio_FIND_VERSION}" "${PULSEAUDIO_VERSION}" PULSEAUDIO_FOUND)
else (PULSEAUDIO_INCLUDE_DIR AND PULSEAUDIO_LIBRARY)
   set(PULSEAUDIO_FOUND FALSE)
endif (PULSEAUDIO_INCLUDE_DIR AND PULSEAUDIO_LIBRARY)

if (PULSEAUDIO_FOUND)
   if (NOT PULSEAUDIO_FIND_QUIETLY)
      message(STATUS "Found PulseAudio: ${PULSEAUDIO_LIBRARY}")
      if (PULSEAUDIO_MAINLOOP_LIBRARY)
          message(STATUS "Found PulseAudio Mainloop: ${PULSEAUDIO_MAINLOOP_LIBRARY}")
      else (PULSAUDIO_MAINLOOP_LIBRARY)
          message(STATUS "Could NOT find PulseAudio Mainloop Library")
      endif (PULSEAUDIO_MAINLOOP_LIBRARY)
   endif (NOT PULSEAUDIO_FIND_QUIETLY)
else (PULSEAUDIO_FOUND)
   message(STATUS "Could NOT find PulseAudio")
endif (PULSEAUDIO_FOUND)

mark_as_advanced(PULSEAUDIO_INCLUDE_DIR PULSEAUDIO_LIBRARY PULSEAUDIO_MAINLOOP_LIBRARY)
