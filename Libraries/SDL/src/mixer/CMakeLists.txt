################################
# Source
################################
option(SDL_MIXER_MUSIC_OGG "Enable Ogg Vorbis music" ON)
option(SDL_MIXER_MUSIC_OGG_SHARED "Dynamically load Ogg Vorbis support" ON)
if (SDL_MIXER_MUSIC_OGG)
    add_definitions(-DOGG_MUSIC)

    find_library(VORBIS_LIBRARY
        NAMES vorbis Vorbis VORBIS
        PATHS
        $ENV{VORBISDIR}/lib
        $ENV{VORBISDIR}
        $ENV{OGGDIR}/lib
        $ENV{OGGDIR}
        $ENV{SDLSOUNDDIR}/lib
        $ENV{SDLSOUNDDIR}
        $ENV{SDLDIR}/lib
        $ENV{SDLDIR}
        /usr/local/lib
        /usr/lib
        /sw/lib
        /opt/local/lib
        /opt/csw/lib
        /opt/lib
        )

    find_library(VORBISFILE_LIBRARY
        NAMES vorbisfile VorbisFile VORBISFILE
        PATHS
        $ENV{OGGDIR}/lib
        $ENV{OGGDIR}
        $ENV{VORBISDIR}/lib
        $ENV{VORBISDIR}
        $ENV{SDLSOUNDDIR}/lib
        $ENV{SDLSOUNDDIR}
        $ENV{SDLDIR}/lib
        $ENV{SDLDIR}
        /usr/local/lib
        /usr/lib
        /sw/lib
        /opt/local/lib
        /opt/csw/lib
        /opt/lib
        )
    if (VORBISFILE_LIBRARY AND NOT A320)
        if (SDL_MIXER_MUSIC_OGG_SHARED)
            add_definitions(-DOGG_DYNAMIC=\"${VORBISFILE_LIBRARY}\")
        else (SDL_MIXER_MUSIC_OGG_SHARED)
            set(SDL2_MIXER_DEPENDENCIES ${SDL2_MIXER_DEPENDENCIES} ${VORBIS_LIBRARY} ${VORBISFILE_LIBRARY})
        endif (SDL_MIXER_MUSIC_OGG_SHARED)
    else (VORBISFILE_LIBRARY AND NOT A320)
        include_directories(${PROJECT_SOURCE_DIR}/lib)
        add_definitions(-DOGG_USE_TREMOR)
        set(SDL2_MIXER_DEPENDENCIES ${SDL2_MIXER_DEPENDENCIES} tremor)
        if (A320)
            message(STATUS "Using tremor for this platform")
        else(A320)
            message(WARNING "Vorbis library not found; using bundled tremor library")
        endif (A320)
    endif (VORBISFILE_LIBRARY AND NOT A320)
endif (SDL_MIXER_MUSIC_OGG)

set(SDL2_MIXER_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/dynamic_flac.c
    ${CMAKE_CURRENT_SOURCE_DIR}/dynamic_flac.c
    ${CMAKE_CURRENT_SOURCE_DIR}/dynamic_fluidsynth.c
    ${CMAKE_CURRENT_SOURCE_DIR}/dynamic_mod.c
    ${CMAKE_CURRENT_SOURCE_DIR}/dynamic_mp3.c
    ${CMAKE_CURRENT_SOURCE_DIR}/dynamic_ogg.c
    ${CMAKE_CURRENT_SOURCE_DIR}/effect_position.c
    ${CMAKE_CURRENT_SOURCE_DIR}/effect_stereoreverse.c
    ${CMAKE_CURRENT_SOURCE_DIR}/effects_internal.c
    ${CMAKE_CURRENT_SOURCE_DIR}/fluidsynth.c
    ${CMAKE_CURRENT_SOURCE_DIR}/load_aiff.c
    ${CMAKE_CURRENT_SOURCE_DIR}/load_flac.c
    ${CMAKE_CURRENT_SOURCE_DIR}/load_ogg.c
    ${CMAKE_CURRENT_SOURCE_DIR}/load_voc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/mixer.c
    ${CMAKE_CURRENT_SOURCE_DIR}/music.c
    ${CMAKE_CURRENT_SOURCE_DIR}/music_cmd.c
    ${CMAKE_CURRENT_SOURCE_DIR}/music_flac.c
    ${CMAKE_CURRENT_SOURCE_DIR}/music_mad.c
    ${CMAKE_CURRENT_SOURCE_DIR}/music_mod.c
    ${CMAKE_CURRENT_SOURCE_DIR}/music_modplug.c
    ${CMAKE_CURRENT_SOURCE_DIR}/music_ogg.c
    #    ${CMAKE_CURRENT_SOURCE_DIR}/playmus.c
    #    ${CMAKE_CURRENT_SOURCE_DIR}/playwave.c
    ${CMAKE_CURRENT_SOURCE_DIR}/wavestream.c
    )

include_directories(${PROJECT_BINARY_DIR}/lib/SDL2)
include_directories(${PROJECT_SOURCE_DIR}/lib/SDL2/include)
add_library(SDL2_mixer STATIC ${SDL2_MIXER_SOURCES})
target_link_libraries(SDL2_mixer SDL2 ${SDL2_MIXER_DEPENDENCIES})
