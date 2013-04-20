AdaDoom3
========

NOTE: THIS PROJECT IS MOSTLY INCOMPLETE

The aim of Ada Doom3 is to re-engineer the id-tech 4 engine with a clear dependency hierarchy, and robust tasking systems along with many other improvements.

Planned features
----------------
* Load and play doom 3 bfg levels.
* Native ports for Windows, Linux X Windows, Mac OS, and SDL.
* Input support for generic and all major game-pads.
* Multi-monitor windowing.
* Threaded rendering with support for using multiple graphics cards.
* SIMD math library.
* Responsive multi-player.
* Steam support for user accounts and achievements.

Dependencies
------------
This code uses Ada 2012 features, your compiler will have to at least support the aspect with clause.

Compiling
---------
In order to compile the code issue the following command:

  `gnatmake -I/$OS -I/$PLATFORM -I/$COMPILER -gnat12 main.adb`

Where

 * $OS - is your operating system (currently supported values: Windows)
 * $PLATFORM - is your architecture (currently supported values: x86_64)
 * $COMPILER - is your compiler (currently supported values: GNAT)

To enable file/subprogram/line printing during stack traces the `-g` compile flag must be used (GNAT only). 

  Example for Windows:

>   `gnatmake -I/Windows -I/x86_64 -I/GNAT -g -gnat12 main.adb`
  
Using GPS/GPRBuild
------------------

An alternative to the compilation method above is to use GPRBuild:
>    `gprbuild adadoom3.gpr`

Or the Gnat Programming Studio IDE may be used, and to compile therein simply click the triangular `Play` button after ensuring that the scenario drop-down boxes have the correct values.

Organization
------------
The project is organized into the following layers, currently work is being done on the foundation and system layers for Windows.
* Foundation 
* System
* Library
* Video
* Audio
* Core
* Game

Roadmap
-------
* Raw input implementation.
* Multi-monitor testing.
* OpenGL dll loading.
* Audio via XAudio2.
* Windows specific network implementation.
* Move on to Library layer.

Tasks
-----
The following are small tasks that are currently needed
* Begin adding Linux support by implementing stubbed functions/subprograms in the following files
  * Linux/neo-system-implementation.adb
  * Linux/neo-system-memory-implementation.adb
  * Linux/neo-system-processor-implementation_for_operating_system.adb

Author
------
jsquirek1[at]student.gsu.edu

License
-------
Unless otherwise noted here, all files and subprograms are GPLv3.
http://www.gnu.org/licenses/gpl-3.0.txt
