AdaDoom3
========

NOTE: THIS PROJECT IS MOSTLY INCOMPLETE AND DOES NOT COMPILE CURRENTLY

The aim of Ada Doom3 is to re-engineer the id-tech 4 engine with a clear dependancy hierarchy, and robust tasking systems along with many other improvments.

Planned features
----------------
* Load and play doom 3 bfg levels
* Native ports for Windows, Linux X Windows, Mac OS, and SDL
* Input support for generic and all major gamepads
* Multi-monitor windowing
* Threaded rendering with support for using multiple graphics cards
* SIMD math library
* Responsive multiplayer
* Steam support for user accounts and achievements

Dependencies
------------
This code uses Ada 2012 features, your compiler will have to at least support the aspect with clause.

Compiling
---------
In order to compile the code issue the following command:

  `gnatmake -l/$OS -l/$PLATFORM -gnat12 main`

Where

 $OS - is your operating system (currently supported values: Windows)

 $PLATFORM - is your architecture (currently supported values: x86_64)

  example for Windows:

  `gnatmake -l/Windows -l/x86_64 -gnat12 main`

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
* Raw input implementation
* Multi-monitor testing
* OpenGL dll loading
* Windows specific network implementation
* Move on to Library layer

Author
------
jsquirek1[at]student.gsu.edu

License
-------
Unless otherwise noted here, all files and subprograms are GPLv3.
http://www.gnu.org/licenses/gpl-3.0.txt
