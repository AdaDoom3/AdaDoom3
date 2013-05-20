AdaDoom3
========

NOTE: THIS PROJECT IS MOSTLY INCOMPLETE

The aim of Ada Doom3 is to re-engineer the id-tech 4 engine with a clear dependency hierarchy, and robust tasking systems along with many other improvements.

Planned features
----------------
* Load and play doom 3 bfg levels.
* Load Valve map workshop levels.
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

>  `gnatmake -I/$OS -I/$PLATFORM -I/$COMPILER -gnat12 main.adb`

Where

 * $OS - is your operating system (currently supported values: Windows)
 * $PLATFORM - is your processor type (currently supported values: x86-64)
 * $COMPILER - is your compiler (currently supported values: GNAT)

Use the `-g` compile flag to enable file/subprogram/line printing during stack traces (GNAT only). 

  Example for Windows:

>   `gnatmake -I/Windows -I/x86-64 -I/GNAT -g -gnat12 main.adb`
  
Build Status
------------
Jenkins polls the git repository hourly for any changes and regularly builds the project.

You can monitor the build status and build history on the following address: http://46.148.150.105:8080/

[![Build Status](http://46.148.150.105:8080/job/AdaDoom3-linux-x86-64/badge/icon)](http://46.148.150.105:8080/job/AdaDoom3-linux-x86-64/) Linux x86-64

[![Build Status](http://46.148.150.105:8080/job/AdaDoom3-windows-vista-x86/badge/icon)](http://46.148.150.105:8080/job/AdaDoom3-windows-vista-x86/) Windows Vista x86

Contact the maintainer at netprobe[at]gmail.com if you would like to add your machine as a build slave.

Using GPS/GPRBuild
------------------

An alternative to the compilation method above is to use GPRBuild:
>    `gprbuild adadoom3.gpr`

Or the Gnat Programming Studio IDE may be used (email is optional):
http://libre.adacore.com/download/

To compile with GPS click the gear and hammer button `✹` after ensuring the scenario drop boxes are correct.

After successfully compiling press play `▶` to run.

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
* Begin adding Linux/Mac support by implementing stubbed functions/subprograms in the following files
  * POSIX/neo-system-implementation.adb
  * POSIX/neo-system-memory-implementation.adb
  * POSIX/neo-system-processor-implementation_for_operating_system.adb

Author
------
exitcode.zero[at]gmail.com

License
-------
Unless otherwise noted here, all files and subprograms are GPLv3.
http://www.gnu.org/licenses/gpl-3.0.txt
