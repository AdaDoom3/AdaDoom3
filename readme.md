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

Using Git
---------

Github's tutorial on installing and using commandline git:
https://help.github.com/categories/54/articles

Frequent commands:

>    `git init`

>    `git config --global user.email ""`

>    `git config --global user.name ""`

>    `git remote add origin https://github.com/AdaDoom3/AdaDoom3.git`

>    `git pull origin master`

>    `git pull origin branch`

>    `git commit -am ''`

>    `git push origin master`

>    `git push origin master:branch`

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


Getting Started
---------------
To begin developing for AdaDoom3, first examine all of the “base” system and foundation components
in the /Code directory.

Each package in this folder (as all packages in Ada) consists of a specification (ads) and a body file (adb).
For clarity, everything but subprogram, package, protected type, and task bodies are placed in the
specification (including global variables and private function prototypes) allowing an entire overview of
user-important information in one file.

It is highly recommended to examine the foundation files first as they will be included at almost every
level in the design. They contain all of the basic components: data types, utilities for testing packages,
and redirectable text input and output. The data types file is especially notable, because it renames all
elementary Ada data types to be more descriptive by including each type’s byte size and range
(i.e. renaming Integer to Integer_4_Signed). Although it is possible to use the non-renamed data types
with the renamed ones interchangeably, it is not recommended.

More to come…

Organization
------------
The project is planned to be organized into the following layers, currently work is being done on the foundation and system layers for Windows.
* Foundation 
* System
* Library
* Video
* Audio
* Core
* Game

Outline of API. For comments see https://github.com/AdaDoom3/AdaDoom3/issues/21
* Main
* Neo
* Neo.Foundation
* Neo.Foundation.Text_IO
* Neo.Foundation.Data_Types
* Neo.Foundation.Package_Testing
* Neo.System
* Neo.System.Implementation
* Neo.System.Processor
* Neo.System.Processor.Implementation_For_Architecture
* Neo.System.Processor.Implementation_For_Operating_System
* Neo.System.Memory
* Neo.System.Memory.Implementation
* Neo.System.Text
* Neo.System.Text.Implementation
* Neo.System.Network
* Neo.System.Network.Implementation
* Neo.System.Input
* Neo.System.Input.Implementation
* Neo.System.OpenGL
* Neo.System.OpenGL.Implementation
* Neo.System.OpenGLES
* Neo.System.OpenGLES.Implementation
* Neo.System.DirectX
* Neo.System.DirectX.Implementation
* Neo.System.Sony
* Neo.System.Sony.Implementation
* Neo.System.Nintendo
* Neo.System.Nintendo.Implementation
* Neo.System.OpenAL
* Neo.System.OpenAL.Implementation
* Neo.System.XAudio
* Neo.System.XAudio.Implementation
* Neo.Library
* Neo.Library.Task_Jobs
* Neo.Library.Allocator
* Neo.Library.Byte_Ordering
* Neo.Library.Dynamic_String
* ...
* Neo.Library.Audio
* Neo.Library.Audio.OGG
* Neo.Library.Audio.WAV
* Neo.Library.Video
* Neo.Library.Video.OGV
* Neo.Library.Mathmatics
* Neo.Library.Matricies
* Neo.Library.Variables
* Neo.Library.Angles
* Neo.Library.Radix_64
* Neo.Library.Assembly
* Neo.Library.Assembly.x86_64
* Neo.Library.Assembly.PowerPC
* Neo.Library.Assembly.ARM
* ...
* Neo.Library.Timers
* Neo.Library.Vectors
* Neo.Library.Compression
* Neo.Library.Compression.ZIP
* Neo.Library.Graphics
* Neo.Library.Graphics.PNG
* Neo.Library.Graphics.JPEG
* Neo.Library.Graphics.TGA
* Neo.Library.Graphics.BMP
* Neo.Library.Graphics.GIF
* Neo.Library.Graphics.DXT
* Neo.Renderer
* Neo.Renderer.Shaders
* Neo.Renderer.Hardware_Textures
* Neo.Renderer.Decals
* Neo.Renderer.Definitions
* Neo.Renderer.Portals
* Neo.Renderer.Traces
* Neo.Renderer.Scene_Recorder
* Neo.Renderer.Materials
* Neo.Renderer.Menu
* Neo.Renderer.Resolution_Scale
* Neo.Renderer.Compact_Rectangle
* Neo.Renderer.Entity_Light_Interaction
* Neo.Renderer.Vertex_Cache
* Neo.Renderer.Platform
* Neo.Renderer.Platform.DirectX
* Neo.Renderer.Platform.OpenGL
* Neo.Renderer.Platform.OpenGLES
* Neo.Renderer.Platform.Sony
* Neo.Renderer.Platform.Nintendo
* Neo.Renderer.Model
* Neo.Renderer.Model.Sprites
* Neo.Renderer.Model.Particles
* Neo.Renderer.Model.Liquids
* Neo.Renderer.Model.Beams
* Neo.Renderer.Model.Players
* Neo.Renderer.Model.Maps
* Neo.Renderer.Model.Light_Wave
* Neo.Renderer.Model.Blender
* Neo.Renderer.Model.Maya
* Neo.Renderer.Model.Studio_Max
* Neo.Renderer.Execution
* Neo.Renderer.Execution.Front
* Neo.Renderer.Execution.Back
* Neo.Core
* Neo.Core.Debugging
* Neo.Core.Networking
* Neo.Core.Console
* Neo.Core.Commands
* Neo.Core.Impulses
* Neo.Core.Particles
* Neo.Core.Entities
* Neo.Core.Collisions
* Neo.Core.Special_Effects
* Neo.Core.Skins
* Neo.Core.Menu
* Neo.Core.Menu.Choices
* Neo.Core.Menu.Edits
* Neo.Core.Menu.Fields
* Neo.Core.Menu.Sliders
* Neo.Core.Menu.Renderers
* Neo.Game
* Neo.Game.Dialogs
* Neo.Game.Weapons
* Neo.Game.Triggers
* Neo.Game.Targets
* Neo.Game.Players
* Neo.Game.Achivements
* Neo.Game.Articulated_Figures
* Neo.Game.News
* Neo.Game.Friends
* Neo.Game.File
* Neo.Game.File.Bot_Awareness_Maps
* Neo.Game.File.Saves
* Neo.Game.File.Recordings
* Neo.Game.File.Localizations
* Neo.Game.File.Configurations
* -- ...
