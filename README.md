Ada Doom 3
==========

NOTE: THIS PROJECT IS MOSTLY INCOMPLETE

## Ultimate goal

To create a game engine capable of loading Doom 3: BFG Edition levels -- including maps (.map, .proc, .aas), images (.tga), and models (.md5~, .lwo, .mb).

## Additional goals

- Font (Post Script Type 1) and vector (.svg) loading for higher fidelity user interface components.
- Cross-platform (including mobile via SDL)
- Steam community support
- Support for other common media formats may be implemented

Although id Tech 4 makes heavy use of scripting for both game logic and user interfaces I have no plans to implement this; instead, the script logic will be re-written in compiled code.

In all, I hope to at least finish the Windows version.

## Build system

Currently the build system consists of a single Gnat project file which has options according to system requirements as well as game code. The organization and naming of files is based on how the Gnat project handles dependencies where each directory represents one option.

The long term plan is to have a build system which adds on to the project file by identifying system requirements that can run on the three major OSs (Mac, Windows, and Linux) where installing build tools, compiling the engine, and packaging a binary can be done via OS GUI.

To compile with GPRBuild
`gprbuild adadoom3.gpr`

The Gnat Programming Studio IDE (GPS) may be used (email is optional):
http://libre.adacore.com/download/

To compile with GPS click the gear and hammer button `✹` after ensuring the
scenario drop boxes are correct. After successfully compiling press play `▶` to run.

## Contributing

Begin adding Linux/Mac support by implementing stubbed functions/subprograms, as well as writting parsers for unimplemented file formats. The only style requirements are that the naming conventions match the current scheme and tabs are converted to spaces.

## Design

The engine divides into 5 layers. The foundation at the root contains all standard language declarations. The following level, file (neo-file~), contains all external data structures. Above that, the system layer exists solely to isolate API, architecture, and operating system (neo-system~). At the top, the world layer (neo-world~) contains all other functionality needed by the engine except game specific code (which goes on the highest layer neo-game~).

The system (through Neo.System.Graphics.Window.Run and Neo.System.Input) should run with one task per video card in use for multi monitor support and one task for input polling. The main program will also create a separate thread for game logic. All other tasks will be spawned by the game logic and used as needed – probably for processing light and shadow interactions.

...

## Getting Started

To begin developing for AdaDoom3, first examine all of the “base” system and foundation components in the /Engine directory.

Each package in this folder (as all packages in Ada) consists of a specification (ads) and a body file (adb). For clarity, everything but subprogram, package, protected type, and task bodies are placed in the specification (including global variables and private function prototypes) allowing an entire overview of user-important information in one file.

It is highly recommended to examine the foundation files first as they will be included at almost every level in the design. They contain all of the basic components: data types, utilities for testing packages, and redirect-able text input and output. The data types file is especially notable, because it renames all elementary Ada data types to be more descriptive by including each type’s byte size and range (i.e. renaming Integer to Integer_4_Signed). Although it is possible to use the non-renamed data types with the renamed ones interchangeably, it is not recommended.

Github's tutorial on installing and using commandline git
https://help.github.com/categories/54/articles

Frequent commands
- `git init`
- `git config --global user.email ""`
- `git config --global user.name ""`
- `git remote add origin https://github.com/AdaDoom3/AdaDoom3.git`
- `git pull origin master`
- `git pull origin branch`
- `git commit -am ''`
- `git push origin master`
- `git push origin master:branch`

...


