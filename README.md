AdaDoom3
========

[![Join the chat at https://gitter.im/AdaDoom3/Lobby](https://badges.gitter.im/AdaDoom3/Lobby.svg)](https://gitter.im/AdaDoom3/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

NOTE: THIS PROJECT IS MOSTLY INCOMPLETE

The aim of Ada Doom3 is to re-engineer the id-tech 4 engine with a clear dependency
hierarchy, and robust tasking systems.

Planned Features:
- Load and play doom 3 bfg levels
- Load Valve map workshop levels
- Native ports for Windows and SDL
- Input support for XBox 360 controllers
- Multi-monitor windowing
- Threaded rendering with Vulkan
- Responsive multi-player
- Steam support for user accounts and achievements

## Compiling

To compile with GPRBuild:

`gprbuild -p -P neo.gpr `

Or the Gnat Programming Studio IDE may be used (email is optional):
http://libre.adacore.com/download/

To compile with GPS click the gear and hammer button after ensuring the
scenario drop boxes are correct.

After successfully compiling press play ▶ to run.

## Using Git

Github's tutorial on installing and using commandline git:
https://help.github.com/categories/54/articles

Frequent commands:

`git init`

`git config --global user.email ""`

`git config --global user.name ""`

`git remote add origin https://github.com/AdaDoom3/AdaDoom3.git`

`git pull origin master`

`git pull origin branch`

`git commit -am ''`

`git push origin master`

`git push origin master:branch`

## Contributing

Begin adding Linux/Mac support by implementing stubbed functions/subprograms

## Getting Started

To begin developing for AdaDoom3, first examine all of the “base” system and
foundation components in the /Code directory.

Each package in this folder (as all packages in Ada) consists of a specification (ads) and
a body file (adb). For clarity, everything but subprogram, package, protected type, and task
bodies are placed in the specification (including global variables and private function
prototypes) allowing an entire overview of user-important information in one file.

More to come...

## Design
