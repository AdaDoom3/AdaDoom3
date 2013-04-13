AdaDoom3 - work in progress
========

Dependencies
------------
This code uses Ada 2012 features, your compiler will have to at least support the aspect with clause.

Compile
-------
In order to compile the code issue the following command:

  `gnatmake -l/$OS -l/$PLATFORM -gnat12 main`

Where:

 $OS - is your operating system (currently supported values: Windows)

 $PLATFORM - is your architecture (currently supported values: x86_64)

  example for Windows:

  `gnatmake -l/Windows -l/x86_64 -gnat12 main`

Author
------
jsquirek1[at]student.gsu.edu
