# Only here for the build system until this can be settled https://github.com/AdaDoom3/AdaDoom3/issues/15

SYSTEM     = Linux
COMPILER   = GNAT
PROCESSOR  = x86-64
BUILD      = 64-Bit
OBJ_DIR    = Object

GMAKE_OPTS =               \
  -p                       \
  -XSystem=$(SYSTEM)       \
  -XProcessor=$(PROCESSOR) \
  -XCompiler=$(COMPILER)   \
  -XBuild=$(BUILD)

all: build

build:
	gnatmake $(GMAKE_OPTS) -Padadoom3

clean:
	@rm -rf $(OBJ_DIR)/*
