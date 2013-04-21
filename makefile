SYSTEM     = Linux
COMPILER   = GNAT
PROCESSOR  = x86-64
OBJ_DIR    = OBJECT
GMAKE_OPTS = -p -XSystem=$(SYSTEM) -XProcessor=$(PROCESSOR) -XCompiler=$(COMPILER)

all: build

build:
	gnatmake $(GMAKE_OPTS) -Padadoom3

clean:
	@rm -rf $(OBJ_DIR)/*
