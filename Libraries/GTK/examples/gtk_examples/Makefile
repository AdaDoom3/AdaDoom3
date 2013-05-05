EXAMPLES=scribble

all: ${EXAMPLES}

scribble::
	gnatmake -g -gnata -gnatwu $@ `gtkada-config`
clean::
	${RM} *.o *.ali scribble b~*

