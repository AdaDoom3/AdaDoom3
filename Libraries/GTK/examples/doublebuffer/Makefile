all: parameters anim anim2

GNAT_FLAGS = -g -gnata -gnatwu

parameters::
	gnatmake $(GNAT_FLAGS) parameters `gtkada-config`

anim::
	gnatmake $(GNAT_FLAGS) anim `gtkada-config`

anim2::
	gnatmake $(GNAT_FLAGS) anim2 `gtkada-config`

clean:
	${RM} *.o *.ali anim parameters anim2

