all: delete_event specific_tips.o general_tips.o

GNAT_FLAGS = -gnata -gnaty -gnatwu

delete_event:
	gnatmake $(GNAT_FLAGS) delete_event `gtkada-config`

specific_tips.o:
	gnatmake -c $(GNAT_FLAGS) specific_tips `gtkada-config`

general_tips.o:
	gnatmake -c $(GNAT_FLAGS) general_tips `gtkada-config`

clean:
	${RM} *.o *.ali b~* delete_event

