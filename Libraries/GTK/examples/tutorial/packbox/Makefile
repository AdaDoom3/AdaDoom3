all: force
	gnatmake -g -gnata -gnatwu packbox_test `gtkada-config`
run: all
	./packbox_text 1
	./packbox_text 2
	./packbox_text 3
clean: 
	rm -f *.o *~ *.ali packbox_test
force:
