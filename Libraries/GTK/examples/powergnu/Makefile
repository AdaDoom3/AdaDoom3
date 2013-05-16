CFLAGS = -g -O2 -gnata

EXEC = power

all: $(EXEC)

power: force
	gnatmake -m -Pdefault.gpr

force:

clean:
	rm -f *.o *.ali *~ b~* $(EXEC)
