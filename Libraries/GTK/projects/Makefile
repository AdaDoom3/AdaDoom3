include ../src/Makefile.common

MKDIR = mkdir -p

SOVER=${MAJOR}.${MINOR}

# We want users to be able to override the projects installation directory
# from the command line, so we cannot include DESTDIR directly here
# otherwise they have to call make with
#     make DESTDIR=/foo PRJDIR=/foo/usr/share/gpr
PRJDIR=${prefix}/lib/gnat

install: force
	${MKDIR} "${DESTDIR}${PRJDIR}"
	$(MKDIR) "${DESTDIR}$(libdir)/pkgconfig/"
	${CP} gtkada.pc "${DESTDIR}$(libdir)/pkgconfig/"
	-${RM} "${DESTDIR}${prefix}"/projects/gtkada*

	@# We must pass ${libdir} and not ${DESTDIR}${libdir}, since DESTDIR is
	@# really only a temporary directory for packagers, but the package will
	@# eventually be installed in ${libdir}.
	./generate_prj.sh GtkAda ${DEFAULT_LIBRARY_TYPE} ${SOVER} "" "${libdir}"
	${CP} gtkada.gpr "${DESTDIR}${PRJDIR}"

ifeq ($(HAVE_OPENGL), True)
	./generate_prj.sh GtkAda_GL ${DEFAULT_LIBRARY_TYPE} ${SOVER} opengl "${libdir}"
	${CP} gtkada_gl.gpr "${DESTDIR}${PRJDIR}"
endif	

ifeq ($(HAVE_GNOME), True)
	./generate_prj.sh GtkAda_Gnome ${DEFAULT_LIBRARY_TYPE} ${SOVER} gnome "${libdir}"
	${CP} gtkada_gnome.gpr "${DESTDIR}${PRJDIR}"
endif	

ifeq ($(OS),Windows_NT)
	${CP} gtkada_install_*.gpr "${DESTDIR}${incdir}"
endif

clean: force
	${RM} *.gpr 

force:
