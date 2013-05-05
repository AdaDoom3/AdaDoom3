include ../src/Makefile.common

examples=$(prefix)/share/examples/gtkada/

ifeq (${OS},Windows_NT)
DIRSEP=;
else
DIRSEP=:
endif

# Some examples are build with project files, others with gtkada-config, so
# we modify the following two environment variables so that the GtkAda version
# we just installed can be used to compile the examples
export ADA_PROJECT_PATH:=${libdir}/gnat${DIRSEP}${ADA_PROJECT_PATH}
export PATH:=${bindir}:${PATH}

DIRECTORIES=base_widget \
  calendar_combo        \
  composite_widget      \
  display_switch        \
  doublebuffer          \
  documentation         \
  dynamic_cast          \
  gtk_examples          \
  tutorial              \
  handlers		         \
  powergnu	          	\
  images                \
  cursors               \
  user_data

TUTORIAL_DIRS=tutorial/gtkdial \
  tutorial/helloworld  \
  tutorial/helloworld2 \
  tutorial/packbox     \
  tutorial/simple      \
  tutorial/table       \
  tutorial/tictactoe

SHELL=sh

all: ${DIRECTORIES}
	@for file in ${DIRECTORIES}; do \
	  make -C $${file} all || exit 1; \
	done

install:
	mkdir -p "${DESTDIR}$(examples)"
	-for dir in ${DIRECTORIES} ${TUTORIAL_DIRS}; do \
	  mkdir -p "${DESTDIR}$(examples)/$${dir}"; \
	  rm -f $${dir}/b~*; \
	  cp -p $${dir}/*.ad? $${dir}/*.gpr "${DESTDIR}$(examples)/$${dir}" 2>/dev/null; \
	  cp -p $${dir}/*.xmp $${dir}/*.png $${dir}/*.tst "${DESTDIR}$(examples)/$${dir}" 2>/dev/null; \
	  cp -p $${dir}/Makefile "${DESTDIR}$(examples)/$${dir}" 2>/dev/null; \
	  cp -p $${dir}/README "${DESTDIR}$(examples)/$${dir}" 2>/dev/null; \
	done

clean::
	@for file in ${DIRECTORIES}; do \
	  make -C $${file} clean; \
	done

