#############################################################
#
#  Adding some OS specific flags and parameters
#
############################################################

AC_DEFUN(AM_ADD_OS_SPECIFIC_FLAGS,
[
   SO_EXT=.so
   SO_OPTS=-Wl,-soname,
   FPIC=-fPIC
   TARGET_LFLAGS=
   DEFAULT_LIBRARY_TYPE=static

   AC_ARG_ENABLE(static,
     [AC_HELP_STRING(
        [--disable-static],
        [Disable building of static libraries.])
AC_HELP_STRING(
        [--enable-static],
        [Build static libraries (default).])],
     [BUILD_STATIC=$enableval
      if test $enableval = no; then
         DEFAULT_LIBRARY_TYPE=relocatable
      fi],
     [BUILD_STATIC=yes])

   AC_ARG_ENABLE(shared,
     [AC_HELP_STRING(
        [--disable-shared],
        [Disable building of shared libraries (default is to build them if supported on the target)])
AC_HELP_STRING(
        [--enable-shared],
        [Build shared libraries if supported on the target and
make them preselected in project files (static libraries are preselected by default])],
     [CAN_BUILD_SHARED=$enableval
      if test $enableval = yes; then
         DEFAULT_LIBRARY_TYPE=relocatable
      fi],
     [CAN_BUILD_SHARED=yes])

   BUILD_SHARED=$CAN_BUILD_SHARED

   case $build_os in
   aix*)
      BUILD_SHARED=no
      FPIC=
      OS_SPECIFIC_LINK_OPTIONS=-Wl,-bexpall,-berok
      TARGET_LFLAGS=-Wl,-bbigtoc
      SO_OPTS="-o "
      ;;
   hp*)
      SO_EXT=.sl
      SO_OPTS=-Wl,+h,
      BUILD_SHARED=no
      ;;
   *sysv4uw* | *sysv5uw*)
      SO_OPTS=-Wl,-h,
      BUILD_SHARED=no
      FPIC=
      ;;
   *solaris*)
      SO_OPTS=-Wl,-h,
      ;;
   *irix*)
      FPIC=
      ;;
   *osf*)
      OS_SPECIFIC_LINK_OPTIONS=-Wl,-expect_unresolved,\*
      ;;
   *mingw*)
      if test x$CAN_BUILD_SHARED = xyes ; then
         BUILD_SHARED=yes
      fi
      SO_EXT=.dll
      FPIC=
      ac_tmp_GNATDIR=`which gcc | sed 's,/gcc$,,'`
      ac_GNATDIR=`cygpath --mixed $ac_tmp_GNATDIR`
      count=`cd $ac_GNATDIR; ls libgnat-*.dll | wc -l`
      if test $count -gt 1 ; then
         echo "Too many libgnat.dll, in $ac_GNATDIR"
	 echo Found: `cd $ac_GNATDIR; ls libgnat-*.dll`
         exit 1
      fi
      ac_GNATLIB=`cd $ac_GNATDIR; ls libgnat-*.dll | sed 's,lib,,;s,.dll,,'`
      OS_SPECIFIC_LINK_OPTIONS=-Wl,-L$ac_GNATDIR,-l$ac_GNATLIB
      ;;
   *darwin*)
      SO_EXT=.dylib
      if test x$CAN_BUILD_SHARED = xyes ; then
         BUILD_SHARED=yes
      fi
      SO_OPTS="-Wl,-undefined,dynamic_lookup -dynamiclib -Wl,-dylib_install_name,"
      FPIC=-fPIC
      #TARGET_LFLAGS="-Wl,-flat_namespace -Wl,-undefined,suppress"
      ;;
   # ??? The following case has been introduced because of an elaboration
   # problem with the GtkAda dynamic library and GPS (see E511-010). This
   # is a workaround, and shall be removed as soon as the bug is fixed.
   *linux*)
      case $build_cpu in
      *ia64*)
         BUILD_SHARED=no
         FPIC=
         ;;
      esac
      ;;
   esac

  if test x$BUILD_SHARED = xno ; then
     DEFAULT_LIBRARY_TYPE=static
  fi

  AC_SUBST(DEFAULT_LIBRARY_TYPE)
  AC_SUBST(OS_SPECIFIC_LINK_OPTIONS)
  AC_SUBST(BUILD_STATIC)
  AC_SUBST(BUILD_SHARED)
  AC_SUBST(SO_EXT)
  AC_SUBST(SO_OPTS)
  AC_SUBST(FPIC)
  AC_SUBST(TARGET_LFLAGS)

]
)

#############################################################
#  Checking for build type
#############################################################

AC_DEFUN(CHECK_BUILD_TYPE,
[
    AC_ARG_ENABLE(build,
                  [--enable-build=<type>       Default build type for the library (Debug, Production)],
                  BUILD_TYPE=$enableval,
                  BUILD_TYPE=Production)
]
)

#############################################################
#
#  Checking for Gnat
#
#############################################################

conftest_ok="conftest.ok"

AC_DEFUN(AM_PATH_GNAT,
[   
   AC_PATH_PROG(GNATMAKE, gnatmake, no)

   if test x$GNATMAKE = xno ; then
      AC_MSG_ERROR(I could not find gnatmake. See the file 'INSTALL' for more details.)
   fi

   AC_MSG_CHECKING(that your gnat compiler works with a simple example)

   rm -f conftest.adb
   cat << EOF > conftest.adb
with Ada.Text_IO;

procedure Conftest is
   Conftest_Ok : Ada.Text_IO.File_Type;
begin
   Ada.Text_IO.Create (File => Conftest_Ok,
                       Name => "$conftest_ok");
   Ada.Text_IO.Close (Conftest_Ok);
end Conftest;
EOF

   $GNATMAKE -q conftest > /dev/null

   if ( test ! -x conftest ) then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR($GNATMAKE test failed at compile time! Check your configuration.)
   fi

   ./conftest

   if ( test ! -f $conftest_ok ) then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR($GNATMAKE test failed at run time! Check your configuration.)
   fi

   AC_MSG_RESULT(yes)
])


#############################################################
#
#  Checking for gnatprep
#
#############################################################


AC_DEFUN(AM_PATH_GNATPREP,
[
   AC_PATH_PROG(GNATPREP, gnatprep, no)

   if test x$GNATPREP = xno ; then
      AC_MSG_ERROR(I could not find gnatprep. See the file 'INSTALL' for more details.)
   fi

])

#############################################################
#
#  Checking for Perl
#
#############################################################

AC_DEFUN(AM_PATH_PERL,
[
   AC_PATH_PROGS(PERL, perl5 perl)
   
   ### We don't really have any need for a specific version
   ### of perl for the moment, so we don't verify it.

])

#############################################################
#
# Configure paths for GTK+
# Extracted from the aclocal.m4 file of gimp-1.0.0
#
#############################################################

dnl AM_PATH_GTK([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for GTK, and define GTK_CFLAGS and GTK_LIBS
dnl
AC_DEFUN(AM_PATH_GTK,
[dnl 
dnl Get the cflags and libraries from the pkg-config script
dnl
  AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
  min_gtk_version=ifelse([$1], ,1.3.0,$1)
  AC_MSG_CHECKING(for GTK - version >= $min_gtk_version)
  no_gtk=""
  GTK="gtk+-2.0"
  if test "$PKG_CONFIG" = "no" ; then
    no_gtk=yes
  else
    GTK_PREFIX=`$PKG_CONFIG $GTK --variable=prefix`
    GTK_CFLAGS=`$PKG_CONFIG $GTK --cflags`
    GTK_LIBS=`$PKG_CONFIG $GTK --libs`
    gtk_config_major_version=`$PKG_CONFIG $GTK --modversion | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gtk_config_minor_version=`$PKG_CONFIG $GTK --modversion | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gtk_config_micro_version=`$PKG_CONFIG $GTK --modversion | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`

    GTK_LIBS_FOR_GNATMAKE=`echo $GTK_LIBS | sed -e 's/-framework \([^ ]*\)/-Wl,-framework -Wl,\1/g'`

    ac_save_CFLAGS="$CFLAGS"
    ac_save_LIBS="$LIBS"
    CFLAGS="$CFLAGS $GTK_CFLAGS"
    LIBS="$LIBS $GTK_LIBS"
dnl
dnl Now check if the installed GTK is sufficiently new. (Also sanity
dnl checks the results of pkg-config to some extent
dnl
      rm -f conf.gtktest
      AC_TRY_RUN([
#include <gtk/gtk.h>
#include <stdio.h>

int 
main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.gtktest");

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = g_strdup("$min_gtk_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gtk_version");
     exit(1);
   }

  if ((gtk_major_version != $gtk_config_major_version) ||
      (gtk_minor_version != $gtk_config_minor_version) ||
      (gtk_micro_version != $gtk_config_micro_version))
    {
      printf("\n*** 'pkg-config --version' returned %d.%d.%d, but GTK+ (%d.%d.%d)\n", 
             $gtk_config_major_version, $gtk_config_minor_version, $gtk_config_micro_version,
             gtk_major_version, gtk_minor_version, gtk_micro_version);
      printf ("*** was found! If pkg-config was correct, then it is best\n");
      printf ("*** to remove the old version of GTK+. You may also be able to fix the error\n");
      printf("*** by modifying your LD_LIBRARY_PATH enviroment variable, or by editing\n");
      printf("*** /etc/ld.so.conf. Make sure you have run ldconfig if that is\n");
      printf("*** required on your system.\n");
    } 
  else
    {
      if ((gtk_major_version > major) ||
        ((gtk_major_version == major) && (gtk_minor_version > minor)) ||
        ((gtk_major_version == major) && (gtk_minor_version == minor) && (gtk_micro_version >= micro)))
      {
        return 0;
       }
     else
      {
        printf("\n*** An old version of GTK+ (%d.%d.%d) was found.\n",
               gtk_major_version, gtk_minor_version, gtk_micro_version);
        printf("*** You need a version of GTK+ newer than %d.%d.%d. The latest version of\n",
	       major, minor, micro);
        printf("*** GTK+ is always available from ftp://ftp.gtk.org.\n");
        printf("***\n");
      }
    }
  return 1;
}
],, no_gtk=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])

    CFLAGS="$ac_save_CFLAGS"
    LIBS="$ac_save_LIBS"
  fi

  if test "x$no_gtk" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$GTK_CONFIG" = "no" ; then
       echo "*** The pkg-config script could not be found"
       echo "*** If GTK was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path."
     else
       if test -f conf.gtktest ; then
        :
       else
          echo "*** Could not run GTK test program, checking why..."
          CFLAGS="$CFLAGS $GTK_CFLAGS"
          LIBS="$LIBS $GTK_LIBS"
          AC_TRY_LINK([
#include <gtk/gtk.h>
#include <stdio.h>
],      [ return ((gtk_major_version) || (gtk_minor_version) || (gtk_micro_version)); ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding GTK or finding the wrong"
          echo "*** version of GTK. If it is not finding GTK, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH"
          echo "***"
          echo "*** If you have a RedHat 5.0 system, you should remove the GTK package that"
          echo "*** came with the system with the command"
          echo "***"
          echo "***    rpm --erase --nodeps gtk gtk-devel" ],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means GTK was incorrectly installed"
          echo "*** or that you have moved GTK since it was installed." ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     GTK_CFLAGS=""
     GTK_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GTK_PREFIX)
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  AC_SUBST(GTK_LIBS_FOR_GNATMAKE)
  rm -f conf.gtktest
])

#############################################################
#
#  Checking for PANGO_UNDERLINE_ERROR
#
#############################################################

AC_DEFUN(AM_PANGO_UNDERLINE_ERROR,
[   
  ac_save_CFLAGS="$CFLAGS"
  ac_save_LIBS="$LIBS"
  CFLAGS="$CFLAGS $GTK_CFLAGS"
  LIBS="$LIBS $GTK_LIBS"
  CFLAGS="$CFLAGS $GTK_CFLAGS"
  LIBS="$LIBS $GTK_LIBS"
  DEFINE_UNDERLINE_ERROR="#undef HAVE_PANGO_UNDERLINE_ERROR"
  AC_MSG_CHECKING(for PANGO_UNDERLINE_ERROR)
  AC_TRY_LINK([ #include <pango/pango.h> ],
    [ PangoUnderline underline = PANGO_UNDERLINE_ERROR; ],
    [ AC_MSG_RESULT(yes)
      DEFINE_UNDERLINE_ERROR="#define HAVE_PANGO_UNDERLINE_ERROR" ],
    AC_MSG_RESULT(no))
  AC_SUBST(DEFINE_UNDERLINE_ERROR)
  CFLAGS="$ac_save_CFLAGS"
  LIBS="$ac_save_LIBS"
])

#############################################################
#
#  Checking for openGL
#
#############################################################


AC_DEFUN(AM_CHECK_OPENGL,
[   

   # checking for OpenGL libraries
   AC_ARG_WITH(GL,         [  --with-GL=value         Which OpenGL library to compile GtkAda with (auto,GL,GL32,MesaGL,,no)])
   AC_ARG_WITH(GL-prefix,  [  --with-GL-prefix=DIR    Prefix where GL/MesaGL is installed])
   
   if test "x$with_GL_prefix" = "x" ; then
      GL_LDOPTS=""
      GL_CFLAGS=""
   else
      GL_CFLAGS="-I$with_GL_prefix/include"
      case "${host}" in
         *64*)
            GL_LDOPTS="-L$with_GL_prefix/lib64"
            ;;
         *)
            GL_LDOPTS="-L$with_GL_prefix/lib"
            ;;
      esac
   fi

   saved_LIBS="$LIBS"

   if test "x$with_GL" != xno ; then
     AC_MSG_CHECKING([for OpenGL])
     LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lGLU -lGL"
     AC_TRY_LINK( ,[ char glBegin(); glBegin(); ], have_GL=yes, have_GL=no)
     AC_MSG_RESULT($have_GL)
  
     AC_MSG_CHECKING([for GL32])
     LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lglu32 -lopengl32 -lgdi32"
     AC_TRY_LINK([
#include <GL/gl.h>
#include <windows.h>], 
[ glBegin(0); 
  CreateCompatibleDC(NULL); ], have_GL32=yes, have_GL32=no)
     AC_MSG_RESULT($have_GL32)
 
     AC_MSG_CHECKING([for Mesa])
     LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lMesaGLU -lMesaGL"
     AC_TRY_LINK( ,[ char glBegin(); glBegin(); ], have_MesaGL=yes, have_MesaGL=no)
     AC_MSG_RESULT($have_MesaGL)

     if test "x$have_MesaGL" = "xno"; then
       AC_MSG_CHECKING([Mesa with pthreads])
       LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lMesaGLU -lMesaGL -lpthread"
       AC_TRY_LINK( ,[ char glBegin(); glBegin(); ], have_MesaGL_pthread=yes, have_MesaGL_pthread=no)
       AC_MSG_RESULT($have_MesaGL_pthread)
     fi
   fi

   LIBS="$saved_LIBS"
   HAVE_OPENGL="False"

   case "x$with_GL" in
   x|xauto)
      if test "x$have_GL" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lGLU -lGL"
         HAVE_OPENGL="True"
      elif test "x$have_GL32" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lglu32 -lopengl32 -lgdi32"
         HAVE_OPENGL="True"
      elif test "x$have_MesaGL" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lMesaGLU -lMesaGL"
         HAVE_OPENGL="True"
      elif test "x$have_MesaGL_pthread" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lMesaGLU -lMesaGL -lpthread"
         HAVE_OPENGL="True"
      fi
      ;;
   xGL)
      if test "x$have_GL" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lGLU -lGL"
         HAVE_OPENGL="True"
      else
         AC_MSG_ERROR([Missing OpenGL library])
      fi
      ;;
   xGL32)
      if test "x$have_GL" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lglu32 -lopengl32 -lgdi32"
         HAVE_OPENGL="True"
      else
         AC_MSG_ERROR([Missing Windows OpenGL library])
      fi
      ;;
   xMesaGL)
      if test "x$have_MesaGL" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lMesaGLU -lMesaGL"
         HAVE_OPENGL="True"
      elif test "x$have_MesaGL_pthread" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lMesaGLU -lMesaGL -lpthread"
         HAVE_OPENGL="True"
      else
         AC_MSG_ERROR([Missing Mesa library])
      fi
      ;;
   xno)
      ;;
   *)
      AC_MSG_ERROR([Unknown value for "--with-GL" option. Should be either auto, GL32, GL, MesaGL, no])
      ;;
   esac

   if test "x$HAVE_OPENGL" = "xFalse"; then
      AC_MSG_RESULT([*** OpenGL support will not be integrated into GtkAda ***])
   fi


   AC_SUBST(GL_LIBS)
   AC_SUBST(GL_CFLAGS)
   AC_SUBST(HAVE_OPENGL)

])

#############################################################
#
#  Checking for gnome2
#
#############################################################

AC_DEFUN(AM_CHECK_GNOME,
[   
  AC_MSG_CHECKING(for gnome2)
  GNOME_CFLAGS=""
  GNOME_LIBS=""
  GNOME_STATIC_LIBS=""

  if test "$PKG_CONFIG" = "no" ; then
    AC_MSG_RESULT(no)
    HAVE_GNOME="False"
  else
    GNOMEUI="libgnomeui-2.0"
    GNOME_PREFIX=`$PKG_CONFIG $GNOMEUI --variable=prefix`
    if test "x$GNOME_PREFIX" = "x"; then
      HAVE_GNOME="False"
      AC_MSG_RESULT(no)
    else
      AC_MSG_RESULT(yes)
      HAVE_GNOME="True"
      GNOME_CFLAGS=`$PKG_CONFIG $GNOMEUI --cflags`
      GNOME_LIBS=`$PKG_CONFIG $GNOMEUI --libs`
      GNOME_STATIC_LIBS="$GNOME_PREFIX/lib/libgnomeui-2.a $GNOME_PREFIX/lib/libgnome-2.a $GNOME_PREFIX/lib/libart_lgpl.a $GNOME_PREFIX/lib/lib/libpopt.a $GNOME_PREFIX/lib/libbonoboui-2.a" 
    fi
  fi

  AC_SUBST(GNOME_CFLAGS)
  AC_SUBST(GNOME_LIBS)
  AC_SUBST(GNOME_STATIC_LIBS)
  AC_SUBST(HAVE_GNOME)
])

#############################################################
#
#  A small macro to create a file after preprocessing it using gnatprep
#
#############################################################


AC_DEFUN(AM_GNATPREP,
[   
   echo "creating $1"
   $GNATPREP $1.in $1 config.defs
])


#############################################################
#
#  Macro to add for using GNU gettext
#
#############################################################


AC_DEFUN(AM_WITH_NLS,
  [AC_MSG_CHECKING([whether NLS is requested])
    dnl Default is enabled NLS
    AC_ARG_ENABLE(nls,
      [  --disable-nls           do not use Native Language Support],
      USE_NLS=$enableval, USE_NLS=yes)
    AC_MSG_RESULT($USE_NLS)
    AC_SUBST(USE_NLS)

    GETTEXT_INTL="False"
    HAVE_GETTEXT="False"

    dnl If we use NLS figure out what method
    if test "$USE_NLS" = "yes"; then
      AC_DEFINE(ENABLE_NLS)

      dnl Figure out whether gettext is available in the C or intl library.
      nls_cv_header_intl=
      nls_cv_header_libgt=

      AC_CACHE_CHECK([for gettext in libc], gt_cv_func_gettext_libc,
       [AC_TRY_LINK([extern int gettext(char*);], [return (int) gettext ("")],
	gt_cv_func_gettext_libc=yes, gt_cv_func_gettext_libc=no)])

      if test "$gt_cv_func_gettext_libc" != "yes"; then
        AC_CHECK_LIB(intl, bindtextdomain,
         [AC_CACHE_CHECK([for gettext in libintl],
           gt_cv_func_gettext_libintl,
           [AC_CHECK_LIB(intl, gettext,
              gt_cv_func_gettext_libintl=yes,
              gt_cv_func_gettext_libintl=no)],
	    gt_cv_func_gettext_libintl=no)])

	  if test "$gt_cv_func_gettext_libintl" = "yes"; then
            GETTEXT_INTL="True"
          fi
      fi

       if test "$gt_cv_func_gettext_libc" = "yes" \
         || test "$gt_cv_func_gettext_libintl" = "yes"; then
            HAVE_GETTEXT="True"
       fi
    fi

    dnl Make all variables we use known to autoconf.
    AC_SUBST(GETTEXT_INTL)
    AC_SUBST(HAVE_GETTEXT)
  ])

AC_DEFUN(AM_GNU_GETTEXT,
  [AM_WITH_NLS
  ])
