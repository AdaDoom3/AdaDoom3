.. _Debugging_GtkAda_applications:

*****************************
Debugging GtkAda applications
*****************************

This chapter presents a number of technics that can be used when
debugging GtkAda applications. First, the standard tools to debug
Ada applications can be used:

*Compile with -g*
  You should almost always include debugging information when compiling and
  linking your code. This gives you the possibility to use the debugger. See
  below the variable GDK_DEBUG for how to disable grabs.

*bind with -E*
  Using this argument on the `gnatbind` or `gnatmake` command
  line will force the compiler to include backtraces when an exception is
  raised. These backtraces can be converted to symbolic backtraces by
  using the `addr2line` tool.

*Link with -lgmem*
  Using this switch gives access to the `gnatmem` tool, that helps
  you to detect memory leaks or doubly-deallocated memory. The latter
  often results in hard-to-fix Storage_Error exceptions. See the GNAT
  User's guide for more information.

There are also a number of technics specific to GtkAda or gtk+
applications. For most of them, you might need to recompile these
libraries with the appropriate switches to get access to the extended
debugging features.

*Use the `--sync` switch*
  Under unix systems, all applications compiled with gtk+ automatically
  support this switch, which forces events to be processed synchronously,
  thus making it easier to detect problems as soon as they happen.
  This switch is not relevant to Windows systems.

*break on g_log*
  In the debugger, it is often useful to put a breakpoint on the glib
  function `g_log`. When gtk+ is linked dynamically, you will need
  to first start your application with `begin`, then put the
  breakpoint and continue the application with `cont`. This helps
  understand internal errors or warnings reported by gtk+ and glib

*compile glib with `--disable-mem-pools`*
  Glib, the underlying layer that provides system-independent services
  to gtk+, has an extensive and optimized system for memory
  allocation. Bigger chunks of Memory are allocated initially, and then
  subdivided by glib itself. Although this is extremely performant, this
  also make the debugging of memory-related problems (storage_error)
  more difficult. Compiling with the above switch forces glib to use the
  standard malloc() and free() system calls. On GNU/Linux systems, it might
  be useful to set the variable `MALLOC_CHECK_` to 1 to use
  error-detecting algorithms (see the man page for malloc()).

*compile glib and gtk+ with `--enable-debug=yes`*
  It is recommended that you specify this switch on the `configure`
  command line when compiling these two libraries.
  In addition to compiling the libraries with debugging information for
  the debugger, additional runtime debug options (controllable via
  environment variables) become available.
  Specifying `--enable-debug=no` is not recommended for production
  releases (see glib or gtk+ documentation for details).

  For these three variables, the possible values are given below. These
  are lists of colon-separated keywords. You can choose to remove any of
  these value from the variable

  *GOBJECT_DEBUG=objects:signals*
    This sets up the debugging output for glib. The value @samp{objects}
    is probably the most useful, and displays, on exit of the application,
    the list of unfreed objects. This helps detect memory leaks. The
    second value @samp{signals} will display all the signals emitted by
    the objects. Note that this results in a significant amount of output.

  *GDK_DEBUG=updates:nograbs:events:dnd:misc:@*xim:colormap:gdkrgb:gc:pixmap:image:input:cursor*
    This sets up the debugging output for gdk. The most useful value is
    @samp{nograbs}, which prevents the application from ever grabbing the
    mouse or keyboards. If you don't set this, it might happen that the
    debugger becomes unusable, since you don't have access to the mouse
    when the debugger stops on a breakpoint. Another simpler solution is
    to debug remotely from another machine, in which case the grabs
    won't affect the terminal on which the debugger is running.

  *GTK_DEBUG=misc:plugsocket:text:tree:updates:keybindings*
    This sets up the debugging output for gtk. Almost all of these values
    are mostly for internal use by gtk+ developpers, although
    @samp{keybindings} might prove useful sometimes.

.. highlight:: ada

*Import the C function ada_gtk_debug_get_ref_count*
  This function has the following Ada profile::

    function Ref_Count (Add : System.Address) return Guint;
    pragma Import (C, Ref_Count, "ada_gtk_debug_get_ref_count");
    
  and should be called in a manner similar to::

    declare
       Widget : Gtk_Widget;
       Count  : Guint;
    begin
       Count := Ref_Count (Get_Object (Widget));
    end;

  and returns the internal reference counter for the widget. When this
  counter reaches 0, the memory allocated for the widget is
  automatically freed.

  This is mostly a debugging aid for people writting their own
  containers, and shouldn't generally be needed. You shouldn't rely on
  the internal reference counter in your actual code, which is why it
  isn't exported by default in GtkAda.


