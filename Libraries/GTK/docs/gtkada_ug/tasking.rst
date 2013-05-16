.. _Tasking_with_GtkAda:

*******************
Tasking with GtkAda
*******************

Note that Gtk+ under Windows does not interact properly with threads,
so the only safe approach under this operating system is to perform all your
Gtk+ calls in the same task.

On other platforms, the Glib library can be used in a task-safe mode by calling
`Gdk.Threads.G_Init` and `Gdk.Threads.Init` before making any other Glib/Gdk
calls.  Gdk routines may then be called simultaneously by multiple tasks,
thanks to task-safe construction of Gdk's internal data structures. However,
Gdk objects such as hash tables are not automatically protected, so it is the
application's responsibility to prevent simultaneous access to user-defined
objects (e.g. by using protected objects).

When Gdk is initialized to be task-safe, GtkAda becomes task aware. There is a
single global lock that you must acquire with `Gdk.Threads.Enter` before making
any Gdk/Gtk call, and which you must release with `Gdk.Threads.Leave`
afterwards.

`Gtk.Main.Main` should be called with the lock acquired (see example below),
ensuring that all the functions executed in the task that started the main loop
do not need to protect themselves again.

Beware that the GtkAda main loop (`Gtk.Main.Main`) can only be be run inside
one specific task. In other words, you cannot call `Gtk.Main.Main` from any
task other than the one that started the outer level main loop.

Note that `Gdk.Threads` assumes that you are using a tasking run time that maps
Ada tasks to native threads.

.. highlight:: ada

A minimal main program for a tasking GtkAda application looks like::

  with Gdk.Threads;
  with Gtk.Main;
  with Gtk.Enums; use Gtk.Enums;
  with Gtk.Window; use Gtk.Window;

  procedure GtkAda_With_Tasks is
     Window : Gtk_Window;
  begin
     Gdk.Threads.G_Init;
     Gdk.Threads.Init;
     Gtk.Main.Init;

     Gtk_New (Window, Window_Toplevel);
     Show (Window);

     Gdk.Threads.Enter;
     Gtk.Main.Main;
     Gdk.Threads.Leave;
  end GtkAda_With_Tasks;

Callbacks require a bit of attention. Callbacks from GtkAda (signals) are made
within the GtkAda lock. However, callbacks from Glib (timeouts, IO callbacks,
and idle functions) are made outside of the GtkAda lock. So, within a signal
handler you do not need to call `Gdk.Threads.Enter`, but within the other types
of callbacks, you do.

