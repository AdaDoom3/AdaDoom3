.. _Processing_external_events:

**************************
Processing external events
**************************

It often happens that your application, in addition to processing graphical
events through the GtkAda main loop, also needs to monitor external events.
This is the case if, for instance, you are running external processes and need
to display their output, or if you are listening to incoming data on a socket.
If you implement your own main loop to poll for these external events and then
invoke the GUI, the GUI will enter its main loop and not return control back to
you.

There are several ways to handle this situation:

* The cleanest solution, especially if you intend to make the GUI a major part
  of your application (as opposed to just popping up a few dialogs here and
  there), would be to use the gtk+ main loop as the infinite loop, instead of
  yours.

  You can then use gtk+ 'idle callbacks' (which are called every time the gtk+
  loop is not busy processing graphical events) or 'timeout callbacks' (which
  are called every n milliseconds), and in those callbacks do the work you were
  doing before in your own main loop (that assumes the check is relatively
  fast, otherwise the GUI will be frozen during that time). Such callbacks are
  created through packages in glib-main.ads

* Another approach is to not start the gtk+ main loop, but to check
  periodically whether there are some events to be handled.  See the subprogram
  `Gtk.Main.Main_Iteration`.

  This second approach is not necessarily recommended, since you would
  basically duplicate code that's already in gtk+ to manage the main loop, and
  you also get finer control using idle and timeout callbacks
