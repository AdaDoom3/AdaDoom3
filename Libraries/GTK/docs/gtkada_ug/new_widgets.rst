.. _Binding_new_widgets:

*******************
Binding new widgets
*******************

GtkAda comes with a Perl script to help you create a binding to a C widget
(this is the script we have used ourselves).  This will not fully automate the
process, although it should really speed things up. You will probably need less
than 15 min to create a new binding once you will get used to the way GtkAda
works. Note that your C file should have the same format as is used by Gtk+
itself.

To get started on a new binding, launch the script :file:`contrib/binding.pl`
as follows::

  $ touch gtk-button.ads
  $ binding.pl ../include/gtk/gtkbutton.h > temporary
  
This dumps several kind of information on the standard output:

* List of subprograms defined in the :file:`.h` file. Their
  documentation is also added, since binding.pl will parse the :file:`.c` file
  as appropriate.
* List of properties and signals for the widget
* Tentative bodies for the subprograms
  These will often need adjustements, but provide a good start

You can also use this script to update existing bindings::

  $ binding.pl ../include/gtk/*.h
