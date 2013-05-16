.. _Memory_management:

*****************
Memory management
*****************

GtkAda takes care of almost all the memory management for you.  Here is a brief
overview of how this works, you'll have to check the sources if you want more
detailed information.  Gtk+ (the C library) does its own memory management
through reference counting, i.e. any widget is destroyed when it is no longer
referenced anywhere in the application.

In GtkAda itself, a 'user_data' is associated with each object allocated by a
`Gtk_New` procedure. A 'destroy' callback is also associated, to be called when
the object to which the user_data belongs is destroyed.  Thus, every time a C
object is destroyed, the equivalent Ada structure is also destroyed (see
`Gtk.Free_User_Data`).

Concerning widgets containing children, every container holds a reference to
its children, whose reference counting is thus different from 0 (and generally
1). When the container is destroyed, the reference of all its children and
grand-children is decremented, and they are destroyed in turn if needed. So the
deallocation of a widget hierarchy is also performed automatically.

