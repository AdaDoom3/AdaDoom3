.. _Signal_handling:

***************
Signal handling
***************

In GtkAda, the interaction between the interface and the core application is
done via signals. Most user actions on the graphical application trigger some
signals to be @samp{emitted}.

A signal is a message that an object wants to broadcast. It is identified by
its name, and each one is associated with certain events which happen during
the widget's lifetime. For instance, when the user clicks on a Gtk_Button, a
'clicked' signal is emitted by that button. More examples of signals can be
found in the GtkAda reference manual.

It is possible to cause the application to react to such events by
@samp{connecting} to a signal a special procedure called a @samp{handler} or
@samp{callback}.  This handler will be called every time that signal is
emitted, giving the application a chance to do any processing it needs. More
than one handler can be connected to the same signal on the same object; the
handlers are invoked in the order they were connected.

Predefined signals
==================

Widgets, depending on their type, may define zero or more different signals.
The signals defined for the parent widget are also automatically inherited;
thus every widget answers many signals.

The easiest way to find out which signals can be emitted by a widget is to look
at the GtkAda reference manual. Every widget will be documented there. The
GtkAda RM explains when particular signals are emitted, and the general form
that their handlers should have (although you can always add a `User_Data` if
you wish, see below).

You can also look directly at the C header files distributed with the gtk+
library. Each widget is described in its own C file and has two C structures
associated with it. One of them is the 'class' structure, which contains a
series of pointers to functions. Each of these functions has the same name as
the signal name.

.. highlight:: c

For instance, consider the following extract from gtkbutton.h::

  struct _GtkButtonClass
  {
    GtkBinClass        parent_class;

    void (* pressed)  (GtkButton *button);
    void (* released) (GtkButton *button);
    void (* clicked)  (GtkButton *button);
    void (* enter)    (GtkButton *button);
    void (* leave)    (GtkButton *button);
  };

This means that the Gtk_Button widget redefines five new signals
called `pressed`, `released`, and so on, respectively.

The profile of the handler can also be deduced from those pointers:
The handler has the same arguments, plus an optional `User_Data` parameter
that can be used to pass any kind of data to the handler.  When the
`User_Data` parameter is used, the value of this data is specified when
connecting the handler to the signal. It is then given back to the
handler when the signal is raised.

.. highlight:: ada

Therefore, the profile of a handler should look like::

  procedure Pressed_Handler
    (Button    : access Gtk_Button_Record'Class;
     User_Data : ...);

The callback does not need to use all the arguments. It is legal to use
a procedure that "drops" some of the last arguments.
There is one special case, however: if, at connection time, you decided to
use `User_Data`, your callback must handle it.  This is checked by
the compiler.

Any number of arguments can be dropped as long as those arguments are
the last ones in the list and you keep the first one. For instance,
the signal "button_press_event" normally can be connected to a
handler with any of the following profiles::

  --  with a user_data argument
  procedure Handler
    (Widget    : access Gtk_Widget_Record'Class;
     Event     : Gdk.Event.Gdk_Event;
     User_Data : ...);
  procedure Handler
    (Widget    : access Gtk_Widget_Record'Class;
     User_Data : ...);

  --  without a user_data argument
  procedure Handler
    (Widget : access Gtk_Widget_Record'Class;
     Event  : Gdk.Event.Gdk_Event);
  procedure Handler (Widget : access Gtk_Widget_Record'Class);
  
Beware that adding new arguments is not possible, since no value would
be provided for them. When connecting a handler, GtkAda will not always
verify that your handler does not have more arguments than expected, so
caution is recommended (it only does so if you use the `Gtk.Marshallers`
package, see below).

.. _Connecting_signals:

Connecting signals
==================

All signal handling work is performed using services provided
by the `Gtk.Handlers` package. This package is self-documented,
so please read the documentation for this package either in the GtkAda
Reference Manual or in the specs themselves. The rest of this section assumes
that you have this documentation handy.

A short, annotated example of connecting signals follows; a complete
example can be found in create_file_selection.adb (inside the :file:`testgtk/`
directory). In our example, an application opens a file selector to
allow the user to select a file.  GtkAda provides a high-level widget
called Gtk_File_Selection which can be used in this case::

  declare
     Window : Gtk_File_Selection;
  begin
     Gtk.File_Selection.Gtk_New (Window, Title => "Select a file");
  end;

When the 'OK' button is pressed, the application needs to retrieve the

selected file and then close the dialog. The only information that the
handler for the button press needs is which widget to operate upon.
This can be achieved by the following handler::

  procedure OK (Files : access Gtk_File_Selection_Record'Class) is
  begin
     Ada.Text_IO.Put_Line ("Selected " & Get_Filename (Files));
     --  Prints the name of the selected file.
     Destroy (Files);
     --  Destroys the file selector dialog
  end Ok;

We now need to connect the object we created in the first part with the new
callback we just defined. `Gtk.Handlers` defines four types of generic
packages, depending on the arguments one expects in the callback and whether
the callback returns a value or not. Note that you can not use an arbitrary
list of arguments; this depends on the signal, as explained in the previous
section.

In our example, since the callback does not return any value and does not
handle any `User_Data` (that is, we don't pass it extra data, which will be
specified at connection time), the appropriate package to use is
`Gtk.Handlers.Callback`. We thus instantiate that package.

Remember that generic package instantiations in GtkAda must be present in
memory at all times, since they take care of freeing allocated memory when
finished. GtkAda generic package instantiations must therefore always be
performed at the library level, and not inside any inner block::

  package Files_Cb is new
    Handlers.Callback (Gtk_File_Selection_Record);

The `Files_Cb` package now provides a set of Connect subprograms that can be
used to establish a tie between a widget and a handler.  It also provides a set
of other subprograms which you can use to emit the signals manually, although
most of the time, the signals are simply emitted internally by GtkAda. We will
not discuss the Emit_By_Name subprograms here.

The general form of handler, as used in `Gtk.Handlers`, expects some handlers
that take two or three arguments: the widget on which the signal was applied,
an array of all the extra arguments sent internally by GtkAda, and possibly
some user data given when the connection was made.

This is the most general form of handler and it covers all the possible cases.
However, it also expects the user to manually extract the needed values from
the array of arguments. This is not always the most convenient solution. This
is why GtkAda provides a second package related to signals, `Gtk.Marshallers`.

The `Gtk.Marshallers` package provides a set of functions that can be used as
callbacks directly for GtkAda, and that will call your application's handlers
after extracting the required values from the array of arguments. Although this
might sound somewhat complicated, in practice it simplifies the task of
connecting signals. In fact, the techniques employed are similar to what is
done internally by gtk+ in C. Because of the similarity of techniques, there is
no overhead involved in using `Gtk.Marshallers` with Ada over the C code in
gtk+.

A set of functions `To_Marshaller` is found in every generic package in
`Gtk.Handlers`. They each take a single argument, the name of the function you
want to call, and return a handler that can be used directly in `Connect`.

The connection is then done with the following piece of code::

  Files_Cb.Object_Connect
    (Get_Ok_Button (Window),  --  The object to connect to the handler
     "clicked",               --  The name of the signal
     Files_Cb.To_Marshaller (Ok'Access),  --  The signal handler
     Slot_Object => Window);
  

Note that this can be done just after creating the widget, in the same block.
As soon as it is created, a widget is ready to accept connections (although no
signals will be emitted before the widget is shown on the screen).

We use `To_Marshaller` since our handler does not accept the array of arguments
as a parameter, and we use the special `Object_Connect` procedure. This means
that the parameter to our callback (Files) will be the Slot_Object given in
Object_Connect, instead of being the button itself.

.. _Handling_user_data:

Handling user data
==================

As described above, it is possible to define some data that is that passed to
the callback when it is called. This data is called user_data, and is passed to
the `Connect` or `Object_Connect` subprograms.

GtkAda will automatically free any memory it has allocated internally to store
this user data. For instance, if you instantiated the generic package
`User_Callback` with a String, it means that you want to be able to have a
callback of the form::

     procedure My_Callback (Widget : access Gtk_Widget_Record'Class;
                             User_Data : String);
  
and connect it with a call similar to::

     Connect (Button, "Clicked", To_Marshaller (My_Callback'Access),
              User_Data => "any string");

GtkAda needs to allocate some memory to store the string (an unconstrained
type). However, this memory is automatically freed when the callback is
destroyed.

There are a few subtleties in the use of user_data, most importantly when the
user data is itself a widget.

The following four examples do exactly the same thing: each creates two
buttons, where clicking on the first one will destroy the second one.  They all
work fine the first time, while both buttons exist. However, some of them will
fail if you press on the first button a second time.

Complete, compilable source code for these examples can be found in the
distribution's :file:`examples/user_data` directory, from which the code
samples below are excerpted.

First case: simple user data
----------------------------

This code will fail: even after `Button2` is destroyed, the Ada pointer
continues to reference memory that has been deallocated.  The second call to
`Destroy` will fail with a Storage_Error::

     package User_Callback is new Gtk.Handlers.User_Callback
       (Gtk_Widget_Record, Gtk_Widget);

     procedure My_Destroy2
       (Button : access Gtk_Widget_Record'Class; Data : Gtk_Widget) is
     begin
        Destroy (Data);
     end My_Destroy2;

     begin
        User_Callback.Connect
          (Button1, "clicked",
           User_Callback.To_Marshaller (My_Destroy2'Access),
           Gtk_Widget (Button2));
     end;

Second case: using Object_Connect instead
-----------------------------------------

One of the solutions to fix the above problem is to use
`Object_Connect` instead of `Connect`. In that case, GtkAda
automatically takes care of disconnecting the callback when either of
the two widgets is destroyed::

     procedure My_Destroy (Button : access Gtk_Widget_Record'Class) is
     begin
        Destroy (Button);
     end My_Destroy;

     begin
        Widget_Callback.Object_Connect
          (Button1, "clicked",
           Widget_Callback.To_Marshaller (My_Destroy'Access),
           Button2);
     end;

Third case: manually disconnecting the callback
-----------------------------------------------

Using `Object_Connect` is not always possible. In that case, one
of the possibilities is to store the `Id` of the callback, and
properly disconnect it when appropriate. This is the most complex
method, and very often is not applicable, since you cannot know for
sure when the callback is no longer needed::

     type My_Data3 is record
        Button, Object : Gtk_Widget;
        Id             : Handler_Id;
     end record;
     type My_Data3_Access is access My_Data3;

     package User_Callback3 is new Gtk.Handlers.User_Callback
       (Gtk_Widget_Record, My_Data3_Access);

     procedure My_Destroy3
       (Button : access Gtk_Widget_Record'Class;
        Data   : My_Data3_Access) is
     begin
        Destroy (Data.Button);
        Disconnect (Data.Object, Data.Id);
     end My_Destroy3;

        Id : Handler_Id;
     begin
        Data3 := new My_Data3' (Object => Gtk_Widget (Button1),
                                Button => Gtk_Widget (Button2),
                                Id     => (Null_Signal_Id, null));
        Id := User_Callback3.Connect
          (Button1, "clicked",
           User_Callback3.To_Marshaller (My_Destroy3'Access),
           Data3);
        Data3.Id := Id;
     end;

Fourth case: setting a watch on a specific widget
-------------------------------------------------

GtkAda provides a function `Add_Watch`, that will automatically
disconnect a callback when a given widget is destroyed. This is the
function used internally by `Object_Connect`. In the example
below, the callback is automatically disconnected whenever
`Button2` is destroyed::

     procedure My_Destroy2
       (Button : access Gtk_Widget_Record'Class; Data : Gtk_Widget) is
     begin
        Destroy (Data);
     end My_Destroy2;

        Id : Handler_Id;
     begin
        Id := User_Callback.Connect
          (Button1, "clicked",
           User_Callback.To_Marshaller (My_Destroy2'Access),
           Gtk_Widget (Button2));
        Add_Watch (Id, Button2);
     end;
