******************************
Introduction: What is GtkAda ?
******************************

GtkAda is a high-level portable graphical toolkit, based on the gtk+ toolkit,
one of the official GNU toolkits. It makes it easy to create portable user
interfaces for multiple platforms, including most platforms that have a X11
server and Win32 platforms.

Although it is based on a C library, GtkAda uses some advanced Ada features
such as tagged types, generic packages, access to subprograms, and exceptions
to make it easier to use and design interfaces.  For efficiency reasons, it
does not use controlled types, but takes care of all the memory management for
you in other ways.

As a result, this library provides a *secure*, *easy to use* and *extensible*
toolkit.

Compared to the C library, GtkAda provides type safety (especially in the
callbacks area), and object-oriented programming. As opposed to common
knowledge, it requires *less* type casting than with in C.  Its efficiency is
about the same as the C library through the use of inline subprograms.

GtkAda comes with a complete integration to the graphical interface builder
`Glad`. This makes it even easier to develop interfaces, since you just have to
click to create a description of the window and all the dialogs. Ada code can
simply import that description to bring the windows to life.

Under some platforms, GtkAda also provides a bridge to use OpenGL, with which
you can create graphical applications that display 3D graphics, and display
them in a GtkAda window, as with any other 2D graphics.  This manual does not
document OpenGL at all, see any book on OpenGL, or the specification that came
with your OpenGL library, for more information.

The scheme used for GtkAda's version numbers is the following: the major and
minor version number is the same as for the underlying gtk+ library (e.g 2.24).
The micro version number depends on GtkAda's release number.

This toolkit was tested on the following systems:

* GNU Linux/x86
* GNU Linux/x86-64
* Solaris/sparc
* Solaris/sparc64
* Windows XP/Vista/7

with the latest version of the `GNAT` compiler, developed and supported by
Ada Core Technologies (see `http://www.adacore.com <http://www.adacore.com>`_).

This version of GtkAda is known to be compatible with `gtk+` **2.24.8**.
This release may or may not be compatible with older versions of
gtk+.

This document does not describe all the widgets available in GtkAda, nor does
it try to explain all the subprograms. The GtkAda Reference Manual provides
this documentation instead, as well as the GtkAda sources spec files
themselves, whose extension is :file:`.ads`.

No complete example is provided in this documentation. Instead, please refer to
the examples that you can find in the :file:`testgtk/` and :file:`examples/`
directory in the GtkAda distribution, since these are more up-to-date (and more
extensive).  They are heavily commented, and are likely to contain a lot of
information that you might find interesting.

If you are interested in getting support for GtkAda--including priority bug
fixes, early releases, help in using the toolkit, help in designing your
interface, and on site consulting--please contact AdaCore
(`mailto:sales@adacore.com <mailto:sales@adacore.com>`_).

