.. _Support_for_Glade,_the_Gtk_GUI_builder:

**************************************
Support for Glade, the Gtk GUI builder
**************************************

Introduction
============

GtkAda now comes with support for the GUI builder Glade-3.  Glade-3 provides a
graphical interface for designing windows and dialogs.  The interface
description is saved in an XML file which can be loaded at run-time by your
GtkAda application. With this approach, there is no need to write or generate
Ada code to describe the interface, all is needed is to write the callbacks for
various actions.

Launching Glade
===============

Under UNIX and Linux, Glade is invoked by the command-line script `glade-3`
which is located in the `bin` directory of your GtkAda installation.  Under
Windows, Glade is invoked by clicking on the executable `glade-3.exe`, also
located in the `bin` directory of your GtkAda installation.

Building your interface
=======================

In Glade-3 the interface is done by point-and-clicking. The first step is to
create one or more toplevel window and then placing widgets in these windows.

Detailed tutorials can be found at: `http://live.gnome.org/Glade/Tutorials
<http://live.gnome.org/Glade/Tutorials>`_

In the Preferences for your project (menu Edit->Preferences), make sure that
the preference "Project file format" is set to "GtkBuilder".

Using the interface in your application.
========================================

Once the interface is built and saved in an XML file, you can use it in your
GtkAda application. You will need to use objects defined in the package
`Gtkada.Builder` to load the interface file and to connect subprograms defined
in your application to signals emitted by the interface. See the detailed
explanations and examples in `gtkada-builder.ads`

