.. _Resource_files:

**************
Resource files
**************

Resource files let you parametrize aspects of the widgets in a GtkAda
application without having to recompile it.

A resource file needs to be loaded (`Gtk.Rc.Parse`) `before` setting the
corresponding window.

In this file, it is possible to specify visual characteristics of widgets, such
as their colors and fonts.  Under X, the `xfontsel` command allows you to
easily select a font.  The FontSelection widget is also a simple way to select
fonts.

Here is an example of a resource file::

  # application.rc
  #
  # resource file for "Application"

  # Buttons style
  style "button"
  {
    # BackGround Colors
    #                  Red  Green  Blue
    bg[PRELIGHT] = { 0.0,  0.75, 0.0 } # Green when the mouse is on
                                       # the button
    bg[ACTIVE]   = { 0.75, 0.0,  0.0 } # Red on click
    # ForeGround Colors
    #                  Red  Green  Blue
    fg[PRELIGHT] = { 1.0,  1.0,  1.0 } # White when the mouse is on
                                       # the button
    fg[ACTIVE]   = { 1.0,  1.0,  1.0 } # White on click
  }

  # All the buttons will have the style "button"
  widget_class "*GtkButton*" style "button"

  # Text style
  style "text"
  {
    font = "-adobe-courier-medium-r-normal-*-15-*-*-*-*-*-*-*"
    text[NORMAL] = { 0.0, 0.0, 0.0 } # black
    fg[NORMAL]   = { 0.0, 0.0, 0.0 } # black
    base[NORMAL] = { 1.0, 1.0, 1.0 } # white : background color
  }

  # All Gtk_Text will have the "text" style
  widget_class "*GtkText" style "text"
