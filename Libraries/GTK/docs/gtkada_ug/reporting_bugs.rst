.. _How_to_report_bugs:

******************
How to report bugs
******************

GtkAda is a mature, stable toolkit that is heavily and widely used on a variety
of platforms.  We test GtkAda using an Ada version of the :file:`testgtk.c`
file found in the gtk+ distribution, as well as by generating a significant
number of interfaces using the GUI builder and Gate.  For code examples that
demonstrate the use of this toolkit, look within the :file:`testgtk/`
directory.

There are two kinds of problems you can encounter:

* If the gtk library itself was compiled with warnings turned on, you may get
  some  warning messages,  mainly because of types problems.  These warnings
  should not appear, as we have tried to be as type safe as possible  in this
  package. To know exactly where the problem is, compile your program with
  debug information, run gdb, and set a breakpoint on the function `g_log`.
  Then run your program as usual, using the `run` command. Then send us the
  result of the `where` command. Here is a summary::

    $ gnatmake -f -g <your_program_name> `gtkada-config`
    $ gdb <your_program_name>
    (gdb) break main
    (gdb) run
    (gdb) break g_log
    (gdb) continue
    ....
    (gdb) where

* In  some  (hopefully) rare cases,   you can even get a  segmentation
  fault within gtk.  That means there is definitly something wrong either
  in your program or in the toolkit.  Please check your program carefully
  and, if you think this is a problem in GtkAda itself, send us an e-mail.

If you are a supported user of GNAT, send mail to `mailto:report@gnat.com
<mailto:report@gnat.com>`_ to report errors, otherwise send mail to the GtkAda
list (`mailto:gtkada@lists.adacore.com <mailto:gtkada@lists.adacore.com>`_)
explaining exactly what your are  doing,  what  is  the  expected  result  and
what  you actually get. Please include the required sources to reproduce the
problem, in a  format usable  by `gnatchop`  (basically, insert all  the
required sources at  the end of  the mail). Please  try to provide as small as
possible a  subset of your sources.

Of course, we will  welcome any patch   you can provide, so  that this toolkit
may be as useful as possible.

