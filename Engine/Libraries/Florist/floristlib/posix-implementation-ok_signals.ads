package POSIX.Implementation.OK_Signals is

   --  OK (Sig) = True iff we can use Sig with sigwait ().

   OK : constant array (0 .. 64) of Boolean :=
     (False,  True,  True,  True,  True,  True,  True,  True,  True, False,
       True,  True,  True,  True,  True,  True,  True,  True,  True, False,
       True,  True,  True,  True,  True,  True,  True,  True,  True,  True,
       True,  True, False, False,  True,  True,  True,  True,  True,  True,
       True,  True,  True,  True,  True,  True,  True,  True,  True,  True,
       True,  True,  True,  True,  True,  True,  True,  True,  True,  True,
       True,  True,  True,  True,  True);

   --  Default_Is_Ignore (Sig) = True iff we need to override the default
   --  treatment of Sig with a do-nothing handler before we try to
   --  use sigwait() with it.

   Default_Is_Ignore : constant array (0 .. 64) of Boolean :=
     (False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False);

   --  Default_Is_Stop (Sig) = True iff the default action of Sig
   --  is to stop the process.

   Default_Is_Stop : constant array (0 .. 64) of Boolean :=
     (False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False);

end POSIX.Implementation.OK_Signals;
