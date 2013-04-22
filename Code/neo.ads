--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
package Neo
  is
  ----------------
  -- Directives --
  ----------------
    pragma Pure;
  ---------------
  -- Constants --
  ---------------
    NAME    : constant String := "AdaDoom3";
    VERSION : constant Float  := -1.0;

  Private
    -- Added a renames so that the Package "System" sould be accessable
    -- further down the package heiarchy (particularly Neo.System).
    Package Ada_Sys Renames System;
  end Neo;
