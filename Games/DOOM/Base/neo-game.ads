with Neo.System;       use Neo.System;
with Neo.System.Input; use Neo.System.Input;
with Neo.File.Model;   use Neo.File.Model;
package Neo.Game is
private
  procedure Run;
  package Task_Main is new Tasks(Run);
  Main_Task : Task_Main.Protected_Task;
end Neo.Game;
