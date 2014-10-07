package Neo.Game is
  procedure Preform_Backward (Binding : in Record_Binding);
  procedure Preform_Forward  (Binding : in Record_Binding);
  procedure Preform_Right    (Binding : in Record_Binding);
  procedure Preform_Left     (Binding : in Record_Binding);
  procedure Preform_Look     (Binding : in Record_Binding);
  package Backward is new Impulse("back",     Preform_Backward);
  package Forward  is new Impulse("foreward", Preform_Forward);
  package Right    is new Impulse("right",    Preform_Right);
  package Left     is new Impulse("left",     Preform_Left);
  package Look     is new Impulse("look",     Preform_Look);
end Neo.Game;
