package body Neo.Game is
  procedure Preform_Backward(Binding : in Record_Binding) is
    begin
    end Preform_;
  procedure Preform_Forward(Binding : in Record_Binding) is
    begin
    end Preform_;
  procedure Preform_Right(Binding : in Record_Binding) is
    begin
    end Preform_;
  procedure Preform_Left(Binding : in Record_Binding) is
    begin
    end Preform_;
  procedure Preform_Look(Binding : in Record_Binding) is
    begin

      eye._z += (x - x_pos) * 0.1f;
	  eye._x -= (x - x_pos) * 0.1f;
	  eye._y += (y - y_pos) * 0.1f;
      else
	  rot._x += (y - y_pos);
	  rot._y += (x - x_pos);
  x_pos = x;
  y_pos = y;

    end Preform_Look;
begin
  Set_Camera;
  Add_Model();
end Neo.Game;
