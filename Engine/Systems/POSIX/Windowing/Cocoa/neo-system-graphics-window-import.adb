separate(Neo.System.Graphics.Window) package body Import is
  procedure Iconize is
    begin
      raise Unimplemented_Feature;         
    end Iconize;
  function Is_Fullscreen_Only return Boolean is
    begin
      raise Unimplemented_Feature;  
      return False;
    end Is_Fullscreen_Only;
  procedure Set_Cursor(X, Y : in Integer_8_Signed) is
    begin
      raise Unimplemented_Feature;  
    end Set_Cursor;
  function Get_Borders return Vector_Record_Border.Unprotected.Vector is
    Borders : Vector_Record_Border.Unprotected.Vector;
    begin
      raise Unimplemented_Feature;  
      return Borders;
    end Get_Borders;
  function Get_Decoration return Record_Border is
    begin
      raise Unimplemented_Feature;  
      return (others => <>);
    end Get_Decoration;
  procedure Set_Cursor_Style(Cursor : in Enumerated_Cursor := Inactive_Cursor) is
    begin
      raise Unimplemented_Feature;  
    end Set_Cursor_Style;
  procedure Assert_Only_Instance is
    begin
      raise Unimplemented_Feature;  
    end Assert_Only_Instance;
  procedure Clip_Mouse(Undo : in Boolean := False; Do_Hide : in Boolean := False) is
    begin
      raise Unimplemented_Feature;  
    end Clip_Mouse;
  procedure Adjust(X, Y : in Integer_4_Signed; Width, Height : in Integer_4_Positive; Do_Fullscreen : in Boolean) is
    begin
      raise Unimplemented_Feature;  
    end Adjust;
  procedure Adjust_Fullscreen is
    begin
      raise Unimplemented_Feature;  
    end Adjust_Fullscreen;
  procedure Adjust_Windowed(Width, Height : in Integer_4_Positive) is
    begin
      raise Unimplemented_Feature;  
    end Adjust_Windowed;
  procedure Initialize_Multi_Monitor is
    begin
      raise Unimplemented_Feature;  
    end Initialize_Multi_Monitor;
  procedure Finalize_Multi_Monitor is
    begin
      raise Unimplemented_Feature;  
    end Finalize_Multi_Monitor;
  procedure Initialize is
    begin
      raise Unimplemented_Feature;  
    end Initialize;
  function Update return Boolean is
    begin
      raise Unimplemented_Feature;  
      return false;
    end Update;
  procedure Finalize is
    begin
      raise Unimplemented_Feature;  
    end Finalize;
end Import;
