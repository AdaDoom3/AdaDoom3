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
generic
  type Type_To_Protect
    is private;
  Default_Type_To_Protect : Type_To_Protect := <>;
package Neo.Foundation.Generic_Protected
  is
  ---------------
  -- Protected --
  ---------------
    protected type Data
      is
        function Get
          return Type_To_Protect;
        procedure Set(
          New_Data : in Type_To_Protect);
        procedure Set_Initialized(
          New_Status : in Boolean);
        function Is_Initialized
          return Boolean;
      private
        Data           : Type_To_Protect := Default_Type_To_Protect;
        Is_Initialized : Boolean         := False;
      end Data;
  ---------------
  -- Accessors --
  ---------------
    type Access_Data
      is access all Data;    
  end Neo.Foundation.Generic_Protected;