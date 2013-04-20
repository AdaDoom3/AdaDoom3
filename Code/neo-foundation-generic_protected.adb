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
package body Neo.Foundation.Generic_Protected
  is
  ----------
  -- Data --
  ----------
    protected body Data
      is
      function Get
        return Type_To_Protect
        is
        begin
          return Data;
        end Get;
      procedure Set(
        New_Data : in Type_To_Protect)
        is
        begin
          Data := New_Data;
        end Set;
      procedure Set_Initialized(
        New_Status : in Boolean)
        is
        begin
          Status := New_Status;
        end Set_Initialized;
      function Is_Initialized
        return Boolean
        is
        begin
          return Status;
        end Is_Initialized;
      end Data;
  end Neo.Foundation.Generic_Protected;