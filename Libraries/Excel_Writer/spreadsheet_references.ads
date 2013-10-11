-- Freeware, author: G. de Montmollin

package Spreadsheet_references is

  type Reference_style is (A1, R1C1);

  function Reference(
    row, column: Positive;
    style      : Reference_style:= A1
  )
  return String;

  Invalid_spreadsheet_reference: exception;

  function Row(reference: String) return Positive;

  function Column(reference: String) return Positive;

  procedure Split(reference: String; row, column: out Positive);

end Spreadsheet_references;
