-- UnZip.Decompress.Huffman
---------------------------
-- Huffman tree generation and deletion.
-- Originally from info-zip's unzip, data structure rewritten by G. de Montmollin

private package UnZip.Decompress.Huffman is

  type HufT_table;
  type p_HufT_table is access HufT_table;

  type HufT is record
    extra_bits : Natural;
    bits       : Natural;
    n          : Natural;
    next_table : p_HufT_table;
  end record;

  invalid: constant:= 99; -- invalid value for extra bits

  type HufT_table is array( Integer range <> ) of aliased HufT;

  type p_HufT is access all HufT;

  -- Linked list just for destroying Huffman tables

  type Table_list;
  type p_Table_list is access Table_list;

  type Table_list is record
    table: p_HufT_table;
    next : p_Table_list;
  end record;

  type Length_array is array(Integer range <>) of Natural;

  empty : constant Length_array( 1..0 ):= ( others=> 0 );

  -- Free huffman tables starting with table where t points to
  procedure HufT_free ( tl: in out p_Table_list );

  -- Build huffman table from code lengths given by array b.all
  procedure HufT_build ( b    : Length_array;
                         s    : Integer;
                         d, e : Length_array;
                         tl   :    out p_Table_list;
                         m    : in out Integer;
              huft_incomplete :    out Boolean);

  -- Possible exceptions occuring in huft_build
  huft_error,                    -- bad tree constructed
  huft_out_of_memory: exception; -- not enough memory

end UnZip.Decompress.Huffman;
