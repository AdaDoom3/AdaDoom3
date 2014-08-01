-- GID.Decoding_PNG.Huffman
---------------------------
-- Huffman tree generation and deletion.
-- Copy of UnZip.Decompress.Huffman

private package GID.Decoding_PNG.Huffman is

  -- Variants A and B.

  -- A/ Simplistic huffman trees, pointerless

  type Length_code_pair is record
    length: Natural;
    code  : Natural;
  end record;

  type Huff_descriptor is array(Natural range <>) of Length_code_pair;

  nil: constant:= 0;
  root: constant:= 1;

  type Huff_node is record
    n: Natural; -- value
    zero, one: Natural:= nil; -- index of next node, if any
  end record;

  max_size: constant:= 800;

  type Huff_node_list is array(1..max_size) of Huff_node;

  type Huff_tree is record
    last: Natural:= nil;
    node: Huff_node_list;
  end record;

  procedure Build(t: out Huff_tree; descr: in Huff_descriptor);

  -- B/ Huffman tables: several steps in the binary tree
  -- in one jump.
  -- Pro: probably faster
  -- Contra: complicated, relies on pointers, large data.

  type HufT_table;
  type p_HufT_table is access HufT_table;

  invalid: constant:= 99; -- invalid value for extra bits

  type HufT is record
    extra_bits : Natural:= invalid;
    bits       : Natural;
    n          : Natural;
    next_table : p_HufT_table:= null;
  end record;

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

end GID.Decoding_PNG.Huffman;
