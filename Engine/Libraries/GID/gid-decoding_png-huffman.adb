with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body GID.Decoding_PNG.Huffman is

  procedure Build(t: out Huff_tree; descr: in Huff_descriptor) is
    curr, alloc: Natural;
    code, mask: Unsigned_32;
  begin
    alloc:= root;
    for i in descr'Range loop
      if descr(i).length > 0 then
        curr:= root;
        code:= Unsigned_32(descr(i).code);
        mask:= Shift_Left(Unsigned_32'(1), descr(i).length-1);
        for j in 0..descr(i).length-1 loop
          if (code and mask) /= 0 then
            if t.node(curr).one = nil then
              alloc:= alloc + 1;
              t.node(curr).one:= alloc;
            end if;
            curr:= t.node(curr).one;
          else
            if t.node(curr).zero = nil then
              alloc:= alloc + 1;
              t.node(curr).zero:= alloc;
            end if;
            curr:= t.node(curr).zero;
          end if;
          mask:= Shift_Right(mask, 1);
        end loop;
        t.node(curr).n:= i;
      end if;
    end loop;
    t.last:= alloc;
  end Build;

  -- Free huffman tables starting with table where t points to

  procedure HufT_free ( tl: in out p_Table_list ) is

    procedure  Dispose is new
      Ada.Unchecked_Deallocation( HufT_table, p_HufT_table );
    procedure  Dispose is new
      Ada.Unchecked_Deallocation( Table_list, p_Table_list );

    current: p_Table_list;
    tcount : Natural; -- just a stat. Idea: replace table_list with an array
    tot_length: Natural;

  begin
    if full_trace then
      Ada.Text_IO.Put("[HufT_Free... ");
      tcount:= 0;
      tot_length:= 0;
    end if;
    while tl /= null loop
      if full_trace then
        tcount:= tcount+1;
        tot_length:= tot_length + tl.table'Length;
      end if;
      Dispose( tl.table ); -- destroy the Huffman table
      current:= tl;
      tl     := tl.next;
      Dispose( current );  -- destroy the current node
    end loop;
    if full_trace then
      Ada.Text_IO.Put_Line(
        Integer'Image(tcount)& " tables, of" &
        Integer'Image(tot_length)& " tot. length]"
      );
    end if;
  end HufT_free;

  -- Build huffman table from code lengths given by array b

  procedure HufT_build ( b    : Length_array;
                         s    : Integer;
                         d, e : Length_array;
                         tl   :    out p_Table_list;
                         m    : in out Integer;
              huft_incomplete :    out Boolean)
  is
    b_max  : constant:= 16;
    b_maxp1: constant:= b_max + 1;

    -- bit length count table
    count : array( 0 .. b_maxp1 ) of Integer:= (others=> 0);

    f   : Integer;                    -- i repeats in table every f entries
    g   : Integer;                    -- max. code length
    i,                                -- counter, current code
      j : Integer;                    -- counter
    kcc : Integer;                    -- number of bits in current code

    c_idx, v_idx: Natural;            -- array indices

    current_table_ptr : p_HufT_table:= null;
    current_node_ptr  : p_Table_list:= null; -- curr. node for the curr. table
    new_node_ptr      : p_Table_list;        -- new node for the new table

    new_entry: HufT;                  -- table entry for structure assignment

    u : array( 0..b_max ) of p_HufT_table;   -- table stack

    n_max : constant:= 288;
    -- values in order of bit length
    v : array( 0..n_max ) of Integer:= (others=> 0);
    el_v, el_v_m_s: Integer;

    w : Natural:= 0;                        -- bits before this table

    offset, code_stack : array( 0..b_maxp1 ) of Integer;

    table_level : Integer:= -1;
    bits : array( Integer'(-1)..b_maxp1 ) of Integer;
    -- ^bits(table_level) = # bits in table of level table_level

    y  : Integer;                     -- number of dummy codes added
    z  : Natural:= 0;                 -- number of entries in current table
    el : Integer;                     -- length of eob code=code 256

    no_copy_length_array: constant Boolean:= d'Length=0 or e'Length=0;

  begin
    if full_trace then
      Ada.Text_IO.Put("[HufT_Build...");
    end if;
    tl:= null;

    if b'Length > 256 then -- set length of EOB code, if any
      el := b(256);
    else
      el := b_max;
    end if;

    -- Generate counts for each bit length

    for k in b'Range loop
      if b(k) > b_max then
        -- m := 0; -- GNAT 2005 doesn't like it (warning).
        raise huft_error;
      end if;
      count( b(k) ):= count( b(k) ) + 1;
    end loop;

    if count(0) = b'Length then
      m := 0;
      huft_incomplete:= False; -- spotted by Tucker Taft, 19-Aug-2004
      return; -- complete
    end if;

    -- Find minimum and maximum length, bound m by those

    j := 1;
    while j <= b_max and then count(j) = 0 loop
      j:= j + 1;
    end loop;
    kcc := j;
    if m < j then
      m := j;
    end if;
    i := b_max;
    while i > 0 and then count(i) = 0 loop
      i:= i - 1;
    end loop;
    g := i;
    if m > i then
      m := i;
    end if;

    -- Adjust last length count to fill out codes, if needed

    y := Integer( Shift_Left(Unsigned_32'(1), j) ); -- y:= 2 ** j;
    while j < i loop
      y := y - count(j);
      if y < 0 then
        raise huft_error;
      end if;
      y:= y * 2;
      j:= j + 1;
    end loop;

    y:= y - count(i);
    if y < 0 then
      raise huft_error;
    end if;
    count(i):= count(i) + y;

    -- Generate starting offsets into the value table for each length

    offset(1) := 0;
    j:= 0;
    for idx in 2..i loop
      j:= j + count( idx-1 );
      offset( idx ) := j;
    end loop;

    -- Make table of values in order of bit length

    for idx in b'Range loop
      j := b(idx);
      if j /= 0 then
        v( offset(j) ) := idx-b'First;
        offset(j):= offset(j) + 1;
      end if;
    end loop;

    -- Generate huffman codes and for each, make the table entries

    code_stack(0) := 0;
    i := 0;
    v_idx:= v'First;
    bits(-1) := 0;

    -- go through the bit lengths (kcc already is bits in shortest code)
    for k in kcc .. g loop

      for am1 in reverse 0 .. count(k)-1 loop -- a counts codes of length k

        -- here i is the huffman code of length k bits for value v(v_idx)
        while k > w + bits(table_level) loop

          w:= w + bits(table_level);    -- Length of tables to this position
          table_level:= table_level+ 1;
          z:= g - w;                    -- Compute min size table <= m bits
          if z > m then
            z := m;
          end if;
          j := k - w;
          f := Integer(Shift_Left(Unsigned_32'(1), j)); -- f:= 2 ** j;
          if f > am1 + 2 then   -- Try a k-w bit table
            f:= f - (am1 + 2);
            c_idx:= k;
            loop              -- Try smaller tables up to z bits
              j:= j + 1;
              exit when j >= z;
              f := f * 2;
              c_idx:= c_idx + 1;
              exit when f - count(c_idx) <= 0;
              f:= f - count(c_idx);
            end loop;
          end if;

          if w + j > el and then  w < el  then
            j:= el - w;       -- Make EOB code end at table
          end if;
          if w = 0 then
            j := m;  -- Fix: main table always m bits!
          end if;
          z:= Integer(Shift_Left(Unsigned_32'(1), j)); -- z:= 2 ** j;
          bits(table_level) := j;

          -- Allocate and link new table

          begin
            current_table_ptr := new HufT_table ( 0..z );
            new_node_ptr      := new Table_list'( current_table_ptr, null );
          exception
            when Storage_Error =>
              raise huft_out_of_memory;
          end;

          if current_node_ptr = null then -- first table
            tl:= new_node_ptr;
          else
            current_node_ptr.next:= new_node_ptr;   -- not my first...
          end if;

          current_node_ptr:= new_node_ptr; -- always non-Null from there

          u( table_level ):= current_table_ptr;

          -- Connect to last table, if there is one

          if table_level > 0 then
            code_stack(table_level) := i;
            new_entry.bits          := bits(table_level-1);
            new_entry.extra_bits    := 16 + j;
            new_entry.next_table    := current_table_ptr;

            j :=  Integer(
              Shift_Right( Unsigned_32(i) and
                (Shift_Left(Unsigned_32'(1), w) - 1 ),
                w - bits(table_level-1) )
              );

            -- Test against bad input!

            if j > u( table_level - 1 )'Last then
              raise huft_error;
            end if;
            u( table_level - 1 ) (j) := new_entry;
          end if;

        end loop;

        -- Set up table entry in new_entry

        new_entry.bits      := k - w;
        new_entry.next_table:= null;   -- Unused

        if v_idx >= b'Length then
          new_entry.extra_bits := invalid;
        else
          el_v:= v(v_idx);
          el_v_m_s:= el_v - s;
          if el_v_m_s < 0 then -- Simple code, raw value
            if el_v < 256 then
              new_entry.extra_bits:= 16;
            else
              new_entry.extra_bits:= 15;
            end if;
            new_entry.n := el_v;
          else                    -- Non-simple -> lookup in lists
            if no_copy_length_array then
              raise huft_error;
            end if;
            new_entry.extra_bits := e( el_v_m_s );
            new_entry.n          := d( el_v_m_s );
          end if;
          v_idx:= v_idx + 1;
        end if;

        -- fill code-like entries with new_entry
        f := Integer( Shift_Left( Unsigned_32'(1) , k - w ));
        -- i.e. f := 2 ** (k-w);
        j := Integer( Shift_Right( Unsigned_32(i), w ) );
        while j < z loop
          current_table_ptr(j) := new_entry;
          j:= j + f;
        end loop;

        -- backwards increment the k-bit code i
        j := Integer( Shift_Left( Unsigned_32'(1) , k - 1 ));
        -- i.e.: j:= 2 ** (k-1)
        while ( Unsigned_32(i) and Unsigned_32(j) ) /= 0 loop
          i := Integer( Unsigned_32(i) xor Unsigned_32(j) );
          j :=  j / 2;
        end loop;
        i := Integer( Unsigned_32(i) xor Unsigned_32(j) );

        -- backup over finished tables
        while
          Integer(Unsigned_32(i) and (Shift_Left(1, w)-1)) /=
          code_stack(table_level)
        loop
          table_level:= table_level - 1;
          w:= w - bits(table_level); -- Size of previous table!
        end loop;

      end loop;  -- am1
    end loop;  -- k

    if full_trace then
      Ada.Text_IO.Put_Line("finished]");
    end if;

    huft_incomplete:= y /= 0 and g /= 1;

  exception
    when others =>
      HufT_free( tl );
      raise;
  end HufT_build;

end GID.Decoding_PNG.Huffman;
