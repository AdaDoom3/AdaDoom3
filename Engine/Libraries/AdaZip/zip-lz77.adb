--  Experimental LZ77 encoder, based on LZHUF by OKUMURA & YOSHIZAKI
--  Here the Huffman code is used only to find quickly matching patterns.

procedure Zip.LZ77 is

  N_Char    : constant Integer:= 256-Threshold+Look_Ahead;
  -- Character code (= 0..N_CHAR-1)
  Max_Table     : constant Integer:= N_Char*2-1;

  use Interfaces; -- Make Unsigned_* types visible

  type Text_Buffer is array ( 0..String_buffer_size+Look_Ahead-1 ) of Byte;
  empty_buffer: constant Text_Buffer:= (others=> 32); -- ' '

  -- > The Huffman frequency handling is made generic so we have
  --   one copy of the tree and of the frequency table for Encode
  --   and one for Decode

  generic
  package Huffman is
    --- Pointing parent nodes.
    --- Area [Max_Table..(Max_Table + N_CHAR - 1)] are pointers for leaves
    Parent:  array ( 0..Max_Table+N_Char-1 ) of Natural;
    --- Pointing children nodes (son[], son[] + 1)
    Son   :  array ( 0..Max_Table-1 )  of Natural;

    Root_Position : constant Integer:= Max_Table-1; -- (can be always Son'last ?)

    procedure Start;
    procedure Update_Freq_Tree( C0: Natural );
  end Huffman;

  package body Huffman is

    Freq: array ( 0..Max_Table ) of Natural; -- Cumulative freq table

    Max_Freq: constant := 16#8000#;
    -- ^-- update when cumulative frequency reaches to this value

    procedure Start is
      I: Natural;
    begin
      for J in  0 .. N_Char-1  loop
        Freq(J):= 1;
        Son (J):= J + Max_Table;
        Parent(J + Max_Table):= J;
      end loop;

      I:= 0;
      for J in N_Char .. Root_Position  loop
        Freq(J):= Freq(I)+Freq(I+1);
        Son (J):= I;
        Parent(I):= J;
        Parent(I+1):= J;
        I:= I + 2;
      end loop;

      Freq( Freq'Last ):= 16#FFFF#; -- ( Max_Table )
      Parent( Root_Position ):= 0;
    end Start;

    procedure Update_Freq_Tree( C0: Natural ) is

      procedure Reconstruct_Freq_Tree is
        I,J,K,F,L: Natural;
      begin
        -- Halven cumulative freq for leaf nodes
        J:= 0;
        for I in 0 .. Root_Position loop
          if Son(I) >= Max_Table then
            Freq(J):= (Freq(I)+1) / 2;
            Son (J):= Son(I);
            J:= J + 1;
          end if;
        end loop;

        -- Make a tree : first, connect children nodes
        I:= 0;
        for J in N_Char .. Root_Position loop -- J : free nodes
          K:= I+1;
          F:= Freq(I) + Freq(K); -- new frequency
          Freq(J):= F;
          K:= J-1;
          while F < Freq(K) loop
            K:= K-1;
          end loop;

          K:= K+1;
          L:= J-K; -- 2007: fix: was L:= (J-K)*2, memcopy parameter remain

          Freq( K+1 .. K+L ):= Freq( K .. K+L-1 ); -- shift by one cell right
          Freq(K):= F;
          Son ( K+1 .. K+L ):= Son ( K .. K+L-1 ); -- shift by one cell right
          Son (K):= I;
          I:= I + 2;
        end loop;

        -- Connect parent nodes
        for I in 0 .. Max_Table-1 loop
          K:= Son(I);
          Parent(K):= I;
          if K < Max_Table then
            Parent(K+1):= I;
          end if;
        end loop;

      end Reconstruct_Freq_Tree;

      C,I,J,K,L: Natural;

    begin -- Update_Freq_Tree;
      if Freq( Root_Position ) = Max_Freq then
        Reconstruct_Freq_Tree;
      end if;
      C:= Parent(C0 + Max_Table);
      loop
        Freq(C):= Freq(C) + 1;
        K:= Freq(C);
        -- Swap nodes to keep the tree freq-ordered
        L:= C+1;
        if  K > Freq(L) then
          while K > Freq(L+1) loop
            L:= L + 1;
          end loop;

          Freq(C):= Freq(L);
          Freq(L):= K;

          I:= Son(C);
          Parent(I):= L;
          if I < Max_Table then
            Parent(I+1):= L;
          end if;

          J:= Son(L);
          Son(L):= I;

          Parent(J):= C;
          if J < Max_Table then
            Parent(J+1):= C;
          end if;
          Son(C):= J;

          C := L;
        end if;
        C:= Parent(C);
        exit when C=0;
      end loop;        -- do it until reaching the root
    end Update_Freq_Tree;

  end Huffman;

  Node_Nil : constant Integer:= String_buffer_size;    -- End of tree's node

  Lson,Dad:  array ( 0..String_buffer_size       ) of Natural;
  Rson:      array ( 0..String_buffer_size + 256 ) of Natural;

  procedure Init_Tree is
  begin
    for I in String_buffer_size+1 .. Rson'Last loop
      Rson(I) := Node_Nil;
    end loop; -- root
    for I in 0 .. String_buffer_size-1 loop
      Dad(I)  := Node_Nil;
    end loop; -- node
  end Init_Tree;

  Match_Position : Natural;
  Match_Length   : Natural;

  Text_Buf: Text_Buffer:= empty_buffer;

  procedure Insert_Node (R: Integer) is
    I,P: Integer;
    Geq: Boolean:= True;
    C:   Natural;
  begin
    P:= String_buffer_size + 1 + Integer(Text_Buf(R));
    Rson(R):= Node_Nil;
    Lson(R):= Node_Nil;
    Match_Length := 0;
    loop
      if Geq then
        if Rson(P) = Node_Nil then
          Rson(P):= R;
          Dad(R) := P;
          return;
        end if;
        P:= Rson(P);
      else
        if Lson(P) = Node_Nil then
          Lson(P):= R;
          Dad(R) := P;
          return;
        end if;
        P:= Lson(P);
      end if;
      I:= 1;
      while I < Look_Ahead and then Text_Buf(R+I) = Text_Buf(P+I)  loop
        I:= I + 1;
      end loop;

      Geq:= Text_Buf(R+I) >= Text_Buf(P+I) or I = Look_Ahead;

      if I > Threshold then
        if I > Match_Length then
          Match_Position := (R-P) mod String_buffer_size - 1;
          Match_Length:= I;
          exit when Match_Length >= Look_Ahead;
        end if;
        if I = Match_Length then
          C:= (R-P) mod String_buffer_size - 1;
          if C < Match_Position then
            Match_Position:= C;
          end if;
        end if;
      end if;
    end loop;

    Dad (R):= Dad (P);
    Lson(R):= Lson(P);
    Rson(R):= Rson(P);
    Dad(Lson(P)):= R;
    Dad(Rson(P)):= R;
    if Rson(Dad(P)) = P then
      Rson(Dad(P)):= R;
    else
      Lson(Dad(P)):= R;
    end if;
    Dad(P):= Node_Nil; -- remove P
  end Insert_Node;

  procedure Delete_Node (P: Natural) is
    Q: Natural;
  begin
    if Dad(P) = Node_Nil then  -- unregistered
      return;
    end if;
    if    Rson(P) = Node_Nil then
      Q:= Lson(P);
    elsif Lson(P) = Node_Nil then
      Q:= Rson(P);
    else
      Q:= Lson(P);
      if Rson(Q) /= Node_Nil then
        loop
          Q:= Rson(Q);
          exit when Rson(Q) = Node_Nil;
        end loop;
        Rson(Dad(Q)):= Lson(Q);
        Dad(Lson(Q)):= Dad(Q);
        Lson(Q):= Lson(P);
        Dad(Lson(P)):= Q;
      end if;
      Rson(Q):= Rson(P);
      Dad(Rson(P)):= Q;
    end if;
    Dad(Q):= Dad(P);
    if Rson(Dad(P))=P then
      Rson(Dad(P)):= Q;
    else
      Lson(Dad(P)):= Q;
    end if;
    Dad(P):= Node_Nil;
  end Delete_Node;

  package Huffman_E is new Huffman;

  I,R,S,Last_Match_Length: Natural;
  Len: Integer;
  C: Byte;
begin
  if not More_bytes then
    return;
  end if;
  Huffman_E.Start;
  Init_Tree;
  S:= 0;
  R:= String_buffer_size - Look_Ahead;
  Len:= 0;
  while Len < Look_Ahead and More_bytes loop
    Text_Buf(R+Len):= Read_byte;
    Len:= Len + 1;
  end loop;

  --  Seems: fill dictionary with default value
  --
  --  for I in 1.. Look_Ahead loop
  --    Insert_Node(R - I);
  --  end loop;

  Insert_Node(R);

  loop
    if Match_Length > Len then
      Match_Length:= Len;
    end if;
    if Match_Length <= Threshold then
      Match_Length := 1;
      Huffman_E.Update_Freq_Tree( Natural(Text_Buf(R)) );
      Write_byte( Text_Buf(R) );
    else
      Write_code(Match_Position+1, Match_Length);
    end if;
    Last_Match_Length := Match_Length;
    I:= 0;
    while I < Last_Match_Length and More_bytes loop
      I:= I + 1;
      Delete_Node(S);
      C:= Read_byte;
      Text_Buf(S):= C;
      if  S < Look_Ahead-1 then
        Text_Buf(S+String_buffer_size):= C;
      end if;
      S:= (S+1) mod String_buffer_size;
      R:= (R+1) mod String_buffer_size;
      Insert_Node(R);
    end loop;

    while I < Last_Match_Length loop
      I:= I + 1;
      Delete_Node(S);
      S := (S+1) mod String_buffer_size;
      R := (R+1) mod String_buffer_size;
      Len:= Len - 1;
      if Len > 0 then
        Insert_Node(R);
      end if;
    end loop;

    exit when Len=0;
  end loop;

end Zip.LZ77;
