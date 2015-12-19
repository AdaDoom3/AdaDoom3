------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                  P O S I X . M E S S A G E _ Q U E U E S                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2010, AdaCore                     --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5: 1990  and IEEE STD  --
--  1003.5b: 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams,
     POSIX.Implementation,
     POSIX.Permissions.Implementation,
     System,
     Unchecked_Conversion;

package body POSIX.Message_Queues is

   use Ada.Streams;
   use POSIX.C;
   use POSIX.Implementation;
   use POSIX.Permissions.Implementation;

   function To_int is new Unchecked_Conversion (Bits, int);
   function To_Bits is new Unchecked_Conversion (int, Bits);
   C_File_Mode : constant array (POSIX.IO.File_Mode) of Bits :=
     (POSIX.IO.Read_Only  => O_RDONLY,
      POSIX.IO.Write_Only => O_WRONLY,
      POSIX.IO.Read_Write => O_RDWR);

   function Check_NNeg_And_Restore_Signals
     (Result : Message_Queue_Descriptor;
      Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access) return Message_Queue_Descriptor;

   function Check_NNeg_And_Restore_Signals
     (Result : Message_Queue_Descriptor;
      Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access) return Message_Queue_Descriptor is
   begin
      if Result < 0 then
         Restore_Signals_And_Raise_POSIX_Error
           (Masked_Signals, Old_Mask);
         return Result;
      else
         Restore_Signals (Masked_Signals, Old_Mask);
         return Result;
      end if;
   end Check_NNeg_And_Restore_Signals;

   ------------------------
   --  Set_Max_Messages  --
   ------------------------

   procedure Set_Max_Messages
     (Attrs : in out Attributes;
      Value : Natural) is
   begin
      Attrs.Attrs.mq_maxmsg := long (Value);
   end Set_Max_Messages;

   ------------------------
   --  Get_Max_Messages  --
   ------------------------

   function Get_Max_Messages (Attrs : Attributes) return Natural is
   begin
      return Natural (Attrs.Attrs.mq_maxmsg);
   end Get_Max_Messages;

   --------------------------
   --  Set_Message_Length  --
   --------------------------

   procedure Set_Message_Length
     (Attrs : in out Attributes;
      Value : Natural) is
   begin
      Attrs.Attrs.mq_msgsize := long (Value);
   end Set_Message_Length;

   --------------------------
   --  Get_Message_Length  --
   --------------------------

   function Get_Message_Length (Attrs : Attributes) return Natural is
   begin
      return Natural (Attrs.Attrs.mq_msgsize);
   end Get_Message_Length;

   -------------------
   --  Set_Options  --
   -------------------

   procedure Set_Options
     (Attrs : in out Attributes;
      Value : Message_Queue_Options) is
   begin
      Attrs.Attrs.mq_flags := long (To_int (Option_Set (Value).Option));
   end Set_Options;

   -------------------
   --  Get_Options  --
   -------------------

   function Get_Options (Attrs : Attributes) return Message_Queue_Options is
   begin
      return Message_Queue_Options
        (Option_Set '(Option => To_Bits (int (Attrs.Attrs.mq_flags))));
      --  ????
      --  The above conversion of long value to int is risky.
      --  If the high-order bits are used, we may need to consider
      --  reimplementing Option_Set as long, or changing the POSIX.5b spec.
      --  .... Change POSIX.5b?
      --  It was a mistake to use Option_Set here for a value that the
      --  C-language interface says is a "long".  Option_Set in other places
      --  is only used to map bit-vectors of type "int".
   end Get_Options;

   -------------------------
   --  Get_Message_Count  --
   -------------------------

   function Get_Message_Count (Attrs : Attributes) return Natural is
   begin
      return Natural (Attrs.Attrs.mq_curmsgs);
   end Get_Message_Count;

   ------------
   --  Open  --
   ------------

   function mq_open
     (name  : char_ptr;
      oflag : int;
      mode  : mode_t;
      attr  : mq_attr_ptr) return Message_Queue_Descriptor;
   pragma Import (C, mq_open, mq_open_LINKNAME);

   function Open
     (Name           : POSIX_String;
      Mode           : POSIX.IO.File_Mode;
      Options        : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Masked_Signals : Signal_Masking := RTS_Signals)
     return Message_Queue_Descriptor is
      Name_With_NUL : POSIX_String := Name & NUL;
      Old_Mask : aliased Signal_Mask;
      Result : Message_Queue_Descriptor;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := mq_open (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access,
        To_int (Option_Set (Options).Option or C_File_Mode (Mode)),
        0, null);
      return Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Open;

   ----------------------
   --  Open_Or_Create  --
   ----------------------

   function Open_Or_Create
     (Name           : POSIX_String;
      Mode           : POSIX.IO.File_Mode;
      Permissions    : POSIX.Permissions.Permission_Set;
      Options        : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Masked_Signals : Signal_Masking := RTS_Signals)
     return Message_Queue_Descriptor is
      Name_With_NUL : POSIX_String := Name & NUL;
      Old_Mask : aliased Signal_Mask;
      Result : Message_Queue_Descriptor;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := mq_open (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access,
        To_int (Option_Set (Options).Option or C_File_Mode (Mode) or O_CREAT),
        Form_C_Permission (Permissions), null);
      return Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Open_Or_Create;

   function Open_Or_Create
     (Name           : POSIX_String;
      Mode           : POSIX.IO.File_Mode;
      Permissions    : POSIX.Permissions.Permission_Set;
      Options        : POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set;
      Attrs          : Attributes;
      Masked_Signals : Signal_Masking := RTS_Signals)
     return Message_Queue_Descriptor is
      Name_With_NUL : POSIX_String := Name & NUL;
      Old_Mask : aliased Signal_Mask;
      Result : Message_Queue_Descriptor;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := mq_open (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access,
        To_int (Option_Set (Options).Option or C_File_Mode (Mode) or O_CREAT),
        Form_C_Permission (Permissions), Attrs.Attrs'Unchecked_Access);
      return Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Open_Or_Create;

   -------------
   --  Close  --
   -------------

   procedure Close (MQ : in out Message_Queue_Descriptor) is
      function mq_close (mqdes : Message_Queue_Descriptor) return int;
      pragma Import (C, mq_close, mq_close_LINKNAME);
   begin
      Check (mq_close (MQ));
   end Close;

   ----------------------------
   --  Unlink_Message_Queue  --
   ----------------------------

   procedure Unlink_Message_Queue (Name : POSIX_String) is
      function mq_unlink (name : char_ptr) return int;
      pragma Import (C, mq_unlink, mq_unlink_LINKNAME);
      Name_With_NUL : POSIX_String := Name & NUL;
   begin
      Check (mq_unlink (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access));
   end Unlink_Message_Queue;

   ------------
   --  Send  --
   ------------

   function mq_send
     (mqdes    : Message_Queue_Descriptor;
      msg_ptr  : char_ptr;
      msg_len  : size_t;
      msg_prio : unsigned) return int;
   pragma Import (C, mq_send, mq_send_LINKNAME);

   procedure Send
     (MQ             : Message_Queue_Descriptor;
      Message        : Ada.Streams.Stream_Element_Array;
      Priority       : Message_Priority;
      Masked_Signals : Signal_Masking := RTS_Signals) is
      Old_Mask : aliased Signal_Mask;
      Result : int;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := mq_send
        (MQ, To_char_ptr (Message (Message'First)'Address),
         size_t (Message'Length),
         unsigned (Priority));
      Check_NNeg_And_Restore_Signals
        (Result, Masked_Signals, Old_Mask'Unchecked_Access);
   end Send;

   ---------------
   --  Receive  --
   ---------------

   function mq_receive
     (mqdes    : Message_Queue_Descriptor;
      msg_ptr  : System.Address;
      msg_len  : size_t;
      msg_prio : access unsigned) return ssize_t;
   pragma Import (C, mq_receive, mq_receive_LINKNAME);

   procedure Receive
     (MQ             : Message_Queue_Descriptor;
      Message        : out Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset;
      Priority       : out Message_Priority;
      Masked_Signals : Signal_Masking := RTS_Signals) is
      Old_Mask : aliased Signal_Mask;
      Prio : aliased unsigned;
      Result : ssize_t;
   begin
      Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
      Result := mq_receive
        (MQ, Message (Message'First)'Address,
         size_t (Message'Length),
         Prio'Unchecked_Access);
      Check_NNeg_And_Restore_Signals
        (int (Result), Masked_Signals, Old_Mask'Unchecked_Access);
      Priority := Message_Priority (Prio);
      Last := Message'First + Stream_Element_Offset (Result) - 1;
   end Receive;

   package body Generic_Message_Queues is

      SES : constant Stream_Element_Offset := Stream_Element'Size;
      Buffer_Length : constant Stream_Element_Offset :=
        (Message_Type'Size + SES - 1) / SES;
      Buffer : aliased Stream_Element_Array (1 .. Buffer_Length);
      Length : Stream_Element_Offset;

      ------------
      --  Send  --
      ------------

      procedure Send
        (MQ             : Message_Queue_Descriptor;
         Message        : Message_Type;
         Priority       : Message_Priority;
         Masked_Signals : Signal_Masking := RTS_Signals) is
         Old_Mask : aliased Signal_Mask;
         Result : int;
      begin
         Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
         Result := mq_send
           (MQ, To_char_ptr (Message'Address),
            size_t ((Message'Size + char'Size - 1) / char'Size),
            unsigned (Priority));
         Check_NNeg_And_Restore_Signals
           (Result, Masked_Signals, Old_Mask'Unchecked_Access);
      end Send;

      ---------------
      --  Receive  --
      ---------------

      type Message_Ptr is access all Message_Type;
      function To_Message_Ptr is
        new Unchecked_Conversion (System.Address, Message_Ptr);

      procedure Receive
        (MQ             : Message_Queue_Descriptor;
         Message        : out Message_Type;
         Priority       : out Message_Priority;
         Masked_Signals : Signal_Masking := RTS_Signals) is
         Old_Mask : aliased Signal_Mask;
         Prio : aliased unsigned;
         Result : ssize_t;
      begin
         Mask_Signals (Masked_Signals, Old_Mask'Unchecked_Access);
         Result := mq_receive
           (MQ, Buffer'Address,
            size_t (Buffer'Size / char'Size),
            Prio'Unchecked_Access);
         Check_NNeg_And_Restore_Signals
           (int (Result), Masked_Signals, Old_Mask'Unchecked_Access);
         Length := Stream_Element_Offset (Result);
         if Result /= Buffer'Size / char'Size then
            raise Constraint_Error;
         end if;
         Priority := Message_Priority (Prio);
         Message := To_Message_Ptr (Buffer'Address).all;
      end Receive;

      ------------------------
      --  Get_Error_Buffer  --
      ------------------------

      function Get_Error_Buffer return Ada.Streams.Stream_Element_Array is
      begin
         return Buffer (1 .. Length);
      end Get_Error_Buffer;

   end Generic_Message_Queues;

   ----------------------
   --  Request_Notify  --
   ----------------------

   type Event_Ptr is access all POSIX.Signals.Signal_Event;
   function mq_notify
     (mqdes        : Message_Queue_Descriptor;
      notification : Event_Ptr) return int;
   pragma Import (C, mq_notify, mq_notify_LINKNAME);

   procedure Request_Notify
     (MQ    : Message_Queue_Descriptor;
      Event : POSIX.Signals.Signal_Event) is
      E : aliased POSIX.Signals.Signal_Event := Event;
   begin
      Check (mq_notify (MQ, E'Unchecked_Access));
   end Request_Notify;

   ---------------------
   --  Remove_Notify  --
   ---------------------

   procedure Remove_Notify (MQ : Message_Queue_Descriptor) is
   begin
      Check (mq_notify (MQ, null));
   end Remove_Notify;

   ----------------------
   --  Set_Attributes  --
   ----------------------

   function mq_setattr
     (mqdes   : Message_Queue_Descriptor;
      mqstat  : mq_attr_ptr;
      omqstat : mq_attr_ptr) return int;
   pragma Import (C, mq_setattr, mq_setattr_LINKNAME);

   procedure Set_Attributes
     (MQ        : Message_Queue_Descriptor;
      New_Attrs : Attributes;
      Old_Attrs : out Attributes) is
   begin
      Check (mq_setattr
        (MQ,
         New_Attrs.Attrs'Unchecked_Access,
         Old_Attrs.Attrs'Unchecked_Access));
   end Set_Attributes;

   ----------------------
   --  Set_Attributes  --
   ----------------------

   procedure Set_Attributes
     (MQ        : Message_Queue_Descriptor;
      New_Attrs : Attributes) is
   begin
      Check (mq_setattr (MQ, New_Attrs.Attrs'Unchecked_Access, null));
   end Set_Attributes;

   ----------------------
   --  Get_Attributes  --
   ----------------------

   function Get_Attributes (MQ : Message_Queue_Descriptor) return Attributes is
      function mq_getattr
        (mqdes  : Message_Queue_Descriptor;
         mqstat : access struct_mq_attr) return int;
      pragma Import (C, mq_getattr, mq_getattr_LINKNAME);
      Attrs : Attributes;
   begin
      Check (mq_getattr (MQ, Attrs.Attrs'Unchecked_Access));
      return Attrs;
   end Get_Attributes;

end POSIX.Message_Queues;
