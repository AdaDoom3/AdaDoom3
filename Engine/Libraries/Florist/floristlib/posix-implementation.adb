pragma Source_Reference (1, "libsrc/posix-implementation.gpb");
------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                  P O S I X . I M P L E M E N T A T I O N                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

with Ada.Exceptions,
--! #    if HAVE_Safe_Errno then
--! #    else
--!      POSIX.Error_Codes,
--! #    end if;
     System.Interrupt_Management.Operations,
     GNAT.Task_Lock,
     System.Soft_Links;

package body POSIX.Implementation is

   use POSIX.C;

   package SIM renames System.Interrupt_Management;
   package SIMO renames System.Interrupt_Management.Operations;

--! #  if HAVE_Safe_Errno then

   procedure Set_Ada_Error_Code (Error : Error_Code) is
   begin
      Store_Errno (Error);
   end Set_Ada_Error_Code;

   function Get_Ada_Error_Code return Error_Code is
   begin
      return Fetch_Errno;
   end Get_Ada_Error_Code;

--! #  else
--!
--!    procedure Set_Ada_Error_Code (Error : Error_Code) is
--!    begin
--!       POSIX.Error_Codes.Set_Value (Error);
--!    end Set_Ada_Error_Code;
--!
--!    function Get_Ada_Error_Code return Error_Code is
--!    begin
--!       return POSIX.Error_Codes.Value;
--!    end Get_Ada_Error_Code;
--!
--! #  end if;

   --  .... It would be nice if we had a way to check whether we
   --  are in a critical section, at the points (below) where we are
   --  about to raise an exception.  These routines should never be
   --  called from inside a critical section, but that is an easy
   --  mistake to make.

   ------------------------------
   --  Begin_Critical_Section  --
   ------------------------------

   procedure Begin_Critical_Section is
   begin
      GNAT.Task_Lock.Lock;
   end Begin_Critical_Section;

   ----------------------------
   --  End_Critical_Section  --
   ----------------------------

   procedure End_Critical_Section is
   begin
      GNAT.Task_Lock.Unlock;
   end End_Critical_Section;

   ----------------------
   --  Defer_Abortion  --
   ----------------------

   procedure Defer_Abortion is
   begin
      System.Soft_Links.Abort_Defer.all;
   end Defer_Abortion;

   ------------------------
   --  Undefer_Abortion  --
   ------------------------

   procedure Undefer_Abortion is
   begin
      System.Soft_Links.Abort_Undefer.all;
   end Undefer_Abortion;

   -------------------------
   --  Raise_POSIX_Error  --
   -------------------------

   procedure Raise_POSIX_Error (Error : Error_Code := No_Error) is
      Tmp : Error_Code := Error;
   begin
      --  .... see note on critical sections above
      if Error = No_Error then
         Tmp := Fetch_Errno;
      end if;
      Set_Ada_Error_Code (Tmp);
      Ada.Exceptions.Raise_Exception
           (POSIX_Error'Identity, Image (Tmp));
   end Raise_POSIX_Error;

   -------------
   --  Check  --
   -------------

   procedure Check (Condition : Boolean;
                    Error : Error_Code;
                    Old_Mask : Signal_Mask_Access := null) is
   begin
      --  .... see note on critical sections above
      if not Condition then
         if Old_Mask /= null then
            Restore_Signals (Old_Mask);
         end if;
         Raise_POSIX_Error (Error);
      end if;
   end Check;

   procedure Check (Result : int; Old_Mask : Signal_Mask_Access := null) is
   begin
      --  .... see note on critical sections above
      if Result = -1 then
         if Old_Mask /= null then
            Restore_Signals (Old_Mask);
         end if;
         Raise_POSIX_Error (Fetch_Errno);
      end if;
   end Check;

   function Check (Result : int; Old_Mask : Signal_Mask_Access := null)
                  return int is
   begin
      --  .... see note on critical sections above
      if Result = -1 then
         if Old_Mask /= null then
            Restore_Signals (Old_Mask);
         end if;
         Raise_POSIX_Error (Fetch_Errno);
      end if;
      return Result;
   end Check;

   --  ....is there a better work-around????
   --  Provenzano's threads seem to
   --  return nonstandard negative values for some calls,
   --  like "close".

   procedure Check_NNeg (Result : int) is
   begin
      --  .... see note on critical sections above
      if Result < 0 then
         Raise_POSIX_Error (Fetch_Errno);
      end if;
   end Check_NNeg;

   --  ....is there a better work-around????
   --  Provenzano's threads seem to
   --  return nonstandard negative values for some calls,
   --  like "close".

   function Check_NNeg (Result : int) return int is
   begin
      --  .... see note on critical sections above.
      if Result < 0 then
         Raise_POSIX_Error (Fetch_Errno);
      end if;
      return Result;
   end Check_NNeg;

   procedure Check_NZ (Result : int) is
   begin
      --  .... see note on critical sections above.
      if Result /= 0 then
         Raise_POSIX_Error (Error_Code (Result));
      end if;
   end Check_NZ;

   -------------------
   --  Form_String  --
   -------------------

   function strlen (str : char_ptr) return size_t;
   pragma Import (C, strlen, "strlen");

   function Form_String (Str : char_ptr) return String is
   begin
      if Str = null then
         return "";
      end if;
      declare
         subtype Substring is String (1 .. Integer (strlen (Str)));
         type Substring_Ptr is access Substring;
         pragma Warnings (Off);
         function char_ptr_to_pssptr is new Unchecked_Conversion
           (char_ptr, Substring_Ptr);
         pragma Warnings (On);
      begin
         return char_ptr_to_pssptr (Str).all;
      end;
   end Form_String;

   ---------------------------
   --  Trim_Leading_Blanks  --
   ---------------------------

   function Trim_Leading_Blank (S : String) return String is
   begin
      if S (S'First) /= ' ' then
         return S;
      end if;
      return S (S'First + 1 .. S'Last);
   end Trim_Leading_Blank;

   --------------------
   --  Nulterminate  --
   --------------------

   type Big_POSIX_String_Ptr is access all POSIX_String (Positive'Range);

   function From_Address is new Unchecked_Conversion
     (System.Address, Big_POSIX_String_Ptr);

   procedure Nulterminate
     (To : out POSIX_String;
      From :  String) is
      L : constant Positive := From'Length;
   begin
      if To'Length <= L then
         raise Constraint_Error;
      end if;
      To (1 .. L) := From_Address (From'Address) (1 .. L);
      To (L + 1) := NUL;
   end Nulterminate;

   -----------------------
   --  Not_Implemented  --
   -----------------------

   function Not_Implemented_Neg_One return int is
   begin
      Store_Errno (ENOSYS);
      return -1;
   end Not_Implemented_Neg_One;

   function Not_Implemented_Direct return int is
   begin
      return ENOSYS;
   end Not_Implemented_Direct;

   function Not_Supported_Neg_One return int is
   begin
      Store_Errno (ENOTSUP);
      return -1;
   end Not_Supported_Neg_One;

   function Not_Supported_Direct return int is
   begin
      return ENOTSUP;
   end Not_Supported_Direct;

   ----------------------
   --  Signal Masking  --
   ----------------------

   --  For RTS_Signals we mask all the signals identified as reserved
   --  by the tasking RTS. However, we leave SIGABRT alone since it is being
   --  used as the signal for abortion which needs to be invoked for
   --  POSIX.Signals.Interrupt_Task. Do not mask SIGTRAP either because
   --  this signal is used by the debugger.
   --  ...Fix POSIX.5b????
   --  It seems we are deviating here from what the standard says, but for
   --  very good reasons.

   procedure Mask_Signals
     (Masking  : Signal_Masking;
      Old_Mask : Signal_Mask_Access)
   is
      use type SIM.Interrupt_ID;
   begin
      if Masking /= No_Signals then
         declare
            New_Mask : aliased Signal_Mask;
         begin
            Begin_Critical_Section;

            SIMO.Get_Interrupt_Mask (New_Mask'Unchecked_Access);
            SIMO.Copy_Interrupt_Mask (Old_Mask.all, New_Mask);
            if Masking = RTS_Signals then
               for J in 1 .. SIM.Interrupt_ID'Last loop
                  if SIM.Reserve (J) and J /= SIGABRT and J /= SIGTRAP then
                     SIMO.Add_To_Interrupt_Mask (New_Mask'Unchecked_Access, J);
                  end if;
               end loop;
            else --  All_Signals
               SIMO.Fill_Interrupt_Mask (New_Mask'Unchecked_Access);
            end if;
            SIMO.Set_Interrupt_Mask (New_Mask'Unchecked_Access);
            End_Critical_Section;
         end;
      end if;
   end Mask_Signals;

   procedure Restore_Signals
      (Masking : Signal_Masking;
       Old_Mask : Signal_Mask_Access) is
   begin
      if Masking /= No_Signals then
         Begin_Critical_Section;
         SIMO.Set_Interrupt_Mask (Old_Mask);
         End_Critical_Section;
      end if;
   end Restore_Signals;

   procedure Restore_Signals
       (Old_Mask : Signal_Mask_Access) is
   begin
      Begin_Critical_Section;
      SIMO.Set_Interrupt_Mask (Old_Mask);
      End_Critical_Section;
   end Restore_Signals;

   -------------------------------------
   --  Check_..._And_Restore_Signals  --
   -------------------------------------

   procedure Restore_Signals_And_Raise_POSIX_Error
     (Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access) is
      Error : constant Error_Code := Fetch_Errno;
   begin
      Restore_Signals (Masked_Signals, Old_Mask);
      Raise_POSIX_Error (Error);
   end Restore_Signals_And_Raise_POSIX_Error;

   procedure Check_NNeg_And_Restore_Signals
     (Result : int;
      Masked_Signals : Signal_Masking;
      Old_Mask : Signal_Mask_Access) is
   begin
      if Result < 0 then
         Restore_Signals_And_Raise_POSIX_Error
           (Masked_Signals, Old_Mask);
      else
         Restore_Signals (Masked_Signals, Old_Mask);
      end if;
   end Check_NNeg_And_Restore_Signals;

   --------------------------
   --  To_Struct_Timespec  --
   --------------------------

   function To_Struct_Timespec (D : Duration) return struct_timespec is
      S : time_t;
      F : Duration;
   begin
      S := time_t (Long_Long_Integer (D));
      F := D - Duration (S);
      --  If F has negative value due to a round-up, adjust for positive F
      --  value.
      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;
      return struct_timespec'(tv_sec => S,
        tv_nsec => long (Long_Long_Integer (F * NS_per_S)));
   end To_Struct_Timespec;

   function To_Struct_Timespec (T : Timespec) return struct_timespec is
   begin
      return To_Struct_Timespec (To_Duration (T));
   end To_Struct_Timespec;

   -------------------
   --  To_Duration  --
   -------------------

   function To_Duration (TS : struct_timespec) return Duration is
   begin
      return Duration (TS.tv_sec) + Duration (TS.tv_nsec) / NS_per_S;
   end To_Duration;

   -------------------
   --  To_Timespec  --
   -------------------

   function To_Timespec (TS : struct_timespec) return Timespec is
   begin
      return Timespec'
        (Val => Duration (TS.tv_sec) + Duration (TS.tv_nsec) / NS_per_S);
   end To_Timespec;

   -------------------
   --  To_Duration  --
   -------------------

   function To_Duration (TV : struct_timeval) return Duration is
   begin
      return Duration (TV.tv_sec) + Duration (TV.tv_usec) / MS_per_S;
   end To_Duration;

   -------------------------
   --  To_Struct_Timeval  --
   -------------------------

   function To_Struct_Timeval (D : Duration) return struct_timeval is
      S : time_t;
      F : Duration;
   begin
      S := time_t (Long_Long_Integer (D));
      F := D - Duration (S);
      --  If F has negative value due to a round-up, adjust for positive F
      --  value.
      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;
      return struct_timeval'(tv_sec => S,
        tv_usec => suseconds_t (Long_Long_Integer (F * MS_per_S)));
   end To_Struct_Timeval;

end POSIX.Implementation;
