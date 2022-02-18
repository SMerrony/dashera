-- Copyright ©2022 Steve Merrony

-- This file is a part of DasherA.

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

with Ada.Directories;
with Ada.Text_IO;

with Redirector; use Redirector;

package body Xmodem is

   function BA_To_String (BA : in Byte_Arr) return string is
      Str : string (1..BA'Length);
   begin
      for Ix in Str'Range loop
         Str(Ix) := Byte_To_Char (BA(Ix));
      end loop;
      return Str;
   end BA_To_String;

   function CRC_16 (BA : in Byte_Arr) return Unsigned_16 is
      CRC : Unsigned_16 := 0;
      Part  : Unsigned_16;
   begin
      for U8 of BA loop
         Part := Unsigned_16(U8);
         CRC := CRC xor Shift_Left (Part, 8);
         for I in 0 .. 7 loop
            if (CRC and 16#8000#) > 0 then
               CRC := Shift_Left(CRC, 1) xor 16#1021#;
            else
               CRC := Shift_Left(CRC, 1);
            end if;
         end loop;
      end loop;
      return CRC;
   end CRC_16;

   function CRC_16_Fixed_Len (BA : in Byte_Arr; FL : in Positive) return Unsigned_16 is
      CRC : Unsigned_16 := 0;
   begin
      -- the data part...
      CRC := CRC_16 (BA);

      -- the padding to the fixed length...
      for C in 0 .. (FL - BA'Length - 1) loop
         CRC := CRC xor 16#0400#;
         for I in 0 .. 7 loop
            if (CRC and 16#8000#) > 0 then
               CRC := Shift_Left(CRC, 1) xor 16#1021#;
            else
               CRC := Shift_Left(CRC, 1);
            end if;
         end loop;
      end loop;

      return CRC;
   end CRC_16_Fixed_Len;

   procedure Send_Block (Data : in Byte_Arr; Block_Num : in Natural; Block_Size : in Packet_Size) is
      Start_Bytes : string (1..3);
      Block_Pos : constant Unsigned_8  := Unsigned_8(Block_Num mod 256);
      Block_Inv : constant Unsigned_8  := not Block_Pos;
      CRC       : constant Unsigned_16 := CRC_16_Fixed_Len (Data, Block_Size'Enum_Rep);
      CRC_Str   : String (1..2);
      Padding_String : string (1..1);
   begin
      if Block_Size = Short then
         Start_Bytes(1) := Ascii.SOH;
      else 
         Start_Bytes(1) := Ascii.STX;
      end if;
      Start_Bytes(2) := Character'Val(Block_Pos);
      Start_Bytes(3) := Character'Val(Block_Inv);
      if Tracing then
         Ada.Text_IO.Put_Line ("DEBUG: X-Modem sending start byte and block number: " & Block_Pos'Image);
      end if;
      Router.Send_Data (Start_Bytes);

      -- Send the actual data
      Router.Send_Data (BA_To_String (Data));

      -- Pad out block
      if Data'Length < Block_Size'Enum_Rep then
         Padding_String(1) := Ascii.EOT;
         for Ix in Data'Length .. Block_Size'Enum_Rep loop
            Router.Send_Data (Padding_String);
         end loop;
      end if;

      CRC_Str(1) := Byte_To_Char ( Unsigned_8(Shift_Right (CRC, 8)));
      CRC_Str(2) := Byte_To_Char ( Unsigned_8(CRC and 16#00ff#));
      Router.Send_Data (CRC_Str);

   end Send_Block;

   procedure Receive (Filename : in String) is
   begin
      if Ada.Directories.Exists (Filename) then
         raise Already_Exists;
         return;
      end if;
      Create (RX_File, Name => Filename);
      Receiver_Task.Start;
      loop
         select
            Receiver_Task.Done;
            Ada.Text_IO.Put_Line ("DEBUG: Xmodem Receive is complete");
            -- TODO
            exit;
         or
            delay 1.0;
            Ada.Text_IO.Put_Line ("DEBUG: Xmodem waiting for Receive to complete");
         end select;
      end loop;
      Close (RX_File);
   end Receive;

   task body Receiver is
      Finished    : Boolean;
      Packet_Size : Natural;
      New_Packet  : Boolean;
      Packet_Count, Inverse_Packet_Count : Unsigned_8;
      This_Packet_Len : Positive;
      Rxd_CRC, Calcd_CRC : Unsigned_16;
   begin
      accept Start do
         Finished := False;
         New_Packet := True;
      end Start;
      Router.Send_Data ("" & 'C'); -- POLL
      while not Finished loop -- per packet
         accept Accept_Data (Char : in Character) do
            case Char is
               when Ascii.EOT => 
                  Router.Send_Data ("" & Ascii.ACK); 
                  Finished := True;
               when Ascii.SOH =>
                  Packet_Size := 128; -- short packets
               when Ascii.STX =>
                  Packet_Size := 1024; -- long packets
               when Ascii.CAN =>
                  raise Sender_Cancelled;
               when others =>
                  raise Protocol_Error;
            end case;
         end Accept_Data;
         if Finished then exit; end if;

         accept Accept_Data (Char : in Character) do
            Packet_Count := Char_To_Byte (Char);
         end Accept_Data; 

         accept Accept_Data (Char : in Character) do
            Inverse_Packet_Count := Char_To_Byte (Char);
         end Accept_Data; 

         if (not Packet_Count) /= Inverse_Packet_Count then
            Router.Send_Data ("" & Ascii.NAK);
            goto Next_Packet;
         end if;

         This_Packet_Len := Positive(Packet_Count);

         declare
            Packet : Byte_Arr(0 .. This_Packet_Len - 1);
         begin
            for B in Packet'Range loop
               accept Accept_Data (Char : in Character) do
                  Packet(B) := Char_To_Byte (Char);
               end Accept_Data;
            end loop;
         

            accept Accept_Data (Char : in Character) do
               Rxd_CRC := Unsigned_16(Char_To_Byte (Char));
            end Accept_Data;
            Rxd_CRC := Shift_Left (Rxd_CRC, 8);
            accept Accept_Data (Char : in Character) do
               Rxd_CRC := Rxd_CRC + Unsigned_16(Char_To_Byte (Char));
            end Accept_Data;

            Calcd_CRC := CRC_16 (Packet);

            if Rxd_CRC = Calcd_CRC then
               Router.Send_Data ("" & Ascii.ACK);
            else
               Router.Send_Data ("" & Ascii.NAK);
               Ada.Text_IO.Put_Line ("WARNING: Xmodem sending ACK due to CRC error");
            end if;

         end;

         <<Next_Packet>>
      end loop;
   end Receiver;

end Xmodem;