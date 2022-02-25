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

   function CRC_16 (Data : in Vector) return Unsigned_16 is
      CRC : Unsigned_16 := 0;
      Part  : Unsigned_16;
   begin
      for C of Data loop
         Part := Unsigned_16(Char_To_U8(C));
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

   function CRC_16_Fixed_Len (Data : in Vector; FL : in Positive) return Unsigned_16 is
      CRC : Unsigned_16 := 0;
   begin
      -- the data part...
      CRC := CRC_16 (Data);

      -- the padding to the fixed length...
      for C in 0 .. (FL - Positive(Data.Length) - 1) loop
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

   procedure Send_Block (Data : in Vector; Block_Num : in Natural; Block_Size : in Packet_Size) is
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
      for C of Data loop
         Router.Send_Data ("" & C);
      end loop;

      -- Pad out block
      if Data.Length < Block_Size'Enum_Rep then
         Padding_String(1) := Ascii.EOT;
         for Ix in Data.Length .. Block_Size'Enum_Rep loop
            Router.Send_Data (Padding_String);
         end loop;
      end if;

      CRC_Str(1) := Byte_To_Char ( Unsigned_8(Shift_Right (CRC, 8)));
      CRC_Str(2) := Byte_To_Char ( Unsigned_8(CRC and 16#00ff#));
      Router.Send_Data (CRC_Str);

   end Send_Block;

   procedure Receive (Filename : in String; Trace_Flag : in Boolean) is
      RX_File : File_Type;
      RX_Stream : Stream_Access;
   begin
      Tracing := Trace_Flag;
      if Ada.Directories.Exists (Filename) then
         raise Already_Exists;
         return;
      end if;
      Create (RX_File, Name => Filename);
      RX_Stream := Stream(RX_File);
      Ada.Text_IO.Put_Line ("DEBUG: Xmodem Created file: " & Filename);
      Receiver_Task := new Receiver;
      Router.Set_Handler (Handlr => File_Transfer);
      Receiver_Task.Start (RX_Stream);
      loop
         select
            Receiver_Task.Done;
            Ada.Text_IO.Put_Line ("DEBUG: Xmodem Receive is complete");
            Close (RX_File);
            Router.Set_Handler (Handlr => Visual);
            exit;
         or
            delay 1.0;
            Ada.Text_IO.Put_Line ("DEBUG: Xmodem waiting for Receive to complete");
         end select;
      end loop;
   end Receive;

   task body Receiver is
      Finished    : Boolean;
      Packet_Size : Natural;
      Packet_Count, Inverse_Packet_Count : Unsigned_8;
      Rxd_CRC, Calcd_CRC : Unsigned_16;
      Write_Stream : Stream_Access;

      File_Blob, Packet : Vector;
      Pkt_Hdr : Character;

   begin
      accept Start (RX_Stream : Stream_Access) do
         Write_Stream := RX_Stream;
         Finished := False;
         File_Blob.Clear;
         Packet.Clear;
      end Start;
      Ada.Text_IO.Put_Line ("DEBUG: Xmodem Sending POLL");
      Router.Send_Data ("" & 'C'); -- POLL
      while not Finished loop -- per packet
         Packet.Clear;

         accept Accept_Data (Char : in Character) do
            Pkt_Hdr := Char;
         end Accept_Data;

         case Pkt_Hdr is
            when Ascii.EOT | Ascii.SUB => 
               Ada.Text_IO.Put_Line ("DEBUG: Xmodem Got EOT (End of Transmission)");
               Router.Send_Data ("" & Ascii.ACK); 
               Ada.Text_IO.Put_Line ("DEBUG: Xmodem Sent final ACK");
               Finished := True;
            when Ascii.SOH =>
               Ada.Text_IO.Put_Line ("DEBUG: Xmodem Got SOH (Short packets indicator)");
               Packet_Size := 128; -- short packets
            when Ascii.STX =>
               Ada.Text_IO.Put_Line ("DEBUG: Xmodem Got STX (Long packets indicator)");
               Packet_Size := 1024; -- long packets
            when Ascii.CAN =>
               raise Sender_Cancelled;
            when others =>
               raise Protocol_Error;
         end case;

         if Finished then 
            Router.Set_Handler (Handlr => Visual);
            -- final packet may have trailing EOFs
            while File_Blob(File_Blob.Last_Index) = Ascii.SUB loop
               File_Blob.Delete (File_Blob.Last_Index);
            end loop;
            for C of File_Blob loop
               Character'Write (Write_Stream, C);
            end loop;
            accept Done;

         else

            accept Accept_Data (Char : in Character) do
               Packet_Count := Char_To_U8 (Char);
            end Accept_Data; 
            Ada.Text_IO.Put_Line ("DEBUG: Xmodem Got Packet Count " & Packet_Count'Image);

            accept Accept_Data (Char : in Character) do
               Inverse_Packet_Count := Char_To_U8 (Char);
            end Accept_Data; 
            Ada.Text_IO.Put_Line ("DEBUG: Xmodem Got Inverse Packet Count " & Inverse_Packet_Count'Image);

            if (not Packet_Count) /= Inverse_Packet_Count then
               Ada.Text_IO.Put_Line ("DEBUG: Xmodem Packet counts not right - sending NAK");
               Router.Send_Data ("" & Ascii.NAK);
               goto Next_Packet;
            end if;

            for B in 1 .. Packet_Size loop
               accept Accept_Data (Char : in Character) do
                  Packet.Append (Char);
               end Accept_Data;
            end loop;
         
            Ada.Text_IO.Put_Line ("DEBUG: Xmodem - Packet received");

            accept Accept_Data (Char : in Character) do
               Rxd_CRC := Unsigned_16(Char_To_U8 (Char));
            end Accept_Data;
            Rxd_CRC := Shift_Left (Rxd_CRC, 8);
            accept Accept_Data (Char : in Character) do
               Rxd_CRC := Rxd_CRC + Unsigned_16(Char_To_U8 (Char));
            end Accept_Data;
            Ada.Text_IO.Put_Line ("DEBUG: Xmodem Received CRC is " & Rxd_CRC'Image);

            Calcd_CRC := CRC_16 (Packet);
            Ada.Text_IO.Put_Line ("DEBUG: Xmodem Calculated CRC is " & Calcd_CRC'Image);

            if Rxd_CRC = Calcd_CRC then
               Ada.Text_IO.Put_Line ("WARNING: Xmodem sending ACK");
               Router.Send_Data ("" & Ascii.ACK);
               for C of Packet loop
                  File_Blob.Append (C);
               end loop;
            else
               Ada.Text_IO.Put_Line ("WARNING: Xmodem sending NAK due to CRC error");
               Router.Send_Data ("" & Ascii.NAK);

            end if;

         end if;

         <<Next_Packet>>
      end loop;
   end Receiver;

end Xmodem;