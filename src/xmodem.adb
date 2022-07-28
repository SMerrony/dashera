--  Copyright ©2022 Steve Merrony
--
--  This file is a part of DasherA.
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

with Ada.Directories;

with Logging;    use Logging;
with Redirector; use Redirector;

package body Xmodem is

   function CRC_16 (Data : Vector) return Unsigned_16 is
      CRC : Unsigned_16 := 0;
      Part  : Unsigned_16;
   begin
      for C of Data loop
         Part := Unsigned_16 (Char_To_U8 (C));
         CRC := CRC xor Shift_Left (Part, 8);
         for I in 0 .. 7 loop
            if (CRC and 16#8000#) > 0 then
               CRC := Shift_Left (CRC, 1) xor 16#1021#;
            else
               CRC := Shift_Left (CRC, 1);
            end if;
         end loop;
      end loop;
      return CRC;
   end CRC_16;

   function CRC_16_Fixed_Len (Data : Vector; FL : Positive) return Unsigned_16 is
      CRC : Unsigned_16 := 0;
   begin
      --  the data part...
      CRC := CRC_16 (Data);

      --  the padding to the fixed length...
      for C in 0 .. (FL - Positive (Data.Length) - 1) loop
         CRC := CRC xor 16#0400#;
         for I in 0 .. 7 loop
            if (CRC and 16#8000#) > 0 then
               CRC := Shift_Left (CRC, 1) xor 16#1021#;
            else
               CRC := Shift_Left (CRC, 1);
            end if;
         end loop;
      end loop;

      return CRC;
   end CRC_16_Fixed_Len;

   procedure Send_Block (Data : in out Vector; Block_Num : Natural; Block_Size : Packet_Size) is
      Start_Bytes : String (1 .. 3);
      Block_Pos : constant Unsigned_8  := Unsigned_8 (Block_Num mod 256);
      Block_Inv : constant Unsigned_8  := not Block_Pos;
      CRC       : Unsigned_16;
      CRC_Str   : String (1 .. 2);
      Padding_String : String (1 .. 1);
   begin
      if Block_Size = Short then
         Start_Bytes (1) := ASCII.SOH;
      else
         Start_Bytes (1) := ASCII.STX;
      end if;
      Start_Bytes (2) := Character'Val (Block_Pos);
      Start_Bytes (3) := Character'Val (Block_Inv);
      if Tracing then
         Log (DEBUG, "X-Modem sending start byte and block number: " & Block_Pos'Image);
         Log (DEBUG, "X-Modem ... Actual data length: " & Data.Length'Image);
      end if;
      Router.Send_Data (Start_Bytes);

      --  Send the actual data
      for C of Data loop
         Router.Send_Data ("" & C);
      end loop;

      --  Pad out block
      if Data.Length < Block_Size'Enum_Rep then
         if Tracing then
            Log (DEBUG, "X-Modem ... Padding packet to full size");
         end if;
         Padding_String (1) := ASCII.EOT;
         for Ix in Data.Length + 1 .. Block_Size'Enum_Rep loop
            Router.Send_Data (Padding_String);
            Data.Append (ASCII.EOT);
         end loop;
         if Tracing then
            Log (DEBUG, "X-Modem ... Packet size now: " & Data.Length'Image);
         end if;
      end if;

      CRC := CRC_16 (Data);
      CRC_Str (1) := Byte_To_Char (Unsigned_8 (Shift_Right (CRC and 16#ff00#, 8)));
      CRC_Str (2) := Byte_To_Char (Unsigned_8 (CRC and 16#00ff#));
      if Tracing then
         Log (DEBUG, "X-Modem checksum: " & CRC'Image & ", sending: " & CRC_Str);
      end if;
      Router.Send_Data (CRC_Str);

   end Send_Block;

   procedure Receive (Filename : String; Trace_Flag : Boolean) is
      RX_File   : File_Type;
      RX_Stream : Stream_Access;
   begin
      Tracing := Trace_Flag;
      if Ada.Directories.Exists (Filename) then
         raise Already_Exists;
         return;
      end if;
      Create (RX_File, Name => Filename);
      RX_Stream := Stream (RX_File);
      Log (INFO, "Xmodem Created file: " & Filename);
      Router.Set_Handler (Handlr => Xmodem_Rx);
      Receiver_Task := new Receiver;
      Receiver_Task.Start (RX_Stream);
      loop
         select
            Receiver_Task.Done;
            Log (INFO, "Xmodem Receive is complete");
            Close (RX_File);
            Router.Set_Handler (Handlr => Visual);
            exit;
         or
            delay 1.0;
            if Tracing then
               Log (DEBUG, "Xmodem waiting for Receive to complete");
            end if;
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
      Purged  : Boolean;
   begin
      accept Start (RX_Stream : Stream_Access) do
         Write_Stream := RX_Stream;
         Finished := False;
         File_Blob.Clear;
         Packet.Clear;
      end Start;
      if Tracing then
         Log (DEBUG, "Xmodem Sending POLL");
      end if;
      Router.Send_Data ("" & 'C'); --  POLL
      while not Finished loop --  per packet
         Packet.Clear;

         if Tracing then
            Log (DEBUG, "Xmodem Ready for Packet Header");
         end if;
         accept Accept_Data (Char : Character) do
            Pkt_Hdr := Char;
         end Accept_Data;

         case Pkt_Hdr is
            when ASCII.EOT | ASCII.SUB =>
               if Tracing then
                  Log (DEBUG, "Xmodem Got EOT (End of Transmission)");
               end if;
               Router.Send_Data ("" & ASCII.ACK);
               if Tracing then
                  Log (DEBUG, "Xmodem Sent final ACK");
               end if;
               Finished := True;
            when ASCII.SOH =>
               if Tracing then
                  Log (DEBUG, "Xmodem Got SOH (Short packets indicator)");
               end if;
               Packet_Size := 128; --  short packets
            when ASCII.STX =>
               if Tracing then
                  Log (DEBUG, "Xmodem Got STX (Long packets indicator)");
               end if;
               Packet_Size := 1024; --  long packets
            when ASCII.CAN =>
               raise Sender_Cancelled;
            when others =>
               raise Protocol_Error;
         end case;

         if Finished then
            Router.Set_Handler (Handlr => Visual);
            --  final packet may have trailing EOFs
            while File_Blob (File_Blob.Last_Index) = ASCII.SUB loop
               File_Blob.Delete (File_Blob.Last_Index);
            end loop;
            for C of File_Blob loop
               Character'Write (Write_Stream, C);
            end loop;
            accept Done;

         else

            accept Accept_Data (Char : Character) do
               Packet_Count := Char_To_U8 (Char);
            end Accept_Data;
            if Tracing then
               Log (DEBUG, "Xmodem Got Packet Count " & Packet_Count'Image);
            end if;

            accept Accept_Data (Char : Character) do
               Inverse_Packet_Count := Char_To_U8 (Char);
            end Accept_Data;
            if Tracing then
               Log (DEBUG, "Xmodem Got Inverse Packet Count " & Inverse_Packet_Count'Image);
            end if;

            if (not Packet_Count) /= Inverse_Packet_Count then
               if Tracing then
                  Log (DEBUG, "Xmodem Packet counts not right - sending NAK");
               end if;
               Purged := False;
               while not Purged loop
                  select
                     accept Accept_Data (Char : Character) do
                        pragma Unreferenced (Char);
                     end Accept_Data;
                  or
                     delay 1.0;
                        Purged := True;
                  end select;
               end loop;
               Router.Send_Data ("" & ASCII.NAK);
               goto Next_Packet;
            end if;

            for B in 1 .. Packet_Size loop
               accept Accept_Data (Char : Character) do
                  Packet.Append (Char);
               end Accept_Data;
            end loop;

            if Tracing then
               Log (DEBUG, "Xmodem - Packet received");
            end if;

            accept Accept_Data (Char : Character) do
               Rxd_CRC := Unsigned_16 (Char_To_U8 (Char));
            end Accept_Data;
            Rxd_CRC := Shift_Left (Rxd_CRC, 8);
            accept Accept_Data (Char : Character) do
               Rxd_CRC := Rxd_CRC + Unsigned_16 (Char_To_U8 (Char));
            end Accept_Data;
            if Tracing then
               Log (DEBUG, "Xmodem Received CRC is " & Rxd_CRC'Image);
            end if;

            Calcd_CRC := CRC_16 (Packet);
            if Tracing then
               Log (DEBUG, "Xmodem Calculated CRC is " & Calcd_CRC'Image);
            end if;

            if Rxd_CRC = Calcd_CRC then
               if Tracing then
                  Log (DEBUG, "Xmodem CRCs OK - sending ACK");
               end if;
               Router.Send_Data ("" & ASCII.ACK);
               for C of Packet loop
                  File_Blob.Append (C);
               end loop;
            else
               Log (WARNING, "Xmodem sending NAK due to CRC error");
               Purged := False;
               while not Purged loop
                  select
                     accept Accept_Data (Char : Character) do
                        pragma Unreferenced (Char);
                     end Accept_Data;
                  or
                     delay 1.0;
                        Purged := True;
                  end select;
               end loop;
               Router.Send_Data ("" & ASCII.NAK);

            end if;

         end if;

         <<Next_Packet>>
      end loop;
   end Receiver;

   task body Sender is
      Packet_Sz     : Packet_Size;
      Packet_Length : Positive;
      Read_Stream   : Stream_Access;
      This_Block_No : Natural;
      Retries       : Natural;
      Block         : Vector;
      Ix            : Natural;
      Sent_OK       : Boolean;
      Finished      : Boolean;
   begin
      accept Start (TX_Stream : Stream_Access; Pkt_Len : Packet_Size) do
         Read_Stream   := TX_Stream;
         Packet_Sz     := Pkt_Len;
         Packet_Length := Pkt_Len'Enum_Rep;
         Finished      := False;
         Retries       := 1;
      end Start;
      if Tracing then
         Log (INFO, "Xmodem Sender waiting for POLL");
      end if;
      select
         accept Accept_Data (Char : Character) do
            if Char /= 'C' then
               Retries := Retries + 1;
               if Retries > 8 then
                  raise Protocol_Error with "Did not get POLL character";
               end if;
               if Tracing then
                  Log (INFO, "Xmodem Sender did not get POLL - retrying");
               end if;
            else
               Retries := 0;
            end if;
         end Accept_Data;
         while Retries /= 0 loop
            accept Accept_Data (Char : Character) do
               if Char /= 'C' then
                  Retries := Retries + 1;
                  if Retries > 8 then
                     raise Protocol_Error with "Did not get POLL character";
                  end if;
                  if Tracing then
                     Log (INFO, "Xmodem Sender did not get POLL - retrying");
                  end if;
               else
                  Retries := 0;
               end if;
            end Accept_Data;
         end loop;

         if Tracing then
            Log (DEBUG, "Xmodem Sender got POLLed");
         end if;
         This_Block_No := 1; --  1st block is #1, not 0

         while not Finished loop
            Block.Clear;
            Ix := 0;
            --  Read a packet's worth of data from the file
            while Ix < Packet_Length and then not Finished loop
               declare
                  One_Char : Character;
               begin
                  Character'Read (Read_Stream, One_Char);
                  Block.Append (One_Char);
                  Ix := Ix + 1;
               exception
                  when End_Error =>
                     Finished := True;
               end;
            end loop;

            Retries := 0;
            Sent_OK := False;
            --  attempt to send the packet up to 9 times
            while not Sent_OK and then Retries < 9 loop
               Send_Block (Data => Block, Block_Num => This_Block_No, Block_Size => Packet_Sz);
               select
                  accept Accept_Data (Char : Character) do
                     case Char is
                        when ASCII.ACK =>
                           This_Block_No := This_Block_No + 1;
                           if Tracing then
                              Log (DEBUG, "Xmodem Sent block ACKed");
                           end if;
                           Sent_OK := True;
                           if This_Block_No = 256 then
                              This_Block_No := 0;
                           end if;
                        when ASCII.NAK =>
                           if Tracing then
                              Log (DEBUG, "Xmodem Sent block NAKed");
                           end if;
                           Sent_OK := False;
                           Retries := Retries + 1;
                        when others =>
                           raise Protocol_Error with "unexpected response to data packet";
                     end case;
                  end Accept_Data;
               or
                  delay 5.0;
                  raise Timeout with "exceeded timeout waiting for ACK";
               end select;
            end loop; --  retries
            if not Sent_OK then
               raise Too_Many_Retries;
            end if;
         end loop;
         Router.Send_Data ("" & ASCII.EOT);
         accept Done;
      or
         delay 30.0;
            raise Timeout with "exceeded timeout waiting for POLL";
      end select;

   end Sender;

   procedure Send (Filename : String; Pkt_Len : Packet_Size; Trace_Flag : Boolean) is
      TX_File   : File_Type;
      TX_Stream : Stream_Access;
   begin
      Tracing := Trace_Flag;
      if not Ada.Directories.Exists (Filename) then
         raise File_Does_Not_Exist;
         return;
      end if;
      Open (File => TX_File, Mode => In_File, Name => Filename);
      Router.Set_Handler (Handlr => Xmodem_Tx);
      Sender_Task := new Sender;
      TX_Stream := Stream (TX_File);
      Sender_Task.Start (TX_Stream => TX_Stream, Pkt_Len => Pkt_Len);
      loop
         select
            Sender_Task.Done;
            Log (INFO, "Xmodem Transmit is complete");
            Close (TX_File);
            Router.Set_Handler (Handlr => Visual);
            exit;
         or
            delay 1.0;
            if Tracing then
               Log (DEBUG, "Xmodem waiting for Transmission to complete");
            end if;
         end select;
      end loop;
   exception
      when others =>
         raise File_Access_Error;
   end Send;

end Xmodem;