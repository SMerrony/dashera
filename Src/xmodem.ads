-- Copyright ©2022 Steve Merrony

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

with Ada.Streams.Stream_IO;      use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;

with Interfaces;  use Interfaces;

package Xmodem is

   type Packet_Size is (Short, Long);
   for Packet_Size use (Short => 128, Long => 1024);

   task type Receiver is
		entry Start;
      entry Accept_Data (Char : in Character);
      entry Done;
      entry Stop;
	end Receiver;
	type Receiver_Acc is access Receiver;

   Receiver_Task : Receiver_Acc;

   procedure Receive (Filename : in String);

   -- procedure Send ( Pkt_Len : in Packet_Size);

   Already_Exists, 
   Protocol_Error,
   Sender_Cancelled : exception;

private

   type Byte_Arr is array (Positive range<>) of Unsigned_8;


   function Char_To_Byte        is new Ada.Unchecked_Conversion(Character, Unsigned_8);
   function Byte_To_Char        is new Ada.Unchecked_Conversion(Unsigned_8, Character);

   function BA_To_String (BA : in Byte_Arr) return string;
   
   function CRC_16 (BA : in Byte_Arr) return Unsigned_16;
   -- Calculate the CRC-16 value of the provided block of data

   function CRC_16_Fixed_Len (BA : in Byte_Arr; FL : in Positive) return Unsigned_16;
   -- Calculate the CRC-16 Constant for the provided block of data

   procedure Send_Block (Data : in Byte_Arr; Block_Num : in Natural; Block_Size : in Packet_Size);

   Tracing : Boolean;
   RX_File : File_Type;


end Xmodem;