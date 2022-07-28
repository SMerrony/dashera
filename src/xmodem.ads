--  Copyright ©2022 Steve Merrony
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

with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;      use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;

with Interfaces;  use Interfaces;

package Xmodem is

   type Packet_Size is (Short, Long);
   for Packet_Size use (Short => 128, Long => 1024);

   package Char_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Character);
   use Char_Vectors;

   task type Receiver is
      entry Start (RX_Stream : Stream_Access);
      entry Accept_Data (Char : Character);
      entry Done;
      --  entry Stop;
   end Receiver;
   type Receiver_Acc is access Receiver;

   Receiver_Task : Receiver_Acc;

   procedure Receive (Filename : String; Trace_Flag : Boolean);

   task type Sender is
      entry Start (TX_Stream : Stream_Access; Pkt_Len : Packet_Size);
      entry Accept_Data (Char : Character);
      entry Done;
   end Sender;
   type Sender_Acc is access Sender;

   Sender_Task : Sender_Acc;

   procedure Send (Filename : String; Pkt_Len : Packet_Size; Trace_Flag : Boolean);

   Already_Exists,
   File_Does_Not_Exist,
   File_Access_Error,
   Protocol_Error,
   Sender_Cancelled,
   Timeout,
   Too_Many_Retries  : exception;

private

   function Char_To_U8   is new Ada.Unchecked_Conversion (Character, Unsigned_8);
   function Byte_To_Char is new Ada.Unchecked_Conversion (Unsigned_8, Character);

   function CRC_16 (Data : Vector) return Unsigned_16;
   --  Calculate the CRC-16 value of the provided block of data

   function CRC_16_Fixed_Len (Data : Vector; FL : Positive) return Unsigned_16;
   --  Calculate the CRC-16 Constant for the provided block of data

   procedure Send_Block (Data : in out Vector; Block_Num : Natural; Block_Size : Packet_Size);

   Tracing : Boolean;

end Xmodem;