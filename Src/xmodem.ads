-- Copyright (C)2022 Steve Merrony

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

with Interfaces;  use Interfaces;

package Xmodem is

   type Packet_Size is (Short, Long);

   -- procedure Receive;
   -- procedure Send ( Pkt_Len : in Packet_Size);

private

   type Byte_Arr is array (Positive range<>) of Unsigned_8;

   function CRC_16 (BA : in Byte_Arr) return Unsigned_16;
   -- Calculate the CRC-16 value of the provided block of data

   function CRC_16_Fixed_Len (BA : in Byte_Arr; FL : in Positive) return Unsigned_16;
   -- Calculate the CRC-16 Constant for the provided block of data

   -- procedure Send_Block (Blk_Num : in Natural);

end Xmodem;