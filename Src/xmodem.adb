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


package body Xmodem is

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


end Xmodem;