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

end Xmodem;