-- Copyright (C) 2021 Steve Merrony

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

with Gdk.Pixbuf; use Gdk.Pixbuf;
with Glib;       use Glib;

package BDF_Font is

   Max_Chars : constant Positive := 128;
   BPP       : constant Positive := 8;
   -- raw font dimensions
   Font_Width  : constant Gint := 40;
   Font_Height : constant Gint := 44;

   type Zoom_T is (Large, Normal, Smaller, Tiny);

   type Matrix is array(0..Font_Width-1,0..Font_Height-1) of Boolean;

   type BDF_Char is record
      Loaded                                : Boolean;
      Pix_Buf, Dim_Pix_Buf, Reverse_Pix_Buf : Gdk_Pixbuf;
      Pixels : Matrix;
   end record;

   type Font_Array is array (0..Max_Chars-1) of BDF_Char;

   type Decoded_T is record
      Font : Font_Array;
      Char_Width, Char_Height : Gint;
   end record;

   Decoded : Decoded_T;

   -- function Load_Font (File_Name : String; Zoom : Zoom_T)
   --    return Decoded_Acc_T;
   procedure Load_Font (File_Name : String; Zoom : Zoom_T);

   OPEN_FAILURE,
   BDF_DECODE : exception;

end BDF_Font;
