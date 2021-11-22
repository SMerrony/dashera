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

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Text_IO;       use Ada.Text_IO;
with Interfaces;        use Interfaces;

package body BDF_Font is

   procedure Parse_BBX
     (Font_Line             : in     String; Font_Line_Length : in Positive;
      Pix_Width, Pix_Height :    out Integer; X_Offset, Y_Offset : out Integer)
   is
      Start_Pos, End_Pos : Positive;
   begin
      Start_Pos := 5; -- "BBX n..."
      End_Pos   := Start_Pos;
      while Font_Line (End_Pos) /= ' ' loop
         End_Pos := End_Pos + 1;
      end loop;
      Pix_Width := Integer'Value (Font_Line (Start_Pos .. End_Pos - 1));

      Start_Pos := End_Pos + 1;
      End_Pos   := Start_Pos;
      while Font_Line (End_Pos) /= ' ' loop
         End_Pos := End_Pos + 1;
      end loop;
      Pix_Height := Integer'Value (Font_Line (Start_Pos .. End_Pos - 1));

      Start_Pos := End_Pos + 1;
      End_Pos   := Start_Pos;
      while Font_Line (End_Pos) /= ' ' loop
         End_Pos := End_Pos + 1;
      end loop;
      X_Offset := Integer'Value (Font_Line (Start_Pos .. End_Pos - 1));

      Start_Pos := End_Pos + 1;
      End_Pos   := Font_Line_Length;
      Y_Offset  := Integer'Value (Font_Line (Start_Pos .. End_Pos));

   end Parse_BBX;

   function Load_Font (File_Name : String; Zoom : Zoom_Type) return Decoded_Acc_T is
      
      Font                                              : aliased Decoded_Acc_T := new Decoded_T;
      Char_Count                                        : Positive;
      Font_File                                         : File_Type;
      Font_Line                                         : String (1 .. 80);
      Font_Line_Length                                  : Natural;
      Tmp_Pix_Buf, Tmp_Dim_Pix_Buf, Tmp_Reverse_Pix_Buf : Gdk_Pixbuf;
      Pixels, Dim_Pixels, Reverse_Pixels                : Rgb_Buffer_Access;
      Pixel_Index                                       : Guint;
      ASCII_Code                                        : Natural;
      Pix_Width, Pix_Height                             : Integer;
      X_Offset, Y_Offset                                : Integer;
      Line_Byte                                         : Unsigned_8;
      Num_Chans, Row_Stride                             : Integer;

   begin
      case Zoom is
         when Large =>
            Font.Char_Width  := 10;
            Font.Char_Height := 24;
         when Normal =>
            Font.Char_Width  := 10;
            Font.Char_Height := 18;
         when Smaller =>
            Font.Char_Width  := 8;
            Font.Char_Height := 12;
         when Tiny =>
            Font.Char_Width  := 7;
            Font.Char_Height := 10;
      end case;

      begin
         Open (File => Font_File, Mode => In_File, Name => File_Name);

      exception
         when others =>
            raise OPEN_FAILURE with "ERROR: Cannot open the file '" & File_Name &
               "'. Does it exist?";
      end;

      loop
         Get_Line (Font_File, Font_Line, Font_Line_Length);
         Put_Line ("DEBUG: " & Font_Line (1 .. Font_Line_Length));
         exit when Font_Line (1 .. Font_Line_Length) = "ENDPROPERTIES";
      end loop;
      Get_Line (Font_File, Font_Line, Font_Line_Length);
      if Font_Line (1 .. 5) /= "CHARS" then
         raise BDF_DECODE with "ERROR: BDF_Font - CHARS line not found";
      end if;
      Put_Line ("DEBUG: " & Font_Line (1 .. Font_Line_Length));

      Char_Count := Positive'Value (Font_Line (7 .. Font_Line_Length));

      Tmp_Pix_Buf         := Gdk_New (Width => Gint(Font_Width), Height => Gint(Font_Height));
      Tmp_Dim_Pix_Buf     := Gdk_New (Width => Gint(Font_Width), Height => Gint(Font_Height));
      Tmp_Reverse_Pix_Buf := Gdk_New (Width => Gint(Font_Width), Height => Gint(Font_Height));

      for CC in 0 .. Char_Count - 1 loop
         Put_Line ("DEBUG: Loading char No. " & Integer'Image(CC));

         Get_Line (Font_File, Font_Line, Font_Line_Length);
         if Font_Line (1 .. 9) /= "STARTCHAR" then
            raise BDF_DECODE with "ERROR: BDF_Font - STARTCHAR line not found";

         end if;

         Get_Line (Font_File, Font_Line, Font_Line_Length);
         if Font_Line (1 .. 8) /= "ENCODING" then
           raise BDF_DECODE with "ERROR: BDF_Font - ENCODING line not found";
         end if;
         ASCII_Code := Natural'Value (Font_Line (10 .. Font_Line_Length));

         -- skip 2 lines
         Get_Line (Font_File, Font_Line, Font_Line_Length);
         Get_Line (Font_File, Font_Line, Font_Line_Length);

         Get_Line (Font_File, Font_Line, Font_Line_Length);
         Parse_BBX
           (Font_Line, Font_Line_Length, Pix_Width, Pix_Height, X_Offset,
            Y_Offset);

         -- skip the BITMAP line
         Get_Line (Font_File, Font_Line, Font_Line_Length);

      -- load the actual bitmap for this char a row at a time from the top down
         Fill (Tmp_Pix_Buf, 0);
         Fill (Tmp_Dim_Pix_Buf, 0);
         Fill (Tmp_Reverse_Pix_Buf, 16#00FF_0000#);
         Pixels := Get_Pixels(Tmp_Pix_Buf);
         Dim_Pixels := Get_Pixels(Tmp_Dim_Pix_Buf);
         Reverse_Pixels := Get_Pixels(Tmp_Reverse_Pix_Buf);
         -- Put_Line ("DEBUG: Pixels Length: " & Integer'Image(Pixels') & ", Reverse_Pixels Length: " & Integer'Image(Reverse_Pixels'Length));

         for Bitmap_Line in reverse 0 .. Pix_Height - 1 loop
            Get_Line (Font_File, Font_Line, Font_Line_Length);
            Line_Byte := Unsigned_8'Value ("16#" & Font_Line (1 .. 2) & "#");
            for I in 0 .. Pix_Width - 1 loop
               if (Line_Byte and 16#80#) /= 0 then
                  Num_Chans  := 1; -- Integer(Get_N_Channels (Tmp_Pix_Buf));
                  Row_Stride := Integer(Get_Rowstride (Tmp_Pix_Buf));
                  Pixel_Index := Guint(((Y_Offset + Bitmap_Line) * Row_Stride) + ((X_Offset + I) * Num_Chans));
                  Pixels(Pixel_Index).Green := 255;
                  Dim_Pixels(Pixel_Index).Green := 128;
                  -- RGBBA := Get_Pixels(Tmp_Reverse_Pix_Buf);
                  -- declare
                  --    RL : Integer := RGBBA'Length;
                  -- begin
                  --    RGBBA(Pixel_Index).Green := 0;
                  -- end;
                  -- RGBBA(Pixel_Index).Green := 0;
                  Reverse_Pixels(Pixel_Index).Green := 0;
                  Font.Font(ASCII_Code).Pixels (X_Offset + I, Y_Offset + Bitmap_Line) := True;
               end if;
               Line_Byte := Shift_Left (Line_Byte, 1);
            end loop;
         end loop;

         Font.Font(ASCII_Code).Pix_Buf := Tmp_Pix_Buf;
         Font.Font(ASCII_Code).Dim_Pix_Buf := Tmp_Dim_Pix_Buf;
         Font.Font(ASCII_Code).Reverse_Pix_Buf := Tmp_Reverse_Pix_Buf;
         Font.Font(ASCII_Code).Loaded  := true;
         -- skip the ENDCHAR line
         Get_Line (Font_File, Font_Line, Font_Line_Length);

      end loop;

      return Font;

   end Load_Font;

end BDF_Font;
