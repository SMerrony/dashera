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

with Ada.Text_IO;       use Ada.Text_IO;
with Interfaces;        use Interfaces;

package body BDF_Font is

   procedure Parse_BBX
     (Font_Line             : in String; 
      Font_Line_Length      : in Positive;
      Pix_Width, Pix_Height : out Integer; 
      X_Offset, Y_Offset    : out Integer)
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

   -- function Load_Font (File_Name : String; Zoom : Zoom_T) return Decoded_Acc_T is
   procedure Load_Font (File_Name : String; Zoom : Zoom_T) is
      -- Font                                              : aliased Decoded_Acc_T := new Decoded_T;
      Char_Count                                        : Positive;
      Font_File                                         : File_Type;
      Font_Line                                         : String (1 .. 80);
      Font_Line_Length                                  : Natural;
      Tmp_Pix_Buf, Tmp_Dim_Pix_Buf, Tmp_Reverse_Pix_Buf,
      Green_Pix_Buf, Dim_Pix_Buf, Black_Pix_Buf         : Gdk_Pixbuf;
      ASCII_Code                                        : Natural;
      Pix_Width, Pix_Height                             : Integer;
      X_Offset, Y_Offset                                : Integer;
      X, Y                                              : Gint;
      Line_Byte                                         : Unsigned_8;

   begin
      case Zoom is
         when Large =>
            Decoded.Char_Width  := 10;
            Decoded.Char_Height := 24;
         when Normal =>
            Decoded.Char_Width  := 10;
            Decoded.Char_Height := 18;
         when Smaller =>
            Decoded.Char_Width  := 8;
            Decoded.Char_Height := 12;
         when Tiny =>
            Decoded.Char_Width  := 7;
            Decoded.Char_Height := 10;
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
         -- Put_Line ("DEBUG: " & Font_Line (1 .. Font_Line_Length));
         exit when Font_Line (1 .. Font_Line_Length) = "ENDPROPERTIES";
      end loop;
      Get_Line (Font_File, Font_Line, Font_Line_Length);
      if Font_Line (1 .. 5) /= "CHARS" then
         raise BDF_DECODE with "ERROR: BDF_Font - CHARS line not found";
      end if;
      Put_Line ("DEBUG: BDF Font " & Font_Line (1 .. Font_Line_Length));

      Char_Count := Positive'Value (Font_Line (7 .. Font_Line_Length));

      Tmp_Pix_Buf         := Gdk_New (Has_Alpha => False, Width => Font_Width, Height => Font_Height);
      Tmp_Dim_Pix_Buf     := Gdk_New (Width => Font_Width, Height => Font_Height);
      Tmp_Reverse_Pix_Buf := Gdk_New (Width => Font_Width, Height => Font_Height);

      Green_Pix_Buf := Gdk_New (Width => 1, Height => 1);
      Fill (Green_Pix_Buf, 16#00ff00ff#);
      Dim_Pix_Buf := Gdk_New (Width => 1, Height => 1);
      Fill (Dim_Pix_Buf, 16#008800ff#);
      Black_Pix_Buf := Gdk_New (Width => 1, Height => 1);
      Fill (Black_Pix_Buf, 16#000000ff#);

      for CC in 0 .. Char_Count - 1 loop
         -- Put_Line ("DEBUG: Loading char No. " & Integer'Image(CC));

         loop
            Get_Line (Font_File, Font_Line, Font_Line_Length);
            exit when Font_Line (1 .. 9) = "STARTCHAR";
         end loop;

         Get_Line (Font_File, Font_Line, Font_Line_Length);
         if Font_Line (1 .. 8) /= "ENCODING" then
           raise BDF_DECODE with "ERROR: BDF_Font - ENCODING line not found";
         end if;
         ASCII_Code := Natural'Value (Font_Line (10 .. Font_Line_Length));
         -- Put_Line ("DEBUG: ... ASCII Code: " & ASCII_Code'Image);

         -- skip 2 lines
         Get_Line (Font_File, Font_Line, Font_Line_Length);
         Get_Line (Font_File, Font_Line, Font_Line_Length);

         Get_Line (Font_File, Font_Line, Font_Line_Length);
         Parse_BBX (Font_Line, Font_Line_Length, Pix_Width, Pix_Height, X_Offset, Y_Offset);

         -- skip the BITMAP line
         Get_Line (Font_File, Font_Line, Font_Line_Length);

      -- load the actual bitmap for this char a row at a time from the top down
         Fill (Tmp_Pix_Buf, 0);
         Fill (Tmp_Dim_Pix_Buf, 0);
         Fill (Tmp_Reverse_Pix_Buf, 16#00FF_0000#);
         for Bitmap_Line in 0 .. Pix_Height - 1 loop
            Get_Line (Font_File, Font_Line, Font_Line_Length);
            Line_Byte := Unsigned_8'Value ("16#" & Font_Line (1 .. 2) & "#");
            for I in 0 .. Pix_Width - 1 loop
               if (Line_Byte and 16#80#) /= 0 then
                  X := Gint(X_Offset + I);
                  Y := Gint(Bitmap_Line + 12 - Pix_Height - Y_Offset);
                  Gdk.Pixbuf.Copy_Area (Src_Pixbuf => Green_Pix_Buf, Src_X => 0, Src_Y => 0, Width => 1, Height => 1, 
                                        Dest_Pixbuf => Tmp_Pix_Buf, Dest_X => X, Dest_Y => Y);
                  Gdk.Pixbuf.Copy_Area (Src_Pixbuf => Dim_Pix_Buf, Src_X => 0, Src_Y => 0, Width => 1, Height => 1, 
                                        Dest_Pixbuf => Tmp_Dim_Pix_Buf, Dest_X => X, Dest_Y => Y);
                  Gdk.Pixbuf.Copy_Area (Src_Pixbuf => Black_Pix_Buf, Src_X => 0, Src_Y => 0, Width => 1, Height => 1, 
                                        Dest_Pixbuf => Tmp_Reverse_Pix_Buf, Dest_X => X, Dest_Y => Y);
                  -- Decoded.Font(ASCII_Code).Pixels (X_Offset + I, Y_Offset + Bitmap_Line) := True;
               end if;
               Line_Byte := Shift_Left (Line_Byte, 1);
            end loop;
         end loop;

         Decoded.Font(ASCII_Code).Pix_Buf         := Gdk.Pixbuf.Scale_Simple (Src => Tmp_Pix_Buf, Dest_Width => 10, Dest_Height =>18, Inter_Type => Interp_Bilinear);
         Decoded.Font(ASCII_Code).Dim_Pix_Buf     := Gdk.Pixbuf.Scale_Simple (Src => Tmp_Dim_Pix_Buf, Dest_Width => 10, Dest_Height =>18, Inter_Type => Interp_Bilinear);
         Decoded.Font(ASCII_Code).Reverse_Pix_Buf := Gdk.Pixbuf.Scale_Simple (Src => Tmp_Reverse_Pix_Buf, Dest_Width => 10, Dest_Height =>18, Inter_Type => Interp_Bilinear);
         Decoded.Font(ASCII_Code).Loaded  := true;

      end loop;

      -- return Font;

   end Load_Font;

end BDF_Font;
