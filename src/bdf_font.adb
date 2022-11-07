--  Copyright ©2021,2022 Steve Merrony
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

with Ada.Streams; use Ada.Streams;
with Interfaces;  use Interfaces;

with Embedded;
with Logging;     use Logging;

package body BDF_Font is

   procedure Parse_BBX
     (Font_Line             : String;
      Font_Line_Length      : Positive;
      Pix_Width, Pix_Height : out Integer;
      X_Offset, Y_Offset    : out Integer)
   is
      Start_Pos, End_Pos : Positive;
   begin
      Start_Pos := 5; --  "BBX n..."
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

   protected body Font is

      procedure Load_Font (File_Name : String;
                           Zoom : Zoom_T;
                           Font_Colour : Font_Colour_T) is
         --  Font   : aliased Decoded_Acc_T := new Decoded_T;
         Char_Count : Positive;
         Font_Emb   : constant Embedded.Content_Type := Embedded.Get_Content (File_Name);
         Font_Blob  : Stream_Element_Array (1 .. 16000);
         Font_Blob_Ix : Ada.Streams.Stream_Element_Offset := 1;
         Font_Line  : String (1 .. 132);
         Font_Line_Length                                  : Natural;
         Tmp_Pix_Buf, Tmp_Dim_Pix_Buf, Tmp_Reverse_Pix_Buf,
         Normal_Pix_Buf, Dim_Pix_Buf, Black_Pix_Buf        : Gdk_Pixbuf;
         ASCII_Code                                        : Natural;
         Pix_Width, Pix_Height                             : Integer;
         X_Offset, Y_Offset                                : Integer;
         X, Y                                              : Gint;
         Line_Byte                                         : Unsigned_8;

         procedure Get_Font_Line (Line : in out String; Last : out Natural) is
            Char        : Character;
         begin
            Last := 1;
            loop
               Char := Character'Val (Font_Blob (Font_Blob_Ix));
               Font_Blob_Ix := Font_Blob_Ix + 1;
               exit when Char = Character'Val (10);
               Line (Last) := Char;
               Last := Last + 1;
            end loop;
            Last := Last - 1;
         end Get_Font_Line;

      begin
         for Val of Font_Emb.Content.all loop
            Font_Blob (Font_Blob_Ix) := Val;
            Font_Blob_Ix := Font_Blob_Ix + 1;
         end loop;
         Font_Blob_Ix := 1;

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

         for C of Decoded.Font loop
            C.Loaded := False;
         end loop;

         loop
            --  Get_Font_Line (Font_Line, Font_Line_Length);
            Get_Font_Line (Font_Line, Font_Line_Length);
            --  Log (DEBUG, "" & Font_Line (1 .. Font_Line_Length));
            exit when Font_Line (1 .. Font_Line_Length) = "ENDPROPERTIES";
         end loop;
         Get_Font_Line (Font_Line, Font_Line_Length);
         if Font_Line (1 .. 5) /= "CHARS" then
            raise BDF_DECODE with "ERROR: BDF_Font - CHARS line not found";
         end if;
         Log (INFO, "BDF Font " & Font_Line (1 .. Font_Line_Length));

         Char_Count := Positive'Value (Font_Line (7 .. Font_Line_Length));

         Tmp_Pix_Buf         := Gdk_New (Has_Alpha => False, Width => Font_Width, Height => Font_Height);
         Tmp_Dim_Pix_Buf     := Gdk_New (Width => Font_Width, Height => Font_Height);
         Tmp_Reverse_Pix_Buf := Gdk_New (Width => Font_Width, Height => Font_Height);

         Normal_Pix_Buf := Gdk_New (Width => 1, Height => 1);
         Dim_Pix_Buf := Gdk_New (Width => 1, Height => 1);
         Black_Pix_Buf := Gdk_New (Width => 1, Height => 1);
         case Font_Colour is
            when Green =>
               Fill (Normal_Pix_Buf, 16#00ff00ff#);
               Fill (Dim_Pix_Buf, 16#008800ff#);
            when White =>
               Fill (Normal_Pix_Buf, 16#ffffffff#);
               Fill (Dim_Pix_Buf, 16#888888ff#);
            when Amber =>
               Fill (Normal_Pix_Buf, 16#ffbf00ff#);
               Fill (Dim_Pix_Buf, 16#885f00ff#);
         end case;
         Fill (Black_Pix_Buf, 16#000000ff#);

         for CC in 0 .. Char_Count - 1 loop
            Log (DEBUG, "Loading char No. " & Integer'Image (CC));

            loop
               Get_Font_Line (Font_Line, Font_Line_Length);
               exit when Font_Line (1 .. 9) = "STARTCHAR";
            end loop;

            Get_Font_Line (Font_Line, Font_Line_Length);
            if Font_Line (1 .. 8) /= "ENCODING" then
               raise BDF_DECODE with "ERROR: BDF_Font - ENCODING line not found";
            end if;
            ASCII_Code := Natural'Value (Font_Line (10 .. Font_Line_Length));
            Log (DEBUG, "... ASCII Code: " & ASCII_Code'Image);

            --  skip 2 lines
            Get_Font_Line (Font_Line, Font_Line_Length);
            Get_Font_Line (Font_Line, Font_Line_Length);

            Get_Font_Line (Font_Line, Font_Line_Length);
            Parse_BBX (Font_Line, Font_Line_Length, Pix_Width, Pix_Height, X_Offset, Y_Offset);

            --  skip the BITMAP line
            Get_Font_Line (Font_Line, Font_Line_Length);

         --  load the actual bitmap for this char a row at a time from the top down
            Fill (Tmp_Pix_Buf, 0);
            Fill (Tmp_Dim_Pix_Buf, 0);
            case Font_Colour is
               when Green => Fill (Tmp_Reverse_Pix_Buf, 16#00ff00ff#);
               when White => Fill (Tmp_Reverse_Pix_Buf, 16#ffffffff#);
               when Amber => Fill (Tmp_Reverse_Pix_Buf, 16#ffbf00ff#);
            end case;

            for Bitmap_Line in 0 .. Pix_Height - 1 loop
               Get_Font_Line (Font_Line, Font_Line_Length);
               Line_Byte := Unsigned_8'Value ("16#" & Font_Line (1 .. 2) & "#");
               for I in 0 .. Pix_Width - 1 loop
                  if (Line_Byte and 16#80#) /= 0 then
                     X := Gint (X_Offset + I);
                     Y := Gint (Bitmap_Line + 12 - Pix_Height - Y_Offset);
                     Gdk.Pixbuf.Copy_Area (Src_Pixbuf => Normal_Pix_Buf, Src_X => 0, Src_Y => 0, Width => 1, Height => 1,
                                          Dest_Pixbuf => Tmp_Pix_Buf, Dest_X => X, Dest_Y => Y);
                     Gdk.Pixbuf.Copy_Area (Src_Pixbuf => Dim_Pix_Buf, Src_X => 0, Src_Y => 0, Width => 1, Height => 1,
                                          Dest_Pixbuf => Tmp_Dim_Pix_Buf, Dest_X => X, Dest_Y => Y);
                     Gdk.Pixbuf.Copy_Area (Src_Pixbuf => Black_Pix_Buf, Src_X => 0, Src_Y => 0, Width => 1, Height => 1,
                                          Dest_Pixbuf => Tmp_Reverse_Pix_Buf, Dest_X => X, Dest_Y => Y);
                     --  Decoded.Font(ASCII_Code).Pixels (X_Offset + I, Y_Offset + Bitmap_Line) := True;
                  end if;
                  Line_Byte := Shift_Left (Line_Byte, 1);
               end loop;
            end loop;

            Decoded.Font (ASCII_Code).Pix_Buf         := Gdk.Pixbuf.Scale_Simple (Src => Tmp_Pix_Buf,
                                                         Dest_Width => Decoded.Char_Width,
                                                         Dest_Height => Decoded.Char_Height,
                                                         Inter_Type => Interp_Bilinear);
            Decoded.Font (ASCII_Code).Dim_Pix_Buf     := Gdk.Pixbuf.Scale_Simple (Src => Tmp_Dim_Pix_Buf,
                                                         Dest_Width => Decoded.Char_Width,
                                                         Dest_Height => Decoded.Char_Height,
                                                         Inter_Type => Interp_Bilinear);
            Decoded.Font (ASCII_Code).Reverse_Pix_Buf := Gdk.Pixbuf.Scale_Simple (Src => Tmp_Reverse_Pix_Buf,
                                                         Dest_Width => Decoded.Char_Width,
                                                         Dest_Height => Decoded.Char_Height,
                                                         Inter_Type => Interp_Bilinear);
            Decoded.Font (ASCII_Code).Loaded  := True;

         end loop;

      end Load_Font;

      function Get_Char_Width return Gint is
         (Decoded.Char_Width);

      function Get_Char_Height return Gint is
         (Decoded.Char_Height);

      function Is_Loaded (Ix : Natural) return Boolean is
         (Decoded.Font (Ix).Loaded);

      function Get_Dim_Pixbuf (Ix : Natural) return Gdk_Pixbuf is
         (Decoded.Font (Ix).Dim_Pix_Buf);

      function Get_Rev_Pixbuf (Ix : Natural) return Gdk_Pixbuf is
         (Decoded.Font (Ix).Reverse_Pix_Buf);

      function Get_Pixbuf (Ix : Natural) return Gdk_Pixbuf is
         (Decoded.Font (Ix).Pix_Buf);

   end Font;

end BDF_Font;
