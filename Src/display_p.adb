-- Copyright ©2021,2022 Steve Merrony

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

package body Display_P is   

   protected body Display is

      procedure Set_Dirty   is begin Dirty := True;  end Set_Dirty;
      procedure Clear_Dirty is begin Dirty := False; end Clear_Dirty;
      function  Is_Dirty return Boolean is (Dirty);
      function  Get_Visible_Cols  return Positive is (Disp.Visible_Cols);
      function  Get_Visible_Lines return Positive is (Disp.Visible_Lines);
      procedure Set_Visible_Cols  (Cols : in Positive)  is begin Disp.Visible_Cols := Cols;   end Set_Visible_Cols;
      procedure Set_Visible_Lines (Lines : in Positive) is begin Disp.Visible_Lines := Lines; end Set_Visible_Lines;
      function  Is_Blink_Enabled return Boolean is (Disp.Blink_Enabled);
      procedure Set_Blink_Enabled (Blink : in Boolean) is begin Disp.Blink_Enabled := Blink; end Set_Blink_Enabled;
      function  Get_Cursor_X return Natural is (Disp.Cursor_X);
      function  Get_Cursor_Y return Natural is (Disp.Cursor_Y);

      procedure Init is
      begin
         Disp.Visible_Lines := Default_Lines;
         Disp.Visible_Cols := Default_Cols;
         for Line in 0 .. Total_Lines - 1 loop
            for Col in 0 .. Total_Cols - 1 loop
               Disp.Cells(Line, Col).Clear_To_Space;
            end loop;
         end loop;
         Disp.Cells(12,39).Set (Value => 'O', Blnk => False, Dm => False, Rv => False, Under => False, Prot => False);
         Disp.Cells(12,40).Set (Value => 'K', Blnk => False, Dm => False, Rv => False, Under => False, Prot => False);
         Disp.Blink_Enabled := True;
         History.First := 0;
         History.Last  := 0;
         for C in Empty_History_Line'Range loop
            Empty_History_Line(C).Clear_To_Space;
         end loop;
         for HL in History.Lines'Range loop
            for Col in 0 .. Total_Cols - 1 loop
               History.Lines(HL)(Col).Clear_To_Space;
            end loop;
         end loop;
         Set_Scrolled_Back (False);
      end Init;

      procedure Copy (Src : in out Display_T; Dest : out Display_T) is
      begin
         for Line in 0 .. Src.Visible_Lines-1 loop
            for Col in 0 .. Src.Visible_Cols-1 loop
               Cell.Copy (Src => Src.Cells(Line,Col), Dest => Dest.Cells(Line,Col));
            end loop;
         end loop;
         Dest.Blink_Enabled := Src.Blink_Enabled;
         Dest.Cursor_X      := Src.Cursor_X;
         Dest.Cursor_Y      := Src.Cursor_Y;
         Dest.Visible_Cols  := Src.Visible_Cols;
         Dest.Visible_Lines := Src.Visible_Lines;
      end Copy;

      procedure Clear_Cell (Line, Col : in Natural) is
      begin
         Disp.Cells(Line, Col).Clear_To_Space;
      end Clear_Cell;

      procedure Clear_Unprotected_Cell (Line, Col : in Natural) is
      begin
         Disp.Cells(Line, Col).Clear_If_Unprotected;
      end Clear_Unprotected_Cell;

      procedure Get_Cell (Line, Col : in Natural; Value : out Character; Blnk, Dm, Rv, Under, Prot : out Boolean) is
      begin
         Disp.Cells(Line,Col).Get (Value => Value, Blnk => Blnk, Dm => Dm, Rv => Rv, Under => Under, Prot => Prot);
      end Get_Cell;
      
      procedure Set_Cell (Line, Col : in Natural; Char : in Character;
                        Blink, Dim, Rev, Under, Prot : in Boolean) is
      begin
         Disp.Cells(Line,Col).Set (Value => Char, Blnk => Blink, Dm => Dim, 
                                 Rv => Rev, Under => Under, Prot => Prot);
      end Set_Cell;
      
      procedure Set_Cursor (X, Y : in Natural) is
      begin
         Disp.Cursor_X := X;
         Disp.Cursor_Y := Y;
      end Set_Cursor;

      procedure Clear_Line (Line : in Integer) is
      begin
         for Col in 0 .. Total_Cols - 1 loop
            Disp.Cells(Line, Col).Clear_To_Space;
         end loop;
      end Clear_Line;
      
      procedure Copy_Line (Src, Dest : in Integer) is
      begin
         for Col in 0 .. Total_Cols - 1 loop
            Cell.Copy (Src => Disp.Cells(Src,Col), Dest => Disp.Cells(Dest,Col));
         end loop;
      end Copy_Line;

      procedure Copy_Line_To_History (Src : in Integer) is
      begin
         History.Last := History.Last + 1;
         if History.Last = History_Lines then
            -- wrap-around
            History.Last := 0;
         end if;
         -- has the tail hit the head?
         if History.Last = History.First then
            History.First := History.First + 1;
            if History.First = History_Lines then
               History.First := 0;
            end if;
         end if;

         for C in History.Lines(History.Last)'Range loop
            Cell.Copy (Src => Disp.Cells(Src,C), Dest => History.Lines(History.Last)(C));
         end loop;
      end Copy_Line_To_History;

      procedure Copy_Line_From_History (Src, Dest : in Natural) is
         HL : History_Line;
         Ix : Integer;
      begin
         if History.First = History.Last then -- no history yet
            for C in Empty_History_Line'Range loop
               Cell.Copy (Src => Empty_History_Line(C), Dest => HL(C));
            end loop;
         else
            Ix := History.Last - Src;
            if Ix < 0 then
               Ix := Ix + History_Lines;
            end if;
            for C in History.Lines(Ix)'Range loop
               Cell.Copy (Src => History.Lines(Ix)(C), Dest => HL(C));
            end loop;
         end if;

         for Col in 0 .. Total_Cols - 1 loop
            Cell.Copy (Src => HL(Col), Dest => Disp.Cells(Dest,Col));
         end loop;
      end Copy_Line_From_History;

      procedure Scroll_Up (Lines : in Natural) is
      begin
         for L in 1 .. Lines loop
            Copy_Line_To_History (0);
            for R in 1 .. Disp.Visible_Lines loop
               Copy_Line (Src => R, Dest => R - 1);
               Clear_Line (R);
            end loop;
            Clear_Line (Disp.Visible_Lines - 1);
         end loop;
      end Scroll_Up;
      
      function Is_Scrolled_Back return Boolean is
         (Scrolled_Back);

      procedure Set_Scrolled_Back (Back : in Boolean) is
      begin
         Scrolled_Back := Back;
      end Set_Scrolled_Back;

      procedure Scroll_Back (Start_Line : in Natural) is
      begin
         if not Scrolled_Back then
            Copy (Src => Disp, Dest => Saved_Disp);
            Scrolled_Back := True;
         end if;
         -- there are two cases: we are already scrolled back beyond the 'live' screen, 
         -- or we are partially showing it
         if Start_Line < Disp.Visible_Lines then
            declare
               On_Screen_Line, Live_Line : Natural := 0;
            begin
               for HL in reverse 0 .. Start_Line loop
                  Copy_Line_From_History (HL, On_Screen_Line);
                  On_Screen_Line := On_Screen_Line + 1;
               end loop;
               while On_Screen_Line < Disp.Visible_Lines loop
                  Copy_Line_From_History (Live_Line, On_Screen_Line);
                  Live_Line := Live_Line + 1;
                  On_Screen_Line := On_Screen_Line + 1;
               end loop;
            end;
         else
            -- all 'history' - easier
            for L in 0 .. Disp.Visible_Lines loop
               Copy_Line_From_History (Start_Line - L, L);
            end loop;
         end if;
         Set_Dirty;
      end Scroll_Back;

      procedure Cancel_Scroll_Back is
      begin 
         Copy (Src => Saved_Disp, Dest => Disp);
         Scrolled_Back := False;
         Set_Dirty;
      end Cancel_Scroll_Back;
      
   end Display;

end Display_P;