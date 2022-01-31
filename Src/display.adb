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

package body Display is   

   -- function Create return Display_Acc_T is
   --    D : aliased Display_Acc_T := new Display_T;
   -- begin
   --    D.Visible_Lines := Default_Lines;
   --    D.Visible_Cols := Default_Cols;
   --    D.Cells(12,39).Char_Value := 'O';
   --    D.Cells(12,40).Char_Value := 'K';
   --    return D;
   -- end Create;

   procedure Init is
   begin
      Disp.Visible_Lines := Default_Lines;
      Disp.Visible_Cols := Default_Cols;
      for Line in 0 .. Total_Lines - 1 loop
         for Col in 0 .. Total_Cols - 1 loop
            Disp.Cells(Line, Col).Clear_To_Space;
         end loop;
      end loop;
      Disp.Cells(12,39).Char_Value := 'O';
      Disp.Cells(12,40).Char_Value := 'K';
      Disp.Blink_Enabled := True;
      History.First := 0;
      History.Last  := 0;
      for C in Empty_History_Line'Range loop
         Empty_History_Line(C).Clear_To_Space;
      end loop;
      Scrolled_Back := False;
   end Init;

   procedure Clear_Cell (Line, Col : in Integer) is
   begin
      Disp.Cells(Line, Col).Clear_To_Space;
   end Clear_Cell;

   procedure Clear_Line (Line : in Integer) is
   begin
      for Col in 0 .. Total_Cols - 1 loop
         Disp.Cells(Line, Col).Clear_To_Space;
      end loop;
   end Clear_Line;

   procedure Copy_Line (Src, Dest : in Integer) is
   begin
      for Col in 0 .. Total_Cols - 1 loop
         Disp.Cells(Dest,Col).Copy_From (Disp.Cells(Src,Col));
      end loop;
   end Copy_Line;

   procedure Set_Cursor (X, Y : in Natural) is
   begin
      Disp.Cursor_X := X;
      Disp.Cursor_Y := Y;
   end Set_Cursor;

   procedure Add_To_History (HL : in History_Line) is
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
      History.Lines(History.Last) := HL;
   end Add_To_History;

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

   procedure Copy_Line_To_History (Src : in Integer) is
      HL : History_Line;
   begin
      for Col in 0 .. Total_Cols - 1 loop
         HL(Col).Copy_From (Disp.Cells(Src,Col));
      end loop;
      Add_To_History (HL);
   end Copy_Line_To_History;

   procedure Copy_Line_From_History (Src, Dest : in Natural) is
      HL : constant History_Line := Get_Nth_History_Line (Src);
   begin
      for Col in 0 .. Total_Cols - 1 loop
         Disp.Cells(Dest,Col).Copy_From (HL(Col));
      end loop;
   end Copy_Line_From_History;

   function Get_Nth_History_Line (N : in Natural) return History_Line is
      HL : History_Line;
      Ix : Integer;
   begin
      if History.First = History.Last then -- no history yet
         HL := Empty_History_Line;
      else
         Ix := History.Last - N;
         if Ix < 0 then
            Ix := Ix + History_Lines;
         end if;
         HL := History.Lines(Ix);
      end if;
      return HL;
   end Get_Nth_History_Line;

   procedure Scroll_Back (Start_Line : in Natural) is
   begin
      if not Scrolled_Back then
         Saved_Disp := Disp;
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
   end Scroll_Back;

   procedure Cancel_Scroll_Back is
   begin 
      Disp := Saved_Disp;
      Scrolled_Back := False;
   end Cancel_Scroll_Back;

end Display;