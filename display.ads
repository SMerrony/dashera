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

with Cell;

package Display is

   Default_Lines : constant Natural := 24;
   Default_Cols  : constant Natural := 80;

   Total_Lines   : constant Natural := 96;
   Total_Cols    : constant Natural := 208;
   History_Lines : constant Natural := 1000;

   type Cell_Array    is array(0..Total_Lines-1, 0..Total_Cols-1) of Cell.Cell_T;
   type History_Line  is array(0..Total_Cols-1)    of Cell.Cell_T;
   type History_Array is array(0..History_Lines-1) of History_Line;

   type Display_T is record
      Cells : Cell_Array;
      Visible_Lines, Visible_Cols : Integer;
      Cursor_X, Cursor_Y : Natural;
      Blink_Enabled : Boolean;
      Dirty : Boolean;
   end record;

   type History_T is record
      Lines       : History_Array;
      First, Last : Natural;
   end record;

   Disp, Saved_Disp   : Display_T;
   History            : History_T;
   Empty_History_Line : History_Line;
   Scrolled_Back : Boolean;

   procedure Init;
   procedure Clear_Cell (Line, Col : in Integer);
   procedure Clear_Line (Line : in Integer);
   -- procedure Set_Cell (This : in out Display_T;
   --                     Line, Col : in Integer;
   --                     Char : in Character;
   --                     Blink, Dim, Rev, Under, Prot : in Boolean);
   procedure Copy_Line (Src, Dest : in Integer);
   procedure Set_Cursor (X, Y : in Natural);

   procedure Add_To_History (HL : in History_Line);
   -- Inserts a line into the circular history buffer

   procedure Scroll_Up (Lines : in Natural);

   procedure Copy_Line_To_History (Src : in Integer);

   function Get_Nth_History_Line (N : in Natural) return History_Line;

   procedure Scroll_Back (Start_Line : in Natural);
   procedure Cancel_Scroll_Back;
end Display;