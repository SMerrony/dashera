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

   Total_Lines : constant Natural := 96;
   Total_Cols  : constant Natural := 208;

   type Cell_Array is array(1..Total_Lines,1..Total_Cols) of Cell.Cell_T;

   type Display_T is record
      Cells : Cell_Array;
      Visible_Lines, Visible_Cols : Integer;
      Dirty : Boolean;
   end record;

   Disp : Display_T;

   -- function Create return Display_Acc_T;
   procedure Init;
   procedure Clear_Cell (This : in out Display_T; Line, Col : in Integer);

end Display;