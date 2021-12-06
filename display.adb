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
      Disp.Cells(12,39).Char_Value := 'O';
      Disp.Cells(12,40).Char_Value := 'K';
   end Init;

   procedure Clear_Cell (This : in out Display_T; Line, Col : in Integer) is
   begin
      This.Cells(Line, Col).Clear_To_Space;
   end Clear_Cell;

end Display;