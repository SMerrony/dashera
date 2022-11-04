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

with Cell;

package Display_P is

   Default_Lines : constant Natural := 24;
   Default_Cols  : constant Natural := 80;

   Total_Lines   : constant Natural := 96;
   Total_Cols    : constant Natural := 208;
   History_Lines : constant Natural := 1000;

   type Cell_Array    is array (0 .. Total_Lines - 1, 0 .. Total_Cols - 1) of Cell.Cell_T;
   type History_Array is array (0 .. History_Lines - 1, 0 .. Total_Cols - 1) of Cell.Cell_T;
   type History_Line  is array (0 .. Total_Cols - 1) of Cell.Cell_T;

   type Display_T is record
      Cells : Cell_Array;
      Visible_Lines, Visible_Cols : Positive;
      Cursor_X, Cursor_Y : Natural;
      Blink_Enabled : Boolean;
   end record;

   type History_T is record
      Cells       : History_Array;
      First, Last : Natural;
   end record;

   protected Display is
      procedure Set_Dirty;
      procedure Clear_Dirty;
      function  Is_Dirty return Boolean;
      procedure Init;
      function  Cell_Is_Dirty (Line, Col : Natural) return Boolean;
      procedure Cell_Clear_Dirty (Line, Col : Natural);
      procedure Cell_Set_Dirty_If_Blinking (Line, Col : Natural);
      procedure Get_Cell (Line, Col : Natural; Value : out Character; Blnk, Dm, Rv, Under, Prot : out Boolean);
      procedure Clear_Cell (Line, Col : Natural);
      procedure Clear_Unprotected_Cell (Line, Col : Natural);
      procedure Clear_Line (Line : Integer);
      procedure Set_Cell (Line, Col : Natural; Char : Character;
                          Blink, Dim, Rev, Under, Prot : Boolean);
      procedure Set_Cursor (X, Y : Natural);
      function  Get_Cursor_X return Natural;
      function  Get_Cursor_Y return Natural;
      procedure Copy (Src : in out Display_T; Dest : out Display_T);
      procedure Copy_Line (Src, Dest : Integer);
      procedure Copy_Line_To_History (Src : Integer);
      --  Inserts a line into the circular history buffer
      function  Get_First_History_Line return Integer;
      function  Get_Last_History_Line return Integer;
      function  Get_History_Line (Line : Integer) return String;

      procedure Scroll_Up (Lines : Natural);

      function  Get_Visible_Cols  return Positive;
      function  Get_Visible_Lines return Positive;
      procedure Set_Visible_Cols  (Cols  : Positive);
      procedure Set_Visible_Lines (Lines : Positive);
      function  Is_Blink_Enabled return Boolean;
      procedure Set_Blink_Enabled (Blink : Boolean);
   private
      Disp, Saved_Disp   : Display_T;
      History            : History_T;
      Dirty              : Boolean;
   end Display;

end Display_P;