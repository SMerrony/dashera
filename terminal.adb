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
with Ada.Text_IO; 
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

with BDF_Font;
with Crt;
with Display;

package body Terminal is

   function Create (Emul : in Emulation_T) return Terminal_Acc_T is
      T : aliased constant Terminal_Acc_T := new Terminal_T;
   begin
      T.Emulation := Emul;
      T.Connection := Local;
      T.Cursor_X := 0;
      T.Cursor_Y := 0;
      T.Roll_Enabled := True;
      T.Protection_Enabled := True;
      T.Skip_Byte := False;
      T.Holding := False;
      T.Logging := False;
      T.Scrolled_Back := False;
      T.Expecting := False;
      T.Raw_Mode := False;
      T.Blinking := False;
      T.Dimmed := False;
      T.Reversed := False;
      T.Underscored := False;
      T.Protectd := False;

      T.Updated := True;
      
      return T;
   end Create;

   function Str_To_BA (S : in String) return Byte_Arr is
      BA : Byte_Arr (S'Range);
   begin
      for I in S'Range loop
         BA(I) := Byte(Character'Pos(S(I)));
      end loop;
      return BA;
   end Str_To_BA;

   procedure Self_Test (T : in out Terminal_T) is 
      HRule1 : constant String := "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012245";
      HRule2 : constant String := "         1         2         3         4         5         6         7         8         9         10        11        12        13    ";
      Chars  : constant String := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!""$%^&*";
      NLine  : constant String := "3 Normal : ";
      DLine  : constant String := "4 Dim    : ";
      BLine  : constant String := "5 Blink  : ";
      RLine  : constant String := "6 Reverse: ";
      ULine  : constant String := "7 Under  : ";
      NL, Op : Byte_Arr (1..1);
   begin
      NL(1) := Dasher_NL;

      T.Process (Str_To_BA (HRule1(1..Display.Disp.Visible_Cols)));
      T.Process (Str_To_BA (HRule2(1..Display.Disp.Visible_Cols)));

      T.Process (Str_To_BA (NLine));
      T.Process (Str_To_BA (Chars));

      T.Process (Str_To_BA (DLine));
      Op(1) := Dasher_Dim_On;
      T.Process (Op);
      T.Process (Str_To_BA (Chars));
      Op(1) := Dasher_Dim_Off;
      T.Process (Op);

      T.Process (Str_To_BA (BLine));
      Op(1) := Dasher_Blink_On;
      T.Process (Op);
      T.Process (Str_To_BA (Chars));
      Op(1) := Dasher_Blink_Off;
      T.Process (Op);

      T.Process (Str_To_BA (RLine));
      Op(1) := Dasher_Rev_On;
      T.Process (Op);
      T.Process (Str_To_BA (Chars));
      Op(1) := Dasher_Rev_Off;
      T.Process (Op);

      T.Process (Str_To_BA (ULine));
      Op(1) := Dasher_Underline;
      T.Process (Op);
      T.Process (Str_To_BA (Chars));
      Op(1) := Dasher_Normal;
      T.Process (Op);   

      for L in 8 .. Display.Disp.Visible_Lines loop
         if L > 8  then
            T.Process (NL);
         end if;
         T.Process (Str_To_BA (Ada.Strings.Fixed.Trim (L'Image, Ada.Strings.Left)));
      end loop;
   
   end Self_Test;

   -- Process is to be called with a Byte_Arr whenever there is any data for 
   -- the terminal to display or otherwise handle.
   procedure Process (T : in out Terminal_T; BA : in Byte_Arr) is
      B : Byte;
      B_Int : Integer;
      C : Character;
   begin
      
      for Ix in BA'Range loop
         B := BA(Ix);
         B_Int := Integer(B);

         Ada.Text_IO.Put_Line ("DEBUG: Terminal.Process got: " & B'Image);

         T.Skip_Byte := False;

         -- wrap due to hitting margin or new line?
         if T.Cursor_X = Display.Disp.Visible_Cols or B = Dasher_NL then
            -- hit bottom of screen?
            if T.Cursor_Y = Display.Disp.Visible_Lines - 1 then
               if T.Roll_Enabled then
                  T.Scroll_Up (1);
               else
                  T.Cursor_Y := 0;
                  Display.Clear_Line (Display.Disp, T.Cursor_Y);
               end if;
            else
               T.Cursor_Y := T.Cursor_Y + 1;
               if not T.Roll_Enabled then
                  Display.Clear_Line (Display.Disp, T.Cursor_Y);
               end if;
            end if;
            T.Cursor_X := 0;
         end if;

         case B is
            when 0 =>
               T.Skip_Byte := True;
            when Dasher_Bell =>
               Ada.Text_IO.Put (Ada.Characters.Latin_1.BEL); -- on running terminal...
               T.Skip_Byte := True;
            when Dasher_Blink_On =>
               T.Blinking := True;
               T.Skip_Byte := True;
            when Dasher_Blink_Off =>
               T.Blinking := False;
               T.Skip_Byte := True;  
            when Dasher_Blink_Enable =>
               Display.Disp.Blink_Enabled := True;  -- Modifies Display
               T.Skip_Byte := True;
            when Dasher_Blink_Disable =>
               Display.Disp.Blink_Enabled := False; -- Modifies Display
               T.Skip_Byte := True;
            when Dasher_Cursor_Down =>
               if T.Cursor_Y < Display.Disp.Visible_Lines - 1 then
                  T.Cursor_Y := T.Cursor_Y + 1;
               else
                  T.Cursor_Y := 0;
               end if;
               T.Skip_Byte := True;
            when Dasher_Cursor_Left =>
               if T.Cursor_X > 0 then
                  T.Cursor_X := T.Cursor_X - 1;
               else  
                  T.Cursor_X := Display.Disp.Visible_Cols - 1;
                  if T.Cursor_Y > 0 then
                     T.Cursor_Y := T.Cursor_Y - 1;
                  else
                     T.Cursor_Y := Display.Disp.Visible_Lines - 1;
                  end if;
               end if;
               T.Skip_Byte := True;
            when Dasher_Cursor_Right =>
               if T.Cursor_X < Display.Disp.Visible_Cols - 1 then
                  T.Cursor_X := T.Cursor_X + 1;
               else  
                  T.Cursor_X := 0;
                  if T.Cursor_Y < Display.Disp.Visible_Lines - 1 then
                     T.Cursor_Y := T.Cursor_Y + 1;
                  else
                     T.Cursor_Y := 0;
                  end if;
               end if;
               T.Skip_Byte := True;   
            when Dasher_Cursor_Up =>
               if T.Cursor_Y > 0 then
                  T.Cursor_Y := T.Cursor_Y - 1;
               else
                  T.Cursor_Y := Display.Disp.Visible_Lines - 1;
               end if;
               T.Skip_Byte := True;
            when Dasher_Dim_On =>
               T.Dimmed := True;
               T.Skip_Byte := True;
            when Dasher_Dim_Off =>
               T.Dimmed := False;
               T.Skip_Byte := True;
            when Dasher_Erase_EOL =>
               for Col in T.Cursor_X .. Display.Disp.Visible_Cols - 1 loop
                  Display.Disp.Cells(T.Cursor_Y,col).Clear_To_Space;
               end loop;
               T.Skip_Byte := True;
            when Dasher_Erase_Page =>
               T.Scroll_Up (Display.Disp.Visible_Lines);
               T.Cursor_X := 0;
               T.Cursor_Y := 0;
               T.Skip_Byte := True;
            when Dasher_Home =>
               T.Cursor_X := 0;
               T.Cursor_Y := 0;
               T.Skip_Byte := True;
            when Dasher_Rev_On =>
               T.Reversed := True;
               T.Skip_Byte := True;
            when Dasher_Rev_Off =>
               T.Reversed := False;
               T.Skip_Byte := True;
            when Dasher_Underline =>
               T.Underscored := True;
               T.Skip_Byte := True;
            when Dasher_Normal =>
               T.Underscored := False;
               T.Skip_Byte := True;   
            when Dasher_Tab =>
               T.Cursor_X := T.Cursor_X + 1; -- always at least 1 column
               while (T.Cursor_X + 1) mod 8 /= 0 loop
                  if T.Cursor_X >= Display.Disp.Visible_Cols - 1 then
                     T.Cursor_X := 0; -- TODO What about Cursor_Y ???
                  else
                     T.Cursor_X := T.Cursor_X + 1;
                  end if;
               end loop; 
               T.Skip_Byte := True;
            when others =>
               null;        
         end case;

         if T.Skip_Byte then
            goto Continue;
         end if;

         -- CR or NL?
         if B = Dasher_CR or B = Dasher_NL then
            T.Cursor_X := 0;
            -- TODO handle Expect case
            goto Continue;
         end if;

         if T.Skip_Byte then
            goto Continue;
         end if;

         -- Finally! Put the character in the displayable matrix
         if B_Int > 0 and B_Int < BDF_Font.Max_Chars and BDF_Font.Decoded.Font(B_Int).Loaded then
            C := Character'Val(B_Int);
         else
            C := Character'Val(127); -- the 'unknown character' character
         end if;
         Display.Disp.Cells(T.Cursor_Y, T.Cursor_X).Set (Value => C, Blnk => T.Blinking, Dm => T.Dimmed, 
                                                         Rv => T.Reversed, Under => T.Underscored, Prot => T.Protectd);
         T.Cursor_X := T.Cursor_X + 1;
         -- TODO handle Expect case

         --- DasherG sends an update signal here
        Crt.Tube.DA.Queue_Draw;

         <<Continue>>
      end loop;

   end Process;

   procedure Scroll_Up (T : in out Terminal_T; Lines : in Integer) is
   begin
      for L in 1 .. Lines loop
         -- TODO History
         for R in 1 .. Display.Disp.Visible_Lines loop
            Display.Copy_Line (This => Display.Disp, Src => R, Dest => R - 1);
            Display.Clear_Line (Display.Disp, R);
         end loop;
         Display.Clear_Line (Display.Disp, Display.Disp.Visible_Lines - 1);
      end loop;
   end Scroll_Up;

end Terminal;