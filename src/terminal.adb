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

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

with Gdk.Main;
with Glib.Main;

with BDF_Font;
with Crt;
with Dasher_Codes;   use Dasher_Codes;
with Display_P;      use Display_P;
with Logging;        use Logging;
with Session_Logger;
with Mini_Expect;
with Redirector;
with Telnet;
--  with Viewer;

package body Terminal is

   function Create (Emul : Emulation_T; Text_Only : Boolean) return Terminal_Acc_T is
      Term : aliased constant Terminal_Acc_T := new Terminal_T;
   begin
      Term.Emulation := Emul;
      Term.Text_Only := Text_Only;
      Term.Cursor_X := 0;
      Term.Cursor_Y := 0;
      Term.In_Command := False;
      Term.In_Extended_Command := False;
      Term.Getting_X_Addr := False;
      Term.Getting_Y_Addr := False;
      Term.Roll_Enabled := True;
      Term.Protection_Enabled := True;
      Term.Skip_Byte := False;
      Term.Holding := False;
      Term.Expecting := False;
      Term.Raw_Mode := False;
      Term.Blinking := False;
      Term.Dimmed := False;
      Term.Reversed := False;
      Term.Underscored := False;
      Term.Protectd := False;
      Term.Updated := True;

      T := Term;

      return Term;
   end Create;

   procedure Self_Test is
      HRule1 : constant String := "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012245";
      HRule2 : constant String := "         1         2         3         4         5         6         7         8         9         10        11        12        13    ";
      Chars  : constant String := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!""$%^&*";
      NLine  : constant String := "3 Normal : ";
      DLine  : constant String := "4 Dim    : ";
      BLine  : constant String := "5 Blink  : ";
      RLine  : constant String := "6 Reverse: ";
      ULine  : constant String := "7 Under  : ";
      NL, Op : String (1 .. 1);
   begin
      NL (1) := Dasher_NL;

      Op (1) := Dasher_Erase_Page;
      Process (Op);

      Process (HRule1 (1 .. Display.Get_Visible_Cols));
      Process (HRule2 (1 .. Display.Get_Visible_Cols));

      Process (NLine);
      Process (Chars);
      Process (NL);

      Process (DLine);
      Op (1) := Dasher_Dim_On;
      Process (Op);
      Process (Chars);
      Op (1) := Dasher_Dim_Off;
      Process (Op);
      Process (NL);

      Process (BLine);
      Op (1) := Dasher_Blink_On;
      Process (Op);
      Process (Chars);
      Op (1) := Dasher_Blink_Off;
      Process (Op);
      Process (NL);

      Process (RLine);
      Op (1) := Dasher_Rev_On;
      Process (Op);
      Process (Chars);
      Op (1) := Dasher_Rev_Off;
      Process (Op);
      Process (NL);

      Process (ULine);
      Op (1) := Dasher_Underline;
      Process (Op);
      Process (Chars);
      Op (1) := Dasher_Normal;
      Process (Op);
      Process (NL);

      for L in 8 .. Display.Get_Visible_Lines loop
         if L > 8  then
            Process (NL);
         end if;
         Process (Ada.Strings.Fixed.Trim (L'Image, Ada.Strings.Left));
      end loop;

   end Self_Test;

   procedure Set_Cursor (T : in out Terminal_T; X, Y : Natural) is
   begin
      T.Cursor_X := X;
      T.Cursor_Y := Y;
   end Set_Cursor;

   function Beep return Boolean is
   begin
      Gdk.Main.Beep;
      return False;
   end Beep;

   procedure Send_Model_ID (T : Terminal_T) is
      Response : String (1 .. 6);
   begin
      Response (1) := Character'Val (8#036#); --  Header 1
      Response (1) := Character'Val (8#157#); --  Header 2
      Response (3) := Character'Val (8#043#); --  Model Report Follows
      case T.Emulation is
         when D200 =>
            Response (4) := Character'Val (8#041#);      --  D100/D200
            Response (5) := Character'Val (2#01011010#); --  see p.2-7 of D100/D200 User Manual (="Z")
            Response (6) := Character'Val (8#003#);      --  Firmware Code
         when D210 =>
            Response (4) := Character'Val (8#050#);      --  D210
            Response (5) := Character'Val (2#01010001#); --  See p.3-9 of D210/D211 User Manual
            Response (6) := Character'Val (8#132#);      --  Firmware Code
      end case;
      Redirector.Send_Data (Response);
   end Send_Model_ID;

   --  Process is to be called with a String whenever there is any data for
   --  the terminal to display or otherwise handle.
   procedure Process (Str : String) is
      B : Character;
      B_Int : Integer;
      C : Character;
      Unused_SI : Glib.Main.G_Source_Id;
   begin

      for Ix in Str'Range loop
         B := Str (Ix);
         B_Int := Character'Pos (B);

         --  Ada.Text_IO.Put_Line ("DEBUG: Terminal.Process got: " & B'Image);

         T.Skip_Byte := False;

         if T.Getting_X_Addr then --  host is setting cursor address
            T.New_X_Addr := Natural (B_Int mod 127);
            if T.New_X_Addr = 127 then
               --  special case - x stays the same - see D410 User Manual p.3-25
               T.New_X_Addr := T.Cursor_X;
            elsif T.New_X_Addr >= Display.Get_Visible_Cols then
               T.New_X_Addr := T.New_X_Addr - Display.Get_Visible_Cols;
            end if;
            T.Getting_X_Addr := False;
            T.Getting_Y_Addr := True;
            goto Redraw_Tube;
         end if;

         if T.Getting_Y_Addr then
            T.New_Y_Addr := Natural (B_Int mod 127);
            if T.New_Y_Addr = 127 then
               T.New_Y_Addr := T.Cursor_Y;
            elsif T.New_Y_Addr >= Display.Get_Visible_Lines then
               --  see end of p.3-24 in D410 User Manual
               if T.Roll_Enabled then
                  Display.Scroll_Up (T.New_Y_Addr - (Display.Get_Visible_Lines - 1));
               end if;
               T.New_Y_Addr := T.New_Y_Addr - Display.Get_Visible_Lines;
            end if;
            T.Set_Cursor (T.New_X_Addr, T.New_Y_Addr);
            T.Getting_Y_Addr := False;
            goto Redraw_Tube;
         end if;

         --  Log it if required
         if Session_Logger.Logging then
            Session_Logger.Log_Char (Character'Val (B_Int));
         end if;

         --  short DASHER commands
         if T.In_Command then
            case B is
               when 'C' =>
                  T.Send_Model_ID;
               when 'D' =>
                  T.Reversed := True;
               when 'E' =>
                  T.Reversed := False;
               when 'F' =>
                  T.In_Extended_Command := True;
               when others =>
                  Log (WARNING, "Unrecognised Break-CMD code:" & B_Int'Image);
            end case;
            T.In_Command := False;
            goto Redraw_Tube;
         end if;

         --  D210 Command(s)
         if T.In_Extended_Command then
            case B is
               when 'F' => --  erase unprotected to end of screen
                  --  clear to end of current line
                  for Col in T.Cursor_X .. (Display.Get_Visible_Cols - 1) loop
                     Display.Clear_Unprotected_Cell (Line => T.Cursor_Y, Col => Col);
                  end loop;
                  --  clear all lines below
                  for Line in (T.Cursor_Y + 1) .. (Display.Get_Visible_Lines - 1) loop
                     for Col in 0 .. (Display.Get_Visible_Cols - 1) loop
                        Display.Clear_Unprotected_Cell (Line => T.Cursor_Y, Col => Col);
                     end loop;
                  end loop;
               when others =>
                  Log (WARNING, "Unrecognised Break-CMD F code:" & B_Int'Image);
            end case;
            T.In_Extended_Command := False;
            goto Redraw_Tube;
         end if;

         case B is
            when Dasher_Null =>
               T.Skip_Byte := True;
            when Dasher_Bell =>
               declare
                  Dummy_ID : Glib.Main.G_Source_Id;
               begin
                  Dummy_ID := Glib.Main.Idle_Add (Beep'Access);
               end;
               Log (INFO, "*** BEEP! ***" & Ada.Characters.Latin_1.BEL); --  on running terminal...
               T.Skip_Byte := True;
            when Dasher_Blink_On =>
               T.Blinking := True;
               T.Skip_Byte := True;
            when Dasher_Blink_Off =>
               T.Blinking := False;
               T.Skip_Byte := True;
            when Dasher_Blink_Enable =>
               Display.Set_Blink_Enabled (True);  --  Modifies Display
               T.Skip_Byte := True;
            when Dasher_Blink_Disable =>
               Display.Set_Blink_Enabled (False); --  Modifies Display
               T.Skip_Byte := True;
            when Dasher_Command =>
               T.In_Command := True; --  next char will form (part of) a command
               T.Skip_Byte := True;
            when Dasher_Cursor_Down =>
               if T.Cursor_Y < Display.Get_Visible_Lines - 1 then
                  T.Cursor_Y := T.Cursor_Y + 1;
               else
                  T.Cursor_Y := 0;
               end if;
               T.Skip_Byte := True;
            when Dasher_Cursor_Left =>
               if T.Cursor_X > 0 then
                  T.Cursor_X := T.Cursor_X - 1;
               else
                  T.Cursor_X := Display.Get_Visible_Cols - 1;
                  if T.Cursor_Y > 0 then
                     T.Cursor_Y := T.Cursor_Y - 1;
                  else
                     T.Cursor_Y := Display.Get_Visible_Lines - 1;
                  end if;
               end if;
               T.Skip_Byte := True;
            when Dasher_Cursor_Right =>
               if T.Cursor_X < Display.Get_Visible_Cols - 1 then
                  T.Cursor_X := T.Cursor_X + 1;
               else
                  T.Cursor_X := 0;
                  if T.Cursor_Y < Display.Get_Visible_Lines - 1 then
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
                  T.Cursor_Y := Display.Get_Visible_Lines - 1;
               end if;
               T.Skip_Byte := True;
            when Dasher_Dim_On =>
               T.Dimmed := True;
               T.Skip_Byte := True;
            when Dasher_Dim_Off =>
               T.Dimmed := False;
               T.Skip_Byte := True;
            when Dasher_Erase_EOL =>
               for Col in T.Cursor_X .. Display.Get_Visible_Cols - 1 loop
                  Display.Clear_Cell (T.Cursor_Y, Col);
               end loop;
               T.Skip_Byte := True;
            when Dasher_Erase_Page =>
               Display.Scroll_Up (Display.Get_Visible_Lines);
               T.Set_Cursor (0, 0);
               T.Skip_Byte := True;
            when Dasher_Home =>
               T.Set_Cursor (0, 0);
               T.Skip_Byte := True;
            when Dasher_Read_Window_Addr => --  REQUIRES RESPONSE - see D410 User Manual p.3-18
               declare
                  B3_Arr : String (1 .. 3);
               begin
                  B3_Arr (1) := Character'Val (31);
                  B3_Arr (2) := Character'Val (T.Cursor_X);
                  B3_Arr (3) := Character'Val (T.Cursor_Y);
                  Redirector.Send_Data (B3_Arr);
                  --  select
                  --     Redirector.Send_Data (B3_Arr);
                  --  or
                  --     delay 0.5; -- FIXME this was to prevent hang if other end not waiting
                  --  end select;
               end;
               T.Skip_Byte := True;
            when Dasher_Rev_On =>
               if T.Emulation /= D200 then --  only for D210 and later models
                  T.Reversed  := True;
                  T.Skip_Byte := True;
               end if;
            when Dasher_Rev_Off =>
               if T.Emulation /= D200 then
                  T.Reversed  := False;
                  T.Skip_Byte := True;
               end if;
            when Dasher_Roll_Disable =>
               T.Roll_Enabled := False;
               T.Skip_Byte    := True;
            when Dasher_Roll_Enable =>
               T.Roll_Enabled := True;
               T.Skip_Byte    := True;
            when Dasher_Underline =>
               T.Underscored  := True;
               T.Skip_Byte    := True;
            when Dasher_Normal =>
               T.Underscored  := False;
               T.Skip_Byte    := True;
            --  TAB handling removed, according to the docs it is handled at the host end, not locally
            --  ... and re-added because I don't think it can ever cause a problem...
            when Dasher_Tab =>
               T.Cursor_X := T.Cursor_X + 1; --  always at least 1 column
               while (T.Cursor_X + 1) mod 8 /= 0 loop
                  if T.Cursor_X >= Display.Get_Visible_Cols - 1 then
                     T.Cursor_X := 0; --  ??? What about Cursor_Y ???
                  else
                     T.Cursor_X := T.Cursor_X + 1;
                  end if;
               end loop;
               T.Skip_Byte := True;
            when Dasher_Write_Window_Addr =>
               T.Getting_X_Addr := True;
               T.Skip_Byte := True;
            when others =>
               null;
         end case;

         if T.Skip_Byte then
            goto Redraw_Tube;
         end if;

         --  wrap due to hitting margin or new line?
         if T.Cursor_X = Display.Get_Visible_Cols or else B = Dasher_NL then
            --  hit bottom of screen?
            if T.Cursor_Y = Display.Get_Visible_Lines - 1 then
               if T.Roll_Enabled then
                  Display.Scroll_Up (1);
               else
                  T.Cursor_Y := 0;
                  Display.Clear_Line (T.Cursor_Y);
               end if;
            else
               T.Cursor_Y := T.Cursor_Y + 1;
               if not T.Roll_Enabled then
                  Display.Clear_Line (T.Cursor_Y);
               end if;
            end if;
            T.Cursor_X := 0;
         end if;

         --  CR or NL?
         if B = Dasher_CR or else B = Dasher_NL then
            T.Cursor_X := 0;
            --  TODO handle Expect case
            goto Redraw_Tube;
         end if;

         --  if Mini_Expect.Runner.Is_Expecting then
         if Mini_Expect.Expecting then
            --  Mini_Expect.Runner_Task.Expect (B);
            declare
               Finished : Boolean;
            begin
               Mini_Expect.Handle_Char (B, Finished);
               --  if not Finished then
               --     Mini_Expect.Runner.Execute;
               --  end if;
            end;
         end if;

         --  Finally! Put the character in the displayable matrix
         C := Character'Val (127); --  the 'unknown character' character
         if B_Int < 128 and then BDF_Font.Is_Loaded (B_Int) then
            C := B;
         end if;

         Display.Set_Cell (Line => T.Cursor_Y, Col => T.Cursor_X, Char => C, Blink => T.Blinking, Dim => T.Dimmed,
                           Rev => T.Reversed, Under => T.Underscored, Prot => T.Protectd);

         T.Cursor_X := T.Cursor_X + 1;

      <<Redraw_Tube>>
         Display.Set_Cursor (T.Cursor_X, T.Cursor_Y);
         Display.Set_Dirty;
         --  if T.Text_Only then
         --     Unused_SI := Glib.Main.Idle_Add (Viewer.Update_CB'Access);
         --     --  Viewer.Update;
         --  else
         Crt.Tube.DA.Queue_Draw;
         --  end if;
      end loop;
   end Process;

   function Process_CB return Boolean is
      Data : constant String := Telnet.Get_Data_Block;
   begin
      Telnet.Unlock_Data_Block;
      Process (Data);
      return False;
   end Process_CB;

end Terminal;
