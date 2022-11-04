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
with Dasher_Codes;   use Dasher_Codes;
with Display_P;      use Display_P;
with Logging;        use Logging;
with Session_Logger;
with Mini_Expect;
with Redirector;

package body Terminal is

   function Create (Emul : Emulation_T) return Terminal_Acc_T is
      T : aliased constant Terminal_Acc_T := new Terminal_T;
   begin
      T.Emulation := Emul;
      T.Cursor_X := 0;
      T.Cursor_Y := 0;
      T.In_Command := False;
      T.In_Extended_Command := False;
      T.Getting_X_Addr := False;
      T.Getting_Y_Addr := False;
      T.Roll_Enabled := True;
      T.Protection_Enabled := True;
      T.Skip_Byte := False;
      T.Holding := False;
      T.Expecting := False;
      T.Raw_Mode := False;
      T.Blinking := False;
      T.Dimmed := False;
      T.Reversed := False;
      T.Underscored := False;
      T.Protectd := False;

      T.Updated := True;

      Processor_Task := new Processor;
      Processor_Task.Start (T);

      return T;
   end Create;

   procedure Self_Test (T : in out Terminal_T) is
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
      T.Process (Op);

      T.Process (HRule1 (1 .. Display.Get_Visible_Cols));
      T.Process (HRule2 (1 .. Display.Get_Visible_Cols));

      T.Process (NLine);
      T.Process (Chars);
      T.Process (NL);

      T.Process (DLine);
      Op (1) := Dasher_Dim_On;
      T.Process (Op);
      T.Process (Chars);
      Op (1) := Dasher_Dim_Off;
      T.Process (Op);
      T.Process (NL);

      T.Process (BLine);
      Op (1) := Dasher_Blink_On;
      T.Process (Op);
      T.Process (Chars);
      Op (1) := Dasher_Blink_Off;
      T.Process (Op);
      T.Process (NL);

      T.Process (RLine);
      Op (1) := Dasher_Rev_On;
      T.Process (Op);
      T.Process (Chars);
      Op (1) := Dasher_Rev_Off;
      T.Process (Op);
      T.Process (NL);

      T.Process (ULine);
      Op (1) := Dasher_Underline;
      T.Process (Op);
      T.Process (Chars);
      Op (1) := Dasher_Normal;
      T.Process (Op);
      T.Process (NL);

      for L in 8 .. Display.Get_Visible_Lines loop
         if L > 8  then
            T.Process (NL);
         end if;
         T.Process (Ada.Strings.Fixed.Trim (L'Image, Ada.Strings.Left));
      end loop;

   end Self_Test;

   task body Processor is
      TA : Terminal_Acc_T;
   begin
      accept Start (Termin : Terminal_Acc_T) do
         TA := Termin;
      end Start;
      loop
         select
            accept Accept_Data (Str : String) do
               TA.Process (Str);
            end Accept_Data;
         or
            accept Stop;
               exit;
         or
            terminate;
         end select;
      end loop;
      Log (ERROR, "Terminal Processor has exited");
   end Processor;

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
      Redirector.Router.Send_Data (Response);
   end Send_Model_ID;

   --  Process is to be called with a Byte_Arr whenever there is any data for
   --  the terminal to display or otherwise handle.
   procedure Process (T : in out Terminal_T; Str : String) is
      B : Character;
      B_Int : Integer;
      C : Character;
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
                  select
                     Redirector.Router.Send_Data (B3_Arr);
                  or
                     delay 0.5;
                  end select;
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
         if B_Int < 128 and then BDF_Font.Font.Is_Loaded (B_Int) then
            C := B;
         end if;

         Display.Set_Cell (Line => T.Cursor_Y, Col => T.Cursor_X, Char => C, Blink => T.Blinking, Dim => T.Dimmed,
                           Rev => T.Reversed, Under => T.Underscored, Prot => T.Protectd);

         T.Cursor_X := T.Cursor_X + 1;

      <<Redraw_Tube>>
         Display.Set_Cursor (T.Cursor_X, T.Cursor_Y);
         Display.Set_Dirty;
      end loop;
   end Process;

end Terminal;
