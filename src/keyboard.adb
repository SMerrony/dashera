--  Copyright (C)2021,2022 Steve Merrony
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

--  See include/gtkada/gtkada.relocatable/gtkada/gdk-types-keysyms.ads for key symbol definitions

with Dasher_Codes; use Dasher_Codes;
with Redirector;

package body Keyboard is

   procedure Handle_Key_Press (Key  : Gdk_Key_Type) is
   begin
      case Key is
         when GDK_Control_L | GDK_Control_R => Ctrl_Pressed  := True;
         when GDK_Shift_L   | GDK_Shift_R   => Shift_Pressed := True;
         when others => null;
      end case;
      -- is the user holding a key down?
      if Last_Pressed = Key then
         Process_Key (Key);
      end if;
      Last_Pressed := Key;
   end Handle_Key_Press;

   procedure Enqueue_Key (Ch : Character) is
      Str : String (1 .. 1);
   begin
      Str (1) := Ch;
      Redirector.Router.Send_Data (Str);
   end Enqueue_Key;

   function Modify (C : Character) return Character is
      MC : Character := C;
   begin
      if C >= Dasher_C1 and then C <= Dasher_C4 then
         if Shift_Pressed then
            MC := Character'Val (Character'Pos (MC) - 4);
         end if;
      else
         if Shift_Pressed then
            MC := Character'Val (Character'Pos (MC) - 16);
         end if;
         if Ctrl_Pressed  then
            MC := Character'Val (Character'Pos (MC) - 64);
         end if;
      end if;
      return MC;
   end Modify;

   procedure Enqueue_Pair (C1, C2 : Character) is
      Str2 : String (1 .. 2);
   begin
      Str2 (1) := C1;
      Str2 (2) := C2;
      Redirector.Router.Send_Data (Str2);
   end Enqueue_Pair;

   procedure Handle_Key_Release (Key  : Gdk_Key_Type) is
   begin
      Process_Key (Key);
      Last_Pressed := GDK_VoidSymbol;
   end Handle_Key_Release;

   procedure Process_Key (Key : Gdk_Key_Type) is
        Char : Character;
   begin
      --  Ada.Text_IO.Put_Line ("DEBUG: Handle_Key_Release got key:" & Key'Image);
      case Key is
         when GDK_Control_L | GDK_Control_R => Ctrl_Pressed  := False;
         when GDK_Shift_L   | GDK_Shift_R   => Shift_Pressed := False;

         when GDK_Return   => Enqueue_Key (Dasher_NL); --  convert PC-style Return to DG NL
         when GDK_KP_Enter => Enqueue_Key (Dasher_CR); --  convert PC Keypad Enter to DG CR

         when GDK_Tab    => Enqueue_Key (Dasher_Tab);
         when GDK_Down   => Enqueue_Key (Dasher_Cursor_Down);
         when GDK_Up     => Enqueue_Key (Dasher_Cursor_Up);
         when GDK_Left   => Enqueue_Key (Dasher_Cursor_Left);
         when GDK_Right  => Enqueue_Key (Dasher_Cursor_Right);
         when GDK_Home   => Enqueue_Key (Dasher_Home);

         --  The Backspace / DEL key must map to 127 which is the DASHER DEL code
         when GDK_BackSpace | GDK_Delete => Enqueue_Key (Dasher_Delete);

         when GDK_Escape => Enqueue_Key (Dasher_Escape);

         --  N.B. At least on Debian with $TERM set to "d210-dg", the host
         --  expects both bytes to arrive in the same packet...
         when GDK_F1  => Enqueue_Pair (Dasher_Command, Modify (Dasher_F1));
         when GDK_F2  => Enqueue_Pair (Dasher_Command, Modify (Dasher_F2));
         when GDK_F3  => Enqueue_Pair (Dasher_Command, Modify (Dasher_F3));
         when GDK_F4  => Enqueue_Pair (Dasher_Command, Modify (Dasher_F4));
         when GDK_F5  => Enqueue_Pair (Dasher_Command, Modify (Dasher_F5));
         when GDK_F6  => Enqueue_Pair (Dasher_Command, Modify (Dasher_F6));
         when GDK_F7  => Enqueue_Pair (Dasher_Command, Modify (Dasher_F7));
         when GDK_F8  => Enqueue_Pair (Dasher_Command, Modify (Dasher_F8));
         when GDK_F9  => Enqueue_Pair (Dasher_Command, Modify (Dasher_F9));
         when GDK_F10 => Enqueue_Pair (Dasher_Command, Modify (Dasher_F10));
         when GDK_F11 => Enqueue_Pair (Dasher_Command, Modify (Dasher_F11));
         when GDK_F12 => Enqueue_Pair (Dasher_Command, Modify (Dasher_F12));
         when GDK_F13 => Enqueue_Pair (Dasher_Command, Modify (Dasher_F13));
         when GDK_F14 => Enqueue_Pair (Dasher_Command, Modify (Dasher_F14));
         when GDK_F15 => Enqueue_Pair (Dasher_Command, Modify (Dasher_F15));

         --  Special codes from the virtual key buttons on the GUI
         when GDK_3270_EraseEOF => Enqueue_Key (Dasher_Erase_Page);
         when GDK_3270_EraseInput => Enqueue_Key (Dasher_Erase_EOL);
         when GDK_F29 => Enqueue_Key (Character'Val (19)); --  Fake Ctrl-S
         when GDK_F30 => Enqueue_Key (Character'Val (17)); --  Fake Ctrl-Q
         when GDK_F31 => Enqueue_Pair (Dasher_Command, Modify (Dasher_C1));
         when GDK_F32 => Enqueue_Pair (Dasher_Command, Modify (Dasher_C2));
         when GDK_F33 => Enqueue_Pair (Dasher_Command, Modify (Dasher_C3));
         when GDK_F34 => Enqueue_Pair (Dasher_Command, Modify (Dasher_C4));

         when others =>
            if Key < 256 then
               Char := Character'Val (Key);
               if Ctrl_Pressed then
                  Char := Character'Val (Character'Pos (Char) mod 32);
               end if;
               Enqueue_Key (Char);
            end if;
      end case;
   end Process_Key;
end Keyboard;