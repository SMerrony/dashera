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

-- See include/gtkada/gtkada.relocatable/gtkada/gdk-types-keysyms.ads for key symbol definitions

with Ada.Text_IO;

with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;

with Dasher_Codes; use Dasher_Codes;
with Queues;

package body Keyboard is

   procedure Init (Term : in Terminal_Acc_T) is
   begin
      Term_Acc := Term;
   end Init;

   procedure Set_Destination (Dest : in Connection_T) is
   begin
      Destination := Dest;
   end Set_Destination;

   procedure Handle_Key_Press (Key  : in Gdk_Key_Type) is
   begin
      case Key is 
         when GDK_Control_L | GDK_Control_R => Ctrl_Pressed := True;
         when GDK_Shift_L | GDK_Shift_R     => Shift_Pressed := True;
         when others => null;
      end case;
   end Handle_Key_Press;

   procedure Enqueue_Key (Byt : in Byte) is 
      BA          : Byte_Arr(1..1);
   begin
      BA(1) := Byt;
      Queues.Keyboard_Enqueue (BA);
   end Enqueue_Key;


   function Modify (B : in Byte) return Byte is
      MB : byte := B;
   begin
      if Shift_Pressed then MB := MB - 16; end if;
      if Ctrl_Pressed  then MB := MB - 64; end if;
      return MB;
   end Modify;

   procedure Handle_Key_Release (Key  : in Gdk_Key_Type) is
      Char_Byte : Byte;
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Handle_Key_Release got key:" & Key'Image); 
      case Key is
         when GDK_Control_L | GDK_Control_R => Ctrl_Pressed := False;
         when GDK_Shift_L | GDK_Shift_R => Shift_Pressed := False;

         when GDK_Return   => Enqueue_Key (Dasher_NL); -- convert PC-style Return to DG NL
         when GDK_KP_Enter => Enqueue_Key (Dasher_CR); -- convert PC Keypad Enter to DG CR

         when GDK_Tab    => Enqueue_Key (Dasher_Tab);
         when GDK_Down   => Enqueue_Key (Dasher_Cursor_Down);
         when GDK_Up     => Enqueue_Key (Dasher_Cursor_Up);
         when GDK_Left   => Enqueue_Key (Dasher_Cursor_Left);
         when GDK_Right  => Enqueue_Key (Dasher_Cursor_Right);
         when GDK_Home   => Enqueue_Key (Dasher_Home);

         -- The DEL key must map to 127 which is the DASHER DEL code
         when GDK_Delete => Enqueue_Key (Dasher_Delete); 

         when GDK_Escape => Enqueue_Key (Dasher_Escape);

         when GDK_F1  => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (113));
         when GDK_F2  => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (114));
         when GDK_F3  => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (115));
         when GDK_F4  => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (116));
         when GDK_F5  => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (117));
         when GDK_F6  => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (118));
         when GDK_F7  => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (119));
         when GDK_F8  => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (120));
         when GDK_F9  => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (121));
         when GDK_F10 => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (122));
         when GDK_F11 => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (123));
         when GDK_F12 => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (124));
         when GDK_F13 => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (125));
         when GDK_F14 => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (126));
         when GDK_F15 => Enqueue_Key (Dasher_Command); Enqueue_Key (Modify (112));

         when others =>
            if Key < 256 then
               Char_Byte := Byte(Key);
               if Ctrl_Pressed then
                  Char_Byte := Char_Byte mod 32;
               end if;
               Enqueue_Key (Char_Byte);
            end if;
      end case;
   end Handle_Key_Release;

end Keyboard;