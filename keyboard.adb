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

-- TODO Why is this a task?  Is there really any need?

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

   procedure Route_Key (Byt : in Byte) is 
      BA          : Byte_Arr(1..1);
   begin
      case Destination is
         when Disconnected =>
            BA(1) := Byt;
            Term_Acc.Process (BA);
         when Serial => -- TODO
            null; 
         when Telnet => -- TODO
            null;
      end case;
   end Route_Key;

   procedure Handle_Key_Release (Key  : in Gdk_Key_Type) is
      Char_Byte : Byte;
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Handle_Key_Release got key:" & Key'Image); 
      case Key is
         when GDK_Control_L | GDK_Control_R => Ctrl_Pressed := False;
         when GDK_Shift_L | GDK_Shift_R => Shift_Pressed := False;

         when GDK_Return => Route_Key (Dasher_NL); -- convert PC-style Return to DG NL
         when GDK_Down   => Route_Key (Dasher_Cursor_Down);
         when GDK_Up     => Route_Key (Dasher_Cursor_Up);
         when GDK_Left   => Route_Key (Dasher_Cursor_Left);
         when GDK_Right  => Route_Key (Dasher_Cursor_Right);
         when others =>
            if Key < 256 then
               Char_Byte := Byte(Key);
               Route_Key (Char_Byte);
            end if;
      end case;
   end Handle_Key_Release;

end Keyboard;