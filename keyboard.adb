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

with Interfaces; use Interfaces;

package body Keyboard is

   task body Key_Handler is
      Destination : Terminal.Connection_T;
      Key_Byte    : Unsigned_8;
      Stopping    : Boolean := False;
   begin
      accept Start (Dest : in Terminal.Connection_T) do
         Destination := Dest;
      end Start;
      loop
         select 
            accept Stop do
               Stopping := True;
            end Stop;
         or
            accept Press (Key : in Gdk.Types.Gdk_Key_Type) do
               Ada.Text_IO.Put_Line ("DEBUG: Key_Handler got key press for:" & Key'Image); 
            end Press;
         or
            accept Release (Key : in Gdk.Types.Gdk_Key_Type) do
               Ada.Text_IO.Put_Line ("DEBUG: Key_Handler got key release for:" & Key'Image); 
               case Key is
                  when others =>
                     Key_Byte := Unsigned_8 (Key);
               end case;
            end Release;
         or 
            terminate;
         end select;
         exit when Stopping;
      end loop;
   end Key_Handler;

end Keyboard;