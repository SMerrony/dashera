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

-- with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Unchecked_Conversion;

-- with Mini_Expect;
with Serial;
with Telnet;
with Terminal;

package body Redirector is

   task body Router is
   begin
      loop
         select
            accept Set_Destination (Dest : in Connection_T) do
               Destination := Dest;
            end Set_Destination;
         or
            accept Get_Destination (Dest : out Connection_T) do
               Dest := Destination;
            end Get_Destination;
         -- or
         --    accept Set_Expecting (Exp : in Boolean) do
         --       Expecting := Exp;
         --    end Set_Expecting;
         -- or
         --    accept Get_Expecting (Exp : out Boolean) do
         --       Exp := Expecting;
         --    end Get_Expecting;
         or
            accept Send_Data (BA : in Byte_Arr) do
               case Destination is
                  when Local => Terminal.Processor_Task.Accept_Data (BA);
                  when Async => Serial.Keyboard_Sender_Task.Accept_Data (BA);
                  when Network => Telnet.Keyboard_Sender_Task.Accept_Data (BA);
               end case;
            end Send_Data;
         or
            accept Handle_Data (BA : in Byte_Arr) do
               Terminal.Processor_Task.Accept_Data (BA);
            end Handle_Data;
         or
            terminate; 
         end select;
      end loop;
   end Router;

   function Char_To_Byte is new Ada.Unchecked_Conversion(Character, Byte);

   function String_To_BA (Str : in String) return Byte_Arr is
      BA : Byte_Arr(1..Str'Length);
      Ix : Positive := 1;
   begin
      for C of Str loop
         BA(Ix) := Char_To_Byte(C);
         Ix := Ix + 1;
      end loop;
      return BA;
   end String_To_BA;

end Redirector;