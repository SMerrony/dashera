-- Copyright (C)2021,2022 Steve Merrony

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

with Serial;
with Telnet;
with Terminal;
with Xmodem;

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
         or
            accept Send_Data (Data : in String) do
               case Destination is
                  when Local => Terminal.Processor_Task.Accept_Data (Data);
                  when Async => Serial.Keyboard_Sender_Task.Accept_Data (Data);
                  when Network => Telnet.Keyboard_Sender_Task.Accept_Data (Data);
               end case;
            end Send_Data;
         or
            accept Set_Handler (Handlr : in Handler_T) do
               Handler := Handlr;
            end Set_Handler;
         or
            accept Handle_Data (C : in Character) do
               case Handler is
                  when Visual => Terminal.Processor_Task.Accept_Data ("" & C);
                  when File_Transfer => Xmodem.Receiver_Task.Accept_Data (C);
               end case;
            end Handle_Data;
         or
            terminate; 
         end select;
      end loop;
   end Router;

end Redirector;