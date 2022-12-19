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

with Serial;
with Telnet;
with Terminal;
with Xmodem;

package body Redirector is
   
   procedure Set_Destination (Dest : Connection_T) is
   begin
      Destination := Dest;
   end Set_Destination;

   function  Get_Destination return Connection_T is
      (Destination);

   procedure Set_Handler (Handlr : Handler_T) is
   begin
      Handler := Handlr;
   end Set_Handler;

   procedure Handle_Data (C: Character) is 
   begin
      case Handler is
         when Visual    => Terminal.Process ("" & C);
         when Xmodem_Rx => Xmodem.Receiver_Task.Accept_Data (C);
         when Xmodem_Tx => Xmodem.Sender_Task.Accept_Data (C);
      end case;
   end Handle_Data;

   procedure Send_Data (Data : String) is
   begin
      case Destination is
         when Local => Terminal.Process (Data);
         when Async => Serial.Keyboard_Sender_Task.Accept_Data (Data);
         when Network => Telnet.Keyboard_Sender_Task.Accept_Data (Data);
      end case;
   exception
      when Telnet.Disconnected =>
         Destination := Local;
         Handler     := Visual;
   end Send_Data;

end Redirector;