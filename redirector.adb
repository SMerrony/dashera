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

with Telnet;

package body Redirector is

   procedure Set_Destination (Termin : in Terminal_Acc_T; Dest : in Connection_T) is
   begin
      Term := Termin;
      Destination := Dest;
   end Set_Destination;

   procedure Send_Data (BA : in Byte_Arr) is
   begin
      case Destination is
         when Local => Terminal.Processor_Task.Accept_Data (BA);
         when Async => null;
         when Network => Telnet.Keyboard_Sender_Task.Accept_Data (BA);
      end case;
   end Send_Data;

   procedure Handle_Data (BA : in Byte_Arr) is
   begin
      Terminal.Processor_Task.Accept_Data (BA);
   end Handle_Data;

end Redirector;