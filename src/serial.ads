--  Copyright (C)2022 Steve Merrony
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Serial_Communications; use GNAT.Serial_Communications;

package Serial is

   task type Receiver is
      entry Start;
   end Receiver;
   type Receiver_Acc is access Receiver;

   Receiver_Task : Receiver_Acc;

   task type Keyboard_Sender is
      entry Start;
      entry Accept_Data (Data : String);
      entry Send_Break;
      entry Stop;
   end Keyboard_Sender;
   type Sender_Acc is access Keyboard_Sender;

   Keyboard_Sender_Task : Sender_Acc;
   Port_US : Unbounded_String;

   procedure Open (Port_Str  : String;
                   Rate      : Data_Rate;
                   Bits      : Data_Bits;
                   Parity    : Parity_Check;
                   Stop_Bits : Stop_Bits_Number);

   procedure Close;

private

   Port : aliased Serial_Port;

   User_Rate      : Data_Rate;
   User_Bits      : Data_Bits;
   User_Parity    : Parity_Check;
   User_Stop_Bits : Stop_Bits_Number;

end Serial;