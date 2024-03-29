--  Copyright ©2022 Steve Merrony

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

--  Note the serial break does not seem to be provided by GNAT.Serial_Communications
--  It is claimed that...
--
--  Sending break can be achieved by:
--
--  * lowering the bit-rate
--  * sending 0x00 which will seem as break.
--  * change bit-rate back.
--
--  During the break, it won't be possible to receive data since the bit-rate is not correct.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Streams;    use Ada.Streams;

with Logging;        use Logging;
with Redirector;

package body Serial is

   procedure Open (Port_Str  : String;
                   Rate      : Data_Rate;
                   Bits      : Data_Bits;
                   Parity    : Parity_Check;
                   Stop_Bits : Stop_Bits_Number) is
   begin
      GNAT.Serial_Communications.Open (Port, Port_Name (Port_Str));
      GNAT.Serial_Communications.Set (Port, Rate, Bits, Stop_Bits, Parity);
      --  Save the user-specified settings so we can reset after sending a Break
      User_Rate      := Rate;
      User_Bits      := Bits;
      User_Parity    := Parity;
      User_Stop_Bits := Stop_Bits;
      Log (DEBUG, "Serial port opened and set-up");
      Port_US := To_Unbounded_String (Port_Str);
      Receiver_Task := new Receiver;
      Receiver_Task.Start;
      Redirector.Set_Destination (Redirector.Async);
      Log (DEBUG, "Serial port open complete");
   end Open;

   procedure Close is
   begin
      Close (Port);
      Redirector.Set_Destination (Redirector.Local);
   end Close;

   task body Receiver is
      B : Character;
   begin
      accept Start do
         Log (DEBUG, "Serial Receiver Started");
      end Start;
      loop
         begin
            Character'Read (Port'Access, B);
            Redirector.Handle_Data (B);
         exception
            when Ada.IO_Exceptions.End_Error =>
               null;
         end;
      end loop;
   exception
      when Error : others =>
      Log (WARNING, Exception_Information (Error));
      Log (INFO,  "Serial Receiver loop exited");
      Close;
   end Receiver;

   procedure Send (Data : String) is
      SEA : Stream_Element_Array (1 .. Data'Length);
   begin
      for I in 1 .. Data'Length loop
         SEA (Stream_Element_Offset (I)) := Stream_Element (Character'Pos (Data (I)));
      end loop;
      Write (Port, SEA);
   end Send;

   procedure Send_Break is
      SEA : Stream_Element_Array (1 .. 1);
   begin
      --  Set a very slow data rate
      GNAT.Serial_Communications.Set (Port, B110, CS8, Two, None);
      SEA (1) := 0; --  all zeroes
      Write (Port, SEA);
      --  Reset port to user settings
      GNAT.Serial_Communications.Set (Port, User_Rate, User_Bits, User_Stop_Bits, User_Parity);
   end Send_Break;

end Serial;