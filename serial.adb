-- Copyright (C)2022 Steve Merrony

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

-- Note the serial break does not seem to be provided by GNAT.Serial_Communications
-- It is claimed that...
--
-- Sending break can be achieved by:
--
-- * lowering the bit-rate
-- * sending 0x00 which will seem as break.
-- * change bit-rate back.
--
-- During the break, it won't be possible to receive data since the bit-rate is not correct.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Streams;	   use Ada.Streams;
with Ada.Text_IO;

with Redirector;

package body Serial is

   procedure Open (Port_Str  : in String;
                   Rate      : in Data_Rate;
                   Bits      : in Data_Bits;
                   Parity    : in Parity_Check;
                   Stop_Bits : in Stop_Bits_Number) is
   
   begin
      GNAT.Serial_Communications.Open (Port, Port_Name(Port_Str));
      GNAT.Serial_Communications.Set (Port, Rate, Bits, Stop_Bits, Parity);
      -- Save the user-specified settings so we can reset after sending a Break
      User_Rate      := Rate;
      User_Bits      := Bits;
      User_Parity    := Parity;
      User_Stop_Bits := Stop_Bits;
      Ada.Text_IO.Put_Line ("DEBUG: Serial port opened and set-up");
      Receiver_Task := new Receiver;
      Receiver_Task.Start;
      Keyboard_Sender_Task := new Keyboard_Sender;
      Keyboard_Sender_Task.Start;
      Ada.Text_IO.Put_Line ("DEBUG: Serial port open complete");
   end Open;

   procedure Close is
   begin
      Close (Port);
      Keyboard_Sender_Task.Stop;
   end Close;

   procedure Send_Break is
      SEA : Stream_Element_Array (1..1);
   begin
      Keyboard_Sender_Task.Stop;
      -- Set a very slow data rate
      GNAT.Serial_Communications.Set (Port, B110, CS8, Two, None);
      SEA(1) := 0; -- all zeroes
      Write (Port, SEA);
      -- Reset port to user settings
      GNAT.Serial_Communications.Set (Port, User_Rate, User_Bits, User_Stop_Bits, User_Parity);
      Keyboard_Sender_Task := new Keyboard_Sender;
      Keyboard_Sender_Task.Start;
      Ada.Text_IO.Put_Line ("DEBUG: BREAK signal sent");
   end Send_Break;

   task body Receiver is
      B : Byte;
      One_Char_BA : Byte_Arr(1..1);
   begin
      accept Start do
         Ada.Text_IO.Put_Line ("DEBUG: Serial Receiver Started");
      end Start;
      loop
         begin
            Byte'Read (Port'Access, B);
            One_Char_BA(1) := B;
            Redirector.Router.Handle_Data (One_Char_BA);
         exception
            when Ada.IO_Exceptions.END_ERROR =>
               null;
         end;
      end loop;
   exception
      when Error: others =>
      Ada.Text_IO.Put_Line (Exception_Information (Error));
      Ada.Text_IO.Put_Line ("INFO: Serial Receiver loop exited");
      Close;
   end Receiver;

   task body Keyboard_Sender is
   begin
      accept Start do
         Ada.Text_IO.Put_Line ("DEBUG: Serial Keyboard_Sender Started");
      end Start;
      loop
         select 
            accept Accept_Data (BA : in Byte_Arr) do
               declare
                  SEA : Stream_Element_Array (1..BA'Length);
               begin
                  for I in 1 .. BA'Length loop
                     SEA(Stream_Element_Offset(I)) := Stream_Element(BA(I));
                  end loop;
                  Write (Port, SEA);
               end;
            end Accept_Data;
         or
            accept Send_Break do
               declare
                  SEA : Stream_Element_Array (1..1);
               begin
                  -- Set a very slow data rate
                  GNAT.Serial_Communications.Set (Port, B110, CS8, Two, None);
                  SEA(1) := 0; -- all zeroes
                  Write (Port, SEA);
                  -- Reset port to user settings
                  GNAT.Serial_Communications.Set (Port, User_Rate, User_Bits, User_Stop_Bits, User_Parity);
               end;
            end Send_Break;
         or
            accept Stop;
               Ada.Text_IO.Put_Line ("DEBUG: Serial Keyboard_Sender Stopped");
               exit;
         or 
            terminate;
         end select;
      end loop;
   end Keyboard_Sender;

end Serial;