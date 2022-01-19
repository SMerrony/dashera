-- Copyright (C) 2022 Steve Merrony

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
            Redirector.Handle_Data (One_Char_BA);
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
            accept Stop;
               exit;
         or 
            terminate;
         end select;
      end loop;
   end Keyboard_Sender;

end Serial;