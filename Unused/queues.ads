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

with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

with Dasher_Codes; use Dasher_Codes;

package Queues is

   package Byte_Arrs is
      new Ada.Containers.Indefinite_Holders (Byte_Arr);

   package Byte_Arrs_Queue_Interfaces is 
      new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Byte_Arrs.Holder);

   package Byte_Arr_Queues is
      new Ada.Containers.Unbounded_Synchronized_Queues
            (Queue_Interfaces => Byte_Arrs_Queue_Interfaces);

   Keyboard_Q  : Byte_Arr_Queues.Queue;

   procedure Keyboard_Enqueue (BA : in Byte_Arr);
   function  Keyboard_Dequeue return Byte_Arr;
   function  Keyboard_Data_Waiting return Boolean;

end Queues;

