--  Copyright (C)2021,2022 Steve Merrony

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

package Terminal is

   type Emulation_T is (D200, D210);

   type Terminal_T is tagged record
      Emulation                        : Emulation_T;
      Cursor_X, Cursor_Y               : Natural;
      In_Command, In_Extended_Command  : Boolean;
      Getting_X_Addr, Getting_Y_Addr   : Boolean;
      New_X_Addr, New_Y_Addr           : Natural;
      Roll_Enabled, Protection_Enabled : Boolean;
      Holding                          : Boolean;
      Skip_Byte                        : Boolean;
      Expecting                        : Boolean;
      Raw_Mode : Boolean; --  in rawMode all host data is passed straight through to rawChan
      Blinking, Dimmed, Reversed, Underscored, Protectd : Boolean;
      Updated                                           : Boolean;
   end record;

   type Terminal_Acc_T is access all Terminal_T;

   task type Processor is
      entry Start (Termin : Terminal_Acc_T);
      entry Accept_Data (Str : String);
      entry Stop;
   end Processor;
   type Processor_Acc is access Processor;
   Processor_Task : Processor_Acc;

   function Create (Emul : Emulation_T) return Terminal_Acc_T;
   procedure Self_Test (T : in out Terminal_T);
   procedure Process (T : in out Terminal_T; Str : String);

end Terminal;
