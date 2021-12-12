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

package Terminal is

   type Emulation_T is (D200, D210);
   type Connection_T is (Disconnected, Serial, Telnet);

   type Byte is mod 2**8;
   type Byte_Arr is array (Positive range <>) of Byte;

   type Terminal_T is tagged record
      Emulation                        : Emulation_T;
      Connection                       : Connection_T;
      Cursor_X, Cursor_Y               : Natural;
      Roll_Enabled, Protection_Enabled : Boolean;
      Holding, Logging, Scrolled_Back  : Boolean;
      Skip_Byte                        : Boolean;
      Expecting                        : Boolean;
      Raw_Mode : Boolean; -- in rawMode all host data is passed straight through to rawChan
      -- Selection_Region...
      -- Log_File : File;
      -- Display : Display_Acc_T;
      Blinking, Dimmed, Reversed, Underscored, Protectd : Boolean;
      Updated                                           : Boolean;
   end record;

   type Terminal_Acc_T is access all Terminal_T;

   Dasher_Print_Form       : constant Byte := 1;
   Dasher_Rev_Off          : constant Byte := 2; -- from D210 onwards
   Dasher_Blink_Enable     : constant Byte := 3; -- for the whole screen
   Dasher_Blink_Disable    : constant Byte := 4; -- for the whole screen
   Dasher_Read_Window_Addr : constant Byte := 5; -- requires response...
   Dasher_Ack              : constant Byte := 6; -- sent to host to indicatelocal print is complete
   Dasher_Bell             : constant Byte := 7;
   Dasher_Home             : constant Byte := 8; -- window home
   Dasher_Tab              : constant Byte := 9;
   Dasher_NL               : constant Byte := 10;
   Dasher_Erase_EOL        : constant Byte := 11;
   Dasher_Erase_Page       : constant Byte := 12;
   Dasher_CR               : constant Byte := 13;
   Dasher_Blink_On         : constant Byte := 14;
   Dasher_Blink_Off        : constant Byte := 15;
   Dasher_Write_Window_Addr: constant Byte := 16; -- followed by col then row
	Dasher_Print_Screen     : constant Byte := 17;
	Dasher_Roll_Enable      : constant Byte := 18;
	Dasher_Roll_Disable     : constant Byte := 19;
   Dasher_Underline        : constant Byte := 20;
   Dasher_Normal           : constant Byte := 21; -- cancels Underline
   Dasher_Rev_On           : constant Byte := 22; -- from D210 onwards
   Dasher_Cursor_Up        : constant Byte := 23;
   Dasher_Cursor_Right     : constant Byte := 24;
   Dasher_Cursor_Left      : constant Byte := 25;
   Dasher_Cursor_Down      : constant Byte := 26;
   Dasher_Escape           : constant Byte := 27;
   Dasher_Dim_On           : constant Byte := 28;
   Dasher_Dim_Off          : constant Byte := 29;
   Dasher_Command          : constant Byte := 30;

   Dasher_Delete           : constant Byte := 127;

   function Create (Emul : in Emulation_T) return Terminal_Acc_T;
   procedure Self_Test (T : in out Terminal_T);
   procedure Process (T : in out Terminal_T; BA : in Byte_Arr);
   procedure Scroll_Up (T : in out Terminal_T; Lines : in Integer);

end Terminal;
