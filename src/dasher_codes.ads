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

package Dasher_Codes is

   pragma Pure;

   Dasher_Null             : constant Character := Character'Val (0);
   Dasher_Print_Form       : constant Character := Character'Val (1);
   Dasher_Rev_Off          : constant Character := Character'Val (2); --  from D210 onwards
   Dasher_Blink_Enable     : constant Character := Character'Val (3); --  for the whole screen
   Dasher_Blink_Disable    : constant Character := Character'Val (4); --  for the whole screen
   Dasher_Read_Window_Addr : constant Character := Character'Val (5); --  requires response...
   Dasher_Ack              : constant Character := Character'Val (6); --  sent to host to indicatelocal print is complete
   Dasher_Bell             : constant Character := Character'Val (7);
   Dasher_Home             : constant Character := Character'Val (8); --  window home
   Dasher_Tab              : constant Character := Character'Val (9);
   Dasher_NL               : constant Character := Character'Val (10);
   Dasher_Erase_EOL        : constant Character := Character'Val (11);
   Dasher_Erase_Page       : constant Character := Character'Val (12);
   Dasher_CR               : constant Character := Character'Val (13);
   Dasher_Blink_On         : constant Character := Character'Val (14);
   Dasher_Blink_Off        : constant Character := Character'Val (15);
   Dasher_Write_Window_Addr : constant Character := Character'Val (16); --  followed by col then row
   Dasher_Print_Screen     : constant Character := Character'Val (17);
   Dasher_Roll_Enable      : constant Character := Character'Val (18);
   Dasher_Roll_Disable     : constant Character := Character'Val (19);
   Dasher_Underline        : constant Character := Character'Val (20);
   Dasher_Normal           : constant Character := Character'Val (21); --  cancels Underline
   Dasher_Rev_On           : constant Character := Character'Val (22); --  from D210 onwards
   Dasher_Cursor_Up        : constant Character := Character'Val (23);
   Dasher_Cursor_Right     : constant Character := Character'Val (24);
   Dasher_Cursor_Left      : constant Character := Character'Val (25);
   Dasher_Cursor_Down      : constant Character := Character'Val (26);
   Dasher_Escape           : constant Character := Character'Val (27);
   Dasher_Dim_On           : constant Character := Character'Val (28);
   Dasher_Dim_Off          : constant Character := Character'Val (29);
   Dasher_Command          : constant Character := Character'Val (30);

   Dasher_Space            : constant Character := Character'Val (32);

   Dasher_F15              : constant Character := Character'Val (112);
   Dasher_F1               : constant Character := Character'Val (113);
   Dasher_F2               : constant Character := Character'Val (114);
   Dasher_F3               : constant Character := Character'Val (115);
   Dasher_F4               : constant Character := Character'Val (116);
   Dasher_F5               : constant Character := Character'Val (117);
   Dasher_F6               : constant Character := Character'Val (118);
   Dasher_F7               : constant Character := Character'Val (119);
   Dasher_F8               : constant Character := Character'Val (120);
   Dasher_F9               : constant Character := Character'Val (121);
   Dasher_F10              : constant Character := Character'Val (122);
   Dasher_F11              : constant Character := Character'Val (123);
   Dasher_F12              : constant Character := Character'Val (124);
   Dasher_F13              : constant Character := Character'Val (125);
   Dasher_F14              : constant Character := Character'Val (126);

   Dasher_C1               : constant Character := Character'Val (92);
   Dasher_C2               : constant Character := Character'Val (93);
   Dasher_C3               : constant Character := Character'Val (94);
   Dasher_C4               : constant Character := Character'Val (95);
   Dasher_Shift_C1         : constant Character := Character'Val (88);
   Dasher_Shift_C2         : constant Character := Character'Val (89);
   Dasher_Shift_C3         : constant Character := Character'Val (90);
   Dasher_Shift_C4         : constant Character := Character'Val (91);

   Dasher_Delete           : constant Character := Character'Val (127);

end Dasher_Codes;