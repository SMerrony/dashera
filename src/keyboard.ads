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

--  See include/gtkada/gtkada.relocatable/gtkada/gdk-types-keysyms.ads for key symbol definitions

with Gdk.Types;         use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;

package Keyboard is

   Ctrl_Pressed, Shift_Pressed : Boolean := False;
   Last_Pressed                : Gdk_Key_Type := GDK_VoidSymbol;

   procedure Handle_Key_Press   (Key  : Gdk_Key_Type);
   -- Handle the pressing (or holding-down) of a key

   procedure Handle_Key_Release (Key  : Gdk_Key_Type);
   --  Handle the release of a key (and cancel any repeating)

   procedure Process_Key (Key : Gdk_Key_Type);
   --  Process_Key maps PC-style keys to DASHER ones and enqueues the characters(s).

end Keyboard;