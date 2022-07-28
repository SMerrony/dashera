--  Copyright (C)2021,2022 Steve Merrony

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

package Redirector is

   type Connection_T is (Local, Async, Network);
   type Handler_T is (Visual, Xmodem_Rx, Xmodem_Tx);

   task type Router_TT is
      entry Set_Destination (Dest : Connection_T);
      entry Get_Destination (Dest : out Connection_T);
      entry Send_Data (Data : String);
      entry Set_Handler (Handlr : Handler_T);
      entry Handle_Data (C : Character);
   end Router_TT;
   type Router_Acc is access Router_TT;

   Router : Router_Acc;

private
   Destination : Connection_T := Local;
   Handler     : Handler_T := Visual;

end Redirector;