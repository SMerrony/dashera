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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Sockets;          use GNAT.Sockets;

package Telnet is

   Cmd_SE   : constant Character := Character'Val (240);
   Cmd_NOP  : constant Character := Character'Val (241);
   Cmd_DM   : constant Character := Character'Val (242);
   Cmd_BRK  : constant Character := Character'Val (243);
   Cmd_IP   : constant Character := Character'Val (244);
   Cmd_AO   : constant Character := Character'Val (245);
   Cmd_AYT  : constant Character := Character'Val (246);
   Cmd_EC   : constant Character := Character'Val (247);
   Cmd_EL   : constant Character := Character'Val (248);
   Cmd_GA   : constant Character := Character'Val (249);
   Cmd_SB   : constant Character := Character'Val (250);
   Cmd_WILL : constant Character := Character'Val (251);
   Cmd_WONT : constant Character := Character'Val (252);
   Cmd_DO   : constant Character := Character'Val (253);
   Cmd_DONT : constant Character := Character'Val (254);
   Cmd_IAC  : constant Character := Character'Val (255);

   type Message is new String;

   type Session_T is tagged record
      Conn     : GNAT.Sockets.Socket_Type;
      Host_Str : Unbounded_String;  --  The host as specified by our user
      Port_Num : Integer;           --  The port as specified by our user
   end record;

   type Session_Acc_T is access all Session_T;

   function New_Connection (Host_Str : String; Port_Num : Integer) return Session_Acc_T;
   --  Attempt to initiate a new TCPIP/Telnet connection to the specified Host and Port.
   --  Data from the remote host will be directed to the supplied Terminal.
   --  To send data, call the Send procedure

   procedure Send (Sess : Session_Acc_T; Str : String);

   procedure Close_Connection (Sess : in out Session_T);

   task type Receiver is
      entry Start (Sess : Session_Acc_T);
   end Receiver;
   type Receiver_Acc is access Receiver;

   Receiver_Task : Receiver_Acc;

   task type Keyboard_Sender is
      entry Start (S : Session_Acc_T);
      entry Accept_Data (Str : String);
      entry Stop;
   end Keyboard_Sender;
   type Sender_Acc is access Keyboard_Sender;

   Keyboard_Sender_Task : Sender_Acc;

   Disconnected : exception;

end Telnet;