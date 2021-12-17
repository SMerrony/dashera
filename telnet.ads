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

with Glib;				use Glib;
with GNAT.Sockets;

with Dasher_Codes;   use Dasher_Codes;
with Terminal;

package Telnet is

   Telnet_Cmd_SE   : constant Byte := 240;
	Telnet_Cmd_NOP  : constant Byte := 241;
	Telnet_Cmd_DM   : constant Byte := 242;
	Telnet_Cmd_BRK  : constant Byte := 243;
	Telnet_Cmd_IP   : constant Byte := 244;
	Telnet_Cmd_AO   : constant Byte := 245;
	Telnet_Cmd_AYT  : constant Byte := 246;
	Telnet_Cmd_EC   : constant Byte := 247;
	Telnet_Cmd_EL   : constant Byte := 248;
	Telnet_Cmd_GA   : constant Byte := 249;
	Telnet_Cmd_SB   : constant Byte := 250;
	Telnet_Cmd_WILL : constant Byte := 251;
	Telnet_Cmd_WONT : constant Byte := 252;
	Telnet_Cmd_DO   : constant Byte := 253;
	Telnet_Cmd_DONT : constant Byte := 254;
	Telnet_Cmd_IAC  : constant Byte := 255;

   type Session_T is tagged record
      Conn : GNAT.Sockets.Socket_Type;
		Term : Terminal.Terminal_Acc_T;
   end record;

	type Session_Acc_T is access all Session_T;

	function New_Connection (Host_Str : in String; 
									 Port_Num : in Integer; 
									 Term : in Terminal.Terminal_Acc_T) return Session_Acc_T;
	-- Attempt to initiate a new TCPIP/Telnet connection to the specified Host and Port.
	-- Data from the remote host will be directed to the supplied Terminal.
	-- To send data, call the Send procedure

	procedure Send (Sess : in out Session_T; BA : in Byte_Arr);

	procedure Close_Connection (Sess : in out Session_T);

end Telnet;