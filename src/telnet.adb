--  Copyright ©2021,2022 Steve Merrony
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

with Ada.Exceptions;
with Ada.Streams;  use Ada.Streams;
with Ada.Unchecked_Conversion;

with Logging;      use Logging;
with Redirector;

package body Telnet is

   function New_Connection (Host_Str : String; Port_Num : Integer) return Session_Acc_T is
      Sess : aliased constant Session_Acc_T := new Session_T;
      Address : GNAT.Sockets.Sock_Addr_Type;
   begin
      --  Exceptions should be handled by caller
      GNAT.Sockets.Create_Socket (Sess.Conn);
      Log (DEBUG, "Telnet - Host: " & Host_Str & ", Port: " & Port_Num'Image);
      Address.Addr := GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (Host_Str), 1);
      Address.Port := GNAT.Sockets.Port_Type (Port_Num);
      GNAT.Sockets.Connect_Socket (Sess.Conn, Address);
      Log (DEBUG, "Telnet - Socket connected");
      --  GNAT.Sockets.Set_Socket_Option (Socket => Sess.Conn, Option => (No_Delay, True));
      Sess.Host_Str := To_Unbounded_String (Host_Str);
      Sess.Port_Num := Port_Num;
      Receiver_Task := new Receiver;
      Receiver_Task.Start (Sess);
      Keyboard_Sender_Task := new Keyboard_Sender;
      Keyboard_Sender_Task.Start (Sess);
      return Sess;
   end New_Connection;

   --  function Byte_To_Char is new Ada.Unchecked_Conversion(Byte, Character);

   task body Keyboard_Sender is
      Sess : Session_Acc_T;
   begin
      accept Start (S : Session_Acc_T) do
         Sess := S;
      end Start;
      loop
         select
            accept Accept_Data (Str : String) do
               Send (Sess, Str);
            end Accept_Data;
         or
            accept Stop;
               Log (DEBUG, "Telnet Keyboard_Sender task stopping");
               exit;
         or
            terminate;
         end select;
      end loop;
   end Keyboard_Sender;

   procedure Send (Sess : Session_Acc_T; Str : String) is
      SEA              : Ada.Streams.Stream_Element_Array (1 .. Str'Length);
      Dummy_Bytes_Sent : Ada.Streams.Stream_Element_Offset;
   begin
      --  Log (DEBUG, "Telnet.Send called with No. bytes: " & Str'Length'Image);
      for I in 1 .. Str'Length loop
         SEA (Ada.Streams.Stream_Element_Offset (I)) := Ada.Streams.Stream_Element (Character'Pos (Str (I)));
      end loop;
      GNAT.Sockets.Send_Socket (Socket => Sess.Conn,
                                Item => SEA,
                                Last => Dummy_Bytes_Sent
                                --  Flags => Send_End_Of_Record
                                );
      --  Log (DEBUG, "Telnet.Send sent No. Bytes: " & Bytes_Sent'Image);
   exception
      when E : others =>
         Log (WARNING, "Telnet.Send has Failed (disconnected?)");
         Log (WARNING, Ada.Exceptions.Exception_Information (E));
         raise Disconnected;
   end Send;

   procedure Close_Connection (Sess : in out Session_T) is
   begin
      GNAT.Sockets.Shutdown_Socket (Sess.Conn);
      Keyboard_Sender_Task.Stop;
      Redirector.Router.Set_Destination (Redirector.Local);
   exception
      when Socket_Error =>
         Log (WARNING, "Error closing socket (already disconnected?)");
   end Close_Connection;

   task body Receiver is
      Session      : Session_Acc_T;
      Block        : Ada.Streams.Stream_Element_Array (1 .. 2048);
      Offset       : Ada.Streams.Stream_Element_Count;
      One_Byte     : Character;
      Three_Bytes  : String (1 .. 3);
      In_Telnet_Cmd, Got_DO, Got_WILL : Boolean := False;

   begin
      accept Start (Sess : Session_Acc_T) do
         Session := Sess;
         Log (DEBUG, "Telnet Receiver Started");
      end Start;
      loop
         --  Log (DEBUG, "Telnet Receive waiting for data...");
         GNAT.Sockets.Receive_Socket (Session.Conn, Block, Offset);
         --  Log (DEBUG, "...Telnet Receiver got data from host - No. Bytes:" & Offset'Image);
         if Offset = 0 then
            Log (WARNING, "Telnet Receiver Stopping due to empty message from host");
            goto Halt;
         end if;
         for I in 1 .. Offset loop
            One_Byte := Character'Val (Block (I));
            --  Log (DEBUG, "...Telnet Receiver handling byte: " & One_Byte'Image);
            if One_Byte = Cmd_IAC then
               if In_Telnet_Cmd then
                  --  special case - the host really wants to send a 255 - let it through
                  Log (DEBUG, "Telnet - Passing through IAC character");
                  In_Telnet_Cmd := False;
               else
                  In_Telnet_Cmd := True;
                  Log (DEBUG, "Telnet - got IAC command indicator");
                  goto continue;
               end  if;
            end if;

            if In_Telnet_Cmd then
               case One_Byte is
                  when Cmd_DO =>
                     Got_DO := True;
                     Log (DEBUG, "Telnet - Got DO request");
                     goto continue;
                  when Cmd_WILL =>
                     Got_WILL := True;
                     Log (DEBUG, "Telnet - Got WILL request");
                     goto continue;
                  when Cmd_AO | Cmd_AYT | Cmd_BRK | Cmd_DM | Cmd_DONT |
                     Cmd_EC | Cmd_EL  | Cmd_IP  | Cmd_NOP | Cmd_SB | Cmd_SE =>
                     Log (DEBUG, "Telnet - Ignoring Telnet instruction:" & One_Byte'Image);
                     goto continue;
                  when others =>
                     null;
               end case;
            end if;

            if Got_DO then
               --  whatever the host ask us to do we will refuse
               Three_Bytes (1) := Cmd_IAC;
               Three_Bytes (2) := Cmd_WONT;
               Three_Bytes (3) := One_Byte;
               Send (Session, Three_Bytes);
               Log (DEBUG, "Telnet - Denying DO request for: " & One_Byte'Image);
               Got_DO := False;
               In_Telnet_Cmd := False;
               --  TESTING --
               Three_Bytes (2) := Cmd_GA;
               Send (Session, Three_Bytes);
               goto continue;
            end if;

            if Got_WILL then
               --  whatever the host offers to do we will refuse
               Three_Bytes (1) := Cmd_IAC;
               Three_Bytes (2) := Cmd_DONT;
               Three_Bytes (3) := One_Byte;
               Send (Session, Three_Bytes);
               Log (DEBUG, "Telnet - Denying WILL request for: " & One_Byte'Image);
               Got_WILL := False;
               In_Telnet_Cmd := False;
               goto continue;
            end if;

            Redirector.Router.Handle_Data (One_Byte);

         <<continue>>
         end loop;
      end loop;
      <<Halt>>
      Log (DEBUG, "Telnet Receiver loop exited");
      Session.Close_Connection;
   end Receiver;

end Telnet;