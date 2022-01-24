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

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;

with Gdk.Threads;

with Redirector;

package body Mini_Expect is

   -- protected body Runner is

      procedure Prepare (Filename : in String) is
      begin
         if Expecting then
            raise Already_Expecting with "Cannot run mini-Expect script while another is still active";
         end if;
         Open (File => Expect_File, Mode => In_File, Name => Filename);
         Runner_Task := new Runner_T;
      end Prepare;

      function Convert_Line (Script_Line : in String) return String is
      -- Remove leading and trailing double-quotes, convert \n to Dasher NL
         Src_Start : constant Positive := Index (Script_Line, """", Forward) + 1;
         Src_End   : constant Positive := Index (Script_Line, """", Backward);
         Result : String (1 .. Src_End-Src_Start);
         In_Ix  : Positive := Src_Start;
         Out_Ix : Positive := 1;
         Changed : Boolean := False;
      begin
         Put_Line ("DEBUG: Convert_Line called with: " & Script_Line);
         Put_Line ("DEBUG: ... Src_Start set to" & Src_Start'Image & ", Src_End set to" & Src_End'Image);
         Put_Line ("DEBUG: ... Max result length set to" & Result'Length'Image);
         while In_Ix < Src_End loop
            Changed := False;
            if In_Ix < Src_End then
               if Script_Line(In_Ix) = '\' then 
                  if Script_Line(In_Ix + 1) = 'n' then
                     Result(Out_Ix) := Dasher_Char_NL;
                     In_Ix := In_Ix + 2; -- skip over a character
                     Out_Ix := Out_Ix + 1;
                     Changed := True;
                  end if;
               end if;
            end if;
            if not Changed then
               Result(Out_Ix) := Script_Line(In_Ix);
               In_Ix := In_Ix + 1;
               Out_Ix := Out_Ix + 1;
            end if;
         end loop;
         Put_Line ("DEBUG: Convert_Line returning: " & Result(1 .. Out_Ix-1));
         return Result(1 .. Out_Ix-1);
      end Convert_Line;

      procedure Handle_Byte (Byt : in Byte; Done : out Boolean) is
      begin
         if Byt = Dasher_NL or Byt = Dasher_CR then
            -- Reset the search on every new line
            Host_Str := Null_Unbounded_String;
         else
            Host_Str := Host_Str & Character'Val(Byt);
            Put_Line ("DEBUG: ... so far we have: "  & To_String (Host_Str));
            if Length (Host_Str) >= Length (Search_Str) then
               Put_Line ("DEBUG: ... Handle_Byte comparing '" & To_String (Tail(Host_Str, Length (Search_Str)))
                  & "' with '" & To_String (Search_Str));
               if Tail(Host_Str, Length (Search_Str)) = Search_Str then
                  Expecting := False;
                  Put_Line ("DEBUG: ... MATCHED!");
               end if;
            end if;
         end if;
         Done := not Expecting;
      end Handle_Byte;

   --    function Is_Expecting return Boolean is 
   --       (Expecting);

   --    procedure Execute is
   --    begin
   --       while not End_Of_File (Expect_File) loop
   --          Get_Line (Expect_File, Expect_Line, Expect_Line_Length);
   --          Put_Line ("DEBUG: Expect script line: " & Expect_Line (1 .. Expect_Line_Length));
   --          if Expect_Line_Length = 0 then
   --             -- empty line
   --             Put_Line ("DEBUG: ... Skipping empty line");

   --          elsif Expect_Line(1) = '#' then
   --             -- comment line
   --             Put_Line ("DEBUG: ... Skipping comment line");

   --          elsif Expect_Line(1..4) = "send" then
   --             -- send line to host command  
   --             Put_Line ("DEBUG: ... Processing 'send' command");
   --             declare
   --                Converted : constant String := Convert_Line (Expect_Line(6..Expect_Line_Length));
   --                BA : Byte_Arr (1 .. Converted'Length);
   --             begin
   --                for Ix in Converted'Range loop
   --                   BA(Ix) := Byte(Character'Pos(Converted(Ix)));
   --                end loop;
   --                Redirector.Router.Send_Data (BA);
   --                delay 0.2;
   --             end;

   --          elsif Expect_Line(1..6) = "expect" then
   --             -- expect string from host command, no timeout  
   --             Put_Line ("DEBUG: ... Processing 'expect' command");
   --             -- delay 0.2;
   --             Search_Str := To_Unbounded_String (Convert_Line (Expect_Line(8..Expect_Line_Length)));
   --             Put_Line ("DEBUG: ... the search sting is '" & To_String (Search_Str) & "'");
   --             Host_Str   := Null_Unbounded_String;     
   --             Expecting := True;  
   --             return;
   --             -- while Expecting loop
   --             --     delay 0.2;  
   --             --       Put_Line ("DEBUG: ... waiting for match...");
   --             -- end loop;

   --          elsif Expect_Line(1..4) = "exit" then
   --             -- exit script command
   --             exit;

   --          else
   --             Put_Line ("WARNING: Cannot parse mini-Expect command - aborting script");
   --             exit;
   --          end if;
   --       end loop;
   --       Close (Expect_File);
   --       Ada.Text_IO.Put_Line ("DEBUG: Mini-Expect script ***completed***");
   --    end Execute;
   
   -- end Runner;

   task body Runner_T is
      Expect_Line : String(1..132);
      Expect_Line_Length : Natural;
   begin
      Expecting := False;

      while not End_Of_File (Expect_File) loop
         Get_Line (Expect_File, Expect_Line, Expect_Line_Length);
         Put_Line ("DEBUG: Expect script line: " & Expect_Line (1 .. Expect_Line_Length));
         if Expect_Line_Length = 0 then
            -- empty line
            Put_Line ("DEBUG: ... Skipping empty line");

         elsif Expect_Line(1) = '#' then
            -- comment line
            Put_Line ("DEBUG: ... Skipping comment line");

         elsif Expect_Line(1..6) = "expect" then
            -- expect string from host command, no timeout  
            Expecting := True;
            Put_Line ("DEBUG: ... Processing 'expect' command");
            -- delay 0.2;
            Search_Str := To_Unbounded_String (Convert_Line (Expect_Line(8..Expect_Line_Length)));
            Put_Line ("DEBUG: ... the search sting is '" & To_String (Search_Str) & "'");
            Host_Str   := Null_Unbounded_String;       
            while Expecting loop
               Put_Line ("DEBUG: Mini_Expect waiting for match");
               delay 0.1;
            end loop;
            Put_Line ("DEBUG: ... found Expect string: " & To_String (Search_Str));
            delay 0.2;
           
         elsif Expect_Line(1..4) = "send" then
            -- send line to host command  
            Put_Line ("DEBUG: ... Processing 'send' command");
            declare
               Converted : constant String := Convert_Line (Expect_Line(6..Expect_Line_Length));
               BA : Byte_Arr (1 .. Converted'Length);
            begin
               for Ix in Converted'Range loop
                  BA(Ix) := Byte(Character'Pos(Converted(Ix)));
               end loop;
               Redirector.Router.Send_Data (BA);
            end;

         elsif Expect_Line(1..4) = "exit" then
            -- exit script command
            exit;

         else
            Put_Line ("WARNING: Cannot parse mini-Expect command - aborting script");
            exit;
         end if;
      end loop;
      Close (Expect_File);
      Ada.Text_IO.Put_Line ("DEBUG: Mini-Expect script ***completed***");

   end Runner_T;

end Mini_Expect;