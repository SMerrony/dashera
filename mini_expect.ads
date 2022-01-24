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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded; 
with Ada.Text_IO;           use Ada.Text_IO;

with Dasher_Codes;          use Dasher_Codes;

package Mini_Expect is

   task type Runner_T; -- is
   --    entry Start;
   --    -- entry Expect (Byt : in Byte);
   -- end Runner_T;
   type Runner_Acc is access Runner_T;

   procedure Prepare (Filename : in String);
   -- Try to open a DasherA mini-Expect script.
   -- Could also sanity-check it in the future...

   procedure Handle_Byte (Byt : in Byte; Done : out Boolean);

   Expect_File : File_Type;
   Runner_Task : Runner_Acc;
   Expecting   : Boolean;
   Search_Str, 
   Host_Str    : Unbounded_String;

   -- protected Runner is
   --    procedure Prepare (Filename : in String);
   --    -- Try to open a DasherA mini-Expect script.
   --    -- Could also sanity-check it in the future...
   --    procedure Execute;
   --    procedure Handle_Byte (Byt : in Byte; Done : out Boolean);
   --    function Is_Expecting return Boolean;

   -- private
   --    Expect_File : File_Type;
   --    Expecting   : Boolean;
   --    Expect_Line : String(1..132);
   --    Expect_Line_Length : Natural;
   --    Search_Str, 
   --    Host_Str    : Unbounded_String;

   -- end Runner;

   Already_Expecting : exception;

end Mini_Expect;