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

package body Cell is

   procedure Set (This : in out Cell_T; Value : in Character; Blnk, Dm, Rv, Under, Prot : in Boolean) is
   begin
      This.Char_Value := Value;
      This.Blink      := Blnk;
      This.Dim        := Dm;
      This.Rev        := Rv;
      This.Underscore := Under;
      This.Protect    := Prot;
   end Set;

   procedure Clear_To_Space (This : in out Cell_T) is
   begin
      This.Char_Value := ' ';
      This.Blink      := false;
      This.Dim        := false;
      This.Rev        := false;
      This.Underscore := false;
      This.Protect    := false;
   end Clear_To_Space;
end Cell;