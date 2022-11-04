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

package body Cell is

   protected body Cell_T is

      procedure Set (Value : Character; Blnk, Dm, Rv, Under, Prot : Boolean) is
      begin
         Char_Value := Value;
         Blink      := Blnk;
         Dim        := Dm;
         Rev        := Rv;
         Underscore := Under;
         Protect    := Prot;
         Dirty      := True;
      end Set;

      procedure Get (Value : out Character; Blnk, Dm, Rv, Under, Prot : out Boolean) is
      begin
         Value := Char_Value;
         Blnk  := Blink;
         Dm    := Dim;
         Rv    := Rev;
         Under := Underscore;
         Prot  := Protect;
      end Get;

      function Get_Char return Character is
         (Char_Value);

      procedure Clear_To_Space is
      begin
         Char_Value := ' ';
         Blink      := False;
         Dim        := False;
         Rev        := False;
         Underscore := False;
         Protect    := False;
         Dirty      := True;
      end Clear_To_Space;

      procedure Clear_If_Unprotected is
      begin
         if not Protect then
            Clear_To_Space;
         end if;
      end Clear_If_Unprotected;

      function Is_Blinking return Boolean is
         (Blink);

      function Is_Dirty return Boolean is
         (Dirty);

      procedure Clear_Dirty is
      begin
         Dirty := False;
      end Clear_Dirty;

      procedure Set_Dirty is
      begin
         Dirty := True;
      end Set_Dirty;

   end Cell_T;

   procedure Copy (Src : in out Cell_T; Dest : out Cell_T) is
      Value : Character;
      Blnk, Dm, Rv, Under, Prot : Boolean;
   begin
      Src.Get (Value, Blnk, Dm, Rv, Under, Prot);
      Dest.Set (Value, Blnk, Dm, Rv, Under, Prot);
   end Copy;

end Cell;