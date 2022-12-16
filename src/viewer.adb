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

with Gtk.Enums;       use Gtk.Enums;
with Pango.Font;      use Pango.Font;

with Display_P;   use Display_P;
with Logging;     use Logging;

package body Viewer is

   procedure Init is
      Success : Boolean;
   begin
      Log (DEBUG, "Creating text Viewer in lieu of Crt");
      Gtk_New (Tags);

      Gtk_New (Mono, "monospace_tag");
      Add (Tags, Mono);
      Set_Property (Mono, Font_Desc_Property, From_String ("Mono"));

      Parse (Colour, "green", Success);
      Gtk_New (Green_FG, "green_tag");
      Add (Tags, Green_FG);
      Set_Property (Green_FG, Foreground_Rgba_Property, Colour);

      Parse (Colour, "black", Success);
      Gtk_New (Black_BG, "black_tag");
      Add (Tags, Black_BG);
      Set_Property (Black_BG, Background_Rgba_Property, Colour);

      Gtk_New (Buffer, Tags);

      Get_End_Iter (Buffer, Iter);
      Insert (Buffer, Iter, "Initialising...");
      Start_Iter := Iter;
      Backward_Chars (Start_Iter, 16, Success);
      Apply_Tag (Buffer => Buffer, Tag => Mono, Start => Start_Iter, The_End => Iter);
      Apply_Tag (Buffer => Buffer, Tag => Green_FG, Start => Start_Iter, The_End => Iter);
      Apply_Tag (Buffer => Buffer, Tag => Black_BG, Start => Start_Iter, The_End => Iter);

      Gtk_New (View, Buffer);
      View.Set_Editable (False);
      View.Set_Wrap_Mode (Wrap_Char);
      --  Clear_Buffer (Buffer);
      Update;

      if Update_TO = 0 then
         Update_TO := Redraw_Timeout.Timeout_Add (Update_Period_MS, Update_Timeout_CB'Access, True);
      end if;

   end Init;

   procedure Clear_Buffer (B : Gtk_Text_Buffer) is
      Success : Boolean;
      Start_I, End_I : Gtk_Text_Iter;
   begin
      B.Get_Start_Iter (Start_I);
      B.Get_End_Iter (End_I);
      B.Delete (Start_I, End_I);
      for Line in 1 .. Display.Get_Visible_Lines loop
         for Col in 1 .. Display.Get_Visible_Cols loop
            Get_End_Iter (B, Iter);
            Insert (B, Iter, " ");
         end loop;
         Insert (B, Iter, "" & ASCII.LF);
         Start_Iter := Iter;
         Backward_Chars (Start_Iter, Gint (Display.Get_Visible_Cols) + 1, Success);
         Apply_Tag (Buffer => B, Tag => Mono, Start => Start_Iter, The_End => Iter);
         Apply_Tag (Buffer => B, Tag => Green_FG, Start => Start_Iter, The_End => Iter);
         Apply_Tag (Buffer => B, Tag => Black_BG, Start => Start_Iter, The_End => Iter);
      end loop;
   end Clear_Buffer;

   function Update_Timeout_CB (Unused_Bool : Boolean) return Boolean is
   begin
      Update;
      return True;
   end Update_Timeout_CB;

   procedure Update is
      Success : Boolean;
      Start_I, End_I : Gtk_Text_Iter;
      Value : Character;
      Blnk, Dm, Rv, Under, Prot : Boolean;
   begin
      if Display.Is_Dirty then
         Buffer.Get_Start_Iter (Start_I);
         Buffer.Get_End_Iter (End_I);
         Buffer.Delete (Start_I, End_I);
         for Line in 0 .. Display.Get_Visible_Lines - 1 loop
            for Col in 0 .. Display.Get_Visible_Cols -1 loop
               Display.Get_Cell (Line, Col, Value, Blnk, Dm, Rv, Under, Prot);
               Get_End_Iter (Buffer, Iter);
               Insert (Buffer, Iter, "" & Value);
            end loop;
            Insert (Buffer, Iter, "" & ASCII.LF);
            Start_Iter := Iter;
            Backward_Chars (Start_Iter, Gint (Display.Get_Visible_Cols) + 1, Success);
            Apply_Tag (Buffer => Buffer, Tag => Mono, Start => Start_Iter, The_End => Iter);
            Apply_Tag (Buffer => Buffer, Tag => Green_FG, Start => Start_Iter, The_End => Iter);
            Apply_Tag (Buffer => Buffer, Tag => Black_BG, Start => Start_Iter, The_End => Iter);
         end loop;
         Display.Clear_Dirty;
      end if;
   end Update;

end Viewer;