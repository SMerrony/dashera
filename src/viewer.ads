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

--  Viewer provides a text display using Gtk text rather than per-pixel graphics.
--  It may be more efficient than Crt on systems with limited resources.

with Gdk.RGBA;           use Gdk.RGBA;
with Glib;               use Glib;
with Glib.Main;          use Glib.Main;
with Gtk.Text_Buffer;    use Gtk.Text_Buffer;
with Gtk.Text_Iter;      use Gtk.Text_Iter;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Text_Tag;       use Gtk.Text_Tag;
with Gtk.Text_View;      use Gtk.Text_View;

package Viewer is

   package Blink_Timeout is new Glib.Main.Generic_Sources (Boolean);

   Blink_Period_MS : constant Guint := 500;

   Buffer : Gtk_Text_Buffer;
   Tags   : Gtk_Text_Tag_Table;
   Tag    : Gtk_Text_Tag;
   Black_BG, Green_FG, Mono : Gtk_Text_Tag;
   Colour : Gdk_RGBA;
   Iter, Start_Iter : Gtk_Text_Iter;
   View        : Gtk_Text_View;
   Blink_TO    : Glib.Main.G_Source_Id;
   Blink_State : Boolean := False;

   procedure Init;
   procedure Update;
   function Blink_Timeout_CB (Unused_Bool : Boolean) return Boolean;
   function Update_CB return Boolean;

private
   procedure Clear_Buffer (B : Gtk_Text_Buffer);

end Viewer;