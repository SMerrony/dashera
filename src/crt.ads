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

with Cairo;
with Gdk.Event;
with Glib;        use Glib;
with Glib.Main;   use Glib.Main;
with Gtk.Drawing_Area;
with Gtk.Widget;

with BDF_Font;

package Crt is

   package Blink_Timeout is new Glib.Main.Generic_Sources (Gtk.Drawing_Area.Gtk_Drawing_Area);
   package Redraw_Timeout is new Glib.Main.Generic_Sources (Gtk.Drawing_Area.Gtk_Drawing_Area);

   Font_Filename  : constant String := "D410-b-12.bdf";
   Blink_Period_MS : constant Guint := 500;

   type Crt_T is record
      DA         : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Zoom       : BDF_Font.Zoom_T;
      Saved_Font_Colour : BDF_Font.Font_Colour_T;
      Blink_State : Boolean := False;
      --  Timeout_ID : Glib.Main.G_Source_ID := 0;
   end record;

   Tube : Crt_T;
   surface : Cairo.Cairo_Surface;
   Blink_TO, Redraw_TO : Glib.Main.G_Source_Id;

   procedure Init (Zoom : BDF_Font.Zoom_T; Font_Colour : BDF_Font.Font_Colour_T);

   function Configure_Event_CB
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return  Boolean;
   function Draw_CB
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr   : Cairo.Cairo_Context)
      return Boolean;

   Unloaded_Character : exception;

end Crt;