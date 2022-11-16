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

with Cairo;       use Cairo;

with Gdk.Cairo;
with Gdk.Window;

with BDF_Font;    use BDF_Font;
with Display_P;   use Display_P;
with Logging;     use Logging;

package body Crt is

   use type Cairo.Cairo_Surface;

   function Blink_Timeout_CB (DA : Gtk.Drawing_Area.Gtk_Drawing_Area) return Boolean is
      pragma Unreferenced (DA);
   begin
      Tube.Blink_State := not Tube.Blink_State;
      for Line in 0 .. Display.Get_Visible_Lines - 1 loop
         for Col in 0 .. Display.Get_Visible_Cols - 1 loop
            Display.Cell_Set_Dirty_If_Blinking (Line, Col);
         end loop;
      end loop;
      return True;
   end Blink_Timeout_CB;

   function Redraw_Timeout_CB (DA : Gtk.Drawing_Area.Gtk_Drawing_Area) return Boolean is
   begin
      DA.Queue_Draw;
      return True;
   end Redraw_Timeout_CB;

   procedure Init (Zoom : Zoom_T; Font_Colour : Font_Colour_T) is
   begin
      Log (DEBUG, "Creating Crt");
      Font.Load_Font (Font_Filename, Zoom, Font_Colour);
      Tube.Saved_Font_Colour := Font_Colour;
      Gtk.Drawing_Area.Gtk_New (Tube.DA);
      Tube.DA.Set_Size_Request (Font.Get_Char_Width * Gint (Display.Get_Visible_Cols),
                                Font.Get_Char_Height * Gint (Display.Get_Visible_Lines));
      Tube.Zoom := Zoom;

      --  Blink timer
      if Blink_TO = 0 then
         Blink_TO := Blink_Timeout.Timeout_Add (Blink_Period_MS, Blink_Timeout_CB'Access, Tube.DA);
      end if;

      --  TESTING = Redraw timer...
      if Redraw_TO = 0 then
         Redraw_TO := Redraw_Timeout.Timeout_Add (50, Redraw_Timeout_CB'Access, Tube.DA);
      end if;

   end Init;

   procedure Clear_Surface is
      Cr : Cairo.Cairo_Context;
   begin
      Cr := Cairo.Create (surface);
      Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Cairo.Paint (Cr);
      Cairo.Destroy (Cr);
   end Clear_Surface;

   function Configure_Event_CB
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return  Boolean
   is
      pragma Unreferenced (Event);
   begin
      Log (DEBUG, "Entering Configure_Event_CB");
      if surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (surface);
      end if;
      surface :=
         Gdk.Window.Create_Similar_Surface
           (Self.Get_Window,
            Cairo.Cairo_Content_Color,
            Self.Get_Allocated_Width,
            Self.Get_Allocated_Height);
      --  Initialize the surface
      Clear_Surface;

      --  We've handled the configure event, no need for further processing.
      return True;
   end Configure_Event_CB;

   --  Draw_Crt is called from within a Callback - so it's safe to use PixBufs etc.
   procedure Draw_Crt is
      Cr             : Cairo.Cairo_Context;
      Char_Ix        : Natural;
      Char_X, Char_Y, Char_UL : Gdouble;
      Value : Character;
      Blnk, Dm, Rv, Under, Prot : Boolean;
      All_Dirty      : constant Boolean := Display.Is_Dirty;
      Blink_Enabled  : constant Boolean := Display.Is_Blink_Enabled;
      Blink_State    : constant Boolean := Tube.Blink_State;
      Decoded_Height : constant Gint := Font.Get_Char_Height;
      Decoded_Width  : constant Gint := Font.Get_Char_Width;
      use Glib;
   begin
      Cr := Cairo.Create (surface);

      for Line in 0 .. Display.Get_Visible_Lines - 1 loop

         Char_Y  := Gdouble (Gint (Line) * Decoded_Height);

         for Col in 0 .. Display.Get_Visible_Cols - 1 loop
            Char_X  := Gdouble (Gint (Col) * Decoded_Width);

            if All_Dirty or else Display.Cell_Is_Dirty (Line, Col) then

               Display.Get_Cell (Line, Col, Value, Blnk, Dm, Rv, Under, Prot);

               Char_Ix := Character'Pos (Value);
               --  if not Font.Is_Loaded (Char_IX) then
               --     raise Unloaded_Character with "Line:" & Line'Image & " Col:" & Col'Image & " Index :" & Char_Ix'Image;
               --  end if;

               if Blnk and then Blink_Enabled and then Blink_State then
                  Gdk.Cairo.Set_Source_Pixbuf (Cr => Cr,
                                                Pixbuf => Font.Get_Dim_Pixbuf (32),
                                                Pixbuf_X => Char_X, Pixbuf_Y => Char_Y);
               elsif Dm then
                  Gdk.Cairo.Set_Source_Pixbuf (Cr => Cr,
                                                Pixbuf => Font.Get_Dim_Pixbuf (Char_Ix),
                                                Pixbuf_X => Char_X, Pixbuf_Y => Char_Y);
               elsif Rv then
                  Gdk.Cairo.Set_Source_Pixbuf (Cr => Cr,
                                                Pixbuf => Font.Get_Rev_Pixbuf (Char_Ix),
                                                Pixbuf_X => Char_X, Pixbuf_Y => Char_Y);
               else
                  Gdk.Cairo.Set_Source_Pixbuf (Cr => Cr,
                                                Pixbuf => Font.Get_Pixbuf (Char_Ix),
                                                Pixbuf_X => Char_X, Pixbuf_Y => Char_Y);
               end if;
               Cairo.Paint (Cr);

               --  Underlined?
               if Under then
                  Char_UL := (Gdouble (Gint (Line + 1) * Decoded_Height)) - 1.0;
                  case Tube.Saved_Font_Colour is
                     when Green => Cairo.Set_Source_Rgb (Cr, 0.0, 1.0, 0.0);
                     when Amber => Cairo.Set_Source_Rgb (Cr, 1.0, 0.749, 0.0);
                     when White => Cairo.Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
                  end case;
                  Cairo.Rectangle (Cr, Char_X, Char_UL, Gdouble (Decoded_Width), 1.0);
                  Cairo.Fill (Cr);
               end if;
               Display.Cell_Clear_Dirty (Line, Col);
            end if;
         end loop;
      end loop;

      --  Draw the cursor if it's on-screen
      if Display.Get_Cursor_X < Display.Get_Visible_Cols and then Display.Get_Cursor_Y < Display.Get_Visible_Lines then
         Display.Get_Cell (Display.Get_Cursor_Y, Display.Get_Cursor_X, Value, Blnk, Dm, Rv, Under, Prot);
         Char_Ix := Character'Pos (Value);
         if Char_Ix = 0 then
            Char_Ix := 32;
         end if;
         if Rv then
            Gdk.Cairo.Set_Source_Pixbuf (Cr => Cr,
                                         Pixbuf => Font.Get_Pixbuf (Char_Ix),
                                         Pixbuf_X => Gdouble (Gint (Display.Get_Cursor_X) * Decoded_Width),
                                         Pixbuf_Y => Gdouble (Gint (Display.Get_Cursor_Y) * Decoded_Height));
         else
            Gdk.Cairo.Set_Source_Pixbuf (Cr => Cr,
                                         Pixbuf => Font.Get_Rev_Pixbuf (Char_Ix),
                                         Pixbuf_X => Gdouble (Gint (Display.Get_Cursor_X) * Decoded_Width),
                                         Pixbuf_Y => Gdouble (Gint (Display.Get_Cursor_Y) * Decoded_Height));
         end if;
         Cairo.Paint (Cr);
      end if;

      Cairo.Destroy (Cr);
      Display.Clear_Dirty;
   end Draw_Crt;

   function Draw_CB
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr   : Cairo.Cairo_Context)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Draw_Crt;
      Cairo.Set_Source_Surface (Cr, surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      return False;
   end Draw_CB;

end Crt;