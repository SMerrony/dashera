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

with Ada.Text_IO;

with Cairo;               use Cairo;
with Cairo.Image_Surface; use Cairo.Image_Surface;

with Gdk.Threads;
with Gdk.Window;
with Glib.Main;
with Gtk.Drawing_Area;

package body Crt is

   surface : Cairo.Cairo_Surface;
   use type Cairo.Cairo_Surface;

   procedure Create (Disp : in Display.Display_Acc_T; Zoom : in BDF_Font.Zoom_T) is
      -- C : aliased Crt_Acc_T := new Crt_T;
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Creating Crt");
      Gtk.Drawing_Area.Gtk_New (Tube.DA);
      Tube.DA.Set_Size_Request(600, 400);
      Tube.Disp := Disp;
      Tube.Zoom := Zoom;
      -- C.DA.On_Draw (Draw_CB'Access);
      -- C.Timeout_ID := Glib.Main.Timeout_Add (1000, Draw2'Access); -- Draw2'Access
      -- C.Timeout_ID := Glib.Main.Timeout_Add (1, Glib.Main.G_Source_Func (C.Draw));

      -- return C;
   end Create;


   procedure Clear_Surface is
      Cr : Cairo.Cairo_Context;
   begin
      Cr := Cairo.Create (surface);
      Cairo.Set_Source_Rgb (Cr, 1.0, 0.0, 1.0);
      Cairo.Paint (Cr);
      Cairo.Destroy (Cr);
   end Clear_Surface;

   function Configure_Event_CB
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return  Boolean
   is
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Entering Configure_Event_CB");
      if surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (surface);
      end if;
      surface :=
         Gdk.Window.Create_Similar_Surface
           (Self.Get_Window,
            Cairo.Cairo_Content_Color,
            Self.Get_Allocated_Width,
            Self.Get_Allocated_Height);
      -- Initialize the surface
      Clear_Surface;

      -- We've handled the configure event, no need for further processing.
      return True;
   end Configure_Event_CB;

   function Draw_CB
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr   : Cairo.Cairo_Context)
      return Boolean
   is
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Entering Draw_CB");
      Cairo.Set_Source_Surface (Cr, surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      return False;
   end Draw_CB;

   -- function On_Redraw (This : access GObject_Record'Class; Cr : Cairo_Context) return Boolean is
   -- begin
   --    if C.Disp.Dirty then 
   --       Cairo := 
   --       C.Disp.Dirty := false;
   --    end if;
   -- end Draw;

end Crt;