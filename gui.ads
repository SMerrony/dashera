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

-- with Gtkada.Builder; use Gtkada.Builder;

with Gtk.Handlers;
with Gtk.Widget;
with Gtk.Window; 

package GUI is
   package Handlers is new Gtk.Handlers.Callback (Widget_Type => Gtk.Widget.Gtk_Widget_Record);
   
   App_Title        : constant String := "DasherA";
   App_Comment      : constant String := "A Data General DASHER terminal emulator";
   App_Copyright    : constant String := "Copyright ©2021 S.Merrony";
   App_Icon         : constant String := "DGlogoOrange.ico";
   App_Website      : constant String := "https://github.com/SMerrony/DasherG"; -- FIXME

   History_Lines    :  constant Natural := 1000;
      
   -- Crt_Drawing_Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
   
   -- procedure Exit_Main (Object : access Gtkada_Builder_Record'Class);

   -- procedure Init_Gtk (Builder : Gtkada_Builder);

   function Create_Window return Gtk.Window.Gtk_Window;

end GUI;