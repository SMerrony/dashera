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

with Glib.Main;

with Gtk.Box;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Widget;
with Gtk.Window; 

-- with Dasher_Codes; use Dasher_Codes;
-- with Queues;
with Telnet;
with Terminal;

package GUI is
   package Handlers is new Gtk.Handlers.Callback (Widget_Type => Gtk.Widget.Gtk_Widget_Record);
   package SB_Timeout_P is new Glib.Main.Generic_Sources (Gtk.Box.Gtk_Hbox);

   App_SemVer       : constant String := "v0.11.0x";  -- TODO Update Version each release!
   App_Title        : constant String := "DasherA";
   App_Comment      : constant String := "A Data General DASHER terminal emulator";
   App_Author       : constant String := "Stephen Merrony";
   App_Copyright    : constant String := "Copyright ©2021 S.Merrony";
   App_Icon         : constant String := "DGlogoOrange.ico";
   App_Website      : constant String := "https://github.com/SMerrony/DasherA"; -- FIXME

   History_Lines    :  constant Natural := 1000;
      
   -- procedure Init_Gtk (Builder : Gtkada_Builder);

   -- Disp_Acc : Display.Display_Acc_T;

   Main_Window : Gtk.Window.Gtk_Window;
   Vbox : Gtk.Box.Gtk_Vbox;

   Telnet_Sess : Telnet.Session_Acc_T;
   Term        : Terminal.Terminal_Acc_T;
   Online_Label, Host_Label, Logging_Label, Emul_Label : Gtk.Label.Gtk_Label;
   SB_Timeout  : Glib.Main.G_Source_ID := 0;

   function Create_Window return Gtk.Window.Gtk_Window;

end GUI;