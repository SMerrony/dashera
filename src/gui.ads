--  Copyright (C) 2021,2022 Steve Merrony
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

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Gdk.Pixbuf;

with Glib.Main;

with Gtk.Adjustment;
with Gtk.Box;
with Gtk.Grid;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Revealer;
with Gtk.Widget;
with Gtk.Window;              use Gtk.Window;

with BDF_Font;
with Telnet;
with Terminal;

package GUI is
   package Handlers is new Gtk.Handlers.Callback (Widget_Type => Gtk.Widget.Gtk_Widget_Record);
   package SB_Timeout_P is new Glib.Main.Generic_Sources (Gtk.Box.Gtk_Box);

   App_SemVer       : constant String := "0.14.0";  --  TODO Update Version each release!
   App_Title        : constant String := "DasherA";
   App_Comment      : constant String := "A Data General DASHER terminal emulator";
   App_Author       : constant String := "Stephen Merrony";
   App_Copyright    : constant String := "Copyright ©2022 S.Merrony";
   App_Icon         : constant String := "DGlogoOrange.ico";
   App_Website      : constant String := "https://github.com/SMerrony/dashera";

   History_Lines    : constant Natural := 2000;

   Main_Window : Gtk_Window;
   Main_Grid   : Gtk.Grid.Gtk_Grid;
   Icon_PB     : Gdk.Pixbuf.Gdk_Pixbuf;
   Adj         : Gtk.Adjustment.Gtk_Adjustment;

   --  Function keys/labels...
   L_FKeys_Label, R_FKeys_Label : Gtk.Label.Gtk_Label;
   Template_Revealer : Gtk.Revealer.Gtk_Revealer;
   Template_Labels   : array (1 .. 4, 1 .. 17) of Gtk.Label.Gtk_Label;

   Telnet_Sess : Telnet.Session_Acc_T;
   Term        : Terminal.Terminal_Acc_T;
   Saved_Host,
   Saved_Port  : Unbounded_String := Null_Unbounded_String;

   Saved_Font_Colour  : BDF_Font.Font_Colour_T;

   --  Menu items for which we need access...
   Load_Template_Item, Hide_Template_Item,
   Net_Connect_Item, Net_Disconnect_Item,
   Xmodem_Rx_Item, Xmodem_Send_Item, Xmodem_Send1k_Item,
   Serial_Connect_Item, Serial_Disconnect_Item : Gtk_Menu_Item;

   --  Status Bar items...
   Online_Label, Host_Label, Logging_Label,
   Emul_Label, Hold_Label : Gtk.Label.Gtk_Label;
   SB_Timeout  : Glib.Main.G_Source_Id := 0;

   --  Flags
   Trace_Script_Opt, Trace_Xmodem_Opt : Boolean;

   function Create_Window (Host_Arg     : Unbounded_String;
                           Font_Colour  : BDF_Font.Font_Colour_T;
                           Trace_Xmodem : Boolean) return Gtk_Window;

end GUI;