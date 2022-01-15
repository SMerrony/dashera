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

with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;
-- with Gtk.Builder;
-- with Gtkada.Builder;
-- with Gtk.Application;
with Gdk.Threads;
with Gtk.Main;
with Gtk.Window;  use Gtk.Window;

with GUI;

procedure Dashera is

   -- Glade_Filename : constant String := "resources/dashera.glade";  

   -- GUI stuff
   Main_Window : Gtk_Window;

   -- program args etc.
   Arg_Ix : Natural := 1;
   Host_Arg : Unbounded_String := Null_Unbounded_String;
   Trace_Script : Boolean := false;
   Trace_Xmodem : Boolean := false;

   -- Builder : Gtkada.Builder.Gtkada_Builder;
   -- Error : aliased GError;

   procedure Print_Help is
   begin
      Ada.Text_IO.Put_Line ("Usage of dashera:");
      Ada.Text_IO.Put_Line ("  -host <host:port> Host to connect with via Telnet");
      Ada.Text_IO.Put_Line ("  -tracescript      Print trace of Mini-Expect script on STDOUT");
      Ada.Text_IO.Put_Line ("  -tracexmodem      Show details of XMODEM file transfers on STDOUT");
      Ada.Text_IO.Put_Line ("  -version          show the version number of dashera and exit");
   end Print_Help;

begin

   while Arg_Ix <= Argument_Count loop
      if Argument (Arg_Ix) = "-version" then
         Ada.Text_IO.Put_Line ("dashera version " & GUI.App_SemVer);
         return;
      elsif Argument (Arg_Ix) = "-host" then
         Host_Arg := To_Unbounded_String (Argument (Arg_Ix + 1));
         Arg_Ix := Arg_Ix + 1;
      elsif Argument (Arg_Ix) = "-tracescript" then
         Trace_Script := true;
      elsif Argument (Arg_Ix) = "-tracexmodem" then
         Trace_Xmodem := true;   
      elsif Argument (Arg_Ix) = "-h" or Argument (Arg_Ix) = "-help" then
         Print_Help;
         GNAT.OS_Lib.OS_Exit (0);
      end if;
      Arg_Ix := Arg_Ix + 1;
   end loop;

   Gdk.Threads.G_Init;
   Gdk.Threads.Init;
   Gtk.Main.Init;
   Ada.Text_IO.Put_Line ( "DEBUG: Preparing to enter Main GTK event loop...");
   Gdk.Threads.Enter;
   Main_Window := Gui.Create_Window (Host_Arg);
   Main_Window.Show_All;
   Gtk.Main.Main;
   Gdk.Threads.Leave;
  
end Dashera;