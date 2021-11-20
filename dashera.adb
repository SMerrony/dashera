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

with Glib; use Glib;
with Glib.Error; use Glib.Error;

-- with Gtk.Builder;
-- with Gtkada.Builder;
-- with Gtk.Application;
with Gdk.Threads;
with Gtk.Main;
with Gtk.Window;  use Gtk.Window;

-- with Local_Listener;
with GUI;


procedure Dashera is

   Semantic_Version : constant String := "v0.11.0x";  -- TODO Update Version each release!

   -- Glade_Filename : constant String := "resources/dashera.glade";  

   -- GUI stuff
   Main_Window : Gtk_Window;

   task type Local_Listener_Type is
      entry Start;
      entry Echo (C : in Character);
      entry Stop;
   end Local_Listener_Type;

   Local_Listener : Local_Listener_Type;

   Listening_Locally : Boolean := True;

   -- program args etc.
   Arg_Ix : Natural := 1;
   Host_Arg : Unbounded_String;
   Trace_Script : Boolean := false;
   Trace_Xmodem : Boolean := false;

   -- Builder : Gtkada.Builder.Gtkada_Builder;
   -- Error : aliased GError;

   procedure Print_Help is
   begin
      Ada.Text_IO.Put_Line ("Usage of dashera:");
      Ada.Text_IO.Put_Line ("  -host <host>  Host to connect with via Telnet");
      Ada.Text_IO.Put_Line ("  -tracescript  Print trace of Mini-Expect script on STDOUT");
      Ada.Text_IO.Put_Line ("  -tracexmodem  Show details of XMODEM file transfers on STDOUT");
      Ada.Text_IO.Put_Line ("  -version          show the version number of dashera and exit");
   end Print_Help;



   task body Local_Listener_Type is
      One_Char : Character;
   begin
      accept Start do
         Ada.Text_IO.Put_Line ("INFO: Local_Listener starting");
      end Start;

      loop
         select
            accept Echo (C : in Character) do
               One_Char := 'x';
            end Echo;
            -- TODO Debugging - should foward the char on
            Ada.Text_IO.Put_Line ("Echo " & One_Char);
         or
            accept Stop do
               return;
            end Stop;
         or 
            terminate;
         end select;
      end loop;

   end Local_Listener_Type;


   -- App  : Gtk.Application.Gtk_Application;

begin

   while Arg_Ix <= Argument_Count loop
      if Argument (Arg_Ix) = "-version" then
         Ada.Text_IO.Put_Line ("dashera version " & Semantic_Version);
         return;
      elsif Argument (Arg_Ix) = "-host" then
         Host_Arg := To_Unbounded_String (Argument (Arg_Ix + 1));
         Arg_Ix := Arg_Ix + 1;
      elsif Argument (Arg_Ix) = "-tracescript" then
         Trace_Script := true;
      elsif Argument (Arg_Ix) = "-tracexmodem" then
         Trace_Xmodem := true;   
      -- else
      --    Print_Help;
      --    return;
      end if;
      Arg_Ix := Arg_Ix + 1;
   end loop;

   -- App := Gtk.Application.Gtk_Application_New ("Fred", Glib.Application.G_Application_Flags_None);
   -- Gdk.Threads.G_Init;
   -- Gdk.Threads.Init;
   Gtk.Main.Init;

   -- --  Load glade file
   -- Gtk.Builder.Initialize_From_File (Builder, Glade_Filename);
   -- -- Gtk_New (Builder);
   -- -- if Add_From_File (Builder, Glade_Filename, Error'Access) = 0 then
   -- --    Ada.Text_IO.Put_Line ("ERROR: " & Get_Message (Error));
   -- --    Error_Free (Error);
   -- --    return;
   -- -- end if;
   -- Ada.Text_IO.Put_Line ( "DEBUG: Finished loading GLADE resources" );

   -- GUI.Init_Gtk (Builder);

   -- Local_Listener.Start;

   Ada.Text_IO.Put_Line ( "DEBUG: Preparing to enter Main GTK event loop...");
   -- Gdk.Threads.Enter;
   Main_Window := Gui.Create_Window;
   Main_Window.Show_All;
   Gtk.Main.Main;
   -- Gdk.Threads.Leave;
  
end Dashera;