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

with Glib.Main;
with Gtk.Action;
with Gtk.Adjustment;
with Gtk.Box;
with Gtk.Button;
-- with Gtk.Container;
with Gtk.Drawing_Area;
with Gtk.Enums;
with Gtk.Frame;
with Gtk.Handlers; 
with Gtk.Label;
with Gtk.Main;
with Gtk.Menu;              use Gtk.Menu;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Menu_Bar;          use Gtk.Menu_Bar;
with Gtk.Menu_Button;       use Gtk.Menu_Button;
with Gtk.Menu_Item;         use Gtk.Menu_Item;
with Gtk.Scrollbar;
with Gtk.Table;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

with BDF_Font;
with Crt;
with Terminal;

package body GUI is
   -- package Handlers is new Gtk.Handlers.Callback (Gtk_Widget_Record);
   PACKAGE P_Simple_Callback is new Gtk.Handlers.Callback(Gtk_widget_Record);
   use P_Simple_Callback;
   package Return_Handlers is new Gtk.Handlers.Return_Callback
     (Gtk_Widget_Record, Boolean);

   -- procedure Exit_Main (Object : access Gtkada_Builder_Record'Class) is
   --    pragma Unreferenced (Object);
   -- begin
   --    Ada.Text_IO.Put_Line ("DEBUG: Quitting main event loop");
   --    Destroy (Main_Window);
   --    Gtk.Main.Main_Quit;
   -- end Exit_Main;

   -- procedure Init_Gtk (Builder : Gtkada_Builder) is
   -- begin
   --    Ada.Text_IO.Put_Line ("DEBUG: Entering Gui.Init_Gtk ()");
   --    Main_Window := Gtk.Window.Gtk_Window (Get_Object (Builder, "Main_Window"));
   --    Main_Menu_Bar := Gtk.Menu_Bar.Gtk_Menu_Bar (Get_Object (Builder, "Main_Menu_Bar"));
   --    Crt_Drawing_Area := Gtk.Drawing_Area.Gtk_Drawing_Area (Get_Object (Builder, "Crt_Drawing_Area"));
   --    Crt.Init_Crt (Crt_Drawing_Area);

   --    Register_Handler (Builder, "Main_Window_destroy_cb", Exit_Main'Access);
   --    Register_Handler (Builder, "Quit_Menu_Item_activate_cb", Exit_Main'Access);

   --    Do_Connect (Builder);

   --    Main_Window.Show_All;
   --    Ada.Text_IO.Put_Line ("DEBUG: Leaving Gui.Init_Gtk ()");
   -- end Init_Gtk;

   procedure Exit_Gui (Window : access Gtk_Widget_Record'Class) is
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Calling Main_Quit");
      -- Glib.Main.Idle_Add (Gtk.Main.Main_Quit'Access);
      Gtk.Main.Main_Quit;
   end Exit_Gui;

   function Create_Menu_Bar return Gtk.Menu_Bar.Gtk_Menu_Bar is
      Menu_Bar  : Gtk.Menu_Bar.Gtk_Menu_Bar;
      File_Menu  : Gtk.Menu.Gtk_Menu;
      Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Logging_Item, Quit_Item  : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Starting to Create_Menu_Bar");
      Gtk_New (Menu_Bar);
      Gtk_New (Menu_Item, "File");
      Menu_Bar.Append(Menu_Item);
      Gtk_New (File_Menu);
      Menu_Item.Set_Submenu (File_Menu);
      Gtk_New (Logging_Item, "Logging");
      File_Menu.Append (Logging_Item);
      Gtk_New (Quit_Item, "Quit");
      File_Menu.Append (Quit_Item);
      -- Connect (Quit_Item, "activate", Exit_Gui'Access);

      Gtk_New (Menu_Item, "Edit");
      Menu_Bar.Append(Menu_Item);

      Gtk_New (Menu_Item, "Emulation");
      Menu_Bar.Append(Menu_Item);

      Gtk_New (Menu_Item, "Serial");
      Menu_Bar.Append(Menu_Item);

      Gtk_New (Menu_Item, "Network");
      Menu_Bar.Append(Menu_Item);

      Gtk_New (Menu_Item, "Help");
      Menu_Bar.Append(Menu_Item);      

      return Menu_Bar;
   end Create_Menu_Bar;

   procedure Create_Window is
      Vbox : Gtk.Box.Gtk_Vbox;
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Starting to Create_Window");
      Gtk.Window.Gtk_New (Main_Window);
      Ada.Text_IO.Put_Line ("DEBUG: New Window Created");
      Main_Window.Set_Title (App_Title);
      Main_Window.On_Destroy (Exit_Gui'Access);
      
      Gtk.Box.Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 1);
      VBox.Pack_Start (Create_Menu_Bar);
      Main_Window.Add (Vbox);
      Ada.Text_IO.Put_Line ("DEBUG: Calling Show_All");
      Main_Window.Show_All;
   end Create_Window;

end GUI;
