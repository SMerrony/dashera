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

with Gdk.Event;               use Gdk.Event;

with Glib;                    use Glib;

with Gtk.About_Dialog;        use Gtk.About_Dialog;
with Gtk.Action;
with Gtk.Adjustment;
with Gtk.Button;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Drawing_Area;
with Gtk.GEntry;
with Gtk.Enums;
with Gtk.Frame;
with Gtk.Label;
with Gtk.Main;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Bar;            use Gtk.Menu_Bar;
with Gtk.Menu_Button;         -- use Gtk.Menu_Button;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Radio_Menu_Item;     -- use Gtk.Radio_Menu_Item;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
-- with Gtk.Scrollbar;
-- with Gtk.Table;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

with Gtkada.Dialogs;

with BDF_Font;
with Crt;
with Display;
with Keyboard;

package body GUI is  

   procedure Window_Close_CB (Window : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Window);
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Calling Main_Quit at level: " & Gtk.Main.Main_Level'Image);
      Gtk.Main.Main_Quit;
      -- Term.Connection := Terminal.Quitting;
   end Window_Close_CB;

   procedure Quit_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Gtk.Main.Main_Quit;
      -- Term.Connection := Terminal.Quitting;
   end Quit_CB;

   procedure About_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Dialog : Gtk_About_Dialog;
      Dummy_Response : Gtk_Response_Type;
   begin
      Gtk_New (Dialog);
      Dialog.Set_Destroy_With_Parent (True);
      Dialog.Set_Modal (True);
      -- TODO: Dialog.Set_Logo
      Dialog.Set_Authors ((1 => new String'(App_Author)));
      Dialog.Set_Copyright (App_Copyright);
      Dialog.Set_Comments (App_Comment);
      Dialog.Set_Program_Name (App_Title);
      Dialog.Set_Version (App_SemVer);
      Dialog.Set_Website (App_Website);
      Dummy_Response := Dialog.Run;
      Dialog.Destroy;
   end About_CB;

   procedure Self_Test_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Term.Self_Test;
   end Self_Test_CB;

   procedure Telnet_Connect_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Dialog : Gtk_Dialog;
      Vbox   : Gtk.Box.Gtk_Vbox;
      Host_Label, Port_Label : Gtk.Label.Gtk_Label;
      Host_Entry, Port_Entry : Gtk.GEntry.Gtk_Entry;
      Cancel_Unused, Connect_Unused : Gtk.Widget.Gtk_Widget;
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      Gtk_New (Dialog);
      Dialog.Set_Destroy_With_Parent (True);
      Dialog.Set_Modal (True);
      -- TODO: Dialog.Set_Logo
      Dialog.Set_Title (App_Title & " - Telnet Host");
      Vbox := Dialog.Get_Content_Area;
      Gtk.Label.Gtk_New (Host_Label, "Host:");
      Vbox.Pack_Start (Child => Host_Label, Expand => True, Fill => True, Padding => 5);
      Gtk.GEntry.Gtk_New (The_Entry => Host_Entry);
      Vbox.Pack_Start (Child => Host_Entry, Expand => True, Fill => True, Padding => 5);
      Gtk.Label.Gtk_New (Port_Label, "Port:");
      Vbox.Pack_Start (Child => Port_Label, Expand => True, Fill => True, Padding => 5);
      Gtk.GEntry.Gtk_New (The_Entry => Port_Entry);
      Vbox.Pack_Start (Child => Port_Entry, Expand => True, Fill => True, Padding => 5);
      Cancel_Unused := Dialog.Add_Button ("Cancel", Gtk_Response_Cancel);
      Connect_Unused := Dialog.Add_Button ("Connect", Gtk_Response_Accept);
      Dialog.Show_All;
      if Dialog.Run = Gtk_Response_Accept then     
         if Host_Entry.Get_Text_Length = 0 or Port_Entry.Get_Text_Length = 0 then
            Unused_Buttons := Gtkada.Dialogs.Message_Dialog (Msg => "You must enter both Host and Port", 
                                                             Title => "DasherA - Error");
         else
            declare
               Host_Str : constant Glib.UTF8_String := Host_Entry.Get_Text;
               Port_Num : Integer;
            begin
               Port_Num := Integer'Value (Port_Entry.Get_Text);
               Ada.Text_IO.Put_Line ("DEBUG: TODO - Call Telnet-connect...");
               Telnet_Sess := Telnet.New_Connection (String(Host_Str), Port_Num, Term);
               -- TODO handle exceptions
               Keyboard.Set_Destination (Terminal.Network);
               Term.Connection := Terminal.Network;
               -- Terminal.Processor.Stop;
            end;
         end if;
      end if;
      Dialog.Destroy;
   end Telnet_Connect_CB;

   function Handle_Key_Release_Event_CB (Self : access Gtk.Widget.Gtk_Widget_Record'Class; Event : Gdk.Event.Gdk_Event_Key) 
      return Boolean  is
      pragma Unreferenced (Self);
   begin
      Keyboard.Handle_Key_Release (Event.Keyval);
      return True;
   end Handle_Key_Release_Event_CB;

   function Handle_Key_Press_Event_CB (Self : access Gtk.Widget.Gtk_Widget_Record'Class; Event : Gdk.Event.Gdk_Event_Key)
      return Boolean  is
      pragma Unreferenced (Self);
   begin
      Keyboard.Handle_Key_Press (Event.Keyval);
      return True;
   end Handle_Key_Press_Event_CB;

   function Create_Menu_Bar return Gtk.Menu_Bar.Gtk_Menu_Bar is
      Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Sep_Item : Gtk.Separator_Menu_Item.Gtk_Separator_Menu_Item;
      File_Menu, Edit_Menu, Emulation_Menu, Serial_Menu, Network_Menu,
      Help_Menu : Gtk.Menu.Gtk_Menu;
      Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Logging_Item, Expect_Item, Send_File_Item, Xmodem_Rcv_Item,
      Self_Test_Item,
      Xmodem_Send_Item, Xmodem_Send1k_Item, Quit_Item,
      Net_Connect_Item, Net_Disconnect_Item,
      Paste_Item,
      About_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Starting to Create_Menu_Bar");
      Gtk_New (Menu_Bar);

      -- File
      
      Gtk_New (Menu_Item, "File");
      Menu_Bar.Append(Menu_Item);
      Gtk_New (File_Menu);
      Menu_Item.Set_Submenu (File_Menu);

      Gtk_New (Logging_Item, "Logging");
      File_Menu.Append (Logging_Item);
      Gtk_New (Sep_Item);
      File_Menu.Append (Sep_Item);

      Gtk_New (Expect_Item, "Run mini-Expect Script");
      File_Menu.Append (Expect_Item);
      Gtk_New (Sep_Item);
      File_Menu.Append (Sep_Item);

      Gtk_New (Send_File_Item, "Send (Text) File");
      File_Menu.Append (Send_File_Item);
      Gtk_New (Sep_Item);
      File_Menu.Append (Sep_Item);

      Gtk_New (Xmodem_Rcv_Item, "XMODEM-CRC - Receive File");
      File_Menu.Append (Xmodem_Rcv_Item);
      Gtk_New (Xmodem_Send_Item, "XMODEM-CRC - Send File");
      File_Menu.Append (Xmodem_Send_Item);
      Gtk_New (Xmodem_Send1k_Item, "XMODEM-CRC - Send File (1k packets");
      File_Menu.Append (Xmodem_Send1k_Item);
      Gtk_New (Sep_Item);
      File_Menu.Append (Sep_Item);

      Gtk_New (Quit_Item, "Quit");
      File_Menu.Append (Quit_Item);
      Quit_Item.On_Activate (Quit_CB'Access);

      -- Edit

      Gtk_New (Menu_Item, "Edit");
      Menu_Bar.Append(Menu_Item);
      Gtk_New (Edit_Menu);
      Menu_Item.Set_Submenu (Edit_Menu);
      Gtk_New (Paste_Item, "Paste");
      Edit_Menu.Append (Paste_Item);

      -- Emulation

      Gtk_New (Menu_Item, "Emulation");
      Menu_Bar.Append(Menu_Item);
      Gtk_New (Emulation_Menu);
      Menu_Item.Set_Submenu (Emulation_Menu);

      Gtk_New (Sep_Item);
      Emulation_Menu.Append (Sep_Item);

      Gtk_New (Self_Test_Item, "Self-Test");
      Emulation_Menu.Append (Self_Test_Item);
      Self_Test_Item.On_Activate (Self_Test_CB'Access);

      -- Serial

      Gtk_New (Menu_Item, "Serial");
      Menu_Bar.Append(Menu_Item);
      Gtk_New (Serial_Menu);
      Menu_Item.Set_Submenu (Serial_Menu);

      -- Network

      Gtk_New (Menu_Item, "Network");
      Menu_Bar.Append(Menu_Item);
      Gtk_New (Network_Menu);
      Menu_Item.Set_Submenu (Network_Menu);
      Gtk_New (Net_Connect_Item, "Connect");
      Network_Menu.Append (Net_Connect_Item);
      Net_Connect_Item.On_Activate (Telnet_Connect_CB'Access);

      Gtk_New (Net_Disconnect_Item, "Disconnect");
      Network_Menu.Append (Net_Disconnect_Item);

      -- Help

      Gtk_New (Menu_Item, "Help");
      Menu_Bar.Append(Menu_Item);    
      Gtk_New (Help_Menu);
      Menu_Item.Set_Submenu (Help_Menu);

      Gtk_New (About_Item, "About");
      Help_Menu.Append (About_Item);
      About_Item.On_Activate (About_CB'Access);


      return Menu_Bar;
   end Create_Menu_Bar;

   function Create_Status_Box return Gtk.Box.Gtk_Hbox is
      Status_Box : Gtk.Box.Gtk_Hbox;
      Online_Frame, Host_Frame, Logging_Frame, Emul_Frame : Gtk.Frame.Gtk_Frame;
      Online_Label, Host_Label, Logging_Label, Emul_Label : Gtk.Label.Gtk_Label;
   begin
      Gtk.Box.Gtk_New_Hbox (Status_Box, True, 2);
      Gtk.Frame.Gtk_New (Online_Frame);
      Gtk.Label.Gtk_New (Online_Label, "Offline");
      Online_Frame.Add (Online_Label);
      Status_Box.Pack_Start (Online_Frame);

      Gtk.Frame.Gtk_New (Host_Frame);
      Gtk.Label.Gtk_New (Host_Label, "Not Connected");
      Host_Frame.Add (Host_Label);
      Status_Box.Pack_Start (Host_Frame);

      Gtk.Frame.Gtk_New (Logging_Frame);
      Gtk.Label.Gtk_New (Logging_Label, "Not Logging");
      Logging_Frame.Add (Logging_Label);
      Status_Box.Pack_Start (Logging_Frame);

      Gtk.Frame.Gtk_New (Emul_Frame);
      Gtk.Label.Gtk_New (Emul_Label, "D210");
      Emul_Frame.Add (Emul_Label);
      Status_Box.Pack_Start (Emul_Frame);

      return Status_Box;
   end Create_Status_Box;

   function Create_Window return Gtk.Window.Gtk_Window is
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Starting to Create_Window");

      -- Gtk.Window.Initialize (Main_Window);
      Gtk.Window.Gtk_New (Main_Window);
      Ada.Text_IO.Put_Line ("DEBUG: New Window Created");
      Main_Window.Set_Title (App_Title);
      Main_Window.On_Destroy (Window_Close_CB'Access);

      -- Everything is in a Vbox...
      Gtk.Box.Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 1);

      -- Menu
      VBox.Pack_Start (Create_Menu_Bar);

      -- Virtual Function Keys and Template

      -- CRT area
      Display.Init;
      Term := Terminal.Create (Terminal.D210);
      Crt.Init (Zoom => BDF_Font.Normal);
      Crt.Tube.DA.On_Configure_Event (Crt.Configure_Event_CB'Access);
      Crt.Tube.DA.On_Draw (Crt.Draw_CB'Access);
      VBox.Pack_Start (Crt.Tube.DA);

      -- Status Bar
      VBox.Pack_End (Create_Status_Box);

      Main_Window.Add (Vbox);
      Main_Window.Set_Position (Gtk.Enums.Win_Pos_Center);

      -- Keyboard.Key_Handler.Start (Term, Terminal.Disconnected);
      Terminal.Processor.Start (Term);
      Keyboard.Init (Term);
      Keyboard.Set_Destination (Terminal.Local);
      Main_Window.On_Key_Press_Event (Handle_Key_Press_Event_CB'Unrestricted_Access);
      Main_Window.On_Key_Release_Event (Handle_Key_Release_Event_CB'Unrestricted_Access);

      Ada.Text_IO.Put_Line ("DEBUG: Main Window Built");

      return Main_Window;

   end Create_Window;

end GUI;
