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

with Ada.Strings.Fixed;
with Ada.Text_IO;

with Gdk.Event;               -- use Gdk.Event;
with Gdk.Threads;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;

with Glib;                    use Glib;
with Glib.Error;

with Gtk.About_Dialog;        use Gtk.About_Dialog;
with Gtk.Button;
with Gtk.Container;
with Gtk.Css_Provider;        use Gtk.Css_Provider;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Drawing_Area;
with Gtk.GEntry;
with Gtk.Enums;
with Gtk.Frame;
with Gtk.Main;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Bar;            use Gtk.Menu_Bar;
-- with Gtk.Menu_Button;         -- use Gtk.Menu_Button;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Radio_Menu_Item;     -- use Gtk.Radio_Menu_Item;
with Gtk.Scrollbar;           -- use Gtk.Scrollbar;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
-- with Gtk.Scrollbar;
with Gtk.Style_Context;
with Gtk.Style_Provider;
-- with Gtk.Table;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

with Gtkada.Dialogs;

with BDF_Font;
with Crt;
with Display;
with Keyboard;
with Redirector;

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

   procedure D200_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Term.Emulation := Terminal.D200;
   end D200_CB;

   procedure D210_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Term.Emulation := Terminal.D210;
   end D210_CB;

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
               Redirector.Set_Destination (Terminal.Network);
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
      D200_Item, D210_Item, Self_Test_Item,
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
      Gtk_New (D200_Item, "D200");
      Emulation_Menu.Append (D200_Item);
      D200_Item.On_Activate (D200_CB'Access);
      Gtk_New (D210_Item, "D210");
      Emulation_Menu.Append (D210_Item);
      D210_Item.On_Activate (D210_CB'Access);

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

   procedure Handle_Key_Btn_CB (Btn : access Gtk.Button.Gtk_Button_Record'Class) is
      Lab : constant String := Btn.Get_Label;
   begin
      if Lab = "Break" then
         Keyboard.Handle_Key_Release (GDK_Break); -- TODO Serial BREAK handling
      elsif Lab = "Er.Page" then
         Keyboard.Handle_Key_Release (GDK_3270_EraseEOF);
      elsif Lab = "Loc.Print" then
         Keyboard.Handle_Key_Release (GDK_3270_PrintScreen);  -- TODO Local Print
      elsif Lab = "Er.EOL" then
         Keyboard.Handle_Key_Release (GDK_3270_EraseInput);   
      elsif Lab = "CR" then
         Keyboard.Handle_Key_Release (GDK_KP_Enter);
      elsif Lab = "Hold" then
         Term.Holding := not Term.Holding;
      end if; 
   end Handle_Key_Btn_CB;

   function Create_Keys_Box return Gtk.Box.Gtk_Hbox is
      Keys_Box :  Gtk.Box.Gtk_Hbox;
      Break_Btn, Er_Pg_Btn, Loc_Pr_Btn, Er_EOL_Btn, CR_Btn, Hold_Btn : Gtk.Button.Gtk_Button;
   begin
      Gtk.Box.Gtk_New_Hbox (Keys_Box, True, 2);
      Gtk.Button.Gtk_New (Break_Btn, "Break");
      Break_Btn.Set_Tooltip_Text ("Send BREAK signal on Serial Connection");
      Break_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (Break_Btn);

      Gtk.Button.Gtk_New (Er_Pg_Btn, "Er.Page");
      Er_Pg_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (Er_Pg_Btn);

      Gtk.Button.Gtk_New (Loc_Pr_Btn, "Loc.Print");
      Loc_Pr_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (Loc_Pr_Btn);

      Gtk.Button.Gtk_New (Er_EOL_Btn, "Er.EOL");
      Er_EOL_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (Er_EOL_Btn);

      Gtk.Button.Gtk_New (CR_Btn, "CR");
      CR_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (CR_Btn);

      Gtk.Button.Gtk_New (Hold_Btn, "Hold");
      Hold_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (Hold_Btn);

      return Keys_Box;
   end Create_Keys_Box;

   procedure Handle_FKey_Btn_CB (Btn : access Gtk.Button.Gtk_Button_Record'Class) is
      Lab : constant String := Btn.Get_Label;
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Handle_FKey_Btn_CB called for " & Lab);
      if Lab = "F1" then
         Keyboard.Handle_Key_Release (GDK_F1);
      elsif Lab = "F1" then
         Keyboard.Handle_Key_Release (GDK_F1);
      elsif Lab = "F2" then
         Keyboard.Handle_Key_Release (GDK_F2);
      elsif Lab = "F3" then
         Keyboard.Handle_Key_Release (GDK_F3);
      elsif Lab = "F4" then
         Keyboard.Handle_Key_Release (GDK_F4);
      elsif Lab = "F5" then
         Keyboard.Handle_Key_Release (GDK_F5);
      elsif Lab = "F6" then
         Keyboard.Handle_Key_Release (GDK_F6);
      elsif Lab = "F7" then
         Keyboard.Handle_Key_Release (GDK_F7);
      elsif Lab = "F8" then
         Keyboard.Handle_Key_Release (GDK_F8);
      elsif Lab = "F9" then
         Keyboard.Handle_Key_Release (GDK_F9);
      elsif Lab = "F10" then
         Keyboard.Handle_Key_Release (GDK_F10);
      elsif Lab = "F11" then
         Keyboard.Handle_Key_Release (GDK_F11);
      elsif Lab = "F12" then
         Keyboard.Handle_Key_Release (GDK_F12);
      elsif Lab = "F13" then
         Keyboard.Handle_Key_Release (GDK_F13);
      elsif Lab = "F14" then
         Keyboard.Handle_Key_Release (GDK_F14);  
      elsif Lab = "F5" then
         Keyboard.Handle_Key_Release (GDK_F15);                                                                                                                            
      end if;
   end Handle_FKey_Btn_CB;

   function Create_Template_Labels_Revealer return Gtk.Revealer.Gtk_Revealer is 
      Template_Rev : Gtk.Revealer.Gtk_Revealer;
      Template_Grid : Gtk.Grid.Gtk_Grid;
   begin
      Gtk.Revealer.Gtk_New (Template_Rev);
      Gtk.Grid.Gtk_New (Template_Grid);
      for row in 1 .. 4 loop
         for col in 1 .. 17 loop
            Gtk.Label.Gtk_New (Template_Labels(row, col));
            -- Template_Labels(row, col).Set_Markup ("<small><small>small</small></small>"); -- FIXME debugging
            Template_Labels(row, col).Set_Size_Request (Width => 54, Height => 30);
            Template_Grid.Attach (Child => Template_Labels(row, col), Left => Gint(col) - 1, Top => Gint(row) - 1 , Width => 1, Height => 1);
         end loop;
      end loop;
      Template_Labels(1, 6).Set_Markup ("<small><small><b>Ctrl-Shift</b></small></small>");
      Template_Labels(2, 6).Set_Markup ("<small><small><b>Ctrl</b></small></small>");
      Template_Labels(3, 6).Set_Markup ("<small><small><b>Shift</b></small></small>");
      Template_Labels(1, 12).Set_Markup ("<small><small><b>Ctrl-Shift</b></small></small>");
      Template_Labels(2, 12).Set_Markup ("<small><small><b>Ctrl</b></small></small>");
      Template_Labels(3, 12).Set_Markup ("<small><small><b>Shift</b></small></small>");
      Template_Rev.Add (Template_Grid);
      Template_Rev.Set_Reveal_Child (True); -- FIXME DEBUGGING
      return Template_Rev;
   end Create_Template_Labels_Revealer;

   function Create_FKeys_Box return Gtk.Box.Gtk_Box is
      FKeys_Box :  Gtk.Box.Gtk_Box;
      FKeys : array(1 .. 15) of Gtk.Button.Gtk_Button;
      Error : aliased Glib.Error.GError;
      FProvider : constant Gtk.Css_Provider.Gtk_Css_Provider := Gtk.Css_Provider.Gtk_Css_Provider_New;
      Dummy    : Boolean;
 
      package FA is new Gtk.Container.Forall_User_Data (Gtk.Style_Provider.Gtk_Style_Provider);

      procedure Apply_Css (Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                           Provider : Gtk.Style_Provider.Gtk_Style_Provider) is
      begin
         Gtk.Style_Context.Get_Style_Context (Widget).Add_Provider (Provider, Glib.Guint'Last);

         if Widget.all in Gtk.Container.Gtk_Container_Record'Class then
            declare
               Container : constant Gtk.Container.Gtk_Container := Gtk.Container.Gtk_Container (Widget);
            begin
               FA.Forall (Container, Apply_Css'Unrestricted_Access, Provider);
            end;
         end if;

      end Apply_Css;

       CSS : constant String :=
       "* {" & ASCII.LF
     & "  color: green;" & ASCII.LF
     & "  font-family: Monospace;" & ASCII.LF
     -- & "  font-size: small;" & ASCII.LF
     & "  padding: 1px;" & ASCII.LF
     & "}";

   begin

      Gtk.Box.Gtk_New (FKeys_Box, Gtk.Enums.Orientation_Horizontal, 1);  
      FKeys_Box.Set_Homogeneous (True);
      Dummy := FProvider.Load_From_Data (CSS, Error'Access);
      if not Dummy then
         Ada.Text_IO.Put_Line ("ERROR: Loading CSS from data");
      end if;
      for N in FKeys'Range loop
         declare
            Lab : constant String := N'Image;
         begin
            Gtk.Button.Gtk_New(FKeys(N), "F" & Ada.Strings.Fixed.Trim (Lab, Ada.Strings.Both));
            FKeys(N).On_Clicked (Handle_FKey_Btn_CB'Access);
         end;
      end loop; 
      -- we want labels between the groups of 5 f-key buttons
      for F in 1 .. 5 loop
         FKeys_Box.Pack_Start (FKeys(F), False, False);
      end loop;
      Gtk.Label.Gtk_New (L_FKeys_Label, " ");
      FKeys_Box.Pack_Start (L_FKeys_Label);
      for F in 6 .. 10 loop
         FKeys_Box.Pack_Start (FKeys(F));
      end loop;
      Gtk.Label.Gtk_New (R_FKeys_Label, " ");
      FKeys_Box.Pack_Start (R_FKeys_Label);
      for F in 11 .. 15 loop
         FKeys_Box.Pack_Start (FKeys(F));
      end loop;
      Apply_Css (FKeys_Box, +FProvider);
      return FKeys_Box;
   end Create_FKeys_Box;  

   function Update_Status_Box_CB (SB : Gtk.Box.Gtk_Box) return Boolean is
   begin
      Gdk.Threads.Enter;
      case Term.Connection is
         when Terminal.Local =>   Online_Label.Set_Text ("Local (Disconnected)");
         when Terminal.Async =>   Online_Label.Set_Text ("Online (Serial)");
         when Terminal.Network => Online_Label.Set_Text ("Online (Telnet)");
      end case;
      case Term.Emulation is
         when Terminal.D200 => Emul_Label.Set_Text ("D200");
         when Terminal.D210 => Emul_Label.Set_Text ("D210");
      end case;
      if Term.Holding then
         Hold_Label.Set_Text ("HOLD");
      else
         Hold_Label.Set_Text ("");
      end if;
      Gdk.Threads.Leave;
      SB.Queue_Draw;
      return True;
   end Update_Status_Box_CB;

   procedure Adj_Changed_CB (Self : access Gtk.Adjustment.Gtk_Adjustment_Record'Class) is
      Posn : constant Natural := Natural(Self.Get_Value);
   begin
      -- Ada.Text_IO.Put_Line ("DEBUG: Adj changed to " & Posn'Image);
      if Posn = Display.History_Lines then
         Display.Cancel_Scroll_Back;
      else
         Display.Scroll_Back (Display.History_Lines - Posn);
      end if;
   end Adj_Changed_CB;

   function Create_Scrollbar return Gtk.Scrollbar.Gtk_Vscrollbar is
      SB  : Gtk.Scrollbar.Gtk_Vscrollbar;
   begin
      Gtk.Adjustment.Gtk_New (Adjustment => Adj, 
                              Value => Gdouble(History_Lines), 
                              Lower => 0.0, 
                              Upper => Gdouble(History_Lines), 
                              Step_Increment => 1.0, 
                              Page_Increment => 24.0);
      SB := Gtk.Scrollbar.Gtk_Vscrollbar_New (Adj);
      Adj.On_Value_Changed (Adj_Changed_CB'Access, False);
      return SB;
   end Create_Scrollbar;

   function Create_Status_Box return Gtk.Box.Gtk_Box is
      Status_Box : Gtk.Box.Gtk_Box;
      Online_Frame, Host_Frame, Logging_Frame, Emul_Frame, Hold_Frame : Gtk.Frame.Gtk_Frame;
   begin
      Gtk.Box.Gtk_New (Status_Box, Gtk.Enums.Orientation_Horizontal, 2);

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
      Gtk.Label.Gtk_New (Emul_Label, "D100");
      Emul_Frame.Add (Emul_Label);
      Status_Box.Pack_Start (Child => Emul_Frame, Expand => False);

      Gtk.Frame.Gtk_New (Hold_Frame);
      Gtk.Label.Gtk_New (Hold_Label, "    ");
      Hold_Frame.Add (Hold_Label);
      Status_Box.Pack_Start (Child => Hold_Frame, Expand => False);

      SB_Timeout := SB_Timeout_P.Timeout_Add (1000, Update_Status_Box_CB'Access, Status_Box);

      return Status_Box;
   end Create_Status_Box;

   function Create_Window return Gtk.Window.Gtk_Window is
      H_Grid : Gtk.Grid.Gtk_Grid;
   begin
      Ada.Text_IO.Put_Line ("DEBUG: Starting to Create_Window");

      -- Gtk.Window.Initialize (Main_Window);
      Gtk.Window.Gtk_New (Main_Window);
      Ada.Text_IO.Put_Line ("DEBUG: New Window Created");
      Main_Window.Set_Title (App_Title);
      Main_Window.On_Destroy (Window_Close_CB'Access);

      -- Everything is in a Grid...
      Gtk.Grid.Gtk_New (Main_Grid);
      Main_Grid.Set_Orientation (Gtk.Enums.Orientation_Vertical);
      Main_Grid.Set_Row_Spacing (2);

      -- Menu
      Main_Grid.Add (Create_Menu_Bar);

      -- Virtual Keys, Function Keys and Template
      Main_Grid.Add (Create_Keys_Box);
      -- TODO All the labels
      Template_Revealer := Create_Template_Labels_Revealer;
      Main_Grid.Add (Template_Revealer);
      Main_Grid.Add (Create_FKeys_Box);

      -- CRT area
      Display.Init;
      Term := Terminal.Create (Terminal.D210);
      Crt.Init (Zoom => BDF_Font.Normal);
      Crt.Tube.DA.On_Configure_Event (Crt.Configure_Event_CB'Access);
      Crt.Tube.DA.On_Draw (Crt.Draw_CB'Access);
      -- Main_Grid.Add (Crt.Tube.DA);

      Gtk.Grid.Gtk_New (H_Grid);
      H_Grid.Set_Orientation (Gtk.Enums.Orientation_Horizontal);

      H_Grid.Add (Crt.Tube.DA);
      H_Grid.Add (Create_Scrollbar);
      Main_Grid.Add (H_Grid);
      
      -- Status Bar
      Main_Grid.Add (Create_Status_Box);

      Main_Window.Add (Main_Grid);
      Main_Window.Set_Position (Gtk.Enums.Win_Pos_Center);

      -- Keyboard.Key_Handler.Start (Term, Terminal.Disconnected);
      Terminal.Processor.Start (Term);
      Keyboard.Init (Term);
      Redirector.Set_Destination (Terminal.Local);
      Main_Window.On_Key_Press_Event (Handle_Key_Press_Event_CB'Unrestricted_Access);
      Main_Window.On_Key_Release_Event (Handle_Key_Release_Event_CB'Unrestricted_Access);

      Ada.Text_IO.Put_Line ("DEBUG: Main Window Built");

      return Main_Window;

   end Create_Window;

end GUI;
