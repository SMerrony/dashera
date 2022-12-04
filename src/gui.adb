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

with Ada.Directories;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Sequential_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.Serial_Communications; use GNAT.Serial_Communications;

with Gdk.Event;               --  use Gdk.Event;
with Gdk.Threads;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;

with Glib;                    use Glib;
with Glib.Error;              use Glib.Error;

with Gtk.About_Dialog;        use Gtk.About_Dialog;
with Gtk.Button;
with Gtk.Clipboard;
with Gtk.Combo_Box_Text;
with Gtk.Container;
with Gtk.Css_Provider;        use Gtk.Css_Provider;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Drawing_Area;
with Gtk.GEntry;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;
with Gtk.Frame;
with Gtk.Main;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Bar;            use Gtk.Menu_Bar;
with Gtk.Message_Dialog;      use Gtk.Message_Dialog;
with Gtk.Radio_Button;
with Gtk.Scrolled_Window;
with Gtk.Separator;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Stock;
with Gtk.Style_Context;
with Gtk.Style_Provider;
with Gtk.Text_Buffer;
with Gtk.Text_View;
with Gtk.Widget; use Gtk.Widget;

with Gtkada.Dialogs;          use Gtkada.Dialogs;
with Gtkada.File_Selection;

with Interfaces;

with Text_IO.Unbounded_IO;

with BDF_Font;       use BDF_Font;
with Crt;
with Dasher_Codes;
with Display_P;      use Display_P;
with Embedded;
with Keyboard;
with Logging;        use Logging;
with Mini_Expect;
with Session_Logger;
with Redirector;     use Redirector;
with Serial;
with Xmodem;

package body GUI is

   package FA is new Gtk.Container.Forall_User_Data (Gtk.Style_Provider.Gtk_Style_Provider);

   procedure Apply_Css (Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                        Provider : Gtk.Style_Provider.Gtk_Style_Provider) is
   --  Apply the given CSS to the widget (which may be a container)
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

   procedure Window_Close_CB (Window : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Window);
   begin
      Log (DEBUG, "Calling Main_Quit at level: " & Gtk.Main.Main_Level'Image);
      Gtk.Main.Main_Quit;
   exception
      when others =>
         null;
   end Window_Close_CB;

   procedure Quit_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Conn : Connection_T;
   begin
      Router.Get_Destination (Conn);
      case Conn is
         when Local =>
            null;
         when Async =>
            Serial.Close;
         when Network =>
            Telnet_Sess.Close_Connection;
      end case;
      Gtk.Main.Main_Quit;
   end Quit_CB;

   procedure About_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Dialog : Gtk_About_Dialog;
      Dummy_Response : Gtk_Response_Type;
   begin
      Gtk_New (Dialog);
      Dialog.Set_Destroy_With_Parent (True);
      Dialog.Set_Modal (True);
      Dialog.Set_Logo (Icon_PB);
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

   procedure Resize_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Dialog  : Gtk_Dialog;
      Dlg_Box, HB1, HB2, HB3 : Gtk.Box.Gtk_Box;
      Lab : Gtk.Label.Gtk_Label;
      L24, L25, L36, L48, L66,
      C80, C81, C132, C135,
      ZL, ZN, ZS, ZT : Gtk.Radio_Button.Gtk_Radio_Button;
      Cancel_Unused, Resize_Unused : Gtk.Widget.Gtk_Widget;
   begin
      Gtk_New (Dialog);
      Dialog.Set_Destroy_With_Parent (True);
      Dialog.Set_Modal (True);
      Dialog.Set_Title (App_Title & " - Resize Terminal");
      Dlg_Box := Dialog.Get_Content_Area;

      Gtk.Box.Gtk_New (HB1, Orientation => Orientation_Horizontal, Spacing => 6);
      Gtk.Label.Gtk_New (Lab, "Lines");
      HB1.Pack_Start (Child => Lab, Expand => False, Fill => False, Padding => 1);
      L24 := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => null, Label => "24");
      HB1.Pack_Start (Child => L24, Expand => False, Fill => False, Padding => 1);
      L25 := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => L24, Label => "25");
      HB1.Pack_Start (Child => L25, Expand => False, Fill => False, Padding => 1);
      L36 := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => L24, Label => "36");
      HB1.Pack_Start (Child => L36, Expand => False, Fill => False, Padding => 1);
      L48 := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => L24, Label => "48");
      HB1.Pack_Start (Child => L48, Expand => False, Fill => False, Padding => 1);
      L66 := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => L24, Label => "66");
      HB1.Pack_Start (Child => L66, Expand => False, Fill => False, Padding => 1);
      Dlg_Box.Pack_Start (Child => HB1, Expand => False, Fill => False, Padding => 1);
      case Display.Get_Visible_Lines is
         when 24 => L24.Set_Active (True);
         when 25 => L25.Set_Active (True);
         when 36 => L36.Set_Active (True);
         when 48 => L48.Set_Active (True);
         when 66 => L66.Set_Active (True);
         when others => null;
      end case;

      Gtk.Box.Gtk_New (HB2, Orientation => Orientation_Horizontal, Spacing => 6);
      Gtk.Label.Gtk_New (Lab, "Columns");
      HB2.Pack_Start (Child => Lab, Expand => False, Fill => False, Padding => 1);
      C80 := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => null, Label => "80");
      HB2.Pack_Start (Child => C80, Expand => False, Fill => False, Padding => 1);
      C81 := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => C80, Label => "81");
      HB2.Pack_Start (Child => C81, Expand => False, Fill => False, Padding => 1);
      C132 := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => C80, Label => "132");
      HB2.Pack_Start (Child => C132, Expand => False, Fill => False, Padding => 1);
      C135 := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => C80, Label => "135");
      HB2.Pack_Start (Child => C135, Expand => False, Fill => False, Padding => 1);
      Dlg_Box.Pack_Start (Child => HB2, Expand => False, Fill => False, Padding => 1);
      case Display.Get_Visible_Cols is
         when 80 => C80.Set_Active (True);
         when 81 => C81.Set_Active (True);
         when 132 => C132.Set_Active (True);
         when 135 => C135.Set_Active (True);
         when others => null;
      end case;

      Gtk.Box.Gtk_New (HB3, Orientation => Orientation_Horizontal, Spacing => 6);
      Gtk.Label.Gtk_New (Lab, "Zoom");
      HB3.Pack_Start (Child => Lab, Expand => False, Fill => False, Padding => 1);
      ZL := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => null, Label => "Large");
      HB3.Pack_Start (Child => ZL, Expand => False, Fill => False, Padding => 1);
      ZN := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => ZL, Label => "Normal");
      HB3.Pack_Start (Child => ZN, Expand => False, Fill => False, Padding => 1);
      ZS := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => ZL, Label => "Small");
      HB3.Pack_Start (Child => ZS, Expand => False, Fill => False, Padding => 1);
      ZT := Gtk.Radio_Button.Gtk_Radio_Button_New_With_Label_From_Widget (Group => ZL, Label => "Tiny");
      HB3.Pack_Start (Child => ZT, Expand => False, Fill => False, Padding => 1);
      Dlg_Box.Pack_Start (Child => HB3, Expand => False, Fill => False, Padding => 1);
      case Crt.Tube.Zoom is
         when Large   => ZL.Set_Active (True);
         when Normal  => ZN.Set_Active (True);
         when Smaller => ZS.Set_Active (True);
         when Tiny    => ZT.Set_Active (True);
      end case;

      Cancel_Unused := Dialog.Add_Button ("Cancel", Gtk_Response_Cancel);
      Resize_Unused := Dialog.Add_Button ("Resize", Gtk_Response_Accept);
      Dialog.Set_Default_Response (Gtk_Response_Accept);
      Dialog.Show_All;
      if Dialog.Run = Gtk_Response_Accept then
         declare
            New_Zoom : Zoom_T;
            New_Cols, New_Lines : Gint;
         begin
            --  first check if zoom has changed
            if    ZL.Get_Active then
               New_Zoom := Large;
            elsif ZN.Get_Active then
               New_Zoom := Normal;
            elsif ZS.Get_Active then
               New_Zoom := Smaller;
            else
               New_Zoom := Tiny;
            end if;
            if New_Zoom /= Crt.Tube.Zoom then
               Font.Load_Font (Crt.Font_Filename, New_Zoom, Saved_Font_Colour);
               Crt.Tube.Zoom := New_Zoom;
            end if;
            --  resize
            if    L24.Get_Active then
               New_Lines := 24;
            elsif L25.Get_Active then
               New_Lines := 25;
            elsif L36.Get_Active then
               New_Lines := 36;
            elsif L48.Get_Active then
               New_Lines := 48;
            else
               New_Lines := 66;
            end if;
            if C80.Get_Active then
               New_Cols := 80;
            elsif C81.Get_Active then
               New_Cols := 81;
            elsif C132.Get_Active then
               New_Cols := 132;
            else
               New_Cols := 135;
            end if;
            Crt.Tube.DA.Set_Size_Request (BDF_Font.Font.Get_Char_Width * New_Cols,
                               BDF_Font.Font.Get_Char_Height * New_Lines);
            Display.Set_Visible_Lines (Positive (New_Lines));
            Display.Set_Visible_Cols  (Positive (New_Cols));
            --  Ask for window resize to smaller than we are - the effect
            --  is to reduce window size to minimum that contains all content.
            Main_Window.Resize (400, 400);
         end;
      end if;
      Dialog.Destroy;
   end Resize_CB;

   procedure Self_Test_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Term.Self_Test;
   end Self_Test_CB;

   function Label_To_Markup (L : Unbounded_String) return Unbounded_String is
      M : Unbounded_String := Null_Unbounded_String;
   begin
      for Ix in 1 .. Length (L) loop
         if Element (L, Ix) = '\' then
            M := M & ASCII.LF;
         else
            M := M & Element (L, Ix);
         end if;
      end loop;
      return M;
   end Label_To_Markup;

   procedure Send_Text_File_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      use Ada.Text_IO;
      Filename : constant String :=
         Gtkada.File_Selection.File_Selection_Dialog (Title => "DasherA Text File",
                                                      Dir_Only => False,
                                                      Must_Exist => True);
      Text_File : File_Type;
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if Filename'Length > 0 then
         Open (File => Text_File, Mode => In_File, Name => Filename);
         while not End_Of_File (Text_File) loop
            Redirector.Router.Send_Data (Get_Line (Text_File) & Dasher_Codes.Dasher_NL);
         end loop;
         Close (Text_File);
      end if;
   exception
      when others =>
         Unused_Buttons := Message_Dialog (Msg => "Could not open text file",
                                           Title => "DasherA - Error");
   end Send_Text_File_CB;

   procedure Load_Template_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Filename : constant String :=
         Gtkada.File_Selection.File_Selection_Dialog (Title => "DasherA Function Key Template",
                                                      Dir_Only => False,
                                                      Must_Exist => True);
      Templ_File : Ada.Text_IO.File_Type;
      Lab : Unbounded_String;
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if Filename'Length > 1 then
         Log (DEBUG, "Chose template file: " & Filename);
         --  clear the labels
         for K in 1 .. 17 loop
            for R in 1 .. 4 loop
               if K /= 6 and then K /= 12 then
                  Template_Labels (R, K).Set_Markup ("");
               end if;
            end loop;
         end loop;
         Ada.Text_IO.Open (File => Templ_File, Mode => Ada.Text_IO.In_File, Name => Filename);
         Lab := Text_IO.Unbounded_IO.Get_Line (Templ_File);
         L_FKeys_Label.Set_Markup ("<small><small>" & To_String (Lab) & "</small></small>");
         R_FKeys_Label.Set_Markup ("<small><small>" & To_String (Lab) & "</small></small>");
         for K in 1 .. 17 loop
            for R in reverse 1 .. 4 loop
               if K /= 6 and then K /= 12 and then not Ada.Text_IO.End_Of_File (Templ_File) then
                  Lab := "<small><small>" & Label_To_Markup (Text_IO.Unbounded_IO.Get_Line (Templ_File)) &
                         "</small></small>";
                  Template_Labels (R, K).Set_Markup (To_String (Lab));
               end if;
            end loop;
         end loop;
         Ada.Text_IO.Close (Templ_File);
         Template_Revealer.Set_Reveal_Child (True);
         Template_Revealer.Set_Visible (True);
         Hide_Template_Item.Set_Sensitive (True);
         Display.Set_Dirty;
      else
         Log (INFO, "No Template file chosen");
      end if;
   exception
      when others =>
         Unused_Buttons := Message_Dialog (Msg => "Could not load template file",
                                           Title => "DasherA - Error");
   end Load_Template_CB;

   procedure Hide_Template_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Template_Revealer.Set_Reveal_Child (False);
      L_FKeys_Label.Set_Markup ("<small><small> </small></small>");
      R_FKeys_Label.Set_Markup ("<small><small> </small></small>");
      Template_Revealer.Set_Visible (False); --  Required to reclaim vertical space when we resize
      Hide_Template_Item.Set_Sensitive (False);
      --  Ask for window resize to smaller than we are - the effect
      --  is to reduce window size to minimum that contains all content.
      Main_Window.Resize (400, 400);
      Display.Set_Dirty;
   end Hide_Template_CB;

   procedure Expect_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Filename : constant String :=
         Gtkada.File_Selection.File_Selection_Dialog (Title => "DasherA mini-Expect Script",
                                                      Dir_Only => False,
                                                      Must_Exist => True);
   begin
      if Filename'Length > 1 then
         Mini_Expect.Prepare (Filename);
      end if;
   end Expect_CB;

   procedure Logging_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
   begin
      if Session_Logger.Logging then
         Session_Logger.Stop_Logging;
      else
         declare
            Filename : constant String :=
               Gtkada.File_Selection.File_Selection_Dialog (Title => "DasherA Log File",
                                                            Dir_Only => False,
                                                            Must_Exist => False);
            OK : Boolean;
            Message : aliased Gtk_Message_Dialog;
         begin
            if Filename'Length > 1 then
               OK := Session_Logger.Start_Logging (Filename);
               if not OK then
                  Message := Gtk_Message_Dialog_New (Parent => Main_Window,
                              Flags => Modal,
                              The_Type => Message_Error,
                              Buttons => Buttons_Close,
                              Message => "Could not open log file");
               end if;
            end if;
         end;
      end if;
   end Logging_CB;

   procedure Paste_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      use Gtk.Clipboard;
      use Gtkada.Dialogs;
      pragma Unreferenced (Self);
      Clipboard : constant Gtk_Clipboard := Get;
      Unused_Buttons : Message_Dialog_Buttons;
   begin
      if Wait_Is_Text_Available (Clipboard) then
         declare
            Text : constant String := String (Wait_For_Text (Clipboard));
         begin
            Redirector.Router.Send_Data (Text);
         end;
      else
         Unused_Buttons := Message_Dialog (Msg => "Nothing in Clipboard to Paste",
                                           Title => "DasherA - Infomation");
      end if;

   end Paste_CB;

   procedure Serial_Connect_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Dialog   : Gtk_Dialog;
      Dlg_Box  : Gtk.Box.Gtk_Box;
      Ser_Grid : Gtk.Grid.Gtk_Grid;
      Port_Label, Baud_Label, Bits_Label, Parity_Label, Stop_Bits_Label : Gtk.Label.Gtk_Label;
      Port_Entry : Gtk.GEntry.Gtk_Entry;
      Baud_Combo, Bits_Combo, Parity_Combo, Stop_Bits_Combo : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
      Cancel_Unused, Connect_Unused : Gtk.Widget.Gtk_Widget;
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      Gtk_New (Dialog);
      Dialog.Set_Destroy_With_Parent (True);
      Dialog.Set_Modal (True);
      Dialog.Set_Title (App_Title & " - Serial Port");
      Dlg_Box := Dialog.Get_Content_Area;
      Gtk.Grid.Gtk_New (Ser_Grid);

      Gtk.Label.Gtk_New (Port_Label, "Port:");
      Ser_Grid.Attach (Child => Port_Label, Left => 0, Top => 0);
      Gtk.GEntry.Gtk_New (The_Entry => Port_Entry);
      Ser_Grid.Attach (Child => Port_Entry, Left => 1, Top => 0);

      Gtk.Label.Gtk_New (Baud_Label, "Baud:");
      Ser_Grid.Attach (Child => Baud_Label, Left => 0, Top => 1);
      Gtk.Combo_Box_Text.Gtk_New (Baud_Combo);
      Baud_Combo.Append_Text ("300");
      Baud_Combo.Append_Text ("1200");
      Baud_Combo.Append_Text ("2400");
      Baud_Combo.Append_Text ("9600");
      Baud_Combo.Append_Text ("19200");
      Baud_Combo.Append_Text ("38400");
      Baud_Combo.Set_Active (3);
      Ser_Grid.Attach (Child => Baud_Combo, Left => 1, Top => 1);

      Gtk.Label.Gtk_New (Bits_Label, "Data Bits:");
      Ser_Grid.Attach (Child => Bits_Label, Left => 0, Top => 2);
      Gtk.Combo_Box_Text.Gtk_New (Bits_Combo);
      Bits_Combo.Append_Text ("7");
      Bits_Combo.Append_Text ("8");
      Bits_Combo.Set_Active (1);
      Ser_Grid.Attach (Child => Bits_Combo, Left => 1, Top => 2);

      Gtk.Label.Gtk_New (Parity_Label, "Parity:");
      Ser_Grid.Attach (Child => Parity_Label, Left => 0, Top => 3);
      Gtk.Combo_Box_Text.Gtk_New (Parity_Combo);
      Parity_Combo.Append_Text ("None");
      Parity_Combo.Append_Text ("Even");
      Parity_Combo.Append_Text ("Odd");
      Parity_Combo.Set_Active (0);
      Ser_Grid.Attach (Child => Parity_Combo, Left => 1, Top => 3);

      Gtk.Label.Gtk_New (Stop_Bits_Label, "Stop Bits:");
      Ser_Grid.Attach (Child => Stop_Bits_Label, Left => 0, Top => 4);
      Gtk.Combo_Box_Text.Gtk_New (Stop_Bits_Combo);
      Stop_Bits_Combo.Append_Text ("1");
      Stop_Bits_Combo.Append_Text ("2");
      Stop_Bits_Combo.Set_Active (0);
      Ser_Grid.Attach (Child => Stop_Bits_Combo, Left => 1, Top => 4);

      Dlg_Box.Pack_Start (Child => Ser_Grid, Padding => 5);

      Cancel_Unused := Dialog.Add_Button ("Cancel", Gtk_Response_Cancel);
      Connect_Unused := Dialog.Add_Button ("Connect", Gtk_Response_Accept);
      Dialog.Set_Default_Response (Gtk_Response_Accept);
      Dialog.Show_All;
      if Dialog.Run = Gtk_Response_Accept then
         if Port_Entry.Get_Text_Length = 0 then
            Unused_Buttons := Gtkada.Dialogs.Message_Dialog (Msg => "You must enter a Serial Port",
                                                             Title => "DasherA - Error");
         else
            declare
               Port_Str : constant Glib.UTF8_String := Port_Entry.Get_Text;
               Rate : Data_Rate;
               Bits : Data_Bits;
               Stop_Bits : Stop_Bits_Number;
               Parity : Parity_Check;
            begin
               case Baud_Combo.Get_Active is
                  when 0 => Rate := B300;
                  when 1 => Rate := B1200;
                  when 2 => Rate := B2400;
                  when 3 => Rate := B9600;
                  when 4 => Rate := B19200;
                  when 5 => Rate := B38400;
                  when others => null;
               end case;
               case Bits_Combo.Get_Active is
                  when 0 => Bits := CS7;
                  when 1 => Bits := CS8;
                  when others => null;
               end case;
               case Parity_Combo.Get_Active is
                  when 0 => Parity := None;
                  when 1 => Parity := Even;
                  when 2 => Parity := Odd;
                  when others => null;
               end case;
               case Stop_Bits_Combo.Get_Active is
                  when 0 => Stop_Bits := One;
                  when 1 => Stop_Bits := Two;
                  when others => null;
               end case;
               Serial.Open (Port_Str, Rate, Bits, Parity, Stop_Bits);

               Redirector.Router.Set_Destination (Redirector.Async);
               Serial_Connect_Item.Set_Sensitive (False);
               Serial_Disconnect_Item.Set_Sensitive (True);
               Net_Connect_Item.Set_Sensitive (False);
               Net_Disconnect_Item.Set_Sensitive (False);
               Xmodem_Rx_Item.Set_Sensitive (True);
               Xmodem_Send_Item.Set_Sensitive (True);
               Xmodem_Send1k_Item.Set_Sensitive (True);
            exception
               when others =>
                  Unused_Buttons := Gtkada.Dialogs.Message_Dialog (Msg => "Could not open Serial Port",
                                                                   Title => "DasherA - Error");
            end;
         end if;
      end if;
      Dialog.Destroy;
   end Serial_Connect_CB;

   procedure Serial_Disconnect_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Serial.Close;
      Redirector.Router.Set_Destination (Redirector.Local);
      Serial_Connect_Item.Set_Sensitive (True);
      Serial_Disconnect_Item.Set_Sensitive (False);
      Net_Connect_Item.Set_Sensitive (True);
      Net_Disconnect_Item.Set_Sensitive (False);
      Xmodem_Rx_Item.Set_Sensitive (False);
      Xmodem_Send_Item.Set_Sensitive (False);
      Xmodem_Send1k_Item.Set_Sensitive (False);
   end Serial_Disconnect_CB;

   procedure Telnet_Connect_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Dialog : Gtk_Dialog;
      Dlg_Box   : Gtk.Box.Gtk_Box;
      Host_Label, Port_Label : Gtk.Label.Gtk_Label;
      Host_Entry, Port_Entry : Gtk.GEntry.Gtk_Entry;
      Cancel_Unused, Connect_Unused : Gtk.Widget.Gtk_Widget;
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      Gtk_New (Dialog);
      Dialog.Set_Destroy_With_Parent (True);
      Dialog.Set_Modal (True);
      Dialog.Set_Title (App_Title & " - Telnet Host");
      Dlg_Box := Dialog.Get_Content_Area;
      Gtk.Label.Gtk_New (Host_Label, "Host:");
      Dlg_Box.Pack_Start (Child => Host_Label, Expand => True, Fill => True, Padding => 5);
      Gtk.GEntry.Gtk_New (The_Entry => Host_Entry);
      Dlg_Box.Pack_Start (Child => Host_Entry, Expand => True, Fill => True, Padding => 5);
      if Saved_Host /= Null_Unbounded_String then
         Host_Entry.Set_Text (To_String (Saved_Host));
      end if;
      Gtk.Label.Gtk_New (Port_Label, "Port:");
      Dlg_Box.Pack_Start (Child => Port_Label, Expand => True, Fill => True, Padding => 5);
      Gtk.GEntry.Gtk_New (The_Entry => Port_Entry);
      if Saved_Port /= Null_Unbounded_String then
         Port_Entry.Set_Text (To_String (Saved_Port));
      end if;
      Dlg_Box.Pack_Start (Child => Port_Entry, Expand => True, Fill => True, Padding => 5);
      Cancel_Unused := Dialog.Add_Button ("Cancel", Gtk_Response_Cancel);
      Connect_Unused := Dialog.Add_Button ("Connect", Gtk_Response_Accept);
      Dialog.Set_Default_Response (Gtk_Response_Accept);
      Dialog.Show_All;
      if Dialog.Run = Gtk_Response_Accept then
         if Host_Entry.Get_Text_Length = 0 or else Port_Entry.Get_Text_Length = 0 then
            Unused_Buttons := Gtkada.Dialogs.Message_Dialog (Msg => "You must enter both Host and Port",
                                                             Title => "DasherA - Error");
         else
            declare
               Host_Str : constant Glib.UTF8_String := Host_Entry.Get_Text;
               Port_Num : Positive;
            begin
               Port_Num := Positive'Value (Port_Entry.Get_Text);
               Telnet_Sess := Telnet.New_Connection (String (Host_Str), Port_Num);
               Redirector.Router.Set_Destination (Redirector.Network);
               Net_Connect_Item.Set_Sensitive (False);
               Net_Disconnect_Item.Set_Sensitive (True);
               Serial_Connect_Item.Set_Sensitive (False);
               Serial_Disconnect_Item.Set_Sensitive (False);
               Saved_Host := To_Unbounded_String (Host_Str);
               Saved_Port := To_Unbounded_String (Port_Entry.Get_Text);
            exception
               when Error : others =>
                  Log (DEBUG, Exception_Information (Error));
                  Unused_Buttons := Gtkada.Dialogs.Message_Dialog (Msg => "Could not connect. " &
                                                                   Exception_Information (Error),
                                                                   Title => "DasherA - Error");
            end;
         end if;
      end if;
      Dialog.Destroy;
   end Telnet_Connect_CB;

   procedure Telnet_Disconnect_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Telnet_Sess.Close_Connection;
      Redirector.Router.Set_Destination (Redirector.Local);
      Net_Connect_Item.Set_Sensitive (True);
      Net_Disconnect_Item.Set_Sensitive (False);
      Serial_Connect_Item.Set_Sensitive (True);
      Serial_Disconnect_Item.Set_Sensitive (False);
   end Telnet_Disconnect_CB;

   procedure View_History_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      use Gtk.Scrolled_Window;
      use Gtk.Text_Buffer;
      use Gtk.Text_View;
      Dialog     : Gtk_Dialog;
      Dlg_Box    : Gtk.Box.Gtk_Box;
      FProvider  : constant Gtk.Css_Provider.Gtk_Css_Provider := Gtk.Css_Provider.Gtk_Css_Provider_New;
      CSS        : constant String :=
      "textview.view {" & ASCII.LF
      & " font-family: monospace;" & ASCII.LF
      & "}" & ASCII.LF;
      Error      : aliased Glib.Error.GError;
      Dummy      : Boolean;
      Scrollable : Gtk_Scrolled_Window;
      View       : Gtk_Text_View;
      Buffer     : Gtk_Text_Buffer;
      Close_Unused   : Gtk.Widget.Gtk_Widget;
      H_First, H_Last, H_Line : Integer;
   begin
      Gtk_New (Dialog);
      Dialog.Set_Modal (True);
      Dialog.Set_Title (App_Title & " - History");
      Dialog.Set_Size_Request (800, 800);
      Dlg_Box := Dialog.Get_Content_Area;
      Gtk_New (Scrollable);
      Gtk_New (View);
      View.Set_Editable (False);
      --  View.Set_Monospace (True);
      Dummy := FProvider.Load_From_Data (CSS, Error'Access);
      if not Dummy then
         Log (Logging.ERROR, "Loading CSS from data");
      end if;
      Apply_Css (Widget => View, Provider => +FProvider);
      Buffer := View.Get_Buffer;
      Buffer.Set_Text ("   *** Start of History ***");
      H_First := Display_P.Display.Get_First_History_Line;
      H_Last := Display_P.Display.Get_Last_History_Line;
      H_Line := H_First;
      if H_First > H_Last then
         while H_Line < Display_P.History_Lines loop
            Buffer.Insert_At_Cursor (Display_P.Display.Get_History_Line (H_Line) & Dasher_Codes.Dasher_NL);
            H_Line := H_Line + 1;
         end loop;
         H_Line := 0;
      end if;
      while H_Line <= H_Last loop
         Buffer.Insert_At_Cursor (Display_P.Display.Get_History_Line (H_Line) & Dasher_Codes.Dasher_NL);
         H_Line := H_Line + 1;
      end loop;
      Scrollable.Add (View);
      Dlg_Box.Pack_Start (Child => Scrollable, Expand => True, Padding => 5);
      Close_Unused := Dialog.Add_Button ("Close", Gtk_Response_Cancel);
      Dialog.Set_Default_Response (Gtk_Response_Cancel);
      Dialog.Show_All;
      if Dialog.Run = Gtk_Response_Cancel then
         null;
      end if;
      Dialog.Destroy;
   end View_History_CB;

   procedure Xmodem_Rx_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      use Gtk.File_Chooser;   use Gtk.File_Chooser_Dialog;
      use Gtk.Stock;
      FC_Dialog : Gtk_File_Chooser_Dialog;
      Dummy_Button : Gtk_Widget;
      Unused_Buttons : Message_Dialog_Buttons;
   begin
      FC_Dialog := Gtk_File_Chooser_Dialog_New (Title => "DasherA - Xmodem Receive File As...",
                                                Parent => Main_Window,
                                                Action => Action_Save);
      Dummy_Button := FC_Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
      Dummy_Button := FC_Dialog.Add_Button (Stock_Ok, Gtk_Response_OK);
      if FC_Dialog.Run = Gtk_Response_OK then
         Log (DEBUG, "Chosen file for Xmodem Rx: " & FC_Dialog.Get_Filename);
         Xmodem.Receive (String (FC_Dialog.Get_Filename), Trace_Xmodem_Opt);
      end if;
      FC_Dialog.Destroy;
   exception
      when Xmodem.Already_Exists =>
         FC_Dialog.Destroy;
         Unused_Buttons := Gtkada.Dialogs.Message_Dialog (Msg => "The file must not already exist",
                                                          Title => "DasherA - Error");
   end Xmodem_Rx_CB;

   procedure Xmodem_Tx_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Filename : constant String :=
         Gtkada.File_Selection.File_Selection_Dialog (Title => "DasherA - Xmodem-CRC File Send",
                                                      Dir_Only => False,
                                                      Must_Exist => True);
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if Filename'Length > 0 then
         Xmodem.Send (Filename => Filename, Pkt_Len => Xmodem.Short, Trace_Flag => Trace_Xmodem_Opt);
      end if;
   exception
      when Xmodem.Protocol_Error =>
         Unused_Buttons := Message_Dialog (Msg => "Xmodem Protocol Error", Title => "DasherA - Error");
      when others =>
         Unused_Buttons := Message_Dialog (Msg => "Could not open file to send: " & Filename,
                                           Title => "DasherA - Error");
   end Xmodem_Tx_CB;

   procedure Xmodem_Tx_1k_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Filename : constant String :=
         Gtkada.File_Selection.File_Selection_Dialog (Title => "DasherA - Xmodem-CRC 1k File Send",
                                                      Dir_Only => False,
                                                      Must_Exist => True);
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if Filename'Length > 0 then
         Xmodem.Send (Filename => Filename, Pkt_Len => Xmodem.Long, Trace_Flag => Trace_Xmodem_Opt);
      end if;
   exception
      when Xmodem.Protocol_Error =>
         Unused_Buttons := Message_Dialog (Msg => "Xmodem Protocol Error", Title => "DasherA - Error");
      when others =>
         Unused_Buttons := Message_Dialog (Msg => "Could not open file to send: " & Filename,
                                           Title => "DasherA - Error");
   end Xmodem_Tx_1k_CB;

   function Handle_Key_Release_Event_CB (Self : access Gtk.Widget.Gtk_Widget_Record'Class; Event : Gdk.Event.Gdk_Event_Key)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      Keyboard.Handle_Key_Release (Event.Keyval);
      return True;
   end Handle_Key_Release_Event_CB;

   function Handle_Key_Press_Event_CB (Self : access Gtk.Widget.Gtk_Widget_Record'Class; Event : Gdk.Event.Gdk_Event_Key)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      Keyboard.Handle_Key_Press (Event.Keyval);
      return True;
   end Handle_Key_Press_Event_CB;

   function Create_Menu_Bar return Gtk.Menu_Bar.Gtk_Menu_Bar is
      Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Sep_Item : Gtk.Separator_Menu_Item.Gtk_Separator_Menu_Item;
      File_Menu, Edit_Menu, View_Menu, Emulation_Menu, Serial_Menu,
      Network_Menu, Help_Menu : Gtk.Menu.Gtk_Menu;
      Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Logging_Item, Expect_Item, Send_File_Item,
      History_Item, Resize_Item,
      D200_Item, D210_Item, Self_Test_Item,
      Quit_Item,
      Paste_Item,
      About_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Log (DEBUG, "Starting to Create_Menu_Bar");
      Gtk_New (Menu_Bar);

      --  File

      Gtk_New (Menu_Item, "File");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (File_Menu);
      Menu_Item.Set_Submenu (File_Menu);

      Gtk_New (Logging_Item, "Logging");
      File_Menu.Append (Logging_Item);
      Logging_Item.On_Activate (Logging_CB'Access);

      Gtk_New (Sep_Item);
      File_Menu.Append (Sep_Item);

      Gtk_New (Expect_Item, "Run mini-Expect Script");
      File_Menu.Append (Expect_Item);
      Expect_Item.On_Activate (Expect_CB'Access);

      Gtk_New (Sep_Item);
      File_Menu.Append (Sep_Item);

      Gtk_New (Send_File_Item, "Send (Text) File");
      File_Menu.Append (Send_File_Item);
      Gtk_New (Sep_Item);
      File_Menu.Append (Sep_Item);
      Send_File_Item.On_Activate (Send_Text_File_CB'Access);

      Gtk_New (Xmodem_Rx_Item, "XMODEM-CRC - Receive File");
      Xmodem_Rx_Item.Set_Sensitive (False);
      File_Menu.Append (Xmodem_Rx_Item);
      Xmodem_Rx_Item.On_Activate (Xmodem_Rx_CB'Access);
      Gtk_New (Xmodem_Send_Item, "XMODEM-CRC - Send File");
      Xmodem_Send_Item.Set_Sensitive (False);
      Xmodem_Send_Item.On_Activate (Xmodem_Tx_CB'Access);
      File_Menu.Append (Xmodem_Send_Item);
      Gtk_New (Xmodem_Send1k_Item, "XMODEM-CRC - Send File (1k packets");
      Xmodem_Send_Item.On_Activate (Xmodem_Tx_1k_CB'Access);
      Xmodem_Send1k_Item.Set_Sensitive (False);
      File_Menu.Append (Xmodem_Send1k_Item);
      Gtk_New (Sep_Item);
      File_Menu.Append (Sep_Item);

      Gtk_New (Quit_Item, "Quit");
      File_Menu.Append (Quit_Item);
      Quit_Item.On_Activate (Quit_CB'Access);

      --  Edit

      Gtk_New (Menu_Item, "Edit");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (Edit_Menu);
      Menu_Item.Set_Submenu (Edit_Menu);
      Gtk_New (Paste_Item, "Paste");
      Edit_Menu.Append (Paste_Item);
      Paste_Item.On_Activate (Paste_CB'Access);

      --  View
      Gtk_New (Menu_Item, "View");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (View_Menu);
      Menu_Item.Set_Submenu (View_Menu);

      Gtk_New (History_Item, "History");
      View_Menu.Append (History_Item);
      History_Item.On_Activate (View_History_CB'Access);

      Gtk_New (Sep_Item);
      View_Menu.Append (Sep_Item);
      Gtk_New (Load_Template_Item, "Load Func. Key Template");
      View_Menu.Append (Load_Template_Item);
      Load_Template_Item.On_Activate (Load_Template_CB'Access);
      Gtk_New (Hide_Template_Item, "Hide Func. Key Template");
      View_Menu.Append (Hide_Template_Item);
      Hide_Template_Item.Set_Sensitive (False);
      Hide_Template_Item.On_Activate (Hide_Template_CB'Access);

      --  Emulation

      Gtk_New (Menu_Item, "Emulation");
      Menu_Bar.Append (Menu_Item);
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
      Gtk_New (Resize_Item, "Resize Terminal");
      Emulation_Menu.Append (Resize_Item);
      Resize_Item.On_Activate (Resize_CB'Access);

      Gtk_New (Sep_Item);
      Emulation_Menu.Append (Sep_Item);

      Gtk_New (Self_Test_Item, "Self-Test");
      Emulation_Menu.Append (Self_Test_Item);
      Self_Test_Item.On_Activate (Self_Test_CB'Access);

      --  Serial

      Gtk_New (Menu_Item, "Serial");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (Serial_Menu);
      Menu_Item.Set_Submenu (Serial_Menu);
      Gtk_New (Serial_Connect_Item, "Connect");
      Serial_Menu.Append (Serial_Connect_Item);
      Serial_Connect_Item.On_Activate (Serial_Connect_CB'Access);

      Gtk_New (Serial_Disconnect_Item, "Disconnect");
      Serial_Menu.Append (Serial_Disconnect_Item);
      Serial_Disconnect_Item.Set_Sensitive (False);
      Serial_Disconnect_Item.On_Activate (Serial_Disconnect_CB'Access);

      --  Network

      Gtk_New (Menu_Item, "Network");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (Network_Menu);
      Menu_Item.Set_Submenu (Network_Menu);
      Gtk_New (Net_Connect_Item, "Connect");
      Network_Menu.Append (Net_Connect_Item);
      Net_Connect_Item.On_Activate (Telnet_Connect_CB'Access);

      Gtk_New (Net_Disconnect_Item, "Disconnect");
      Network_Menu.Append (Net_Disconnect_Item);
      Net_Disconnect_Item.Set_Sensitive (False);
      Net_Disconnect_Item.On_Activate (Telnet_Disconnect_CB'Access);

      --  Help

      Gtk_New (Menu_Item, "Help");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (Help_Menu);
      Menu_Item.Set_Submenu (Help_Menu);

      Gtk_New (About_Item, "About");
      Help_Menu.Append (About_Item);
      About_Item.On_Activate (About_CB'Access);

      return Menu_Bar;
   end Create_Menu_Bar;

--     procedure Local_Print_Btn_CB (Btn : access Gtk.Button.Gtk_Button_Record'Class) is
--        pragma Unreferenced (Btn);
--        use Gtk.File_Chooser;   use Gtk.File_Chooser_Dialog;
--        use Gtk.Stock;
--        FC_Dialog : Gtk_File_Chooser_Dialog;
--        Dummy_Button : Gtk_Widget;
--        Unused_Buttons : Message_Dialog_Buttons;
--     begin
--        FC_Dialog := Gtk_File_Chooser_Dialog_New (Title => "DasherA - Save Screen Image As...",
--                                                  Parent => Main_Window,
--                                                  Action => Action_Save);
--        Dummy_Button := FC_Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
--        Dummy_Button := FC_Dialog.Add_Button (Stock_Ok, Gtk_Response_OK);
--        if FC_Dialog.Run = Gtk_Response_OK then
--  NULL;
--        end if;
--        FC_Dialog.Destroy;
--     exception
--        when Xmodem.Already_Exists =>
--           FC_Dialog.Destroy;
--           Unused_Buttons := Gtkada.Dialogs.Message_Dialog (Msg => "The file must not already exist",
--                                                            Title => "DasherA - Error");
--     end Local_Print_Btn_CB;

   procedure Handle_Key_Btn_CB (Btn : access Gtk.Button.Gtk_Button_Record'Class) is
      use Redirector;
      Lab : constant String := Btn.Get_Label;
      Dest : Redirector.Connection_T;
   begin
      if Lab = "Break" then
         Keyboard.Handle_Key_Release (GDK_Break);
         Redirector.Router.Get_Destination (Dest);
         if Dest = Async then
            Serial.Keyboard_Sender_Task.Send_Break;
         else
            Log (INFO, "BREAK only implemented for Serial connections");
         end if;
      elsif Lab = "C1" then
         Keyboard.Handle_Key_Release (GDK_F31);
      elsif Lab = "C2" then
         Keyboard.Handle_Key_Release (GDK_F32);
      elsif Lab = "C3" then
         Keyboard.Handle_Key_Release (GDK_F33);
      elsif Lab = "C4" then
         Keyboard.Handle_Key_Release (GDK_F34);
      elsif Lab = "Er.Page" then
         Keyboard.Handle_Key_Release (GDK_3270_EraseEOF);
      --  elsif Lab = "Loc.Print" then
      --     Keyboard.Handle_Key_Release (GDK_3270_PrintScreen);  --  TODO Local Print
      elsif Lab = "Er.EOL" then
         Keyboard.Handle_Key_Release (GDK_3270_EraseInput);
      elsif Lab = "CR" then
         Keyboard.Handle_Key_Release (GDK_KP_Enter);
      elsif Lab = "Hold" then
         Term.Holding := not Term.Holding;
         if Term.Holding then
            Keyboard.Handle_Key_Release (GDK_F29);
         else
            Keyboard.Handle_Key_Release (GDK_F30);
         end if;
      end if;
   end Handle_Key_Btn_CB;

   function Create_Keys_Box return Gtk.Box.Gtk_Box is
      Keys_Box :  Gtk.Box.Gtk_Box;
      C1_Btn, C2_Btn, C3_Btn, C4_Btn,
      Break_Btn, Er_Pg_Btn, Er_EOL_Btn, CR_Btn, Hold_Btn : Gtk.Button.Gtk_Button;
      Sep : Gtk.Separator.Gtk_Vseparator;
   begin
      Gtk.Box.Gtk_New (Keys_Box, Orientation_Horizontal, 1);
      Keys_Box.Set_Homogeneous (False);
      Gtk.Button.Gtk_New (Break_Btn, "Break");
      Break_Btn.Set_Tooltip_Text ("Send BREAK signal on Serial Connection");
      Break_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (Break_Btn);
      --  Break_Btn.Set_Hexpand (True);

      Gtk.Separator.Gtk_New_Vseparator (Sep);
      Keys_Box.Add (Sep);
      Sep.Set_Hexpand (True);

      Gtk.Button.Gtk_New (C1_Btn, "C1");
      C1_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (C1_Btn);

      Gtk.Button.Gtk_New (C2_Btn, "C2");
      C2_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (C2_Btn);

      Gtk.Button.Gtk_New (C3_Btn, "C3");
      C3_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (C3_Btn);

      Gtk.Button.Gtk_New (C4_Btn, "C4");
      C4_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (C4_Btn);

      Gtk.Button.Gtk_New (Er_Pg_Btn, "Er.Page");
      Er_Pg_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      Keys_Box.Add (Er_Pg_Btn);

      --  Gtk.Button.Gtk_New (Loc_Pr_Btn, "Loc.Print");
      --  Loc_Pr_Btn.On_Clicked (Handle_Key_Btn_CB'Access);
      --  Keys_Box.Add (Loc_Pr_Btn);

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
      Log (DEBUG, "Handle_FKey_Btn_CB called for " & Lab);
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
      elsif Lab = "F15" then
         Keyboard.Handle_Key_Release (GDK_F15);
      end if;
   end Handle_FKey_Btn_CB;

   function Create_Template_Labels_Revealer return Gtk.Revealer.Gtk_Revealer is
      Template_Rev : Gtk.Revealer.Gtk_Revealer;
      Template_Grid : Gtk.Grid.Gtk_Grid;
      FProvider : constant Gtk.Css_Provider.Gtk_Css_Provider := Gtk.Css_Provider.Gtk_Css_Provider_New;
      Error     : aliased Glib.Error.GError;
      Dummy     : Boolean;
      CSS       : constant String :=
      "label {" & ASCII.LF
     & "  border-color: white;" & ASCII.LF
     & "  border-width: 1px;" & ASCII.LF
     & "  border-style: solid;" & ASCII.LF
     & "}" & ASCII.LF;
   begin
      Gtk.Revealer.Gtk_New (Template_Rev);
      Gtk.Grid.Gtk_New (Template_Grid);
      for row in 1 .. 4 loop
         for col in 1 .. 17 loop
            Gtk.Label.Gtk_New (Template_Labels (row, col));
            Template_Labels (row, col).Set_Size_Request (Width => 48, Height => 30);
            Template_Labels (row, col).Set_Justify (Gtk.Enums.Justify_Center);
            Template_Grid.Attach (Child => Template_Labels (row, col), Left => Gint (col) - 1, Top => Gint (row) - 1, Width => 1, Height => 1);
         end loop;
      end loop;
      Template_Labels (1, 6).Set_Markup ("<small><small><b>Ctrl-Shift</b></small></small>");
      Template_Labels (2, 6).Set_Markup ("<small><small><b>Ctrl</b></small></small>");
      Template_Labels (3, 6).Set_Markup ("<small><small><b>Shift</b></small></small>");
      Template_Labels (1, 12).Set_Markup ("<small><small><b>Ctrl-Shift</b></small></small>");
      Template_Labels (2, 12).Set_Markup ("<small><small><b>Ctrl</b></small></small>");
      Template_Labels (3, 12).Set_Markup ("<small><small><b>Shift</b></small></small>");
      Template_Rev.Add (Template_Grid);
      Dummy := FProvider.Load_From_Data (CSS, Error'Access);
      if not Dummy then
         Log (Logging.ERROR, "Loading CSS from data");
      end if;
      Apply_Css (Widget => Template_Grid, Provider => +FProvider);
      Template_Rev.Set_Reveal_Child (False);
      return Template_Rev;
   end Create_Template_Labels_Revealer;

   function Create_FKeys_Box return Gtk.Box.Gtk_Box is
      FKeys_Box :  Gtk.Box.Gtk_Box;
      FKeys : array (1 .. 15) of Gtk.Button.Gtk_Button;
      Error : aliased Glib.Error.GError;
      FProvider : constant Gtk.Css_Provider.Gtk_Css_Provider := Gtk.Css_Provider.Gtk_Css_Provider_New;
      Dummy    : Boolean;

      CSS : constant String :=
       "#FKey_Button {" & ASCII.LF
     & "  color: white;" & ASCII.LF
     & "  background-image: none;" & ASCII.LF
     & "  background-color: rgba(31, 220, 232, 1);" & ASCII.LF
     & "  border-color: white;" & ASCII.LF
     & "  font-family: Monospace;" & ASCII.LF
     & "  font-weight: bold;" & ASCII.LF
     & "  padding-left: 2px;" & ASCII.LF
     & "  padding-right: 2px;" & ASCII.LF
     & "}";

   begin
      Gtk.Box.Gtk_New (FKeys_Box, Gtk.Enums.Orientation_Horizontal, 1);
      FKeys_Box.Set_Homogeneous (True);
      Dummy := FProvider.Load_From_Data (CSS, Error'Access);
      if not Dummy then
         Log (Logging.ERROR, "Loading CSS from data");
      end if;
      for N in FKeys'Range loop
         declare
            Lab : constant String := N'Image;
         begin
            Gtk.Button.Gtk_New (FKeys (N), "F" & Ada.Strings.Fixed.Trim (Lab, Ada.Strings.Both));
            FKeys (N).On_Clicked (Handle_FKey_Btn_CB'Access);
            FKeys (N).Set_Size_Request (Width => 40, Height => 28);
            FKeys (N).Set_Name ("FKey_Button");
         end;
      end loop;
      --  we want labels between the groups of 5 f-key buttons
      for F in 1 .. 5 loop
         FKeys_Box.Pack_Start (FKeys (F), False, False);
      end loop;
      Gtk.Label.Gtk_New (L_FKeys_Label, " ");
      FKeys_Box.Pack_Start (L_FKeys_Label);
      for F in 6 .. 10 loop
         FKeys_Box.Pack_Start (FKeys (F), False, False);
      end loop;
      Gtk.Label.Gtk_New (R_FKeys_Label, " ");
      FKeys_Box.Pack_Start (R_FKeys_Label);
      for F in 11 .. 15 loop
         FKeys_Box.Pack_Start (FKeys (F), False, False);
      end loop;
      Apply_Css (FKeys_Box, +FProvider);
      return FKeys_Box;
   end Create_FKeys_Box;

   function Update_Status_Box_CB (SB : Gtk.Box.Gtk_Box) return Boolean is
      Dest : Redirector.Connection_T;
   begin
      Redirector.Router.Get_Destination (Dest);
      Gdk.Threads.Enter;
      case Dest is
         when Redirector.Local =>
            Online_Label.Set_Text ("Local");
            Host_Label.Set_Text ("(Disconnected)");
            Net_Connect_Item.Set_Sensitive (True);
            Net_Disconnect_Item.Set_Sensitive (False);
            Serial_Connect_Item.Set_Sensitive (True);
            Serial_Disconnect_Item.Set_Sensitive (False);
         when Redirector.Async =>
            Online_Label.Set_Text ("Online (Serial)");
            Host_Label.Set_Text (To_String (Serial.Port_US));
         when Redirector.Network =>
            Online_Label.Set_Text ("Online (Telnet)");
            Host_Label.Set_Text (To_String (Telnet_Sess.Host_Str) & ":" &
                                 Ada.Strings.Fixed.Trim (Telnet_Sess.Port_Num'Image, Ada.Strings.Both));
      end case;
      case Term.Emulation is
         when Terminal.D200 => Emul_Label.Set_Text ("D200");
         when Terminal.D210 => Emul_Label.Set_Text ("D210");
      end case;
      if Session_Logger.Logging then
         Logging_Label.Set_Text ("Logging");
      else
         Logging_Label.Set_Text ("Not Logging");
      end if;
      if Term.Holding then
         Hold_Label.Set_Text ("HOLD");
      else
         Hold_Label.Set_Text ("");
      end if;
      SB.Queue_Draw;
      Gdk.Threads.Leave;
      return True;
   end Update_Status_Box_CB;

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

   function Create_Icon_Pixbuf return Gdk.Pixbuf.Gdk_Pixbuf is
      IP :  Gdk.Pixbuf.Gdk_Pixbuf;
      Icon_Emb : constant Embedded.Content_Type := Embedded.Get_Content (App_Icon);
      package IO is new Ada.Sequential_IO (Interfaces.Unsigned_8);
      Tmp_Filename : constant String := "DasherA_Icon.tmp";
      Tmp_File : IO.File_Type;
      Error : aliased Glib.Error.GError;
   begin
      if Ada.Directories.Exists (Tmp_Filename) then
         Ada.Directories.Delete_File (Tmp_Filename);
      end if;
      IO.Create (File => Tmp_File, Name => Tmp_Filename);
      for Val of Icon_Emb.Content.all loop
         IO.Write (Tmp_File, Interfaces.Unsigned_8 (Val));
      end loop;
      IO.Close (Tmp_File);
      Gdk.Pixbuf.Gdk_New_From_File (Pixbuf => IP, Filename => Tmp_Filename, Error => Error);
      if Error /= null then
         Log (WARNING, "Could not find/load icon file: DasherA_Icon.tmp");
      end if;
      Ada.Directories.Delete_File (Tmp_Filename);
      return IP;
   end Create_Icon_Pixbuf;

   function Create_Window (Host_Arg     : Unbounded_String;
                           Font_Colour  : BDF_Font.Font_Colour_T;
                           Trace_Xmodem : Boolean) return Gtk.Window.Gtk_Window is
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      Log (DEBUG, "Starting to Create_Window");
      Trace_Xmodem_Opt := Trace_Xmodem;
      Saved_Font_Colour := Font_Colour;

      --  Gtk.Window.Initialize (Main_Window);
      Gtk.Window.Gtk_New (Main_Window);
      Log (DEBUG, "New Window Created");
      Main_Window.Set_Title (App_Title);
      Main_Window.On_Destroy (Window_Close_CB'Access);

      --  Everything is in a Grid...
      Gtk.Grid.Gtk_New (Main_Grid);
      Main_Grid.Set_Orientation (Gtk.Enums.Orientation_Vertical);
      Main_Grid.Set_Row_Spacing (2);

      --  Menu
      Main_Grid.Add (Create_Menu_Bar);

      --  Virtual Keys, Function Keys and Template
      Main_Grid.Add (Create_Keys_Box);
      Template_Revealer := Create_Template_Labels_Revealer;
      Main_Grid.Add (Template_Revealer);
      Main_Grid.Add (Create_FKeys_Box);

      --  CRT area
      Display_P.Display.Init;
      Term := Terminal.Create (Terminal.D210);
      Crt.Init (BDF_Font.Normal, Font_Colour);
      Crt.Tube.DA.On_Configure_Event (Crt.Configure_Event_CB'Access);
      Crt.Tube.DA.On_Draw (Crt.Draw_CB'Access);
      Main_Grid.Add (Crt.Tube.DA);

      --  Status Bar
      Main_Grid.Add (Create_Status_Box);

      Main_Window.Add (Main_Grid);
      Main_Window.Set_Position (Gtk.Enums.Win_Pos_Center);

      Redirector.Router := new Redirector.Router_TT;

      Redirector.Router.Set_Destination (Redirector.Local);
      Redirector.Router.Set_Handler (Redirector.Visual);
      Main_Window.On_Key_Press_Event (Handle_Key_Press_Event_CB'Unrestricted_Access);
      Main_Window.On_Key_Release_Event (Handle_Key_Release_Event_CB'Unrestricted_Access);

      Icon_PB := Create_Icon_Pixbuf;
      Main_Window.Set_Icon (Icon_PB);

      Log (DEBUG, "Main Window Built");

      if Host_Arg /= Null_Unbounded_String then
         if Count (Host_Arg, ":") /= 1 then
            Unused_Buttons := Gtkada.Dialogs.Message_Dialog (Msg => "You must enter both Host and Port separated by a colon",
                                                             Title => "DasherA - Error");
         else
            declare
               Colon_Ix : constant Natural := Index (Host_Arg, ":");
               Host_Str : constant String  := Slice (Host_Arg, 1, Colon_Ix - 1);
               Port_Num : constant Positive := Positive'Value (Slice (Host_Arg, Colon_Ix + 1, Length (Host_Arg)));
            begin
               Telnet_Sess := Telnet.New_Connection (Host_Str, Port_Num);
               Redirector.Router.Set_Destination (Redirector.Network);
               Net_Connect_Item.Set_Sensitive (False);
               Net_Disconnect_Item.Set_Sensitive (True);
               Serial_Connect_Item.Set_Sensitive (False);
               Serial_Disconnect_Item.Set_Sensitive (False);
               Saved_Host := To_Unbounded_String (Host_Str);
               Saved_Port := To_Unbounded_String (Slice (Host_Arg, Colon_Ix + 1, Length (Host_Arg)));
            exception
               when Error : others =>
                  Unused_Buttons := Gtkada.Dialogs.Message_Dialog (Msg => "Could not connect. " &
                                                                   Exception_Information (Error),
                                                                   Title => "DasherA - Error");
            end;
         end if;
      end if;

      return Main_Window;

   end Create_Window;

end GUI;
