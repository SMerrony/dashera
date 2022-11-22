# Developer's Notes for DasherA

## November 2021...

DasherA is a rewrite of DasherG v0.10.0x in Ada using the GtkAda toolkit and only Gtk3 elements.

The GUI toolkit used for DasherG (go-gtk) now seems to be unmaintained and it is probably a good time to leave Gtk2 behind. 

## Objects

I've wasted too much time taking a 'purist' approach to making things objects.  
DasherA is only ever going to be single-session, there will only ever be a single connection/terminal etc.
Henceforth, try to use objects only where required, elsewhere a traditional Ada procedural approach works just fine.

## Major Packages

* Cell - a single ASCII character and its Dasher attributes
* Crt - handle the drawing of a Display in the application
* Dashera - the main entry point
* Display - holds a matrix of ASCII characters (Cells)
* GUI - the bulk of the Gtk interface
* Keyboard - guess what?!?
* Redirector - routes data according to the current connection
* Serial - handle the serial (async) connection
* Telnet - handle the Telnet connection and protocol
* Terminal - implements the Dasher behaviour

### Tasks

* Mini_Expect.Runner_Task (transient)
* Redirector.Router
* Serial.Keyboard_Sender
* Serial.Receiver
* Telnet.Keyboard_Sender
* Telnet.Receiver
* Terminal.Processor
* Xmodem.Receiver (transient)
* Xmodem.Sender (transient)

### Embedded Resources

The embedded font and icon were generated with this command, run from the top-level dir...
`are --lang=Ada -o src --resource=Embedded --name-access --fileset='**/*.*' share/dashera`

## Build and Clean

Dashera has moved to the [Alire](https://alire.ada.dev/) build system.

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/dashera.json)](https://alire.ada.dev/crates/dashera.html)

Once you have Alire installed you should be able to obtain the latest release of Dashera and build it with just the three commands below...
```
alr get dashera
cd dashera
alr build
```
N.B. If you have not built a GtkAda crate (the GUI toolkit we use) recently then Alire will automatically download and build that before building Dashera itself.  This can take some time when it first happens, subsequent builds should be much faster.

### Non-Alire Build

If you cannot use Alire, it should still be possible to build Dashera with gprbuild... 
```
mkdir obj
gprbuild -Pnon_alire 
```
Ignore the warning about file name not matching project name.
You may append `-Xmode=release` to the gprbuild command for an optimised build.

Without Alire you will have to manually ensure that dependencies (eg. GtkAda) are installed.
Eg. You may need to install the `libgtkada20-dev` package.


## Run with Fatal Warnings...
`./dashera --gtk-fatal-warnings --g-fatal--warnings`

## Terminal setting on Linux host:

Ensure `ncurses-term` package is installed.

`export TERM=d210-dg`

You may have to `stty echo` if no characters appear when you type.

N.B. There are bugs in the termcap database for (all) the DASHER terminals; not many allegedly termcap (ncurses) aware programs actually handle unusual terminal types.  The `htop` program does a fair job of behaving properly - even so, you will see a few glitches over time. Likewise with `iftop`.  The  `nano` editor seems to behave quite well - although you may need to use the function keys to save and exit.

## Serial Port Test Setup (Linux)

Install and then `insmod` `tty0tty`.

Then `/dev/tnt0` is connected to `/dev/tnt1`, etc.

