# Developer's Notes for DasherA

## November 2021...

DasherA is an attempt at rewriting DasherG v0.10.0x in Ada using the
GtkAda toolkit and only Gtk3 elements.

The GUI toolkit used for DasherG (go-gtk) now seems to be unmaintained and it is probably a good time to leave Gtk2 behind. 

## Objects

I've wasted too much time taking a 'purist' approach to making things objects.  
DasherA is only ever going to be single-session, there will only ever be a single connection/terminal etc.
Henceforth, try to use objects only where required, elsewhere a traditional Ada procedural approach works just fine.

## Major Elements

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

* Telnet.Keyboard_Sender
* Telnet.Receiver
* Terminal.Processor

## Build and Clean

Make sure you have all the Ada and GtkAda stuff available.

On Debian-based systems you can use the `gnat` compiler and the `libgtkada19-dev` (or later) GUI
toolkit from the standard provided packages.

On a Linux dev box where I had installed GtkAda from source, I had to do...
```
GPR_PROJECT_PATH=/usr/local/lib/gnat
export GPR_PROJECT_PATH
```
Create the `obj` directory if it does not exist.

Then...

* `gprbuild`
* `gprclean`

Default build is with debugging and without optimisation.  Append `-Xmode=release` for optimised build.

## Run with Fatal Warnings...
`./dashera --gtk-fatal-warnings --g-fatal--warnings`

## Terminal setting on Linux host:

Ensure `ncurses-term` package is installed.

`export TERM=d210-dg`

You may have to `stty echo` if no characters appear when you type.

N.B. There are bugs in the termcap database for (all) the DASHER terminals; not many allegedly termcap (ncurses) aware programs actually handle unusual terminal types.  The `htop` program does a fair job of behaving properly - even so, you will see a few glitches over time. Likewise with `iftop`.

## Serial Port Test Setup (Linux)

Install and then `insmod` `tty0tty`.

Then `/dev/tnt0` is connected to `/dev/tnt1`, etc.

