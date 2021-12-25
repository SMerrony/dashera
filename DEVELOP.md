# Developer's Notes for DasherA

## November 2021...

DasherA is an attempt at rewriting DasherG v0.10.0x in Ada using the
GtkAda toolkit and only Gtk3 elements.

The GUI toolkit used for DasherG (go-gtk) now seems to be unmaintained and it is probably a good time to leave Gtk2 behind. 

## Major Elements

* Cell - a single ASCII character and it's Dasher attributes
* Crt - handle the drawing of a Display in the application
* Dashera - the main entry point
* Display - holds a matrix of ASCII characters (Cells)
* GUI - the bulk of the Gtk interface
* Keyboard - guess what?!?
* Telnet - handle the Telnet protocol
* Terminal - implements the Dasher behaviour


## Build and Clean

Make sure you have all the Ada and GtkAda stuff available.

On my Linux dev box, where I installed GtkAda from source I need to do...
```
GPR_PROJECT_PATH=/usr/local/lib/gnat
export GPR_PROJECT_PATH
```
Then...

* `gprbuild`
* `gprclean`

## Run with Fatal Warnings...
`./obj/dashera --gtk-fatal-warnings --g-fatal--warnings`

## Terminal setting on Linux host:

`export TERM=d210-dg`