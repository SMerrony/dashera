<!--
 Copyright (C) 2021 Stephen Merrony
 
 This file is part of dashera.
 
 dashera is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.
 
 dashera is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with dashera.  If not, see <http://www.gnu.org/licenses/>.
-->

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
* Terminal - implements the Dasher behaviour


## Build and Clean
* gprbuild
* gprclean

## Run with Fatal Warnings...
`./obj/dashera --gtk-fatal-warnings --g-fatal--warnings`

## Terminal setting on Linux host:

`export TERM=d210-dg`