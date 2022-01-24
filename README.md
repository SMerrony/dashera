# DasherA
DasherA is a free terminal emulator for Data General DASHER series character-based terminals.  It is written in GNU Ada using the GtkAda toolkit and should run on all common platforms targeted by those tools.

## Key Features

(This will be updated as features from DasherG are ported across...)

* DASHER D200 & D210 Emulation
* Serial interface support at 300, 1200, 2400, 9600 & 19200 baud, 7 or 8 data bits,
no/odd/even parity, 1 or 2 stop bits (defaults to DG-standard: 9600, 8, n, 1)
* Network Interface (Telnet) support
* May specify ```-host=host:port``` on command line
* Reverse video, blinking, dim and underlined characters
* Pixel-for-pixel copy of D410 character set
* 15 (plus Ctrl & Shift) DASHER Function keys, Erase Page, Erase EOL, Hold, Local Print and Break keys
* Loadable function-key templates (BROWSE, SED and SMI provided as examples)
* 1000-line scrollable terminal history
* Session logging to file
* Various terminal widths, heights and zoom-levels available
* Support for mini-Expect scripts to automate some tasks [see Wiki](https://github.com/SMerrony/DasherG/wiki/DasherG-Mini-Expect-Scripts)
  
Here is the full [Implementation Chart](implementationChart.md) for DasherA.

## Download
DasherA is [hosted on GitHub](https://github.com/SMerrony/dashera).

## Operational Notes
* The DASHER 'CR' (carriage-return, no line-feed) is available from both the GUI 'CR' button and the
numeric keypad enter key (if present).
* The DASHER 'Hold' key is available from both the GUI 'Hold' button and the PC-style 'Pause'
button (if present).

