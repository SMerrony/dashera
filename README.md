# DasherA
DasherA is a free terminal emulator for Data General DASHER series character-based terminals.  It is written in GNU Ada using the GtkAda toolkit and should run on all common platforms targeted by those tools.

## Key Features

(This will be updated as features from DasherG are ported across...)

* DASHER D200 & D210 Emulation
* Network Interface (Telnet) support
* Reverse video, blinking, dim and underlined characters
* Pixel-for-pixel copy of D410 character set
  
Here is the full [Implementation Chart](implementationChart.md) for DasherA.

## Download
DasherA is [hosted on GitHub](https://github.com/SMerrony/dashera).

## Operational Notes
* The DASHER 'CR' (carriage-return, no line-feed) is available from both the GUI 'CR' button and the
numeric keypad enter key (if present).
* The DASHER 'Hold' key is available from both the GUI 'Hold' button and the PC-style 'Pause'
button (if present).

