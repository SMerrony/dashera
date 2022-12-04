# DasherA Implementation Chart

| Command                              | Octal       | Keyboard                   | D200 | D210 | Notes                            |
|--------------------------------------|-------------|----------------------------| :--: | :--: | ----------------------------------|
| Bell                                 | 007         | Ctrl-G                     | Y    | Y    | Must be allowed by your window manager |
| Blink Disable                        | 004         | Ctrl-D                     | Y    | Y    | Disable blinking on screen  |
| Blink Enable                         | 003         | Ctrl-C                     | Y    | Y    | Enable blinking on screen  |
| Blink Off                            | 017         | Ctrl-O                     | Y    | Y    |                            |
| Blink On                             | 016         | Ctrl-N                     | Y    | Y    |                            |
| Break (CMD-Break)                    | -           | Break Button               | Y    | Y    | Only affects Serial operation |
| Carriage Return                      | 015         | Ctrl-M or CR               | Y    | Y    | No implied new line         |
| Cursor Down                          | 032         | Ctrl-Z or ↓                | Y    | Y    |                   |
| Cursor Left                          | 031         | Ctrl-Y or ←                | Y    | Y    |                   |
| Cursor Right                         | 030         | Ctrl-X or →                | Y    | Y    |                   |
| Cursor Up                            | 027         | Ctrl-W or ↑                | Y    | Y    |                   |
| Dim Off                              | 035         | Ctrl-}                     | Y    | Y    |                   |
| Dim On                               | 034         | Ctrl-\                     | Y    | Y    |                   |
| Erase EOL                            | 013         | Ctrl-K or Erase EOL        | Y    | Y    | Erase from Cursor to End of Line |
| Erase (Unprotected) to End of Screen | 036 106 106 | Cmd-Brk FF                 | -    | Y    | Introduced in D210         |
| Erase Page/Window                    | 014         | Ctrl-L or Erase Page       | Y    | Y    | ~Clear Screen on a D200    |
| New Line                             | 012         | Ctrl-J                     | Y    | Y    | ~Enter/Return              |
| Print Form                           | 001         | Ctrl-A or Shift-Local Print | N   | N    | TODO            |
| Print Screen/Window                  | 021         | Ctrl-Q or Local Print      | N    | N    | TODO    |
| Read Model ID                        | 036 103     | Cmd-Brk C                  | Y    | Y    |                 |
| Read Cursor/Window Address           | 005         | Ctrl-E                     | Y    | Y    | Times out after 0.5s if host not listening |   
| Remote Test Enter                    | 036 101     |                            | N    | N    | *Will not implement in emulator* |
| Remote Test Exit                     | 036 102     |                            | N    | N    | *Will not implement in emulator* |
| Reverse Video Off                    | 036 105     | Cmd-Brk E                  | Y    | Y    | Inverse Chars off |
| Reverse Video Off                    | 002         | Ctrl-B                     | -    | Y    | Introduced in D210  |
| Reverse Video On                     | 036 104     | Cmd-Brk D                  | Y    | Y    | Inverse Chars on |
| Reverse Video On                     | 026         | Ctrl-V                     | -    | Y    | Introduced in D210 |
| Roll Disable                         | 023         | Ctrl-S                     | Y    | Y    | Turn on 'paged mode' |
| Roll Enable                          | 022         | Ctrl-R                     | Y    | Y    | Turn on normal scrolling |
| Underscore Off                       | 025         | Ctrl-U                     | Y    | Y    |  |
| Underscore On                        | 024         | Ctrl-T                     | Y    | Y    |  |
| Window Home                          | 010         | Ctrl-H                     | Y    | Y    |  |
| Write Window Address                 | 020 n n     | Ctrl-P col row             | Y    | Y    | Move cursor to addr |

Also: Home (010) and Tab (011) which were not considered to be 'commands' in the DG documentation.
