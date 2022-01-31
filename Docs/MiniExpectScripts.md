# DasherA mini-Expect Scripting

DasherA supports a limited subset of the [Expect](https://en.wikipedia.org/wiki/Expect) scripting language for automating certain types of task on the system to which the terminal emulator is connected.

Only the following commands are implemented...
 * `#` the line is treated as a comment, must be 1st character on line
 * `expect "literal-string"` swallow all responses from the host until "literal-string" is recieved
 * `send "literal-string"` send the "literal-string" to the host, `\n` is sent as a DG newline
 * `exit` signals the end of the script, interaction is returned to the operator (end of script has the same effect)

N.B. The `spawn` command is not implemented; it is assumed that you are connected to a host before you launch the mini-Expect script.

Literal strings must be enclosed in double-quotes (`"`) which are not sent to or expected from the host.

Variables, timeouts, conditions, escape characters (other than `\n`), and all other Expect features not listed above are not supported.

**BEWARE** - this version of Expect is infinitely patient; if you make a mistake in your script, or the host does not respond as expected DasherG will hang and need to be terminated.  When writing scripts the `-tracescript` option to DasherA is your best friend!

## Sample Script
The following script automates the formatting of a disk...
```
# Mini-Expect script for DasherA that formats DPF0 and runs 1 test pattern on it.
# It is assumed that the host is connected and running TBOOT and we are
# at the tape "file number" prompt.
send "2\n"
expect "NEW LINE)? "
send "F\n"
expect "name? "
send "DPF0\n"
expect "27] ? "
send "\n"
expect "name? "
send "\n"
expect "area? [Y]  "
send "\n"
expect "23420]  "
send "\n"
expect "[] "
send "DPF0ID\n"
expect "characters)? [] "
send "DPF0NAME\n"
expect "15 characters)? "
send "+\n"
expect "LINE)? "
send "RE\n"
expect "15 characters)? "
send "\n"
expect "analysis? [N] "
send "Y\n"
expect "number? "
send "1\n"
expect "number? "
send "\n"
expect "like to run? "
send "1\n"
# the analysis takes some time...
expect "done): "
send "\n"
expect "410632] "
send "\n"
expect "620] "
send "\n"
expect "410763] "
send "\n"
expect "176] "
send "\n"
expect "174] "
send "\n"
exit
```

