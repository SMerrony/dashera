# Lessons Learnt Moving a Gtk Application from Go to Ada
- [Lessons Learnt Moving a Gtk Application from Go to Ada](#lessons-learnt-moving-a-gtk-application-from-go-to-ada)
  - [Summary](#summary)
  - [Introduction](#introduction)
    - [Caveat](#caveat)
    - [Application Description](#application-description)
    - [Why Port?](#why-port)
    - [Result](#result)
  - [What was Easy?](#what-was-easy)
    - [Program Logic](#program-logic)
    - [Gtk 2 to Gtk 3](#gtk-2-to-gtk-3)
  - [What was Difficult?](#what-was-difficult)
    - [Documentation](#documentation)
    - [Finding Libraries](#finding-libraries)
    - [Examples](#examples)
    - [Change of Mindset](#change-of-mindset)
    - [Debugging tasks](#debugging-tasks)
  - [Traps for the Unwary](#traps-for-the-unwary)
    - [Rewrite, don't Translate](#rewrite-dont-translate)
    - [GtkAda and Tasks](#gtkada-and-tasks)
    - [Goroutines vs. Tasks](#goroutines-vs-tasks)
    - [Channels](#channels)
    - [Error Handling](#error-handling)
  - [Pleasures and Disappointments](#pleasures-and-disappointments)
    - [Pleasures](#pleasures)
    - [Disappointments](#disappointments)

## Summary

## Introduction
This document describes some of my thoughts following the successful transition of a desktop terminal emulator program from Go and Gtk 2 to Ada and Gtk 3.  The rewrite took place over a period of four months beginning in November 2021.

### Caveat
I do not claim to be an expert in anything that follows.  These days (2022) programming is a hobby for me and all the work described here was undertaken as a 'spare time' project.  This is an 'opinion piece' - I am not going to back up every statement with references!

### Application Description
My DASHER terminal emulators provide free, open source and modern emulations of the most commonly used types of terminal (D200 and D210) that were connected to Data General minicomputers of the 1970s through to the end of the century.

Having both serial and telnet support, the emulators can be connected both to legacy hardware and modern emulations of that hardware.

The first in this series of terminal emulators was written in C++ with a Qt GUI, then came a Java version, and thirdly a Go version using a rather nice Gtk 2 package (binding).

### Why Port?
DasherG was first released in 2019 but by the end of 2021 it had already become difficult to build and maintain.  This was mainly because the GUI toolkit used had become unmaintained leading to significant dependency difficulties.  Also, Gtk 2 is nearing the end of its life.

There appeared to be two options: either find a new GUI toolkit and reuse the guts of the code, or switch to a language with better GUI support.

Unfortunately, Go does not have a good 'story' regarding GUI libraries.  There is no officially-supported library and some of the better-known ones are either very idiosyncratic ('opinionated' in Go parlance) or immature.  I did have a serious try with Fyne and although it's promising it suffers both of the aforementioned problems.  

It seemed that switching to a language with better GUI support would make sense, and I had some recent experience learning Ada and porting some non-GUI Go code to Ada.

Also, it was clear to me that I needed a language, toolkit, and bindings which were likely to be stable for some years.  Ada, Gtk 3 and GtkAda seem to fit the bill nicely.

After following the evolution of Go for many years I feel that the core developers are abandoning the idea of it being a truly general purpose language and shifting their focus to being an excellent web-services/netops domain-specific language.

### Result
DasherA v0.11 written in Ada is believed to be functionally equivalent to DasherG v0.10 - with some minor improvements.  Performance and memory consumption of the two applications is similar.

DasherA is now easy to build and maintain on modern Debian-based systems.

As usual, a rewrite of the code (which had some legacy cruft left over from its previous incarnations) gave an opportunity to improve the structure of the program a little.

Before starting, I guesstimated a timeframe of about six weeks for this, in fact it took four months.  However, the time I have available for projects such as this varies wildly.  I think this effort would have taken about four weeks if I was working on it full-time.

## What was Easy?

### Program Logic
Most of the actual terminal emulation logic is contained the `terminal` sources in both DasherG and DasherA. A quick glance and `terminal.go` and `Terminal.adb` will reveal that they are strikingly similar.

### Gtk 2 to Gtk 3
Although DasherG used a number of the features that changed significantly between Gtk 2 and Gtk 3, the move was made much easier due to the excellent general information available on-line. 

## What was Difficult?

### Documentation
The Go developers and community have done a very good job of building excellent documentation facilities and practices into the language and packages (libraries) right from its inception.

Unfortunately the same cannot be said for Ada.  

I think there are tools somewhat akin to `godoc`, but there are _several_ of them - each using different conventions for creating and accessing any generated documentation.

It was frustrating to come across some Ada packages which seemed to have very good documentation embedded in the code, but no instructions regarding how to generate that documentation in a readable form.

Ada has been around for a long time; unfortunately it is not always clear to the unwary reader to what version of the language online documentation is referring.  This caught me out when I read somewhere about the Ada `Character` type being either only-displayable characters, or seven-bit (I forget which).  This may have been true in some old version of Ada, but nowadays the `Character` type includes all 8-bit values.  I spent some time working around a restriction that does not apply, then throwing away that work.

Similarly, a lot of the Gtk examples and documents on-line actually refer to Gtk 2 - not Gtk 3 (or 4!).

### Finding Libraries
Allied to the above point, it was not straightforward to find the serial I/O, networking and other similar packages.  There does not seem to be a generally agreed-upon central repository for such information in Ada.

### Examples
It was a real struggle to find useful publicly available example code for some of the packages used in DasherA.

It is salutary to compare the number of code examples on `rosettacode.org` for the two languages.  At the time of writing there are 1460 Go examples and 865 Ada ones; shocking when one considers how relatively recently Go was introduced.

A similar story applies to GitHub - according to their API there are 967876 Go repositories and just 4535 Ada ones as of 8th March 2022. 

### Change of Mindset
I would characterise Go as a simple language, and Ada as rich one.

However, Go's apparent simplicity hides inevitable complexity when doing anything non-trivial with Goroutines.  Contrariwise, Ada's complexity can blind one to the simplicity of such things as `protected` types and task `entry` points.

Both Go and Ada seem to me to take an agnostic approach to object-oriented programming.  In both languages there are occasions when you can ask yourself whether a type is really an object.  When moving code over be clear about this, and stick to your decision!  (There is a little work outstanding in DasherA removing some unnecessary 'objects'.)

Also, see the Channels section below.

### Debugging tasks
Urgh!  I need to investigate this further;  `gdb` seems to be quite inadequate in this regard once multiple tasks are involved (cf. the excellent `delve` debugger for Go).

## Traps for the Unwary
Always ensure you are reading up-to-date, reliable documentation - if you can find it.

### Rewrite, don't Translate
When faced with a lot of code to move it is tempting to try to translate between languages on a token-by-token basis.  This is rarely a good idea.  Take a step back, consider what each package, type or func does, then reimplement it using Ada idioms.  (I got better at this as this project progressed, I should revisit some of the first code that was ported.)

### GtkAda and Tasks
It turns out that any time you use ANY Glib, Gdk, or Gtk entity, you MUST either be in a callback, or be inside a `Gdk.Threads.Enter; ...  Gdk.Threads.Leave;` block.  This includes such innocent-seeming things as `Glib.Error`, `Gdk.Types.Keysyms`, and even `Glib.Guint`.

Failure to observe the above rule leads to very strange behaviour - usually resulting in a crash.  Any error messages produced may not be helpful in pointing you to the underlying cause.

I think the above also applies to entities referred to during Ada *elaboration*.

### Goroutines vs. Tasks
Goroutines are so lightweight and easy to use that it is common to overuse them in Go code.  I believe that in GNAT Ada, tasks are implemented as O.S. threads - so they are less lightweight.  

When moving code from Go to Ada always consider whether a task is really needed.  Using fewer Goroutines/tasks may well result in simpler code - despite the seductiveness of Go's facilities.

Having written this, I can now see at least one remaining task in DasherA that should probably be rewritten as a `protected type`.

It is a constant surprise to me that GNAT Ada appears to lack easy-to-use equivalents to Go's race detector and deadlock reporting.  For a language that places so much emphasis on tasking, the lack of these modern tools feels like a large omission.

### Channels
I wasted time recreating Go-like channels in Ada.  Things became much simpler when I had the realisation that *Go's channels are in fact surprisingly analogous to Ada's task entries*.

### Error Handling
I think it is widely acknowledged that error handling is a weak spot in Go.  Unfortunately this can lead to habits that are hard to shake off, even when given the benefit of exceptions in Ada.  Code is likely to need to be consistently restructured once you have worked out your exception handling strategy.

## Pleasures and Disappointments
(These things matter to me as a hobby programmer: it is important to me that programming does not become a trial.)

### Pleasures
* Easy compiler and GUI toolkit installation on Mint Linux using standard package management
* Good task control in Ada
* Fast build times (feels nearly as quick as Go)
* Good performance - even prior to any optimisation
* Language support in VS Code
* Excellent (C-based) documentation and tutorials for Gtk 3
* Data structures are a joy in Ada - the better-constrained types just feel 'right'
* Protected types are a lot easier to deal with than mutexes in Go.
  
### Disappointments
* Documentation...
* Needing to have `/usr/lib/gcc/x86_64-linux-gnu/9/adainclude/` and `/usr/share/ada/adainclude/gtkada/` open at all times to refer to the `.ads` files in those directories
* Lack of real-world GtkAda code examples
* Poor Ada task debugging under `gdb`
* Lack of deadlock and race condition detection (they are built-in with Go)

License: Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)

©2022 Steve Merrony

