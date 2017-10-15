#lang pollen
◊; Script first, article second
◊(define (screenshot path) (figure path #:width "45rem" path))
◊(define video i)
◊define-meta[livejs]{true}
◊(define-meta justfont #f)
◊define-meta[headline]{UTAU Tutorial by Kisaragi Hiu}
◊define-meta[language]{en}
◊;define-meta[publish-date]{2017/09/26}
◊;define-meta[categories]{UTAU}
◊define-meta[toc]{true}

This is the script for my introduction to UTAU.

Welcome to my UTAU tutorial.
◊;video 0: UTAU singing. Use 夏の空と君の傘下で's video.

◊; Assumption
◊;writing-tip{Assume viewer knows Japanese.}
I'm going to assume you know Kana and some Japanese already, 'cause I'm not going to explain Japanese in this series. If you don't, most of this should still make sense; I'd say utau is actually a good way to learn Kana. Anyways.

◊section["UTAU?"]
◊;writing-tip{Summary of UTAU.}
UTAU is a voice synthesizer platform. It is made up of an editor, render engines, voicebanks, and plugins.
◊;video 0: editor -> UTAU interface
◊;video 1: render engines, VBs -> project properties
◊;video 2: plugins -> plugin list

◊section["Installation"]
Because of encoding issues, UTAU has to be installed using the Japanese locale. There are two ways to do this:

◊subsection{"Option 1: Change system locale"}
This will effect the entire system. If you don't mind seeing Japanese occationally on your system, this is my recommendation.

To switch system locale, go to ◊(→→ "Control Center" "Language" "Change system locale" "Japanese(Japan)" "OK") then reboot when it asks you.
◊;figure["/images/utau/system-locale.jpg" #:width "45rem"]{Change system locale}
◊;video 3: system locale switching

◊subsection{"Option 2: Launch UTAU using Locale Emulator"}
This will require launching UTAU with Locale Emulator everytime you use it. If you don't want to change system locale, this will be the best way.

Download Locale Emulator from ◊link["http://pooi.moe/Locale-Emulator/"]{its homepage}, extract to a folder (the install will depend on this folder), then run ◊(→→ "LEInstaller.exe" "Install / Upgrade"), and you should see Locale Emulator available as an option when you right click on a program. ◊(→→ "Right click on UTAU installer" "Locale Emulator" "Run in Japanese").
◊;figure["/images/utau/locale-emulator.jpg" #:width "45rem"]{Use Locale Emulator}
◊;video 4: downloading and setting up locale emulator

After this just install like a normal Windows application.
◊;video 5: install UTAU itself

◊subsection{Installation on Linux}
Create a new 32 bit wineprefix, and install utau in there.
If version 0.4.18 fails to install, use 0.4.16. I don't know why; it happens, and it doesn't matter.
◊code{
env WINEPREFIX=$HOME/.wineprefix/UTAU WINEARCH=win32 wine utau0418e-inst.exe
}

To run, do:
◊code{
env WINEPREFIX=$HOME/.wineprefix/UTAU wine $HOME/.wineprefix/UTAU/drive_c/Program\ Files/UTAU/utau.exe
}

Wine might install a desktop entry in your launcher as well, if that's available you can start UTAU from your launcher.

◊;video 6: install on linux

Start UTAU, and you should see this screen.
◊screenshot{/images/utau/screenshot-first-start.png}

◊;Debug encoding issues here.
If you see weird question marks, there are encoding issues. Make sure you either launch UTAU using Locale Emulator, or have set system locale to Japanese. The default voicebank may be corrupted; run the installer again with locale set up to get it back.

◊section{Piano Roll}
◊describe{The grid part of the interface} is called the Piano Roll.
On the left, the vertical piano keyboard marks what pitch each note is on.
◊; Show left piano as mouse moves across piano roll
The current pitch your mouse cursor is on is shown in a highlighted color.

◊section{Notes}
◊figure["/images/utau/screenshot-notes.png" #:width "45rem"]{Notes and Piano Roll}
To make an UTAUloid sing, add notes onto the piano roll. Do this by clicking the pen button (1), then clicking on the piano roll. Play it back with the play button.
Alternatively, type lyrics into the Lyric field, and press the pink "Insert Lyrics" button.

A song is made up of notes; every note each has its own properties. To edit a note's properties, click on it to select it (2), then press ◊key{Ctrl+E} for "edit". It is also in the menu, ◊(→→ "Edit" "Region Property").

◊section{Basic Note Properties}
In the note properties window, there's an arrow saying "Show" (1). Click it to unhide a few other more advanced properties.
◊figure["/images/utau/screenshot-note-property.png" #:width "45rem"]{Note property window}

Lyric, Note, Length: self-explainatory.
These can be edited on the Piano Roll: Lyric by double-clicking on a note, "Note" (I call it pitch to avoid confution) by moving the note up and down, and Length by dragging the edges of the note.
If a note isn't changing size when you drag its edges, hold down ◊key{Ctrl} to shrink the adjacent note as you expand this, or ◊key{Shift} to resize while pushing everything further.
◊; Video works better here.

Intensity is the volume for the entire note, sort of. It can be visualized on the Piano Roll using the "~" button (2).

Consonant Velocity is how fast a note plays. Increasing the speed of a note makes the consonant sound shorter, hence the name. The consonant is two times longer at a value of 0 (note plays at 50%), and gets shorter the higher this number is. A value of 200 is half the length for the consonant.

◊section{Rest Notes}
In UTAU, every note depends on the previous for their position, meaning there needs to be a rest note to fill in a gap. You can think of it as there being a force of gravity to the left, and they can't just float without the rest notes.
A rest note is added with ◊key{Ctrl-r}.

◊section{Tuning practice}
We'll start by tuning a simple song. I'll use Twinkle Twinkle Little Star as an example. (Thanks, Fukumennkei Noise.)

In the interface, the usual ◊key{Ctrl+z}, ◊key{Ctrl+x}, ◊key{Ctrl+c}, ◊key{Ctrl+v} (Undo, Cut, Copy, Paste) keys work here. Redo is ◊key{Ctrl+Shift+z}.

Here's the Japanese lyrics for it:

きらきらひかる　おそらのほしよ
まばたきしては　みんなをみてる
きらきらひかる　おそらのほしよ

We need to place down notes, resize them to the right timing, move them to the right pitches, and fill in the lyrics. The order doesn't matter.
◊; video: do that.

◊(image "./images/utau/screenshot-tuning1-placing-notes.png" "Place down the notes with lyrics")
◊(image "./images/utau/screenshot-tuning1-adjust-pitch.png" "Adjust the pitch for each note")
◊(image "./images/utau/screenshot-tuning1-timing.png" "Adjust timing of each note")

Describe voice bank.
Describe renderers.

Describe CV, VCV, CVVC.
Tuning demonstration.
Describe plugins.

◊section{CV}
Constanant, Vowel.
"ka" "nu"
Kana → Note.

◊section{CVVC}
CV + v c
"ka" "a n" "nu"
Kana → Note, insert VC

◊section{VCV}
Vowel, Constant Vowel
"- ka" "a nu"
Kana → Note → autocvvc

◊section{Plugin: Autocvvc}
Convert between CV, VCV, CVVC

◊section["Dump"]

To manipulate a note: S-drag = resize note no matter what, C-drag = resize note relative to adjacent note. Notes' position in time depends on lengths of all previous notes. Overlap has to be done with envelopes, and spacing has to be done with the rest note.

Note properties can be changed with the menu, or use C-e for general properties, C-t for pitch curve settings, and C-y for envelope. C-r inserts a rest note, C-w selects all notes between the first and last non-rest notes, C-a selects all notes. C-x, C-c, C-v work as expected; C-v pastes after the current selected note. C-z and C-S-z are undo and redo, respectively. b is built-in editing tools, eg. Suffix Broker; n is plugin list. Just try every button on the keyboard with/without Ctrl held if you can't remember. Everything is in the toolbar as well.

Turn on the option, then you can insert lyrics if there's only one note selected with enter. ◊(→→ "Enter" "type" "Enter")

Space to play.

Project settings: click the region below the utauloid's avatar, where their name is. This window will pop up.
◊figure["/images/utau/screenshot-project-settings.png" #:width "45rem"]{Project settings}

Voice Bank: Select which UTAUloid to use for this project. The drop down contains the list of installed VBs; you can also point to a VB with the second text box.

Rendering Options: Project-wide renderer flags.
Tool1, Tool2: Renderers.

Will be explained when I get to renderers.
