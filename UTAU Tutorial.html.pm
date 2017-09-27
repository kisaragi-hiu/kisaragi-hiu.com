#lang pollen/markup
◊(define (screenshot path) (figure path #:width "45rem" path))

◊headline{UTAU Tutorial by Kisaragi Hiu}
◊define-meta[language]{en}
◊;define-meta[publish-date]{2017/09/26}
◊;define-meta[categories]{UTAU}
◊define-meta[toc]{true}

Welcome to this UTAU tutorial. This is part 0, the Setup.

I'm going to assume you know Kana and some Japanese already, 'cause I'm not going to explain Japanese in this series. If you don't, most of this should still make sense; I'd say utau is actually a good way to learn Kana. Anyways.

◊section["UTAU?"]
UTAU is a voice synthesizer platform. It is made up of an editor, render engines, voicebanks, and plugins.

◊section["Installation"]
Because of encoding issues, UTAU has to be installed using the Japanese locale. There are two ways to do this:

◊subsection["Option 1: Change system locale"]
To switch system locale, go to ◊(→→ "Control Center" "Language" "Change system locale" "Japanese(Japan)" "OK") then reboot when it asks you.
◊;figure["/images/utau/system-locale.jpg" #:width "45rem"]{Change system locale}

◊subsection["Option 2: Launch UTAU using Locale Emulator"]
Download Locale Emulator from ◊link["http://pooi.moe/Locale-Emulator/"]{its homepage}, extract to a folder (the install will depend on this folder), ◊(→→ "LEInstaller.exe" "Install / Upgrade"), and you should see Locale Emulator available as an option when you right click on a program. ◊(→→ "Right click on UTAU installer" "Locale Emulator" "Run in Japanese").
◊;figure["/images/utau/locale-emulator.jpg" #:width "45rem"]{Use Locale Emulator}

After this just install like a normal Windows application.

◊subsection["Installation on Linux"]
Create a new 32 bit wineprefix, and install utau in there.
If version 0.4.18 fails to install, use 0.4.16. I don't know why; it happens; it doesn't matter.
◊code{
env WINEPREFIX=$HOME/.wineprefix/UTAU WINEARCH=win32 wine utau0418e-inst.exe
}

To run, do:
◊code{
env WINEPREFIX=$HOME/.wineprefix/UTAU wine $HOME/.wineprefix/UTAU/drive_c/Program\ Files/UTAU/utau.exe
}

Wine might install a desktop entry in your launcher as well, if that's available you can start UTAU from your launcher.

◊section["Voicebanks"]
Voicebanks are sound definition files of a character's voice. A character with a UTAU voicebank is also called an UTAUloid. I'm going to use Defoko, Xia, and Uzuki in my demo; I don't know about English VBs.
UTAU ships with Defoko by default. I'll demonstrate the UI with her voice for now. We'll look into other voicebanks later.

Now start UTAU, and you should see this screen.
◊screenshot{/images/utau/screenshot-first-start.png}

Contratulations, UTAU is now set-up!

◊section["Note basics."]
An UTAUloid sings by combining "notes" together.
◊figure["/images/utau/screenshot-notes.png" #:width "45rem"]{Notes and Piano Roll}

Notes are arranged on a piano roll. To add a note, click the pen icon (1) and click on the piano roll. To select a note, click the select icon next to (1) and click on a note; selected note(s) are highlighted in red (2).

Each note has its own properties. To open the properties of the selected note(s), press ◊key{Ctrl-e}. Alternatively, it's in the menu ◊(→→ "Edit" "Region Property").
◊figure["/images/utau/screenshot-note-property.png" #:width "45rem"]{Note property window}

There will be an arrow saying "Show": click it to unhide the other properties.

◊subsection{Lyric, Note, Length}
Basic properties of the note.

◊subsection{Intensity}
Basically the volume for the entire note.

◊subsection{Modulation}
Each word corresponds to some voice sample of the voicebank, which will have a bit of pitch instability. A modulation of 0 completely smooths that out, while a modulation of 100 leaves that pitch fluctuation in.

Notes also have two other types of properties: Envelope and Pitch Curve.

◊subsection["Envelope"]
This is basically an intensity / volume curve, along with two parameters saying how much earlier this note should start playing (Preutterance), and how much it would overlap with the previous note. This creates a cross-fade between two notes and actually allows joining them together into one sound.

◊subsection["Pitch Curve"]
This is a curve determining what key a note should be on.
There are two modes for this, Mode1 is a curve relative to its key in the basic properties; Mode2 is a freeform curve. Just use Mode2: it visualizes the pitch curve and allows direct edits on the piano roll.
◊figure["/images/utau/screenshot-mode2.png" #:width "45rem"]{Mode2, with pitch curve  shown clearly on the piano roll}

◊section["Dump"]

To manipulate a note: S-drag = resize note no matter what, C-drag = resize note relative to adjacent note. Notes' position in time depends on lengths of all previous notes. Overlap has to be done with envelopes, and spacing has to be done with the rest note.

Note properties can be changed with the menu, or use C-e for general properties, C-t for pitch curve settings, and C-y for envelope. C-r inserts a rest note, C-w selects all notes between the first and last non-rest notes, C-a selects all notes. C-x, C-c, C-v work as expected; C-v pastes after the current selected note. C-z and C-S-z are undo and redo, respectively. b is built-in editing tools, eg. Suffix Broker; n is plugin list. Just try every button on the keyboard with/without Ctrl held if you can't remember. Everything is in the toolbar as well.

Project settings: click the region below the utauloid's avatar, where their name is. This window will pop up.
◊figure["/images/utau/screenshot-project-settings.png" #:width "45rem"]{Project settings}

Voice Bank: Select which UTAUloid to use for this project. The drop down contains the list of installed VBs; you can also point to a VB with the second text box.

Rendering Options: Project-wide renderer flags.
Tool1, Tool2: Renderers.

Will be explained when I get to renderers.
