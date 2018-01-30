#lang pollen
    Title: current-setup
    Date: 2018-01-31T01:00:31
    Tags: DRAFT

Super-M = Media keys prefix
  hjkl: prev, vol down, vol up, next
  p: pause/unpause
  s: stop

Super-jk: Unmaximize/Minimize,Maximize

Shift-Super-hjkl: edge snap hjkl
Alt-Super-hjkl: send to desktop hjkl
Ctrl-Super-hjkl: go to desktop hjkl

All hjkl have Left Down Up Right bound as well.

keymap concept, made with xmodmap and function keys
osu.xmodmap:
◊highlight['xmodmap]{
! keymap for osu on a keypad
keycode 80 = KP_Up F31 KP_Up F31
keycode 79 = KP_Home F32 KP_Home F32
keycode 83 = KP_Left F33 KP_Left F33
keycode 84 = KP_Begin F34 KP_Begin F34
! f34 is bound to hold grave for a second in openbox config
}

Super-o: osu! keymap
Super-Shift-o: default keymap

openbox, xdotools, xmodmap, plasma, all mangled together.
Media keys are openbox shortcuts that run xdotool, which are picked up by plasma and does whatever.
osu keymap is a openbox shortcut that runs xmodmap to bind keypad to function keys (specifically F31-34), which are picked up by openbox to run xdotool.
