#lang pollen
◊define-meta[title]{Working around Steins;Gate's input problem on Linux}
◊define-meta[date]{2018-01-15T13:51:00}
◊define-meta[language]{en}
◊define-meta[category]{Linux}

Steins;Gate was released on Steam on September 9, 2016, for Windows only. A few months later, as far as I can remember, Wine started being able to run it.

However, for the entirety of 2017, I haven't been able to play it: mouse selection doesn't work at all, menu entries are selected then deselected immediately without my input, the menu is completely unusable. It's almost as if some input is being spammed…?

◊video/gif-esque["/static/sg-spammed-key-showcase.mp4"]

I found the culprit just last week. There's something wrong with Wacom tablet handling in this game, and unplugging the tablet before starting it completely removed the problems. I can't believe I haven't tried this for the longest time…!

Now, time to actually finish reading it when I have the time…
