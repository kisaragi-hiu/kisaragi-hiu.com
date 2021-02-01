#lang pollen

◊define-meta[title]{Discovering Racket through making a osu! skin}
◊define-meta[date]{2017-09-13T17:58:00+0800}
◊define-meta[category]{Meta}
◊define-meta[tags ("Racket" "Pollen" "osu")]

In around March this year, I started trying to make my own osu! skin, or at least mix a skin for my own use.
I started with the ◊link["https://osu.ppy.sh/forum/t/454986"]{Emilia (Re:0) skin by Beatstatic}, with a git repo to track my changes, and copied the hitcircles from ◊link["https://osu.ppy.sh/forum/t/300001"]{Clear Skin Ultra 2.4} (I like its hitcircle design a lot).

Then I stumbled across ◊link["https://www.youtube.com/watch?v=Yih6pz09Z1A"]{a font made for the I script in Re:0} in April, and started wanting to learn the script for the sake of it. As part of the practice, I decided why not make osu!'s elements in the Re:0 writing system? So I made a new folder in my mix skin, then opened Blender to make the elements, because that's the only tool I was comfortable with using. (I've since learned to use Inkscape more.)

Making osu! skin elements with Blender is weird. I am putting multiple elements in the same file and adding a different camera for each of them, binding the cameras onto markers, and so I wondered if there's some way to render every marker and maybe name the output with the marker.
After a bit of Googling, I saw an answer by ◊code{p2or} on StackExchange that fits my needs perfectly. (I added the first two comments.)

◊highlight['python]{# by p2or on Blender StackExchange
# https://blender.stackexchange.com/questions/23121
import bpy
import os

# get the scene
scn = bpy.context.scene

# get the output path
output_path = scn.render.filepath

# iterate through markers and render
for k, m in scn.timeline_markers.items():
    frame = m.frame
    marker_name = m.name
    scn.frame_set(frame)
    scn.render.filepath = os.path.join(output_path, marker_name + ".jpg")
    bpy.ops.render.render( write_still=True )

bpy.context.scene.render.filepath = output_path}

With that, I was able to hack together a shell script to render all blend files. Blender also only allows one resolution per scene, so I had to write some post processing functions that resize some images for me, as well as a sort of interface to them through the file names.

Later in June, I decided to also cover the hitcircles and other elements, and eventually removed all assets not made by me. I also purged all of those out of the git repository; in hindsight, I actually regret that a bit. While refactoring that, I also wanted to do something about the ugly shell script, and I started looking around for different languages.

I thought about ◊link["http://xon.sh/"]{Xonsh}, Python, ◊link["https://scsh.net/"]{◊code{scsh}}, the ◊link["https://github.com/michaelmacinnis/oh"]{◊code{oh}} shell, and some others, but:


◊ul{
◊li{◊code{oh}: I don't really understand. At least not yet.}
◊li{Python: I don't like doing shell script-y stuff in Python currently. Previously I had ported my ◊code{randomwallpaper} script to Python, and it didn't feel better than Bash. This is purely my issue.}
◊li{Xonsh: Looks very interesting, but I got stuck trying to figure out how I should parse command line arguments.}
◊li{◊code{scsh}: A full language + some shell-isms, sort of like Xonsh. Project feels kind of inactive though.}
}

So I just gave up and stayed with Bash, and tried to improve the script further; but the "Scheme shell" idea seemed really interesting to me, so I decided to Google for "Scheme shell" and see what I'd get. Racket showed up in the results, and I met Racket for the first time. It actually looks great!

From there I started with the ◊link["http://docs.racket-lang.org/quick/index.html"]{Quick Intro} by Matthew Flatt, and am now trying to learn Racket. Along the way I have seen Pollen, and wrote a self introduction for school in it; and Frog, which I now build this blog with. I'm still very new — I'm not exactly trying hard on anything — but I have definitely stumbled across a very powerful tool.

About the osu! skin, it's now released as ◊link["https://osu.ppy.sh/forum/t/630525/start=0"]{Retome (currently 0.4)} on the osu! forum.
