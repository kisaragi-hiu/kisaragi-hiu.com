ABH, or accelerated back hopping, is a movement mechanic used in ◊hl2 speedrunning.

## What does it do?

By doing ABH, the player gets accelerated rapidly backwards, so large gaps in maps can be cleared.

## How is it done?

ABH gives the player more speed as they exceed a soft speed limit, which can be done by simply sprinting and jumping. The simplest form of ABH is done by:
1. Sprint-jump backwards.
2. Release movement keys.
2. Hit jump the moment the player lands, typically with a scrollwheel or script.

## How does it work?

There's a soft speed limit on how fast a player should go. If the player exceeds it, the games tries to slow them down by adding a velocity backwards. However, the way the game determines "backwards" is easily abusable:

- If "back" key is held, assume player's going the opposite direction of the viewport. An acceleration is applied towards where the player's looking.
- If not, assume they're going towards the direction of the viewport. An acceleration is applied opposite to where the player's looking.

By looking at the opposite way you're going, the game incorrectly applies acceleration to where you're already going.

## How to do it more effectively

The game tries to slow you down more if you're crouching. This becomes a bigger value added to your velocity each jump when ABHing. Simply crouch in mid-air to gain more speed when you jump off the ground again.

ABH is also really only about the hopping. No matter how you do it, as long as you exceed the soft speed limit and trick the game into speeding you up instead of slowing you down, you will get accelerated.

1. Exceed the soft speed limit, by Sprint-jumping, previous ABH hops, explosion, or whatever.
2. Jump off the ground while tricking the game, by going backwards without holding "back".
2.1. Crouch to receive a bigger boost.

## AFH, ASH

There are two variants of ABH: ASH for side hopping, and AFH for forward hopping.

### AFH

When hopping off the ground, instead of going backwards without holding "back", go forwards while holding "back". In ◊|hl2|, the player will slow down too much if "back" is always held, so it needs to be tapped right before hopping off the ground.

### ASH

AFH, with view looking towards a side at around 45°, and strafe to the other side always held. The view angle and the extra strafing key sort of cancel out the slowdown caused by holding "back", so both of them can be kept held.

ASH accelerates slower than ABH, though as you can actually see where you're going, it's a lot more used than ABH.

1. Exceed the soft speed limit.
2. Hold "back" and one of the strafe keys. Look to the othet side at about 45°.
3. Hit jump the moment you land.
3.1. Crouch for more boost.

## More

To turn in mid-air, just airstrafe. When doing ASH, simply turning the view is enough since that's already airstrafing. See ◊ref["airstrafing"].