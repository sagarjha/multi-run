---
layout: default
---

# Miscellaneous Examples #
## Difference between using term and eshell ##
All eshell terminals are run by Emacs (single-process). Each bash emulator, however, is run as a different process. This can matter, for example, when running the command _sl_ (steam locomotive) using multi-run:

[![multi-run-loop](https://img.youtube.com/vi/L1-uOG-n8n0/0.jpg)](https://www.youtube.com/watch?v=L1-uOG-n8n0)

## Different delay commands ##
The following video shows the difference between the two delay commands by playing some music using the _play_ command. If you pause the video at 1 second, you can see the _select-notes_ function that is run with multi-run. Expect some abrupt sound changes.

[![multi-run-loop](https://img.youtube.com/vi/dl4LFx5f63Q/0.jpg)](https://www.youtube.com/watch?v=dl4LFx5f63Q)

## Running on Stampede Supercomputing cluster ##
I was running some experiments on the Stampede supercomputing cluster on up to 128 nodes. Because of the limited resources, my script wasn't able to run successfully. It was hard to debug because the script would run in their batch scheduling environment. In the interest of time, I decided to manually run them in the terminal. The following video shows one run of the program on 64 terminals:

[![multi-run-loop](https://img.youtube.com/vi/dMCzSSc4Gz8/0.jpg)](https://www.youtube.com/watch?v=dMCzSSc4Gz8)

## Game Idea: Synchronous Escape ##
I like the idea of using games to illustrate a concept. The best I can think of is a maze problem where you have multiple mazes and the goal is to exit all the mazes simultaneously by using the same set of movement commands (L, R, U, D). When a movement command cannot be executed in a maze, the player remains in the same position in that maze, but its position in the other mazes can still change. This game can be played (conceptually, not implemented yet) using multi-run with a terminal for each maze and the movement commands communicated by running them through multi-run. I call this game <span style="color:teal"> _**synchronous escape**_ </span>. Even though this is decidable, it is inspired by the undecidable Post correspondence problem.

The following is an illustration of the gameplay, walking through solving one instance of the game:

![]({{ site.baseurl }}/docs/sync_esc_1.png?raw=true)
*The starting position*

All the four players need to simultaneously reach the goal. We first take one step to the right (multi-run "R").

![]({{ site.baseurl }}/docs/sync_esc_2.png?raw=true)
*one step to the right*

After that, they all take one step down.

![]({{ site.baseurl }}/docs/sync_esc_3.png?raw=true)
*one step down*

Notice that the action has no effect on players 2 and 4 because they can't move down.

Similarly, the sequence of steps "RDDLLU" from here will ensure that all players reach the goal simultaneously.

![]({{ site.baseurl }}/docs/sync_esc_9.png?raw=true)
*solved*

<a href="{{ site.baseurl }}/" target="_self">back</a>
