# GHCJS performance

A simple fact: the current web build, on my (decent-ish) laptop and in Chrome takes more than 50% CPU core just to run the idle loop. This is clearly not a great situation, and I think it's time to investigate this deeper.

Of course, the philosophical problem is that the program is built using too many layers of abstractions. Haskell, compiled with GHCJS to Javascript, compiled with V8 to bytecode, and only then interpreted into machine code — it's just a bit too deep. Yes, if you're lucky, some bytecode paths are JIT-compiled, but this looks finicky and unreliable, since we don't even directly control the JS source.

Having said that, this project isn't about making the most optimized game. It's an exploration, and as such, the are bound to be some concessions along the way. In order to do it's job, the code just needs to run well enough, on a variety of hardware. The native version arguably already does that, with no effort applied. The web version doesn't, and we need to know why, exactly.

The are a few ways we can approach the problem:

* Find existing materials on GHCJS, its inner workings and performance. There aren't too many, maybe it'll be necessary to dive into the source code.
* Take a look at data structures we are using, and find out what kind of JS they are compiled into
* Profiling. This seems impossible with GHCJS at the moment, but ad-hoc experiments like turning off a piece of functionality and seems what's changed might guide to an answer.

I.

I started with the third option: trying to figure out what is actually taking time in the program. Quickly, by disabling parts of code, I arrived at the conclusion that is was actually drawing the hexes that took a huge chunk of time. The solution was to do less work: between drawing any two hexes, it was only necessary to set some GL uniforms and issue a draw call.

This gave a huge boost already. Getting rid of the uniforms altogether would probably save considerably more time. I also suspect that inlining the GL call wrappers (the code I copied from Ombra has "raw" and "wrapped" versions of all procedures) might lead to gains, since WebGL isn't **that** slow by itself.
