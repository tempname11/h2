# GHCJS performance

A simple fact: the current web build, on my (decent-ish) laptop and in Chrome takes more than 50% CPU core just to run the idle loop. This is clearly not a great situation, and I think it's time to investigate this deeper.

Of course, the philosophical problem is that the program is built using too many layers of abstractions. Haskell, compiled with GHCJS to Javascript, compiled with V8 to bytecode, and only then interpreted into machine code — it's just a bit too deep. Yes, if you're lucky, some bytecode paths are JIT-compiled, but this looks finicky and unreliable, since we don't even directly control the JS source.

Having said that, this project isn't about making the most optimized game. It's an exploration of what's possible, and as such, there are bound to be some concessions along the way. In order to do it's job, the code just needs to run well enough on a variety of hardware. The native version arguably already does that, with no effort applied. The web version doesn't, and we need to know why, exactly.

The are a few ways we can approach the problem:

* Find existing materials on GHCJS, its inner workings and performance. There aren't too many, maybe it'll be necessary to dive into the source code.
* Take a look at data structures we are using, and find out what kind of JS they are compiled into
* Profiling. This seems impossible with GHCJS at the moment, but ad-hoc experiments like turning off a piece of functionality and seems what's changed might guide to an answer.

I.

I started with the third option: trying to figure out what is actually taking time in the program. Quickly, by disabling parts of code, I arrived at the conclusion that is was actually drawing the hex outlines that took a huge chunk of time. There is more than a hundred of them drawn each frame, and it was done very inefficiently. The solution was to do less work: between drawing any two hexes, it was only necessary to set some of the GL uniforms and issue a draw call. This gave a huge boost already.

Getting rid of the frequently-changing uniforms altogether (in favor of WebGL instancing: see [this post](https://github.com/SaschaWillems/webgl/tree/master/webgl2_instancing) as an example) gave more gains, as now there is just one draw call per static object type being drawn.

II.

Next, I decided to set up some experiments on the `vector` package. I use plain ol' `console.log` to take a look at the runtime representation of all kinds of `Vector`s: regular boxed, `Storable`, `Unboxed`, with both `Mutable` and immutable versions. What I found was encouraging: inside `Storable`, under some object-y wrapping (corresponding to Haskell data constructors) were actual JS ArrayBuffers. In case of regular vectors, they were represented as JS arrays. Both mutable and immutable versions looked roughly the same, which wasn't really surprising.

With that knowledge in mind, it then became possible to pass a `Storable.Vector (V2 Float)` directly to `glBufferData`, avoiding intermediate structures and unsafe raw memory poking.

III.

Overall, the performance has increased roughly 3x, with the game running at almost 60fps even at Chrome's 6x slowdown mode, which emulates slower devices. Since all this was just a day's work, I'm sure that future performance bottlenecks, if any, will be solvable. Overall this was a positive experience that has increased my good feelings regarding GHCJS.
