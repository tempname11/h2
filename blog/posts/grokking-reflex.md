# Grokking some Reflex

*Disclaimer: this is one of the first blog posts. I stopped writing it midway, then resumed around a week later, and rushed to finish it. Quality isn't that good; some technical details might not be right, and it ends pretty abruptly.*

[Reflex](https://github.com/reflex-frp/reflex/) is an ungodly beast. The API is utterly huge, complex, and full of weird types and classes. What on earth is all that stuff *for*?

At least, those were my thoughts, when I first made an effort of a deep dive into FRP. The situation was aggravated by a lack of accessible documentation. The most recent `reflex` version (0.5) is not even on Hackage, and the generated Haddock docs aren't structured in any way that is suitable for learning. There are a few tutorials and video talks about it, but they all seemed to skip over details that were important for me to understand.

It's not that daunting now, but I think I'd like to write down those things I wish were written or said when I started learning it.

An aside — this post is decidedly not about [reflex-dom](https://github.com/reflex-frp/reflex-dom). I didn't even intend to use it in a DOM setting. What I originally set out to do was implementing a UI system for [my game-in-progress](https://github.com/tempname11/h2). The idea was to implement stateful, mouse-based interfaces in an FRP context, and render them with OpenGL/WebGL.

### My observations

The way I see it, it's a way of modelling a changeable graph (or "network") of nodes with associated data in them. There are two entirely different kinds of nodes: `Event`s and `Behavior`s.

`Behavior`s conceptually hold a single value at any given moment of time — and you can always take a look at it (see `sample`). In a way, they could be seen as a mutable ref. And in fact that's how they are implemented in Spider, the "engine" for Reflex. The mutations happen in a controlled way (this is often referred to as "semantics"), making them much easier to reason about: a `Behavior` is either a combination/computation of other `Behavior`s, or it may be a standalone "memory cell" which is only changed once a certain `Event` is occurring.

`Event`s are the counterpart to `Behavior`s. In some regards, they are more like connections between nodes. At a given moment of time, they either occur, carrying some piece of data — or they don't. Some are created and triggered externally, relative to the FRP network, some are purely internal and non-accessible to the outside world. Those typically are combinations of other events.

To facilitate complex logic, `Event`s and `Behavior`s may sometimes form cycles. They may also contain one another — an event of events, an event of behaviors, a behavior of events, etc.

You typically create and run an FRP network from the outside world in `IO` — see the `Reflex.Host.Class` module for some pretty gory details. Typically you would create some `Event`s together with their associated triggers (see `newEventWithTrigger`), `hold` some state in `Behavior`s and generally set up everything in a monadic action, then `runSpiderHost` it.

Then, in an external IO event loop, you `fireEventsAndRead` some values out of the network: the states of `Behavior`s, and whether or not specific `Event` have occurred (and with which value).

### So can you use it for ad-hoc UI?

A bit of background: at my day job I mostly write [React](https://reactjs.org/) applications. I consider React to be one of the best (if not *the* best) absractions for writing UI. It's core principles are beautifully simple and feel very "FP"-ish. UI Elements are first-class values, a combination of a "component" and "props", which, when "rendered" will expand into other Elements, in a way similar to how a pure function would transform a piece of data into some different data — in fact, a function component in React is a just pure (with caveats) function! The expansion, applied recursively, yields a tree of Elements (sometimes referred to as "Virtual DOM", although, as I understand, this term is deprecated in React documentation). This tree corresponds 1-to-1 to what the browser considers the current UI and hence what the user sees.

The system described above depends on the DOM - another, seriously complex, layer of abstraction. Can we cut the middleman and build UI without it? When you think about it, DOM really does not buy us that much: rendering what basically amounts to a lot of rectangles is not complicated. Can FRP help us here?

Let's pretend to be library designers and the application programmer to be our "user". Let's take the React way of describing components as our starting points. What's the equivalent in FRP? The closest thing I can image is a `Behavior Props -> Behavior ElementTree` function. This takes us a long way: now the current state of the UI is *just* a `Behavior`, which we read after firing some `Event`s (see above).

At the first glance stateful components also seem to match up. With the new React API called Hooks you can just say `const [state, setState] = useState(initialState)` in a "pure" (note the quotes) function and be done with it (the old/class-based API isn't all that different feature-wise, just more verbose). All updates of "state" cause re-renders which in the end makes the visible UI change. FRP version? `hold` looks very promising, being a `... => a -> Event t a -> m (Behavior t a)`. Notice how similar it is to `setState`, with just control of how the state changes inverted: contravariant `Event` vs a covariant callback.

The devil is in the many details, though. Some features which are easy in React seem to be very painful in Reflex with this approach: multiple (potentially nested) children, event flow from children, conditional rendering (with unmount/remount semantics for state). And the types start getting hairy real quick. I have around 4 years of on/off experience with Haskell and even so, it just was too much.

I'd really like to write I somehow found a nice soluton at this point, but I just didn't. That's a bit disappointing, but FRP is just a tool in the toolbox. There certainly may be situations where it is useful, and I will be on the lookout for them. Maybe it's just the wrong tool for this particular problem. Maybe I need to choose a different abstraction.

Right now the plan is to ditch Reflex and use something really lightweight a la [imgui](https://github.com/ocornut/imgui). Hopefully, more on that later.

### Useful links

http://docs.reflex-frp.org/en/latest/index.html
http://hexagoxel.de/haddock/reflex-0.5/html/index.html
