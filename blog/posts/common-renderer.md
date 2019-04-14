# A Common Renderer

Recently, I have (more or less) ported the main game executable from *Native* to *Web*. A bit of clarification: by *Native* I mean a version that is compiled by the bog-standard, normal GHC, using system libraries such as SDL to open a window and draw stuff in it. The *Web* version, however, is compiled by GHCJS, and runs in a web page. *Native* is easier to build, debug and develop generally. However, *Web* is more accessible - if I want to show a feature or a build to someone, it's just a matter of sending a link.

The two version share the same `Main.hs` file and a lot of code in general. There are, however, some differences. One of the big ones is the renderer. The *Native* version historically uses plain SDL2 to render. *Web* wasn't even an idea at the point of implementing that. However, with *Web*, either Canvas API or WebGL were the only viable options. Since **a)** WebGL's performance could be much better and **b)** it shares a lot of similarity with plain OpenGL, it was the better choice.

So when porting, I had to implement the same rendering functionality in WebGL. It's not an exact port at this moment: things like colored outlines of sprites are missing. However, it's good enough; fixing those little things is relatively straightforward. Instead of doing that straight away, I decided first to migrate the *Native* version to OpenGL, reusing as much code as possible.

The rest of this post will be written incrementally as I do this, documenting the non-trivial problems and things of note along the way.

## I

The first step was moving the `Web.Drawing.*` modules to `Heroes.Drawing.*`, fixing all the web-related dependencies along the way. This was relatively painless, mainly thanks to the use of nullary type classes (see GHC's `MultiParamTypeClasses`). That topic probably deserves it's own post but I'll describe it briefly. Suppose you have a module with code like this:

```Haskell
data SomeType = _
someProcedure :: SomeType -> IO ()
someProcedure = _
```

...and you would like to use a different implementation in different circumstances? By declaring...

```Haskell
class SomeCapability where
  data SomeType
  someProcedure :: SomeType -> IO ()
```

...you gain the ability to use the same "interface" while not requiring an "implementation" immediately. You can still use `SomeType` everywhere as a normal (albeit opaque) type, and using `someProcedure` usually will require a `SomeCapability` constraint around the use site.

In this project, whenever I encounter a piece of functionality that, conceptually, I want to share between *Native* and *Web*, I put it into a nullary type class (`GLES` and `Platform` mostly, as of now). This enables me to write the instances in completely separate (Cabal) libraries. Which really is necessary since *Native* code wouldn't compile in GHCJS and vice versa!

## II

Although it wasn't strictly necessary, I decided to keep both the old SDL2 renderer and the new GL renderer together in the codebase for a while. At the very least, I find that having to solve this kind of problem helps overall program architecture. By the very necessity of choosing a suitable abstraction which can hide away both implementations, you are forced to think about the structure of the program in a bit more disciplined way. The program becomes a bit more conceptually clear, less ad-hoc.

There are limits to this, though. Choosing the wrong abstraction can be worse than having none at all. It must "support its own weight", so to speak.

In this case, I decided to factor out all functionality related to the graphics pipeline. It's actually deeper than just rendering — the assets are stored and hence loaded differently as well. I dubbed this the `GFX` module. Soon, `WND` followed — all the code for setting up a "window", doing input- and cursor-related tasks. `SND` for sound like seems a good idea as well. 

## III

Finally I had a `GFX` typeclass and a `Web.GFX'GLES` instance for it. The next step was moving this code to the base platform-independent package. This mainly involved getting rid of GHCJS-related dependencies, again using the nullary typeclass trick.

An aside: when you don't have hard time constraints, refactoring one thing can often lead to another and another - you touch them already, so why not redo them properly. This can be both a blessing and a curse. On one hand you can do a lot of "damage" (in a good sense), on the other you take more time than you expect.

## IV

*Note: this section was written a few months afterwards.*

Overall, this endeavour was a success. Code is better, clearer and more modular, most of it is shared between platforms, the game runs adequately in both while looking exactly the same. GHCJS brings new challenges (partcularly performance-wise), but I'm confident they are surmountable.

These past few months I've worked on this project on and off, fixing a lot of bugs lately, and the result looks good. Time to describe the GHCJS build in the README. And on to the next milestone!
