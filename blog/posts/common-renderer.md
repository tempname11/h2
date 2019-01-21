# A Common Renderer

Recently, I have (more or less) ported the main game executable from *Native* to *Web*. A bit of clarification: by *Native* I mean a version that is compiled by the bog-standard, normal GHC, using system libraries such as SDL to open a window and draw stuff in it. The *Web* version, however, is compiled by GHCJS, and runs in a web page. *Native* is easier to build, debug and develop generally. However, *Web* is more accessible - if I want to show a feature or a build to someone, it's just a matter of sending a link.

The two version share the same `Main.hs` file and a lot of code in general. There are, however, some differences. One of the big ones is the renderer. The *Native* version historically uses plain SDL2 to render. *Web* wasn't even an idea at the point of implementing that. However, with *Web*, either Canvas API or WebGL were the only viable options. Since **a)** WebGL's performance could be much better and **b)** it shares a lot of similarity with plain OpenGL, it was the better choice.

So when porting, I had to implement the same rendering functionality in WebGL. It's not an exact port at this moment: things like colored outlines of sprites are missing. However, it's good enough; fixing those little things is relatively straightforward. Instead of doing that straight away, I decided first to migrate the *Native* version to OpenGL, reusing as much code as possible.

The rest of this post will be written incrementally as I do this, documenting the non-trivial problems and things of note along the way.

## I

_
