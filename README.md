# h2
Unified *Haskell Heroes* repository 🌍

## Quick start
*Also see: [Platform remarks](#platform-remarks).*

First, install [Git](https://git-scm.com/), [Git LFS](https://git-lfs.github.com/) and [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

Then:
```sh
git clone https://github.com/tempname11/h2
cd h2
git submodule init

git submodule update
# this might take some time (and disk space), since one of the submodules is a binary asset LFS repository.

./hhcli stack-native build
# this WILL take a fair amount of time. Stack will typically download and install GHC and all the dependencies at this point.
```

### Preparing the assets
```sh
mkdir .production-assets
mkdir .production-assets/Cursors
ln -s ../h3-assets/Sounds .production-assets/Sounds
ln -s ../pkg/heroes/glsl .production-assets/glsl
./hhclix prepare-assets
./hhcli run-tool compile-essentials
./hhcli run-tool compile-cursors
```

### Running the native version
You will need [SDL2](https://www.libsdl.org/), [SDL_mixer 2.0](https://www.libsdl.org/projects/SDL_mixer/) *with mp3 support*, and [SDL_image 2.0](https://www.libsdl.org/projects/SDL_image/) installed on your system.

```sh
./hhcli run-native
```
Here's what you'll hopefully see:
![screenshot](https://i.imgur.com/JNxfE5Z.jpg)

#### Bonus: web stuff
```sh
./hhcli build-web
```
This will currently produce a broken build, because of a GHCJS bug. If you're feeling brave, see [this](https://github.com/ziocroc/Ombra/blob/master/ghcjs-rts-bug.patch) and [this](https://github.com/ziocroc/Ombra/wiki/Installation) for a description of an identical problem and a hacky solution.

```sh
./hhcli prepare-web
./hhcli run-web
```

Finally, visit localhost:8000.

## Platform remarks
The instructions have been written and tested on Mac OS, but should hopefully be somewhat reproducible on Windows and Linux. All of the dependencies are cross-platform.

### Mac OS
[Homebrew](https://brew.sh/) works for all of the dependencies. Most are straight-forward.

Currently, on Mojave, you need to install SDL2 from *HEAD*:
```sh
brew install sdl2 --HEAD
```

### Windows
Here be dragons. Be the first and tell about your experience!

### Linux
See above.
