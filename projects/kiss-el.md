:DATE 2024-02
:TITLE kiss.el
# kiss.el - A package manager in **ELisp**???

Yes, you read the title right. [kiss.el](https://raw.githubusercontent.com/echawk/kiss.el/master/kiss.el) is in fact a
full featured package manager written in Emacs lisp.

```
Q: Why??
A: Because I can ;)
```

But, on a more serious note (as serious as you can be when
discussing linux package managers), I wrote it because I
was not satisfied with the current state(tm) of the kiss
package manager (or linux package managers more broadly, but
that's a separate article).

I'm also hoping to let more novice users understand how the
proverbial "sausage" is made - the explanations here about
how kiss works apply to nearly every other Linux package
manager. They all really do the same thing :/

## The Problem(s?)

The primary gripe that I have with the current implementation
of kiss is simply the difficulty of implementing extensions
onto kiss itself. While it is technically possible to implement
many commands and whatnot for kiss (see [here](https://github.com/echawk/kiss-personal/tree/master/bin)
for some of the ones that I have written over the years) that
doesn't mean that they are as comprehensive as I would like.

What I'm getting at is the lack of an API into kiss, which is
something that *most* Linux package managers have implemented.

While there is a possibility that this [proposal](https://github.com/echawk/kiss-spec/blob/master/proposals/api.md)
comes to fruition, I wouldn't hold my breath. Things are known
to move [glacially](https://codeberg.org/kiss-community/kiss/commits/branch/master)
slow in Kiss Linux land (this is a feature, not a bug)
so having an API that you can use is likely a pipe dream
for the foreseeable future.

However, these problems disappear if I write my own implementation...

Also, upstream has yet to really implement a test suite?

## Why Emacs?

Emacs was chosen because I wanted to write this package manager
in a lisp, and specifically Elisp just to make the vimmers seethe.
The chanting of "bloat!" is also a big plus.

But in actuality though, Emacs was chosen because it runs just about
*everywhere*. For a relatively comfortable lisp (and more importantly,
debug-able), that's pretty nice.

Emacs also has quite a few language features that make implementing
something like a Linux package manager, rather easy.

* Ability to trivially read/write files
* Easy shell commands
* A huge standard library
* Great functions for string manipulation
* An implementation of (part of) CLOS ([eieio](https://www.gnu.org/software/emacs/manual/html_node/eieio/))
* [Pattern matching](https://www.gnu.org/software/emacs/manual/html_node/elisp/pcase-Macro.html)
* A fantastic & readable regex notation [macro](https://www.gnu.org/software/emacs/manual/html_node/elisp/Rx-Notation.html)
* A way to seamlessly override function definitions ([advice system](https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html))

There's likely some other features that I missed in this list, but those
are the ones that really stick out in my mind.

At the time of starting this project, I had been using Emacs for
about a year and a half, and I wanted to improve my Elisp-fu.

Suffice to say that this project has greatly improved my
knowledge of Elisp. [I wouldn't say that I'm an expert, but I have knowledge.](https://youtu.be/mV_daaldE_I?feature=shared&t=610)

### Alternatives

If I could go back, I may have written kiss.el in Common Lisp, had
I been more aware of some of the [libraries](https://github.com/CodyReichert/awesome-cl) that are available over
in CL land. I still may end up porting kiss.el over to Common Lisp,
if time permits, but not anytime soon.

The real advantage with using CL is knowing that once the project
is done, it will likely continue to work well after I'm dead and
gone. Not sure I could same the same about Elisp...

On second thought though, the [Lisp Curse](http://www.winestockwebdesign.com/Essays/Lisp_Curse.html) does seem to impact CL
projects more often than ELisp ones (if we ignore
the constant fracturing between what is in "core" Emacs and
what is relegated to be a 3rd party package. If you know, you know.)

## So what exactly is kiss?

[kiss](https://github.com/kiss-community/kiss) is an extremely simple
package manager (& ports system) that is written in ~2k lines of
POSIX shell. It also happens to be the name of the [Linux distribution](http://kisscommunity.org/)
that uses aforementioned package manager.

I'll give a brief description of the port system here,
but if you are interested in a more thorough explanation, you can
read the beginnings of a specification
[here](https://github.com/echawk/kiss-spec/blob/master/port-format.md).

Essentially, a port is a directory structure with some specifically named files.
Here's an example port directory (from my [xorg](https://github.com/echawk/kiss-xorg) repository):

```
xorg-server/
├── build
├── checksums
├── depends
├── patches
│   ├── fix-crash.patch
│   └── rootless_modesetting.patch
├── post-install
├── sources
└── version
```


The only file(s) missing in the above example are the `pre-remove` file and a
potential `files/` directory.

Each file in the directory serves a specific purpose:

file   | required? | purpose
-------|-----------|--------
build  | yes       | Conducts the build of the package, must be executable.
checksum | most of the time | Holds b3sums of the sources for the port.
depends | no | Contains a list of all of the dependencies for the package.
post-install | no | Executable file that will be executed after the package is installed.
pre-remove | no | Executable file that will be executed before a package is removed.
sources | no | Contains a list of urls/file paths that are the sources for the port.
version | yes | Contains the version and the release of the port.

What's even better is that all of these files follow some standard conventions
which make the life of an implementer very nice:

* Most are written in plain text
* Most are simply new line separated
* There is no need to "evaluate" the build through sourcing the build files (this practice is *way* more common than you'd think.)

This all lends itself to a very simple to grok and extensible system.

Additionally, the only requirement that is made of the `build` file is that
it be marked executable - there's no need to write it in shell!
Admittedly, you most likely *will* write it in shell, but there is nothing
stopping you from, say, writing it in [Common Lisp](https://github.com/echawk/kiss-xorg/blob/master/community/stumpwm/build).

This reduction in complexity makes it pretty trivial to actually perform builds,
as shown [here in upstream](https://github.com/kiss-community/kiss/blob/6228a9310202a9bcdaae21cb356a159273df414f/kiss#L1094).


## Building a port

Surprisingly, the process for building most software is *typically*
pretty straightforward. Maybe not if you're deploying web apps, but
are they *really* writing software? :)

Regardless, the process goes something like this:

* download sources
* validate sources
* extract sources
* build/patch/configure sources
* install software to a temporary install directory
* turn temporary install directory into a tarball

That's really it.

Sometimes software requires some a more [exotic setup](https://github.com/echawk/kiss-personal/blob/master/gnu/guix/post-install)
after it is installed, hence the need for the post-install file,
but generally most software that you use on a day to day basis
follows this simple formula.

It's pretty easy to see how you can start working on each of these pieces bit
by bit.


## Installing a port

This is admittedly the slightly tricker thing to perform. While the simple
"solution" for installing a tarball is just:

```
if package is installed:
  uninstall package
install package
```

However, the issue with this "solution" is that you will remove files that
are currently being used by the system. The solution that kiss uses, which
[apparently Slackware](https://github.com/kiss-community/kiss/blob/6228a9310202a9bcdaae21cb356a159273df414f/kiss#L1605) uses as well, is to do a little dance
with the files.

First, copy the new file to a temporary file on the system (ideally
something that will have a meaningful name), then move the file to
a new


## Future work on kiss.el

I do have some more ideas for kiss.el in the future, some of which are
outlined in the README - while that is a general goal, I do have some others
which are not mentioned.

I'd really like kiss.el to be the package manager that I can bring with me
*everywhere*. While there are systems like [guix](https://guix.gnu.org/) and [nix](https://nixos.org/) which ameliorate
the cross operating system problem to some degree, they really aren't adequate.
I want something that will work on Linux, FreeBSD, OpenBSD, & macOS with
**zero** fuss. Currently the only system that I am aware of that provides
that level of flexibility is [pkgsrc](https://www.pkgsrc.org/).

What's more is that those systems are essentially a soft vendor lock-in.
While this should be fine since both systems are open source, it's still
something to note.


I'd also like to create a more seamless API into kiss.el, instead of
relying on using the '--' functions. I may even try to have it be the
initial implementation of the proposal I mentioned earlier.

Additionally, the tarballs that kiss.el creates are *not* necessarily tied to
using kiss.el exactly. If you know how to install a port, then there really
is no need for kiss at that point, as you could implement the functionality
yourself.

