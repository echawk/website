Here's a brief tour of my Linux/NIX setup. (Plus some software recommendations.)

If you just want my dotfiles, they are available [here](https://github.com/echawk/dots).

# distro

[Kiss Linux](http://kisscommunity.org/), honestly there isn't anything better.
I remember stumbling on it all the way back in early 2020, however I didn't
start using it until mid 2021 (when I had enough time to go through configuring
the kernel for the first time). I like the distro *so* much that
[I](https://github.com/echawk/kiss-personal)
[maintain](https://github.com/echawk/kiss-xorg)
[multiple](https://github.com/echawk/kiss-tex)
[repos](https://github.com/echawk/kiss-repos)
[for](https://github.com/echawk/kiss-java-boot)
[it](https://github.com/echawk/kiss-spec).

If you want absolute control over your system, along with the best port system,
there really is no other option.

## honorable mentions

If there was a world where kiss did not exist, and my tolerance for reading
random wiki articles was higher, I would likely use
[gentoo](https://www.gentoo.org/), since I am fully on the source-based distro
train.

For binary distros though, [Void](https://voidlinux.org/), [Artix](https://artixlinux.org/), and [Guix](https://guix.gnu.org/) are promising alternatives. Just
try to avoid [systemd](https://nosystemd.org/) & [Red Hat](https://without-systemd.org/public/gnomeasia2014.pdf) when you can :)

The *BSDs are also a good option!

# editor

[Emacs](https://www.gnu.org/software/emacs/) is far and away the best text editor
I have used. ~~I do need to mention that I do use Vim bindings~~, since I grew up
on vim, as I've been using it since I was 15. I chose to move over to Emacs
after I grew frustrated with how tedious & arcane it was to develop my own plugins.

While I did use vim (evil) bindings for the first couple years of using Emacs,
I've now moved over to using the vanilla keybindings for a multitude of reasons.
The bindings are not that bad once you get used to them, and more of Emacs will
*just work* once you switch to them.

Emacs isn't really an editor in the traditional sense. It, like lisp, changes
the way that you think about interacting with your computer. Much of life
on the computer can be navigated through text buffers.
[Emacs optimizes for this way of working](https://youtu.be/urcL86UpqZc?feature=shared&t=210).

If you'd like to check out my configuration for Emacs, it's available
[here](https://github.com/echawk/dots/blob/master/emacs/.config/emacs/init.el).
I may add some articles detailing specific parts of my configuration (since
I think they would be useful to a more general audience).

## honorable mentions

[neovim](https://neovim.io/) - I used to use this way back in the day, and I
even still have [a barebones config](https://github.com/echawk/dots/tree/master/neovim/.config/nvim) still kicking around.
I ended up leaving neovim because of the community, not the lack thereof, but
the high volume of medium quality, re-inventions of the wheel.

# windowing system

### For X11:

* [stumpwm](http://stumpwm.github.io/)
* [EXWM](https://github.com/emacs-exwm/exwm) (if using Emacs)

I personally use both of these window managers, however each has a slightly
different use case. I use stumpwm on my main laptop/development machine,
since I will have quicklisp and all of the dependencies already installed.
EXWM is a better choice on binary distros, since nearly every Linux distro has
Emacs packaged in their repos.

### For Wayland:

* [dwl](https://codeberg.org/dwl/dwl)

If Wayland works for your machine, and you're on the Wayland train,
dwl is one of the better compositors out there. It's pretty straight
forward to configure (much like dwm).

## honorable mentions

### For X11:

I have heard good things about [xmonad](https://xmonad.org/), which I hope to try out in the
near future, and I have historically been a [dwm](https://dwm.suckless.org/) user.

### For Wayland:

[mahogany](https://github.com/stumpwm/mahogany), would be the recommended
Wayland display manager, if it were a bit farther along in it's development.
It's really just waiting for a lisp hacker to get frustrated enough with X11
screen tearing before it'll be amazing.

# programming language

I tend to use whatever is best suited to solve the specific problem.

If I *had* to pick a favorite, I'm definitely partial to the Lisp
family of languages.

## recommendations

If you're looking for languages to learn, there really aren't any "bad" options.
If anything, languages really are only a vessel to learn the embedded paradigms
that they are well suited to express. So I would pick any language who's
general paradigm is different from the current paradigms that you know already.

If you're a C programmer, try out ruby. Or if you know Java, try out Clojure.
The real goal is to expand your breadth of knowledge and increase the different
models of thought that you can use to solve problems.

Here is a undoubtedly "controversial" list of paradigms with an associated language.

* Imperative: C
* Object-Oriented: [ruby](https://www.ruby-lang.org/en/)
* Concurrent: [Erlang](https://www.erlang.org/)
* Functional: [ML](https://smlfamily.github.io/)/[Lisp](https://llthw.common-lisp.dev/)
* Meta: [Lisp](https://racket-lang.org/)/[Forth](https://forth-standard.org/)
* Logic: [Prolog](https://www.metalevel.at/prolog)

The controversy is only due to PL people being very opinionated as to what the
ideal language to learn each of the paradigms is. The more important aspect
of programming languages is to learn the paradigms and semantics, and the
syntax will come more naturally. Ultimately syntax errors will get caught by the
compiler anyways :)

Ultimately it's more important to learn how to *program* than to learn any
programming language. The ideas transcend the languages that they
are expressed in.

I'd also say that I am *entirely uninterested* in languages that have no clear
[bootstrap chain](https://guix.gnu.org/en/blog/2023/the-full-source-bootstrap-building-from-source-all-the-way-down/),
and ones that have heaps of syntax. This stems from
*[reflections on Trusting Trust](https://www.cs.cmu.edu/~rdriley/487/papers/Thompson_1984_ReflectionsonTrustingTrust.pdf)*,
as well as my general aversion to the over-engineering of *typically* simple
concepts. You can imagine what languages this reasoning applies to.

I'm almost certain to expand on this further in a future blog article...
