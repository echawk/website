:DATE 2024-03
:TITLE lesspass-cl
# lesspass-cl - An implementation of lesspass in Common Lisp

## motivation

I really wanted to stop being dependent on other people for my passwords.
Simple as that really - I have been using a lesspass technique for saving
my passwords and I really have enjoyed the process of using it, and it has
been *way* nicer and more convenient than writing all of my passwords down
on paper and keeping them in a folder.

However, what really was the impetus to start working on this library was
the archiving of the [implementation of lesspass](https://github.com/monolifed/lpcli)
that I was using at the time. While not immediately a problem, it does raise
some concerns about the future security of the project.

And while sure, there are other implementations of lesspass out there, such
as [hlesspass](https://github.com/Bannerets/hlesspass), there is something
comforting about using your own code.

## what is lesspass anyways?

Lesspass describes itself as a *stateless* password manager. While this is *mostly*
true, there are some little caveats. Yes, it is true that there is not some database
file (like in keypass, or any of that ilk) that you have to *protect with your life*
to ensure that it stays around. However, it does have state for other things.

Instead of carrying around that database file everywhere, you carry some pretty
trivial information about the password itself in your *brain*.

You need to have the following info stored somewhere in those few cubic centimeters:

* The site itself
* The login
* What character sets that the site uses (one or more of lowercase letters, uppercase letters, digits, and symbols)
* The length of the password
* The password "count" (you can increment this if that password gets compromised in a data breach)
* Your masterpassword

*-Or-* you can write the basic information on *paper* and keep the masterpassword *only* in your head. Choice is yours really.

It's a pretty clever idea, and one that is not even exclusive to lesspass.
There's [spectre](https://spectre.app/) which takes this idea to an even more
minimal extreme.

### the algorithm

Rather surprisingly, I didn't really find a good description of the actual algorithm anywhere
when I was writing this library, I just read the [source](https://github.com/lesspass/lesspass/blob/main/cli/lesspass/password.py)
of the python implementation and went from there. Maybe there is a good description of it, and if
I find it, I'll add it to this section.

However, for other people who are interested, I'll breakdown each component in
pseudo-code:

First is the entropy computation:

```
fn compute_entropy (pass_profile, master_pass) = {
  # Salt string is the site with the login and counter (as hex)
  salt = pass_profile.site \
  + pass_profile.login \
  + pass_profile.counter.as_hex().lowercase()

  bytes = pbkdf2_hash_pasword(
    master_pass.to_bytes(),
    salt.to_bytes(),
    "sha265",
    100000)

  return bytes.as_hex().as_integer()
}
```

This entropy is meant to serve as a "random" number which has entropy "consumed"
throughout the computation.

Naturally, then, we need a way to consume the entropy that we have:

```
fn consume_entropy (password, current_entropy, set_of_chars, max_length) = {
  if (password.length() >= max_length) {
    return password, max_length
  } else {
    quotient, remainder = divmod(current_entropy, set_of_chars.length())
    return consume_entropy(
      password + set_of_chars[remainder],
      quotient,
      set_of_chars,
      max_length
    )
  }
}
```

The consume entropy function will always generate a password of length
"max_length", and will grab a bunch of characters at random. We do *not*
have any assurances though that it will select one of each type. Those
being `lowercase`, `uppercase`, `digits`, and `symbols`.

So, we need a way to use our entropy to get a character from each rule itself:

```
fn get_one_char_per_rule (current_entropy, rules) = {
  ent = current_entropy
  one_char_per_rules = ""

  for (rule in rules) {
    available_chars = rule-to-charset(rule)
    char, ent       = consume_entropy("", ent, available_chars, 1)
    one_char_per_rules += char
  }
  return one_char_per_rules, ent
}
```

This will return us a string of length `rules.length()`, so a max of 4,
with the string always having a lowercase be first, uppercase being second,
digits being third, and a symbol being fourth. (This is the ordering for
the case when we have all four rules there, if one is missing, then naturally
the character for it will be omitted).

While we could just append this string to the end of a generated password
(thus ensuring that we have at least one of every character class in the
password), that seems *wrong*. So instead, we can use our current entropy
value to insert each character into the password.

```
fn insert_string_pseudo_randomly (password, current_entropy, string) = {
  pass = password
  entr = current_entropy

  for (char in string) {
    quotient, remainder = divmod(entr, pass.length())
    pass = pass.insert_at(remainder, char)
    entr = quotient
  }
  return pass
}
```

We don't need to return the entropy here, since the password generation at this
point is over, and we don't need that value anymore.

So now, to tie it all together, the actual creation of the password looks
something like this:

```
fn generate_password(password_profile, master_password) = {
  entropy = calculate_entropy(password_profile, master_password)

  rules        = password_profile.rules.sort()
  set_of_chars = rules_to_charset(rules)

  password, entropy = consume_entropy("",
                                      entropy,
                                      set_of_chars
                                      password_profile.length - rules.length()
                                      )
  chars_to_add, entropy = get_one_char_per_rule(entropy, rules)
  final_pass = insert_string_pseudo_randomly(password, entropy, chars_to_add)

  return final_pass
}
```

And that's really it. Obviously this code isn't executable, but it should
be illustrative enough for you to be able to implement it in
`<your language of choice>`, since it's not a very complex algorithm.

## development process

Developing a lisp application was a good bit different compared to other programming
languages that I've used before. I definitely wasn't new to lisp in general, but
new to the process of having to require libraries and to make sure that my library
was actually usable.

And since I knew that I wanted to write a library and test suite from the start,
I had to jump into the deep end and learn enough of [ASDF](https://asdf.common-lisp.dev/) to get my library
to be `ql:quickload`-able.

I will say that it was somewhat tricky to figure out what I needed to do in order
for the test suite to be usable via `asdf:test-system`, but it turned out to be simple in
the end.

## contrib

In addition to implementing lesspass v2, I also wrote a little companion
program,
[lsps](https://raw.githubusercontent.com/echawk/lesspass-cl/master/contrib/lsps.lisp)
which allows for passwords to be generated interactively (outside of the
CL repl).

While I initially wrote lsps as a sbcl script, I instead rewrote it to be able
to be "image dumped" into an executable using a
[script](https://raw.githubusercontent.com/echawk/lesspass-cl/master/contrib/build-lsps).
That way, you can compile lsps with other lisp implementations, provided they
are supported by `uiop:dump-image`.

### lsps

lsps, as mentioned above, is a helper for using the lesspass library. It
allows you to specify every piece of information through the command line,
or it can interactively prompt the user for the needed information.

Additionally, it supports reading "saved" information for specific sites,
(excluding the login, incase someone were to make their file public by accident)
using a semi-colon separated CSV in `$XDG_CONFIG_HOME/lsps/sites`.


## concluding thoughts

[lesspass-cl](https://github.com/echawk/lesspass-cl) was my first foray into
the world of Common Lisp development
outside of configuring [stumpwm](https://stumpwm.github.io/) and other
miscellaneous scripts.

I have to say, that the experience has left me pretty well convinced that
Common Lisp is one of the better choices when it comes to writing actual
end user applications. Granted, while lesspass-cl is not some gigantic
100kLOC project, with 30+ developers, I still think that the overall
experience of developing an application in Common Lisp can scale up to
teams that large.

See [nyxt](https://github.com/atlas-engineer/nyxt) for an example of a
Common Lisp project that *is* that big!


# thoughts on using CL

## the good

### libraries

The library selections are actually *quite good*! I was definitely worried that
there would be a shortage of libraries, but there is quite a number of
well written code out in the wild. I will say, that some of the libraries
can be quite old (it is not surprising to see commits from 5 years ago), but
that doesn't mean that the library doesn't work.

### repl

I was already a lisper before I started this project, so I was already sold
on repl-driven-development, however this project showed me just *how nice*
life is over in CL land. Slime and Sly are both *fantastic* plugins and
make writing CL a breeze!

### compiler(s)

One of the things that I think most people take for granted is having
*multiple* compilers for a language. While almost no language comes close
to C when it comes to the number of compilers, Common Lisp has four very well
established open source compilers, with a fifth and sixth in development.

These are the open source compilers (still maintained) that I am aware of:

* [sbcl](https://sbcl.org)
* [ecl](https://ecl.common-lisp.dev/)
* [ccl](https://ccl.clozure.com/)
* [abcl](https://armedbear.common-lisp.dev/)
* [clasp](https://github.com/clasp-developers/clasp)
* [sicl](https://github.com/robert-strandh/SICL)

## the ehh

### building executables

I do feel bad for putting this in the "ehh" category, but for most people,
the workflow for building binaries is a little different. Definitely *not*
more difficult, just *different*.

Instead of breaking down a program into a bunch of little compilation units,
and linking them all together, you instead make a core dump of the program
which results in an
"[image](https://www.sbcl.org/manual/#Saving-a-Core-Image)"
that you can ship to customers.

There really isn't a ton to say here, other than adjusting your workflow to
reflect the "Lisp way" of doing things.

I will say, one potential upside (or downside depending on how you view
the "customer") is that the shipped executables contain *everything* that
was accessible in the common lisp image. This includes the repl, and all of
the libraries that were loaded. It is possible to actually start up a
swank/slynk server and connect an editor to the application, and modify
it in *real-time*.
[Grammarly has even used this exact technique in production.](https://www.grammarly.com/blog/engineering/running-lisp-in-production/)

It's mostly a process of learning how things are done.

## the bad

### quicklisp https support

Quicklisp doesn't download libraries using https - while this is done in an
effort to make quicklisp available on all conceivable Lisp implementations,
this is obviously a problem with regards to security.

There are ways to fix this broken behavior, but each has it's own caveats.

Arguably the least intrusive solution is to simply follow this
[blog post](https://web.archive.org/web/20240313162903/https://hiphish.github.io/blog/2022/03/19/securing-quicklisp-through-mitmproxy/)
discussing how to use [mitmproxy](https://github.com/mitmproxy/mitmproxy)
to force https for all local connections, and to simply run mitmproxy when
downloading libraries.

However, this should not need to be done *at all* for simply the secure
downloading of libraries for a programming language. This should simply
be the *default* and it is really unfortunate that it isn't.

NOTE: you can also checkout [ql-https](https://github.com/rudolfochrist/ql-https)
which is a lisp package for doing this without mitmproxy.

### dumped images are rather large
;; FIXME: rewrite

When dumping images to executables, the files they produce are rather hefty.
For example, dumping lsps, even with compression enabled, still produces a
17MB file! While this is fine on modern systems with modern amounts of storage
and ram, it still does seem somewhat wasteful to me, to be forced to ship
the full lisp stack every time you want to make an executable.

**NOTE:** This is only really a problem on all of the *free* lisp implementations,
all of the proprietary implementations allow you to use a technique called
"tree-shaking" to remove all of the unused functions and code from your
executable resulting in a *much* smaller image size.

You can also check out roswell's
[tree-shaker](https://github.com/roswell/roswell/blob/master/lisp/dump-sbcl.lisp)
if you are interested in using roswell (or have lisp installed via roswell).
I personally don't use roswell, since I haven't felt the need for it.
It also forces you to use it's provided lisp binaries AFAICT, so it's a little
redundant for me.

## ideas for improvement

Outside of making the downloading of libraries safer (using https),
I would say that there isn't a *ton* of need for improvement.

There could be some benefit to continuing the development of the command line
library managers, [qlot](https://github.com/fukamachi/qlot) or
[ros](https://github.com/roswell/roswell) come to mind. However, the biggest
issue with these tools is that they are inherently outside the lisp repl,
which is the best place to manage your projects anyways.

I think that if someone where to implement something akin to python's
[virtual environments](https://docs.python.org/3/library/venv.html)
that would be a *huge* boost in productivity,
while allowing for the actual vendoring of libraries, which currently is
done most commonly through using git submodules (yuck).

Admittedly, I don't use either qlot or ros, since I find the repl to be more
than adequate for most of my needs, but writing command line tools would make
it easier to integrate Lisp into existing CI infrastructures.

Having tree-shaking be more accessible to people who do not use roswell would
be rather nice aswell.
