:DATE 2024-08-11
# deterministic ssh keys via common lisp

Have you ever wanted to generate ssh keys *deterministically* in Common Lisp
by cheating the system?

If yes, read on!

**DISCLAIMER:** I am not responsible when/if someone cracks your ssh key. Please
don't use this for anything serious/public facing. If you get pwned, that's your
fault.

Now, with that out of the way:

## the **beef**

Here's the code:
```

;; SPDX-License-Identifier: BSD-2-Clause

(ql:quickload '(:ironclad :cl-ssh-keys))

(defun generate-deterministic-keys (seed-string)
  "Will deterministically generate a ssh key pair from SEED-STRING.

The ssh key pair is returned as a list, with the private key being first
and the public key being second."
  (let* ((checksum-int
           (reduce #'+ (mapcar #'char-int (coerce seed-string 'list))))
         (sk (ironclad:ascii-string-to-byte-array seed-string))
         (pk (ironclad:ed25519-public-key sk))

         (ironclad-priv-key (ironclad:make-private-key :ed25519 :x sk :y pk))
         (ironclad-pub-key  (ironclad:make-public-key  :ed25519 :y pk))

         (key-type (ssh-keys:get-key-type-or-lose :ssh-ed25519 :by :id))

         (pub-key
           (make-instance 'ssh-keys:ed25519-public-key
                          :kind key-type
                          :y (ironclad:ed25519-key-y ironclad-pub-key)))
         (priv-key
           (make-instance 'ssh-keys:ed25519-private-key
                          :public-key pub-key
                          :cipher-name "none"
                          :kdf-name "none"
                          :checksum-int checksum-int
                          :kind key-type
                          :y (ironclad:ed25519-key-y ironclad-priv-key)
                          :x (ironclad:ed25519-key-x ironclad-priv-key))))
    (list priv-key pub-key)))

```

That's it - I should mention that I had to look into the internals of how
cl-ssh-keys generates key pairs to get the above code, hence the license
identifier.

Otherwise the code is rather straightforward: turn our string into a byte array
that we can use with ironclad's `make-private-key` & `make-public-key`
functions. Then use those ed25519 keys that ironclad created as the base of our
ssh keys.

And for the obligatory example code, using the above function:

```
(defun example ()
  (let* ((keyname  "example")
         (seed-str "your-super-secret-seed-string-please-protect-at-all-costs")
         (keys-lst (generate-deterministic-keys seed-str)))

    (ssh-keys:write-key-to-path
     (first keys-lst)
     (concatenate 'string (namestring (user-homedir-pathname)) ".ssh/" keyname))

    (ssh-keys:write-key-to-path
     (second keys-lst)
     (concatenate 'string (namestring (user-homedir-pathname)) ".ssh/" keyname ".pub"))))
```


While principally this is the only code that you need to get up and running, I
thought I'd give you an actually useful script. Since, well, your seed string
has to be rather long to be of any real practical use. It would be feasible
for an attacker to attempt to brute force your seed string if it was typed out
by a mere human.

Instead let's be a tad more clever.

## the *accoutrement*

### using a password manager to get a super long string

If you have read articles on this blog before, you may be familiar with my
common lisp implementation of [lesspass](https://lesspass.com), which we can
contort to be useful for us at present.

Without getting too into the weeds about how lesspass works, it is essentially a
"password manager" that works by generating your password to a website on the
fly when you provide it the same information.

One of the other aspects that lesspass has baked in is the ability to change
your password entirely by changing the "count", essentially just a number that
you can increase should your password be found in a data breach.

However, we can use this feature of lesspass to our advantage, by generating
multiple passwords from the same password profile, but by changing the count, we
can get and arbitrarily long "password".

#### some code...

If we make a couple of assumptions about the password profile itself for
lesspass we can simplify a lot of the logic:

* Assume we are using all rules (lowercase, uppercase, digits, & symbols)
* Assume a length of 32

And that's about it. We can have the user provide the site and login,
say if they wanted to have different ssh keys for different git forges.

We'll get back to the counter.

```
(ql:quickload :lesspass)

;; example of our object.
(setq pass-prof
  (make-instance
   'lesspass:password-profile
   :site ""
   :login ""
   :rules
   '(lesspass:lowercase
    lesspass:uppercase
    lesspass:digits
    lesspass:symbols)
   :counter 1))

(setf (lesspass:site-of pass-prof) "website.com")

(setf (lesspass:login-of pass-prof) "your-name?")
```

Now before we can go about generating a big "password" we have to discuss
what we are going to do with the `counter`. Like we mentioned before we
can arbitrarily set counter to be anything to make a new password.

We can use the following function to get a reproducible 'step-size' for a
string. It's not exactly a perfect idea, but it seems to be obscured enough
away from the actual string that you'll be feeding the function (hint:
your master password) that I feel alright using the number it generates.

```
(defun get-step-size-from-string (str n)
  "Return a pseudo random number for a given STR.

Sums the value of all of the characters in STR and divides them by N."
  (let* ((entropy (reduce
                   #'+
                   (mapcar
                    (lambda (ch)
                      (expt (char-int ch) 2))
                    (coerce str 'list))))
         (step (floor (coerce (/ entropy n) 'float) 1)))
    step))
```

Essentially we just sum up all of the char values in `str` and floor divide it
by `n`. Should give us a good integer to work with.

Now we can bring everything together in a function, aptly named
`get-seed-string`:

```
(defun get-seed-string ()
  "Return a string that is 'good enough' to seed ironclad with."
  (declare (optimize (safety 3)))
  (let* ((iters 50)
         (master-pass   "yourmasterpassword")
         (password-prof pass-prof)

         ;; Get the step value from the master password since that information
         ;; is also secret, thus making it more secure against brute forcing.
         (step (get-step-size-from-string
                master-pass iters)))
    (apply 'concatenate 'string
           (loop :for i :from 1 :to iters
                 ;; Ensure that the counter of the lesspass prof is some
                 ;; pseudo-random value. Each different value for the
                 ;; counter *will* change the password.
                 :do (setf (lesspass:counter-of password-prof)
                           (* i step))
                 :collect
                 (lesspass:generate-password password-prof master-pass)))))
```

Here we have a few more assumptions: have the number of iterations set to 50.
Combined with the passwords of length 32, we will always have a seed string
of length 1600. Seems secure *enough*.

The other important part here is multiplying the step value by i, and setting
that to be our counter before we generate our password. That will ensure
our generated passwords are unique.

### conclusion

That's it. Just integrate that function into the previous code and you're set.

## links

If you want to checkout the version that I use and have in my dotfiles, the link
is [here](https://github.com/echawk/dots/blob/master/scripts/.local/bin/det-ssh-keys.lisp)

And here's the links to the related libraries:
* [ironclad](https://github.com/sharplispers/ironclad)
* [cl-ssh-keys](https://github.com/dnaeon/cl-ssh-keys)
* [lesspass-cl](https://github.com/echawk/lesspass-cl)

