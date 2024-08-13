:DATE 2024-07-06
# automagically installing quicklisp

A while back I was searching for a way to have [quicklisp](https://www.quicklisp.org/beta/)
be automatically installed when using sbcl (and friends),
but I wasn't really able to find a code snippet that I
could add to my `~/.sbclrc`.

However, I wrote the following code snippet a couple months
back, and have now successfully used it on multiple \*NIX
systems to great success, so I figured it warrants it's own
small blog post.

Here's the snippet:

```
(require 'asdf)
(asdf:load-system :uiop)

(defpackage :quicklisp-quickstart
  (:use :cl)
  (:export :install))
(in-package :quicklisp-quickstart)
(defun install () t)

(in-package :cl-user)

#-QUICKLISP
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (unless (probe-file quicklisp-init)
    (uiop:run-program "curl -O https://beta.quicklisp.org/quicklisp.lisp"
                      :output *standard-output*)
    (load "quicklisp.lisp")
    (quicklisp-quickstart:install)
    (uiop:run-program "rm quicklisp.lisp"))

  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
```

The only requirement for this snippet is that `curl` is available
in the system's `$PATH`. Pretty slick!

If you want to see the rest of my `.sbclrc`, you can view it
[here](https://github.com/echawk/dots/blob/master/stumpwm/.sbclrc)

## other lisps...

This snippet also works for `ecl`, `ccl`, and `abcl`. I'm sure
others work as well, but I have tested those three specifically.

## update 8-12-2024

Users may also be interested in adding ultralisp to their list
of quicklisp dists. Here is the updated snippet which will
also automatically install ultralisp:

```
(defpackage :quicklisp-quickstart
  (:use :cl)
  (:export :install))
(in-package :quicklisp-quickstart)
(defun install () t)

(defpackage :ql-dist
  (:use :cl)
  (:export :install-dist :find-dist))
(in-package :ql-dist)
(defun install-dist (arg1 arg2 arg3) (list arg1 arg2 arg3))
(defun find-dist (str) str)

;; Switch back to cl-user package.
(in-package :cl-user)

;;; Install quicklisp if it is not already installed.
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (unless (probe-file quicklisp-init)
    (uiop:run-program "curl -O https://beta.quicklisp.org/quicklisp.lisp"
                      :output *standard-output*)
    (load "quicklisp.lisp")
    (quicklisp-quickstart:install)
    (uiop:run-program "rm quicklisp.lisp"))

  (when (probe-file quicklisp-init)
    (load quicklisp-init))

  ;; Also install the ultralisp dist while we are at it...
  (unless (ql-dist:find-dist "ultralisp")
    (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)))
```

Little bit more code since we have to fake more functions from quicklisp.
