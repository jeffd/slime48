;;; -*- mode: emacs-lisp -*-

;;;;;; SLIME for Scheme48
;;;;;; Emacs side utilities

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(require 'slime)

(defvar slime48-path (and load-file-name
                          (file-name-directory load-file-name))
  "Path to the directory containing the SLIME48 Scheme source.")

(defvar scheme48-program-name "scheme48"
  "*Name of program to start a usual Scheme48 image.")

(defvar scheme48-program-arguments '()
  "*List of arguments to send to the Scheme48 command.")

(if (not (assq 's48 slime-lisp-implementations))
    (setq slime-lisp-implementations
          `((s48 (,scheme48-program-name ,@scheme48-program-arguments)
                 :init slime48-init-command)
            ,@slime-lisp-implementations)))

(defun slime48-init-command (port-filename coding-system)
  "Return a string to initialize a SLIME48 server."
  (ignore coding-system)
  (mapconcat (lambda (line-fmt.args)
               (apply 'format (car line-fmt.args) (cdr line-fmt.args)))
             `((",translate =slime48/ %s" ,slime48-path)
               (",exec ,load =slime48/load.scm")
               (",exec (slime48-start %S)\n" ,port-filename))
             "\n"))

;;; This redefinition is necessary because Scheme doesn't support CL's
;;; #...r syntax.   However, CL doesn't support Scheme's #d, on the
;;; other hand...

(defun slime-presentation-expression (presentation)
  "Return a string of an S-expression accessing a presented object."
  (let ((id (slime-presentation-id presentation)))
    ;; Make sure it works even if *read-base* is not 10.
    (format "(swank:get-repl-result %s%d)"
            (if (let ((case-fold-search t))
                  (string-match "scheme"
                                (slime-lisp-implementation-type)))
                "#d"
                "#10r")
            id)))

(defun slime48-enable-presentations ()
  "Enable presentations in the SLIME48 REPL."
  (interactive)
  (slime-eval-async
   '(swank:interactive-eval "(enable-swank-repl-presentations)")
   nil
   "swank-repl"))

(defun slime48-disable-presentations ()
  "Disable presentations in the SLIME48 REPL."
  (interactive)
  (slime-eval-async
   '(swank:interactive-eval "(disable-swank-repl-presentations)")
   nil
   "swank-repl"))

(defun slime48-toggle-presentations ()
  "Toggle the enablement of presentations in the SLIME48 REPL."
  (interactive)
  (slime-eval-async
   '(swank:interactive-eval "(toggle-swank-repl-presentations)")
   nil
   "swank-repl"))

(defslime-repl-shortcut nil ("in-package" "in")
  (:handler 'slime-repl-set-package)
  (:one-liner "Go into a different package for interaction."))

(defslime-repl-shortcut nil ("scratch")
  (:handler (lambda ()
              (interactive)
              (slime-repl-set-package "(scratch)")))
  (:one-liner "Change to the scratch package."))

(defslime-repl-shortcut nil ("config")
  (:handler (lambda ()
              (interactive)
              (slime-repl-set-package "(config)")))
  (:one-liner "Change to the config package for module definitions."))

(defslime-repl-shortcut slime-repl-use-package ("use-package" "use")
  (:handler (lambda (package)
              (interactive (list (slime-read-package-name "Package: ")))
              (slime-eval-async `(swank:use-package ',package))))
  (:one-liner "Use a package in the current interaction."))
