;;; emjupy-pkg.el --- Package metadata for emjupy  -*- lexical-binding: t; -*-

;; This file is generated/maintained alongside emjupy.el; a multi-file package
;; needs it so package.el knows the name, version and dependencies without
;; loading any code.

(define-package "emjupy" "0.1.0"
  "Interactive Jupyter notebooks in Emacs"
  '((emacs "29.1")
    (websocket "1.15"))
  :keywords '("languages" "tools" "python" "jupyter")
  :url "https://github.com/mathren/emjupy")


;; package.el reads this file to learn the name, version and dependencies; it
;; is never loaded as code, and `define-package' has been obsolete since Emacs
;; 29.1. Compiling it therefore produces nothing but warnings in the user's
;; *Warnings* buffer at install time, so opt out -- the same way package.el
;; marks the -pkg.el files it generates itself.

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; emjupy-pkg.el ends here
