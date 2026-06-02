;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Minimal additions. Keep this file boring.

(package! rainbow-delimiters)

;; ELisp "CIDER-ish" UX
(package! eros)
(package! eval-sexp-fu)
(package! helpful)

(package! ember-theme
  :recipe (:host github :repo "ember-theme/emacs"))

(package! ollama-buddy
  :recipe (:host github :repo "captainflasmr/ollama-buddy"))
