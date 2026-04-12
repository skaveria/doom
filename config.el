;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" doom-user-dir))
(load custom-file 'noerror 'nomessage)

;; Identity
;; (setq user-full-name "Nichole Kernreicht"
;;       user-mail-address "you@example.com")

;; Theme / fonts
(setq doom-font (font-spec :family "MonoLisa" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "MonoLisa" :size 13)
      display-line-numbers-type 'relative
      org-directory "~/org/")

(add-to-list 'custom-theme-load-path "~/.config/doom/themes/ember/")
(load-theme 'ember t)

;; Basic UI
(blink-cursor-mode -1)
(setq-default cursor-type 'bar)

(setq confirm-kill-emacs nil
      ring-bell-function #'ignore
      ns-use-proxy-icon nil
      frame-title-format '("%b — Doom Emacs"))

;; Editing defaults
(delete-selection-mode 1)

(setq-default tab-width 2
              indent-tabs-mode nil
              fill-column 90)

;; Scrolling
(setq scroll-margin 0
      scroll-step 1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll t
      frame-resize-pixelwise t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode -1))

;; Performance / LSP friendliness
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; Vertico
(after! vertico
  (setq vertico-count 12
        vertico-resize t
        vertico-cycle t))

;; Corfu
(after! corfu
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-cycle t))

;; Eglot / Eldoc
(after! eglot
  (setq eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-events-buffer-size 0))

(after! eldoc
  (setq eldoc-echo-area-use-multiline-p nil))

;; Flycheck: useful, but calmer
(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.25
        flycheck-indication-mode nil))

;; Modeline
(setq doom-modeline-height 28
      doom-modeline-bar-width 3
      doom-modeline-buffer-file-name-style 'truncate-upto-root
      doom-modeline-major-mode-icon t)

;; Rainbow delimiters in code buffers
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; macOS
(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-control-modifier 'control
      ns-command-modifier 'super
      ns-option-modifier 'meta
      ns-control-modifier 'control
      ns-use-native-fullscreen t
      ns-transparent-titlebar t)

(setq fancy-splash-image "~/.config/doom/pc.jpg")

;;; ---------------------------------------------------------------------------
;;; vterm — fast, clean, usable
;;; ---------------------------------------------------------------------------

(after! vterm
  ;; performance / feel
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t
        vterm-timer-delay 0.01
        vterm-enable-manual-redraw t
        vterm-term-environment-variable "xterm-256color")

  ;; nicer default shell (optional but good)
  (setq vterm-shell "/bin/zsh"))

;; buffer behavior
(add-hook 'vterm-mode-hook
          (lambda ()
            ;; no visual clutter
            (display-line-numbers-mode -1)
            (setq-local truncate-lines nil)
            (visual-line-mode 1)

            ;; smoother feel
            (setq-local scroll-margin 0)

            ;; cursor
            (setq-local cursor-type 'bar)))

;; keybindings — make vterm actually usable
(map! :map vterm-mode-map
      :i "C-c C-c" #'vterm-send-C-c
      :i "C-c C-z" #'vterm-copy-mode
      :i "C-c C-k" #'vterm-clear
      :i "C-c C-l" #'vterm-clear-scrollback)

(map! :map vterm-copy-mode-map
      :n "q" #'vterm-copy-mode-done)

;; quick launcher
(map! :leader
      :desc "Open vterm" "o t" #'vterm)

(after! cider
  (set-popup-rule! "^\\*cider-repl"
    :ignore t))

(after! cider
  ;; rainbow delimiters in REPL
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

  ;; (optional) also ensure it in clojure buffers explicitly
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))
