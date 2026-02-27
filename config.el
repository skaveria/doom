;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Doom note: you do NOT need to run `doom sync` after editing this file.
;; Keep it runway-clean: stable defaults up top, package config below.
;;
;; Design goals:
;; - quiet, cozy, low-friction UX (Apple Silicon friendly)
;; - one source of truth per behavior (no duplicate/conflicting knobs)
;; - buffer-local polish for REPL-ish buffers (scratch/ielm/eshell)
;; - keymaps that never steal your literal spacebar in insert (espanso-safe)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;;; ---------------------------------------------------------------------------
;;; Core Doom settings (set before packages load)
;;; ---------------------------------------------------------------------------

;; Keep Customize noise out of config.el.
;; Your repo already has doom/custom.el; this makes it the canonical sink.
(setq custom-file (expand-file-name "custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "MonoLisa MonoLisaSkav" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "MonoLisa MonoLisaSkav" :size 13))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setq doom-theme 'doom-molokai)
(setq display-line-numbers-type t)
(setq org-directory "~/org/")
(setq fancy-splash-image "~/.config/doom/pc.jpg")

;;; ---------------------------------------------------------------------------
;;; UI / comfort
;;; ---------------------------------------------------------------------------

;; Softer cursor / less visual violence
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

;; Fewer sharp edges
(setq confirm-kill-emacs nil
      ring-bell-function #'ignore)

;;; Scrolling (Apple Silicon smooth mode)
;; - `scroll-conservatively 0` plays nicer with modern pixel-ish rendering.
;; - `scroll-margin` kept small (0) for precision; bump to 4–8 if you want “cozy”.
(setq scroll-margin 0
      scroll-step 1
      scroll-conservatively 1
      scroll-preserve-screen-position t
      auto-window-vscroll t)

;; Pixel-precision scrolling (Emacs 29+)
;; On some Apple Silicon + Retina + Metal builds this can cause subtle jitter/tearing.
;; Keep it OFF for glassy scrolling; re-enable if your setup behaves.
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode -1))

;; Help Retina resizing avoid micro-jitter.
(setq frame-resize-pixelwise t)

;;; Editing ergonomics
(delete-selection-mode 1)        ; typing replaces selection
(setq mouse-autoselect-window t) ; focus follows mouse (intentional)

;; File / buffer comfort
(setq-default tab-width 2
              indent-tabs-mode nil
              fill-column 90)

;; Don't yank me around with popups
(setq +popup-defaults '(:quit t :select ignore :ttl nil))

;;; ---------------------------------------------------------------------------
;;; Completion (Vertico / Corfu)
;;; ---------------------------------------------------------------------------

(after! vertico
  (setq vertico-count 12
        vertico-resize t
        vertico-cycle t))

(after! corfu
  (setq corfu-auto t
        ;; Aggressive is fine if it's effectively instant.
        corfu-auto-delay 0.0
        corfu-auto-prefix 1
        corfu-cycle t))

;;; ---------------------------------------------------------------------------
;;; LSP / IDE brain (polite delivery)
;;; ---------------------------------------------------------------------------

;; Your stated preference:
;; - full IDE capability
;; - diagnostics only when you ask (hover/list), not constant visual nagging

;; Eglot is enabled via :tools (lsp +eglot) in init.el.
;; Keep it fast, quiet, and low-noise.
(after! eglot
  (setq eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-events-buffer-size 0)
  ;; Keep eldoc "hover" compact.
  (after! eldoc
    (setq eldoc-echo-area-use-multiline-p nil)))

;; Flycheck is still your on-demand "list errors" surface.
;; We keep the checks, but reduce constant visual anxiety.
(after! flycheck
  ;; Keep checks snappy, but avoid a constant red/green lightshow.
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8
        flycheck-display-errors-delay 0.25)
  ;; Minimal visuals: no fringe indicators, no noisy error highlighting by default.
  (setq flycheck-indication-mode nil)
  (setq flycheck-highlighting-mode nil))

;; Leader shortcuts for "ask for diagnostics":
;; - hover current issue (eldoc)
;; - list issues (flycheck)
(map! :leader
      (:prefix ("c" . "code")
       :desc "Hover docs/diagnostics (eldoc)" "h" #'eldoc
       :desc "List diagnostics"            "l" #'flycheck-list-errors
       :desc "Next diagnostic"            "n" #'flycheck-next-error
       :desc "Prev diagnostic"            "p" #'flycheck-previous-error))

;;; ---------------------------------------------------------------------------
;;; Modeline / frame
;;; ---------------------------------------------------------------------------

(setq doom-modeline-height 28
      doom-modeline-bar-width 3
      doom-modeline-buffer-file-name-style 'truncate-upto-root
      doom-modeline-major-mode-icon t)

(setq ns-use-proxy-icon nil
      frame-title-format '("%b — Emacs"))

;;; ---------------------------------------------------------------------------
;;; Delimiters
;;; ---------------------------------------------------------------------------

(use-package! rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (cider-repl-mode . rainbow-delimiters-mode)))

;;; ---------------------------------------------------------------------------
;;; CIDER (minimal + stable)
;;; ---------------------------------------------------------------------------

(after! cider
  (setq cider-repl-display-in-current-window t)
  (set-popup-rule! "^\\*cider-repl" :ignore t))

(defvar nk/uai-nrepl-host "192.168.0.105"
  "Host running the Unaccounted Intelligence nREPL server.")

(defvar nk/uai-nrepl-port 7888
  "Port for the Unaccounted Intelligence nREPL server.")

(defun nk/cider-connect-uai ()
  "Connect CIDER to the UAI nREPL server (remote), without jack-in."
  (interactive)
  (require 'cider)
  (let ((cider-host nk/uai-nrepl-host)
        (cider-port nk/uai-nrepl-port))
    (cider-connect-clj (list :host cider-host :port cider-port))))

;;; ---------------------------------------------------------------------------
;;; Local tooling: diffinator
;;; ---------------------------------------------------------------------------

(when (file-exists-p (expand-file-name "lisp/diffinator.el" doom-user-dir))
  (load! "lisp/diffinator" doom-user-dir))

;; Always allow SPC c p (even in insert/emacs state; avoids M-SPC / espanso).
(after! general
  (when (fboundp 'diffinator-apply-diff-from-kill)
    ;; Leader sequence in normal-ish states (doesn't affect typing)
    (general-define-key
     :states '(normal visual motion)
     :keymaps 'override
     "SPC c p" #'diffinator-apply-diff-from-kill)

    ;; Insert/emacs state: use a non-space chord (safe for typing)
    (general-define-key
     :states '(insert emacs)
     :keymaps 'override
     "C-c p" #'diffinator-apply-diff-from-kill)))

;;; ---------------------------------------------------------------------------
;;; Elisp lab space: *scratch* as a real REPL notebook
;;; ---------------------------------------------------------------------------

(defun nk/scratch-setup ()
  "Polish *scratch*: elisp mode, no line numbers, predictable insert state."
  (when (string= (buffer-name) "*scratch*")
    (emacs-lisp-mode)
    (setq-local display-line-numbers nil)
    (setq-local truncate-lines nil)
    (setq-local comment-start ";; ")
    (setq-local comment-end "")
    (rainbow-delimiters-mode 1)
    (show-paren-mode 1)))

(after! emacs-lisp
  (setq initial-major-mode 'emacs-lisp-mode
        initial-scratch-message "")

  (add-hook 'emacs-lisp-mode-hook #'nk/scratch-setup)

  ;; Make sure the *scratch* buffer that already exists gets polished too.
  (with-current-buffer (get-buffer-create "*scratch*")
    (nk/scratch-setup))

  ;; If you use Evil, keep *scratch* in insert by default.
  (after! evil
    (add-hook! 'doom-first-buffer-hook
      (when (get-buffer "*scratch*")
        (with-current-buffer "*scratch*"
          (evil-insert-state))))))

;;; ---------------------------------------------------------------------------
;;; IELM — make it feel like Lisp
;;; ---------------------------------------------------------------------------

(after! ielm
  (setq ielm-prompt "λ "
        ielm-dynamic-return t)

  (add-hook 'ielm-mode-hook
            (lambda ()
              ;; Visual polish
              (setq-local display-line-numbers nil)
              (setq-local truncate-lines nil)
              (visual-line-mode 1)

              ;; Lisp vibes
              (rainbow-delimiters-mode 1)

              ;; Paren highlighting
              (show-paren-mode 1)

              ;; Make sure font-lock is actually on
              (font-lock-mode 1)
              (font-lock-flush)

              ;; Optional: structured editing in the REPL
              (when (fboundp 'smartparens-mode)
                (smartparens-mode 1)))))

;; If you use smartparens, this makes parens behave well in IELM
(after! smartparens
  (add-hook 'ielm-mode-hook #'smartparens-strict-mode))

;;; ---------------------------------------------------------------------------
;;; Eshell — control plane, not a POSIX cosplay
;;; ---------------------------------------------------------------------------

(after! eshell
  (setq eshell-banner-message ""
        eshell-history-size 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-scroll-to-bottom-on-output t
        eshell-prefer-lisp-functions t)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local display-line-numbers nil)
              (visual-line-mode 1)
              (rainbow-delimiters-mode 1)
              (show-paren-mode 1))))

;;; ---------------------------------------------------------------------------
;;; Runway utilities (tiny, composable, and worth keeping)
;;; ---------------------------------------------------------------------------

(defun nk/reload-config ()
  "Reload Doom config (fast path)."
  (interactive)
  (if (fboundp 'doom/reload)
      (doom/reload)
    (load-file (expand-file-name "config.el" doom-user-dir))))

(map! :leader
      :desc "Reload Doom config" "h R" #'nk/reload-config)

;; A tiny ELisp pretty-printer for when *scratch* is your lab.
(defun nk/pp (expr)
  "Pretty-print the value of EXPR into *pp*."
  (interactive (list (read--expression "ELisp: ")))
  (let ((buf (get-buffer-create "*pp*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pp (eval expr) buf)
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(map! :leader
      :desc "Pretty-print ELisp" "e p" #'nk/pp)

;; Mac port tuning
(setq mac-command-modifier 'meta
      mac-option-modifier 'super
      mac-control-modifier 'control)

;; Smooth scrolling
(setq pixel-scroll-precision-mode t)

;; Native fullscreen
(setq ns-use-native-fullscreen t)

;; Transparent titlebar (modern macOS)
(setq ns-transparent-titlebar t)

;; Remove ugly toolbar
(tool-bar-mode -1)

;; Native tab bar (optional)
(tab-bar-mode 1)

(boundp 'mac-use-metal)
