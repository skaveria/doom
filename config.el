;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; ---------------------------------------------------------------------------
;;; Core files
;;; ---------------------------------------------------------------------------

(setq custom-file (expand-file-name "custom.el" doom-user-dir))
(load custom-file 'noerror 'nomessage)

;;; ---------------------------------------------------------------------------
;;; Identity
;;; ---------------------------------------------------------------------------

;; (setq user-full-name "Nichole Kernreicht"
;;       user-mail-address "you@example.com")

;;; ---------------------------------------------------------------------------
;;; Theme / fonts
;;; ---------------------------------------------------------------------------

(setq doom-font (font-spec :family "MonoLisa" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "MonoLisa" :size 13)
      display-line-numbers-type nil
      org-directory "~/org/")

(add-to-list 'custom-theme-load-path "~/.config/doom/themes/ember/")
(load-theme 'ember t)

(setq fancy-splash-image "~/.config/doom/pc.jpg")

;;; ---------------------------------------------------------------------------
;;; Basic UI
;;; ---------------------------------------------------------------------------

(blink-cursor-mode -1)
(delete-selection-mode 1)

(setq-default cursor-type 'bar
              tab-width 2
              indent-tabs-mode nil
              fill-column 90)

(setq confirm-kill-emacs nil
      ring-bell-function #'ignore
      ns-use-proxy-icon nil
      frame-title-format '("%b — Doom Emacs"))

;;; ---------------------------------------------------------------------------
;;; macOS polish
;;; ---------------------------------------------------------------------------

(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-control-modifier 'control
      ns-command-modifier 'super
      ns-option-modifier 'meta
      ns-control-modifier 'control
      ns-use-native-fullscreen t
      ns-transparent-titlebar t
      frame-resize-pixelwise t)

;;; ---------------------------------------------------------------------------
;;; Scrolling — smooth macOS / image-friendly
;;; ---------------------------------------------------------------------------

;; Scrolling — smooth but not smeary on macOS
(setq scroll-margin 0
      scroll-step 1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      redisplay-dont-pause t
      jit-lock-defer-time 0
      jit-lock-stealth-time nil
      hscroll-margin 2
      hscroll-step 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      ns-use-thin-smoothing t)

(when (fboundp 'pixel-scroll-precision-mode)
  (setq pixel-scroll-precision-use-momentum nil
        pixel-scroll-precision-interpolate-page nil
        pixel-scroll-precision-large-scroll-height 30.0
        pixel-scroll-precision-interpolation-factor 4.0)
  (pixel-scroll-precision-mode 1))

;;; ---------------------------------------------------------------------------
;;; Images / visual buffers
;;; ---------------------------------------------------------------------------

(setq image-cache-eviction-delay 300)

(add-hook 'image-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (setq-local scroll-margin 0
                        auto-window-vscroll nil)))

(after! org
  (setq org-image-actual-width nil)

  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local auto-window-vscroll nil))))

(after! pdf-view
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (setq-local scroll-margin 0
                          auto-window-vscroll nil))))

;;; ---------------------------------------------------------------------------
;;; Performance / LSP friendliness
;;; ---------------------------------------------------------------------------

(setq gc-cons-threshold (* 200 1024 1024)
      read-process-output-max (* 1024 1024))

;;; ---------------------------------------------------------------------------
;;; Vertico
;;; ---------------------------------------------------------------------------

(after! vertico
  (setq vertico-count 12
        vertico-resize t
        vertico-cycle t))

;;; ---------------------------------------------------------------------------
;;; Corfu
;;; ---------------------------------------------------------------------------

(after! corfu
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-cycle t))

;;; ---------------------------------------------------------------------------
;;; Eglot / Eldoc
;;; ---------------------------------------------------------------------------

(after! eglot
  (setq eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-events-buffer-size 0))

(after! eldoc
  (setq eldoc-echo-area-use-multiline-p nil))

;;; ---------------------------------------------------------------------------
;;; Flycheck — useful, but calmer
;;; ---------------------------------------------------------------------------

(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.25
        flycheck-indication-mode nil))

;;; ---------------------------------------------------------------------------
;;; Modeline
;;; ---------------------------------------------------------------------------

(setq doom-modeline-height 28
      doom-modeline-bar-width 3
      doom-modeline-buffer-file-name-style 'truncate-upto-root
      doom-modeline-major-mode-icon t)

;;; ---------------------------------------------------------------------------
;;; Rainbow delimiters
;;; ---------------------------------------------------------------------------

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; ---------------------------------------------------------------------------
;;; vterm — fast, clean, usable
;;; ---------------------------------------------------------------------------

(after! vterm
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t
        vterm-timer-delay 0.01
        vterm-enable-manual-redraw t
        vterm-term-environment-variable "xterm-256color"
        vterm-shell "/bin/zsh"))

(add-hook 'vterm-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (setq-local truncate-lines nil
                        scroll-margin 0
                        cursor-type 'bar)
            (visual-line-mode 1)))

(map! :map vterm-mode-map
      :i "C-c C-c" #'vterm-send-C-c
      :i "C-c C-z" #'vterm-copy-mode
      :i "C-c C-k" #'vterm-clear
      :i "C-c C-l" #'vterm-clear-scrollback)

(map! :map vterm-copy-mode-map
      :n "q" #'vterm-copy-mode-done)

(map! :leader
      :desc "Open vterm" "o t" #'vterm)

;;; ---------------------------------------------------------------------------
;;; CIDER / Clojure
;;; ---------------------------------------------------------------------------

(after! cider
  (set-popup-rule! "^\\*cider-repl"
    :ignore t)

  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;;; ---------------------------------------------------------------------------
;;; Smartparens — comfy structural editing for Clojure + CIDER REPL
;;; ---------------------------------------------------------------------------

(after! smartparens
  (show-smartparens-global-mode 1)

  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil))

(map! :after smartparens
      :map (clojure-mode-map cider-repl-mode-map)
      :localleader
      (:prefix ("s" . "sexp")
       :desc "Forward slurp"  "l" #'sp-forward-slurp-sexp
       :desc "Backward slurp" "h" #'sp-backward-slurp-sexp
       :desc "Forward barf"   "j" #'sp-forward-barf-sexp
       :desc "Backward barf"  "k" #'sp-backward-barf-sexp
       :desc "Splice sexp"    "s" #'sp-splice-sexp
       :desc "Raise sexp"     "r" #'sp-raise-sexp
       :desc "Kill sexp"      "x" #'sp-kill-sexp
       :desc "Copy sexp"      "w" #'sp-copy-sexp
       :desc "Transpose sexp" "t" #'sp-transpose-sexp))

(map! :after smartparens
      :map (clojure-mode-map cider-repl-mode-map)
      :i "C-]" #'sp-forward-slurp-sexp
      :i "C-}" #'sp-forward-barf-sexp
      :i "C-)" #'sp-splice-sexp
      :n "g>"  #'sp-forward-slurp-sexp
      :n "g<"  #'sp-forward-barf-sexp
      :n "gk"  #'sp-splice-sexp
      :n "gr"  #'sp-raise-sexp
      :n "g("  #'sp-wrap-round
      :n "g)"  #'sp-wrap-round
      :i "C-(" #'sp-wrap-round
      :i "C-)" #'sp-wrap-round)

;;; ---------------------------------------------------------------------------
;;; Ollama Buddy
;;; ---------------------------------------------------------------------------

(add-to-list 'load-path "~/.config/emacs/ollama-buddy")
(require 'ollama-buddy)

(global-set-key (kbd "C-c o") #'ollama-buddy-role-transient-menu)
(global-set-key (kbd "C-c O") #'ollama-buddy-transient-menu)
