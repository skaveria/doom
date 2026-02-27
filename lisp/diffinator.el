;;; diffinator.el --- Paste a unified diff, patch the right file -*- lexical-binding: t; -*-

;; Runway edition (simple + sharp):
;; - Reads diff from clipboard (macOS pbpaste preferred; GUI fallback)
;; - If diff contains `diff --git a/X b/X`, tries to find X and apply there
;; - Otherwise applies to the current buffer (must be visiting a file)
;; - Applies via `git apply` against a temp snapshot, then replaces buffer text
;; - Shows a single *diffinator errors* buffer on failure
;; - Prints a success message when the clipboard diff applies

(require 'subr-x)

(defgroup diffinator nil
  "Apply pasted diffs to files/buffers."
  :group 'tools)

(defcustom diffinator-git-executable "git"
  "Git executable to use for applying diffs."
  :type 'string
  :group 'diffinator)

(defcustom diffinator-prefer-pbpaste t
  "Prefer reading the clipboard via pbpaste on macOS."
  :type 'boolean
  :group 'diffinator)

(defun diffinator--errbuf (title body)
  "Show TITLE and BODY in *diffinator errors*."
  (let ((buf (get-buffer-create "*diffinator errors*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert title "\n\n" body)
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun diffinator--pbpaste ()
  "Return clipboard text using pbpaste, or nil."
  (when (and diffinator-prefer-pbpaste
             (eq system-type 'darwin)
             (executable-find "pbpaste"))
    (condition-case _
        (let ((s (shell-command-to-string "pbpaste")))
          (when (and (stringp s) (not (string-empty-p (string-trim s))))
            ;; ensure trailing newline for patch tools
            (if (string-suffix-p "\n" s) s (concat s "\n"))))
      (error nil))))

(defun diffinator--gui-clipboard ()
  "Return GUI clipboard text via gui-get-selection, or nil."
  (when (fboundp 'gui-get-selection)
    (condition-case _
        (let ((s (gui-get-selection 'CLIPBOARD 'STRING)))
          (when (and (stringp s) (not (string-empty-p (string-trim s))))
            s))
      (error nil))))

(defun diffinator--read-clipboard ()
  "Read diff text from system clipboard or raise a user error."
  (let ((s (or (diffinator--pbpaste)
               (diffinator--gui-clipboard))))
    (unless (stringp s)
      (user-error "diffinator: couldn't read text from system clipboard"))
    s))

(defun diffinator--looks-like-diff-p (s)
  "Cheap sanity check for a unified diff."
  (and (stringp s)
       (not (string-empty-p (string-trim s)))
       (or (string-match-p "^diff --git " s)
           (string-match-p "^--- " s)
           (string-match-p "^\\+\\+\\+ " s)
           (string-match-p "^@@ " s))))

(defun diffinator--diff-target-bpath (diff-text)
  "Return the `b/<path>` from the first `diff --git a/... b/...` line, or nil."
  (when (string-match "^diff --git a/\\(.+\\) b/\\(.+\\)$" diff-text)
    (let ((b (match-string 2 diff-text)))
      (when (and (stringp b) (not (string-empty-p b)))
        b))))

(defun diffinator--find-existing-file (bpath)
  "Try to resolve BPATH to an existing file. Return absolute path or nil."
  (let* ((rel (string-remove-prefix "b/" (string-remove-prefix "./" bpath)))
         (base1 (or (and buffer-file-name (file-name-directory buffer-file-name))
                    default-directory))
         (cands (delq nil
                      (list
                       (expand-file-name rel base1)
                       (when (boundp 'doom-user-dir) (expand-file-name rel doom-user-dir))
                       (expand-file-name rel default-directory)
                       (expand-file-name (file-name-nondirectory rel) base1)))))
    (catch 'found
      (dolist (p cands)
        (when (file-exists-p p)
          (throw 'found p)))
      nil)))

(defun diffinator--normalize-diff-to-basename (diff-text file-path)
  "Rewrite DIFF-TEXT headers to target FILE-PATH's basename.
This keeps `git apply` happy when we apply against a temp snapshot named basename."
  (let* ((base (file-name-nondirectory file-path))
         (a (concat "a/" base))
         (b (concat "b/" base))
         (lines (split-string diff-text "\n" nil))
         (out '())
         (in-header nil)
         (saw-header nil))
    (dolist (ln lines)
      (cond
       ((string-prefix-p "diff --git " ln)
        (setq in-header t saw-header t)
        (push (format "diff --git %s %s" a b) out))
       ((and in-header (string-prefix-p "--- " ln))
        (setq saw-header t)
        (push (format "--- %s" a) out))
       ((and in-header (string-prefix-p "+++ " ln))
        (setq saw-header t in-header nil)
        (push (format "+++ %s" b) out))
       ((and (not saw-header) (string-prefix-p "--- " ln))
        (setq in-header t saw-header t)
        (push (format "--- %s" a) out))
       ((and in-header (string-prefix-p "+++ " ln))
        (setq in-header nil saw-header t)
        (push (format "+++ %s" b) out))
       (t
        (push ln out))))
    (let ((norm (mapconcat #'identity (nreverse out) "\n")))
      (if saw-header norm diff-text))))

(defun diffinator--write-string (path s)
  (with-temp-file path
    (insert s)
    (unless (string-suffix-p "\n" s) (insert "\n"))))

(defun diffinator--write-buffer-to (buf path)
  (with-temp-file path
    (insert (with-current-buffer buf
              (buffer-substring-no-properties (point-min) (point-max))))))

(defun diffinator--git-apply (dir patch-path)
  "Run `git apply` on PATCH-PATH in DIR. Return plist."
  (let* ((default-directory dir)
         (out (generate-new-buffer " *diffinator git out*"))
         (err (generate-new-buffer " *diffinator git err*"))
         (argv (list "apply" "--unsafe-paths" "--whitespace=nowarn" "--recount" patch-path))
         exit-code stdout stderr)
    (unwind-protect
        (let ((standard-error err))
          (setq exit-code (apply #'call-process diffinator-git-executable nil out nil argv))
          (setq stdout (with-current-buffer out (buffer-string)))
          (setq stderr (with-current-buffer err (buffer-string)))
          (list :ok (eq exit-code 0) :exit exit-code :stdout stdout :stderr stderr))
      (when (buffer-live-p out) (kill-buffer out))
      (when (buffer-live-p err) (kill-buffer err)))))

(defun diffinator--apply-diff-to-buffer (buf file-path diff-text)
  "Apply DIFF-TEXT to BUF (visiting FILE-PATH). Return FILE-PATH on success."
  (let* ((basename (file-name-nondirectory file-path))
         (tmpdir (make-temp-file "diffinator-" t))
         (workfile (expand-file-name basename tmpdir))
         (patchfile (expand-file-name "patch.diff" tmpdir))
         (was-modified (with-current-buffer buf (buffer-modified-p)))
         (normalized (diffinator--normalize-diff-to-basename diff-text file-path)))
    (unwind-protect
        (progn
          (diffinator--write-buffer-to buf workfile)
          (diffinator--write-string patchfile normalized)
          (let ((res (diffinator--git-apply tmpdir patchfile)))
            (if (plist-get res :ok)
                (let ((newtext (with-temp-buffer
                                 (insert-file-contents workfile)
                                 (buffer-string))))
                  (with-current-buffer buf
                    (atomic-change-group
                      (erase-buffer)
                      (insert newtext)
                      (set-buffer-modified-p (or was-modified t))))
                  file-path)
              (diffinator--errbuf
               "diffinator: git apply failed"
               (concat
                "Target file: " file-path "\n"
                "Temp snapshot: " workfile "\n"
                "Patch file: " patchfile "\n\n"
                "stderr:\n" (plist-get res :stderr) "\n"
                "stdout:\n" (plist-get res :stdout) "\n"))
              (user-error "diffinator: could not apply diff (see *diffinator errors*)"))))
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

;;;###autoload
(defun diffinator-apply-diff-smart ()
  "Apply clipboard diff to the file it targets (single-file), else current buffer."
  (interactive)
  (let* ((diff (diffinator--read-clipboard)))
    (unless (diffinator--looks-like-diff-p diff)
      (diffinator--errbuf
       "diffinator: clipboard doesn't look like a unified diff"
       (substring diff 0 (min 1200 (length diff))))
      (user-error "diffinator: clipboard doesn't look like a unified diff (see *diffinator errors*)"))
    (let* ((bpath (diffinator--diff-target-bpath diff))
           (file (and bpath (diffinator--find-existing-file bpath)))
           (buf  (cond (file (find-file-noselect file))
                       (t (current-buffer))))
           (path (or file (buffer-file-name buf))))
      (unless path
        (user-error "diffinator: no target file found (diff has no diff --git header, and current buffer isn't a file)"))
      (setq path (diffinator--apply-diff-to-buffer buf path diff))
      (message "diffinator: applied clipboard diff → %s" (abbreviate-file-name path)))))

;; Back-compat with your config keybind name.
;;;###autoload
(defalias 'diffinator-apply-diff-from-kill #'diffinator-apply-diff-smart)

;;;###autoload
(defalias 'diffinator-apply-diff-from-clipboard #'diffinator-apply-diff-smart)

(provide 'diffinator)
;;; diffinator.el ends here
