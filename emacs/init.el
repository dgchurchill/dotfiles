;;; -*- lexical-binding: t; -*-

;;; Notes for installing
;; Windows
;; In MSYS, `pacman -S mingw-w64-x86_64-gcc mingw-w64-x86_64-libgccjit` to install dependencies for native compilation

;;; Set up straight and use-package

(setq straight-use-package-by-default t)
(setq straight-use-symlinks t) ; use symlinks, even on Windows (requires Developer Mode to be turned on in Windows to avoid UAC prompts)
(setq straight-use-version-specific-build-dir t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))
(setq use-package-always-defer t) ; Straight will automatically load the autoloads, so shouldn't need to set up :commands explicitly. Can always use :demand to override if necessary.


;;; Core settings

(setq default-directory "~/")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(server-start)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

;; increase amount read from a pipe in one go; supposed to improve performance of subprocess communication (e.g. lsp servers)
(setq read-process-output-max (* 1024 1024)) ;; 1mb


(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :init
  (exec-path-from-shell-initialize))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "pwsh")

  (setq find-program "C:/msys64/usr/bin/find.exe")
  (setq grep-find-command nil)
  (setq grep-host-defaults-alist nil)
  (grep-compute-defaults))

(push '(scroll-bar-width . :never) frameset-filter-alist)
(push '(scroll-bar-height . :never) frameset-filter-alist)

;;; General keybindings

(repeat-mode)
(global-set-key (kbd "M-u") #'universal-argument)  ; originally upcase-word
(define-key universal-argument-map (kbd "M-u") #'universal-argument-more)
(global-set-key (kbd "C-z") #'undo) ; originally suspend-frame


;; Set up escape as a leader
;;
;; <escape> is the escape key when in a window system. It's normally translated to ESC (^[) by
;; function-key-map, but not if we explicitly bind it.  Don't bind to ESC, because that's where all
;; the M- keybindings actually live (via the meta-prefix-char variable).
(global-set-key (kbd "<escape>") ctl-x-map)

;; Previously when I had <escape> bound to its own map I tried to keep the keyboard-escape-quit behaviour
;; but ctl-x-map has its own bindings for ESC. Can still press ^[ ^[ ^[ for keyboard-escape-quit
;; (define-key escape-map (kbd "ESC") #'keyboard-escape-quit)

(global-set-key (kbd "M-o") #'other-window)

;; temporary, because tab-always-indent complete doesn't seem work with Omnisharp
(global-set-key (kbd "C-<tab>") #'completion-at-point)

;;; Themes and appearance

(set-face-attribute 'default nil :family "Iosevka Slab" :height 120)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Slab" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 1.0)

(when (eq system-type 'windows-nt)
  (set-fontset-font t 'emoji "Segoe UI Emoji"))

(use-package auto-dim-other-buffers
  :init (auto-dim-other-buffers-mode))

;; (use-package modus-themes
;;   :demand t
;;   :custom
;;   (modus-themes-headings
;;    '((0 variable-pitch light 1.9)
;;      (1 variable-pitch light 1.8)
;;      (2 variable-pitch regular 1.7)
;;      (3 variable-pitch regular 1.6)
;;      (4 variable-pitch regular 1.5)
;;      (5 variable-pitch 1.4) ; absence of weight means `bold'
;;      (6 variable-pitch 1.3)
;;      (7 variable-pitch 1.2)
;;      (t variable-pitch 1.1)))
;;   (modus-themes-mixed-fonts t)
;;   (modus-themes-variable-pitch-ui t)
;;   (modus-themes-common-palette-overrides
;;    '((fringe unspecified)
;;      (bg-mode-line-active bg-yellow-intense)
;;      (border-mode-line-active bg-yellow-intense)
;;      (border-mode-line-inactive bg-mode-line-inactive)))
;;   :config
;;   (load-theme 'modus-operandi-tinted)
;;   (set-face-attribute 'mode-line nil :height 0.9)
;;   (set-face-attribute 'mode-line-inactive nil :height 0.9))

(use-package ef-themes
  :demand t
  :custom
  (ef-themes-headings
   '((0 variable-pitch light 1.9)
     (1 variable-pitch light 1.8)
     (2 variable-pitch regular 1.7)
     (3 variable-pitch regular 1.6)
     (4 variable-pitch regular 1.5)
     (5 variable-pitch 1.4) ; absence of weight means `bold'
     (6 variable-pitch 1.3)
     (7 variable-pitch 1.2)
     (t variable-pitch 1.1)))
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  :config
  (load-theme 'ef-duo-light)
  (set-face-attribute 'mode-line nil :height 0.9)
  (set-face-attribute 'mode-line-inactive nil :height 0.9))


(use-package diminish)

(use-package ace-window
  :bind ([remap other-window] . ace-window))


;;; Utilities

(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode"))
  ;; :init
  ;; (explain-pause-mode))


;;; Buffer management

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("M-j" . consult-buffer)
         ("M-o" . other-window)))

(use-package ibuffer-vc
  :demand t
  :after ibuffer)


;;; Window management

(use-package transpose-frame)

(defun make-display-buffer-matcher-function (major-modes)
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

(setq
 switch-to-buffer-in-dedicated-window 'pop
 switch-to-buffer-obey-display-actions t

 ;; note: can't detect shell buffers by mode because the `shell` command pops to the buffer before changing mode
 display-buffer-alist
 `((,(regexp-quote "-shell*")		; project shells
    (display-buffer-in-side-window)
    (side . bottom))
   (,(make-display-buffer-matcher-function '(compilation-mode))
    (display-buffer-reuse-mode-window display-buffer-in-side-window)
    (side . bottom))
   (,(regexp-quote "*xref*")
    (display-buffer-in-side-window)
    (side . bottom))
   (,(regexp-quote "*RE-Builder*")
    (display-buffer-in-side-window)
    (side . bottom))
   (,(regexp-quote "*eldoc*")
    (display-buffer-in-side-window)
    (side . right))))

(defun eldoc-turn-on-visual-line-mode ()
  (when (string-match-p " ?\\*eldoc" (buffer-name))
    (visual-line-mode)))
(add-hook 'special-mode-hook 'eldoc-turn-on-visual-line-mode)


;;; Help

(use-package which-key
  :diminish
  :init
  (which-key-mode))


;;; Completion

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion))
     (project-file (styles))))) ;; override the default of `substring` so that matches that orderless would show aren't suppresssed by `substring` matching first

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*.el"))
  :init
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package consult
  :init
  (defun consult-buffer-force ()
    (interactive)
    (let ((switch-to-buffer-obey-display-actions nil))
      (consult-buffer)))
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("M-j" . consult-buffer)                  ;; orig. default-indent-new-line
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x B" . consult-buffer-force)
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
)

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package recentf
  :init
  (recentf-mode))


;;; Editing

(use-package multiple-cursors
  :bind (
	     ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))


;;; Org mode

(use-package org
  :demand t   ;; front-load the startup time because it's almost always used
  :init
  (require 'org-protocol)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  :config
  (org-load-modules-maybe))  ;; module loading is normally done on the first use of org-mode, but can take some time

(use-package org-roam
  :config
  (org-roam-db-autosync-mode))

(defun dgc/org-silent-clock-in ()
  "Clock in without affecting the currently running clock"
  (interactive)
  (require 'org-clock)
  (org-clock-find-position nil)
  (insert-before-markers "\n")
  (backward-char 1)
  (when (and (save-excursion
               (end-of-line 0)
               (org-in-item-p)))
    (beginning-of-line 1)
    (indent-line-to (max 0 (- (current-indentation) 2))))
  (insert org-clock-string " ")
  (org-insert-time-stamp (org-current-time org-clock-rounding-minutes t) 'with-hm 'inactive)
  (org-indent-line))

(defun dgc/org-silent-clock-out ()
  "Clock out without affecting the currently running clock"
  (interactive)
  (require 'org-clock)
  (let ((now (org-current-time org-clock-rounding-minutes))
	    ts te s h m remove)
    (org-clock-find-position nil)
	(beginning-of-line 1)
	(if (and (looking-at (concat "[ \t]*" org-keyword-time-regexp))
		     (equal (match-string 1) org-clock-string))
	    (setq ts (match-string 2))
	  (error "Clock start time is gone"))
	(goto-char (match-end 0))
	(delete-region (point) (point-at-eol))
	(insert "--")
	(setq te (org-insert-time-stamp now 'with-hm 'inactive))
	(setq s (org-time-convert-to-integer
		     (time-subtract
		      (org-time-string-to-time te)
		      (org-time-string-to-time ts)))
		  h (floor s 3600)
		  m (floor (mod s 3600) 60))
	(insert " => " (format "%2d:%02d" h m))
	;; Possibly remove zero time clocks.
    (when (and org-clock-out-remove-zero-time-clocks
		       (= 0 h m))
      (setq remove t)
	  (delete-region (line-beginning-position)
			         (line-beginning-position 2)))
    (org-clock-remove-empty-clock-drawer)
	(message (if remove
		         "Clock stopped at %s after %s => LINE REMOVED"
		       "Clock stopped at %s after %s")
		     te (org-duration-from-minutes (+ (* 60 h) m)))
    ))

(defun dgc/org-table-copy-as-csv ()
  "Copy the org table at point to the kill ring in CSV format."
  (interactive)
  (kill-new (orgtbl-to-csv (org-table-to-lisp) ())))

;;; Magit

(use-package magit
  :demand t
  :after (project)  ; make sure Magit gets a chance to add its option to project's command list
  :custom
  (magit-define-global-key-bindings 'recommended)  ; for some reason setting this in custom.el doesn't work
  :config
  (require 'magit-extras)) 



(defun dgc/magit-browse-remote ()
    (interactive)
  (browse-url (magit-get "remote" (magit-get-remote) "url")))


;;; Utilities

(use-package eat)

(use-package rg
  :bind ("C-c s" . rg-menu))

;;; Progamming / text editing modes

(use-package project
  :demand t)   ;; force load to allow Magit to load at startup time because it's used in most sessions

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-." . eglot-code-actions)))

(use-package eglot-booster
  :straight (:type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :init (eglot-booster-mode))

(use-package tree-sitter)
(use-package tree-sitter-indent)

(use-package yasnippet
  :init (yas-global-mode))

;; (use-package yasnippet-snippets
;;   :demand t
;;   :after (yasnippet))


;; M-x dape, "netcoredbg"
;; M-x dape, "netcoredbg-attach :processId <pid>"
;;
;; running tests:
;; > $env:VSTEST_HOST_DEBUG="1"
;; > dotnet vstest /TestCaseFilter:"Category!=Integration" bin/Debug/net8.0/Tests.dll
;; M-x dape, "netcoredbg-attach :processId <pid>"
(use-package dape
  :config
  (setf (alist-get 'netcoredbg dape-configs)
        '(
          modes (csharp-mode csharp-ts-mode)
          ensure dape-ensure-command
          command "netcoredbg"
          command-args ["--interpreter=vscode" "--log"]
          :request "launch"
          :cwd dape-cwd
          :program (dgc/dotnet-get-target-path)
          :stopAtEntry nil))
  (add-to-list 'dape-configs
               '(netcoredbg-attach
                 modes (csharp-mode csharp-ts-mode)
                 ensure dape-ensure-command
                 command "netcoredbg"
                 command-args ["--interpreter=vscode" "--log"]
                 :request "attach"
                 :cwd dape-cwd
                 :stopAtEntry nil))
  (when (eq system-type 'windows-nt)
    ;; HACK: advise dape to use backslashes in the paths sent to the debug server on Windows.
    ;;
    ;; PDB files are built with backslashes in their path, and netcoredbg does not normalise them.
    ;; dape uses `expand-file-name`, which normalises all the slashes to forward slashes.
    ;; The result is that breakpoints fail to set because the source file names don't match.
    (advice-add 'dape--path-remote :filter-return (lambda (path) (string-replace "/" "\\" path)))))

;;;; Simple modes
(use-package json-mode)
(use-package yaml-mode)
(use-package olivetti)

(use-package terraform-mode
  :custom
  (terraform-format-on-save t))

;;;; dotnet

(cl-defmethod project-root ((project (head dotnet)))
  (nth 1 project))

(cl-defmethod project-name ((project (head dotnet)))
  (nth 2 project))

(defun dgc/locate-dominating-file-regexp (dir match)
  (let ((dominating-dir (locate-dominating-file
               dir
               (lambda (d)
                 (condition-case nil
                     (directory-files d nil match t)
                   (file-missing nil))))))
    (when dominating-dir
      (car (directory-files dominating-dir t match nil 1)))))

(defun project-try-dotnet (dir)
  (let* ((sln-file (dgc/locate-dominating-file-regexp dir "\\`.*\\.sln\\'"))
        (csproj-file (dgc/locate-dominating-file-regexp dir "\\`.*\\.csproj\\'"))
        (file (if (bound-and-true-p eglot-lsp-context) (or sln-file csproj-file) (or csproj-file sln-file)))) ; for eglot prefer the solution first so that all references can be found
    (when file
      (list 'dotnet (file-name-directory file) (file-name-base file)))))

(use-package emacs
  :demand t
  :after (project)
  :config
  (add-to-list 'project-find-functions #'project-try-dotnet))

;; dotnet vstest (dotnet msbuild -v:d -getTargetResult:GetTargetPath | jq -r '.TargetResults.GetTargetPath.Items[0].FullPath') -lt

(defun dgc/json-at-path (json &rest path)
  "Navigate through nested hash tables and arrays in JSON data using a dotted path."
  (let ((result json))
    (dolist (key path result)
      (setq result
            (if (integerp key)
                (aref result key)
              (gethash key result))))))

(defun dgc/dotnet-get-target-path ()
  (let* ((rawJson (with-output-to-string
              (with-current-buffer standard-output
                (call-process "pwsh" nil t nil "-Command" "dotnet msbuild -getTargetResult:GetTargetPath"))))
         (json (json-parse-string rawJson)))
    (dgc/json-at-path json "TargetResults" "GetTargetPath" "Items" 0 "FullPath")))

(defun dgc/dotnet-list-tests ()
  (interactive)
  (call-process "pwsh" nil "*dave*" t "-Command" "dotnet vstest (dotnet msbuild -v:d -getTargetResult:GetTargetPath | jq -r '.TargetResults.GetTargetPath.Items[0].FullPath') -lt"))

(defun dgc/dotnet-watch-test ()
  (interactive)
  (let ((shell-command-buffer-name-async "*dotnet test shell*"))
    (async-shell-command "dotnet watch test --filter \"Category!=Integration\"" nil nil)))
;; (define-key escape-map (kbd "d t") #'dgc/dotnet-watch-test)


;;;; F#

(use-package fsharp-mode
  :mode "\\.fs\\'"
  :init
  (add-hook 'fsharp-mode-hook #'eglot-ensure))

;; note: need to interactively invoke eglot in an fsharp buffer once in order to get FsAutocomplete installed
(use-package eglot-fsharp
  :demand t
  :after (eglot fsharp-mode))

(use-package ob-fsharp
  :demand t
  :after (org))


;;;; C#

(add-to-list 'treesit-language-source-alist '(c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp"))
;; M-x treesit-install-language-grammar c-sharp

(add-hook 'csharp-ts-mode-hook
          (lambda ()
            (setq-local compile-command "dotnet build ")))

;;;; Markdown

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom (markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  )


;;;; Beancount

(use-package beancount-mode
  :straight (:type git :host github :repo "beancount/beancount-mode")
  :mode "\\.beancount\\'"
  :init
  (add-hook 'beancount-mode-hook #'outline-minor-mode)
  :custom
  (beancount-use-ido nil))

(rx-define dgc/date (seq (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
(rx-define dgc/quoted (seq "\"" (+ (or "\\\"" (not "\""))) "\""))

(progn 
  (rx-define bc/date (seq (= 4 digit) ?- (= 2 digit) ?- (= 2 digit)))
  (rx-define bc/string (seq "\"" (* (not "\"")) "\""))
  (rx-define bc/indented-line (seq bol (+ space) (not (any space ?\n)) (* nonl) ?\n))
  (rx-define bc/unknown-posting (seq bol (+ space) (* nonl) (group "Expenses:V1:Unknown") (* nonl) ?\n))
  (rx-define bc/blank-line (seq bol (* space) ?\n))
  (rx-define bc/transaction (seq bol date (+ space) "*" (* nonl) ?\n (+ bc/indented-line)))
  (rx-define bc/transaction-with-unknown (seq bol date (+ space) "*" (* nonl) ?\n (* bc/indented-line) bc/unknown-posting (* bc/indented-line)))
  
  '(seq bc/transaction-with-unknown))

(defun dgc/beancount-allocate-unknown (pattern account)
  "Find transactions matching PATTERN with unknown postings and set them to ACCOUNT."
  (interactive "MPattern: \nMAccount: ")
  (rx-let
      ((date (seq (= 4 digit) ?- (= 2 digit) ?- (= 2 digit)))
       (transaction-start (seq bol date (+ space) "*" (* nonl) (literal pattern) (* nonl) ?\n))
       (indented-line (seq bol (+ space) (not (any space ?\n)) (* nonl) ?\n))
       (unknown-posting (seq bol (+ space) (* nonl) (group "Expenses:V1:Unknown") (* nonl) ?\n))
       (search (seq transaction-start (* indented-line) unknown-posting (* indented-line))))
    (while (re-search-forward (rx search) nil t)
      (replace-match account nil nil nil 1))))

(defun dgc/beancount-find-trans (term)
  "Find all matching transactions"
  (interactive "MPattern: ")
  (let ((regex (rx bol dgc/date (+ blank) "*" (+ blank) (* nonl) (literal term) (* nonl) "\n"
                   (+ (seq (+ blank) (* nonl) "\n")))))
    (occur regex)))

;;;; Go

(use-package go-mode
  :init
  ;; (defun project-find-go-module (dir)
  ;;   (when-let ((root (locate-dominating-file dir "go.mod")))
  ;;     (cons 'go-module root)))

  ;; (cl-defmethod project-root ((project (head go-module)))
  ;;   (cdr project))
  
  ;; (add-hook 'project-find-functions #'project-find-go-module)

  (add-hook 'go-mode-hook #'eglot-ensure))


;;; LLMs
(use-package gptel
  :custom
  (gptel-model "llama3.1")
  :config
  ;; setting gptel-backend in :config instead of :custom to avoid recursive load error
  (setq gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '("llama3.1" "mistral-nemo"))))


;;; environment specific settings

(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load local-config)))

;;; scratch

(when nil
  (defun dgc/start-profiling (&rest r)
    (profiler-start 'cpu+mem))

  (defun dgc/stop-profiling (&rest r)
    (profiler-stop)
    (profiler-report))

  (advice-add 'project-find-file :before #'dgc/start-profiling)
  (advice-add 'project-find-file :after #'dgc/stop-profiling))

;; for errors like: eglot--apply-text-edits: jsonrpc-error: "Edits on ‘...’ require version 0, you have 6"
;; see https://github.com/joaotavora/eglot/issues/1051


;;; automatically added stuff
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
