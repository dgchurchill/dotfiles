;;; -*- lexical-binding: t; -*-

;;; Set up straight and use-package

(setq straight-use-package-by-default t)
(setq straight-use-symlinks t) ; use symlinks, even on Windows (requires Developer Mode to be turned on in Windows to avoid UAC prompts)

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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(server-start)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (exec-path-from-shell-initialize))

(when (eq window-system 'w32)
  (setq explicit-shell-file-name "pwsh"))

;;; General keybindings

(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Set up escape as a leader
;;
;; <escape> is the escape key when in a window system. It's normally translated to ESC (^[) by
;; function-key-map, but not if we explicitly bind it.  Don't bind to ESC, because that's where all
;; the M- keybindings actually live (via the meta-prefix-char variable).
(setq escape-map (make-sparse-keymap))
(global-set-key (kbd "<escape>") escape-map)
(define-key escape-map (kbd "ESC") #'keyboard-escape-quit)
(define-key escape-map (kbd "b k") #'kill-current-buffer)


;;; Themes and appearance

(use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-operandi-tinted))

(set-face-attribute 'default nil :family "Iosevka Slab" :height 140 :weight 'light)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Slab" :height 1.0 :weight 'light)
(set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 1.0 :weight 'light)

;; something about Windows not treating "light" as a weight but as a separate font?
(when (eq window-system 'w32)
  (set-face-attribute 'default nil :font "Iosevka Slab Light-14")
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Slab Light-14")
  (set-face-attribute 'variable-pitch nil :font "Iosevka Etoile Light-14"))

(use-package diminish)


;;; Window management

(use-package transpose-frame)

(defun make-display-buffer-matcher-function (major-modes)
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

(setq
  switch-to-buffer-in-dedicated-window 'pop
  switch-to-buffer-obey-display-actions t

  display-buffer-alist
    `((,(make-display-buffer-matcher-function '(shell-mode))
       (display-buffer-reuse-mode-window display-buffer-in-direction)
       (window . root)
       (window-height . 15)
       (direction . bottom))))


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
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*.el"))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
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

(use-package recentf
  :init
  (recentf-mode))


;;; Org mode

(use-package org
  :init
  (require 'org-protocol)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode))

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


;;; Magit

(use-package magit)

(defun dgc/magit-browse-remote ()
    (interactive)
  (browse-url (magit-get "remote" (magit-get-remote) "url")))


;;; Utilities

(use-package deadgrep
  :bind (:map escape-map
         ("g g" . deadgrep))
)

;;; Progamming / text editing modes

(use-package eglot)


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


;;; environment specific settings

(let ((local-config "~/.emacs.d/local.el"))
  (when (file-exists-p local-config)
      (load local-config)))
