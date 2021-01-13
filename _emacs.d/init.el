;;; -*- lexical-binding: t -*-

(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(server-start)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq eshell-prompt-function
  (function
   (lambda ()
     (concat (abbreviate-file-name (eshell/pwd)) "\n"
	     (if (= (user-uid) 0) " # " " $ ")))))

(use-package all-the-icons)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(cond
  ((eq system-type 'darwin)
    (set-face-font 'default "-*-Source Code Pro-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
    (setq mac-right-command-modifier 'super)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    ;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
    (setq mac-right-option-modifier 'nil)

    (global-set-key (kbd "s-x") 'clipboard-kill-region)
    (global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
    (global-set-key (kbd "s-v") 'clipboard-yank)

    (global-set-key (kbd "s-z") 'undo-tree-undo)
    (global-set-key (kbd "s-Z") 'undo-tree-redo)

    (global-set-key (kbd "s-a") 'mark-whole-buffer)
  )

  ((eq system-type 'windows-nt)
    (set-face-font 'default "-*-Consolas-normal-normal-normal-*-14-*-*-*-m-0-iso8859-1")

    (setq inhibit-compacting-font-caches t) ;; Speed things up when using all-the-icons

    (global-set-key (kbd "C-S-x") 'clipboard-kill-region)
    (global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save)
    (global-set-key (kbd "C-S-v") 'clipboard-yank)

    (global-set-key (kbd "C-S-z") 'undo-tree-undo)
    (global-set-key (kbd "C-S-y") 'undo-tree-redo)

    (global-set-key (kbd "C-S-a") 'mark-whole-buffer)
  )
)

(setq select-enable-clipboard nil)
   
(winner-mode 1)
(setq scroll-conservatively most-positive-fixnum)
(desktop-save-mode 1)

(add-to-list 'load-path (locate-user-emacs-file "extra/"))

(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

(require 'ox-confluence)

(use-package flyspell
  :init
  (when (eq system-type 'darwin) (setq ispell-program-name "/usr/local/bin/aspell"))
  :bind (:map flyspell-mouse-map
              ([mouse-3] . flyspell-correct-word))
)

(use-package impatient-mode)

(use-package vlf
  :config
  (require 'vlf-setup)
)

(use-package minions
  :config
  (minions-mode 1)
)

;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-gruvbox)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-neotree-config)
;;   (doom-themes-org-config)
;; )

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  (doom-modeline-mode . (lambda () (doom-modeline-set-modeline 'custom 'default)))

  :config
  (setq doom-modeline-minor-modes t)

  (doom-modeline-def-segment minor-modes
    (when doom-modeline-minor-modes
      (let ((active (doom-modeline--active)))
        (if (bound-and-true-p minions-mode)
            (concat
             (doom-modeline-spc)
             (format-mode-line minions-mode-line-modes
                               (if active
                                   'doom-modeline-buffer-minor-mode
                                 'mode-line-inactive))
             (doom-modeline-spc))
          (propertize
           (concat
            (replace-regexp-in-string (regexp-quote "%")
                                      "%%%%"
                                      (format-mode-line minor-mode-alist)
                                      t t)
            (doom-modeline-spc))
           'face (if active
                     'doom-modeline-buffer-minor-mode
                   'mode-line-inactive))))))

  (doom-modeline-def-modeline 'custom
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(objed-state misc-info persp-name irc mu4e github debug lsp minor-modes input-method indent-info buffer-encoding process vcs checker))
)

(use-package leuven-theme
  :hook
  (doom-modeline-mode . (lambda ()
    (set-face-foreground 'doom-modeline-info (face-foreground 'mode-line-emphasis))
    (set-face-foreground 'doom-modeline-buffer-minor-mode (face-foreground 'mode-line-emphasis))))

  :config
  (load-theme 'leuven)
  (set-face-background 'default "AntiqueWhite1")
  (set-face-background 'fringe "AntiqueWhite1")
)


(use-package window-purpose
  :config
  (purpose-mode)

  (setq purpose-use-default-configuration nil)
  (setq default-purpose 'general)
  (setq default-file-purpose 'general)

  (purpose-x-magit-multi-on)

  (setq purpose-user-mode-purposes
        '((magit-process-mode . messages)
          (flycheck-error-list-mode . messages)
          (flymake-diagnostics-buffer-mode . messages)
          (elfeed-search-mode . directory)
          (elfeed-show-mode . preview)
          (help-mode . help)))

  (setq purpose-user-name-purposes
        '(("*Org Agenda(a)*" . org-agenda-day)
          ("*eldoc*" . messages)
          ("*Org Agenda(t)*" . org-agenda-todo)))

  (setq purpose-user-regexp-purposes
        '(("main\\.org" . main-org)))

  (add-to-list 'purpose-special-action-sequences
               '(messages
                 purpose-display-reuse-window-buffer
                 purpose-display-reuse-window-purpose
                 purpose-display-at-bottom)
               '(help
                 purpose-display-reuse-window-buffer
                 purpose-display-reuse-window-purpose
                 purpose-display-at-right))

  (purpose-compile-user-configuration)

  (defalias 'ivy-switch-buffer-without-purpose
    (without-purpose-command #'ivy-switch-buffer))

  (define-purpose-prefix-overload purpose-ivy-switch-buffer-overload
    '(ivy-switch-buffer
      ivy-switch-buffer-without-purpose
      purpose-switch-buffer-with-purpose))
)

(defun clean-up-saved-buffers ()
  "Kill all file-visiting buffers that are unmodified."
  (interactive)
  (mapc
    (lambda (buffer) (when (buffer-file-name buffer) (kill-buffer-if-not-modified buffer)))
    (buffer-list)))

(defun clean-up-buffers ()
  "Kill all file-visiting buffers that are unmodified and all buffers with names beginning with an asterisk."
  (interactive)
  (clean-up-saved-buffers)
  (mapc
    (lambda (buffer) (when (string-prefix-p "*" (buffer-name buffer)) (kill-buffer buffer)))
    (buffer-list)))

(setq space-map (make-sparse-keymap))
(global-set-key (kbd "M-SPC") space-map)
(define-key space-map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
(define-key space-map (kbd "c") 'calc-dispatch)
(define-key space-map (kbd "x") 'delete-window)
(define-key space-map (kbd "1") 'delete-other-windows)
(define-key space-map (kbd "-") (lambda () (interactive) (select-window (split-window-below))))
(define-key space-map (kbd "|") (lambda () (interactive) (select-window (split-window-right))))
(define-key space-map (kbd "e n") (lambda () (interactive) (if flycheck-mode (flycheck-next-error) (flymake-goto-next-error))))
(define-key space-map (kbd "e p") (lambda () (interactive) (if flycheck-mode (flycheck-previous-error) (flymake-goto-prev-error))))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "SPC") space-map))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "SPC") space-map))

(with-eval-after-load 'cus-edit
  (define-key custom-mode-map (kbd "SPC") space-map))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-S-s") 'counsel-outline)

(global-set-key "\C-x\C-b" 'ibuffer)

(global-set-key "\M-u" 'universal-argument)
(define-key universal-argument-map "\M-u" 'universal-argument-more)

(use-package evil
  :demand
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-motion-state-map (kbd "SPC") space-map)

  ;; Don't shadow xref key bindings
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "M-,") nil)

  (fset 'evil-visual-update-x-selection 'ignore)
)

(use-package neotree
  :config
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
)

(use-package ace-window
  :bind (:map space-map
         ("w" . ace-window))
)

(use-package counsel
  :init
  (setq ivy-initial-inputs-alist '())
  (setq counsel-git-grep-cmd-default "git --no-pager grep --full-name -n --no-color -i -e \"%s\"")
  (setq counsel-git-grep-skip-counting-lines t)
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  :bind (
    ("C-s" . swiper)
    ("M-x" . counsel-M-x)
    ("<f1> f" . counsel-describe-function)
    ("<f1> v" . counsel-describe-variable)
    ("<f1> l" . counsel-find-library)
    ("<f2> i" . counsel-info-lookup-symbol)
    ("<f2> u" . counsel-unicode-char)
    :map space-map
    ("b" . purpose-ivy-switch-buffer-overload)
    ("f" . counsel-find-file)
    ("SPC" . counsel-M-x))
)

;; This is slow when displaying fsharp project names...
;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-mode 1)
;;   (setq ivy-rich-path-style 'abbrev))

(use-package ivy-hydra)
(use-package smex)

(use-package magit
  :bind (("C-x C-g" . magit-status)
         :map space-map
         ("g s" . magit-status)
         ("g f" . counsel-git)
         ("g g" . counsel-git-grep))
  :hook
  (magit-mode . (lambda () (setq display-line-numbers nil)))
  :config
  (unbind-key "SPC" magit-blame-mode-map)
  (use-package evil-magit
    :demand
    :config
    (evil-define-key 'normal magit-mode-map (kbd "SPC") space-map)
  )
)

(use-package which-key
  :config
  (which-key-mode 1)
  :diminish which-key-mode
)

(use-package projectile
  :bind (:map space-map
         ("p" . projectile-command-map))
)

(use-package company
  :demand t
  :config
  (global-company-mode)
  :bind (("C-SPC" . company-complete)
         :map company-active-map
         ("C-n" . 'company-select-next)
         ("C-p" . 'company-select-previous))
)

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; (use-package flycheck
;;   :bind (:map space-map
;;          ("e n" . flycheck-next-error)))

(use-package eglot)
  ;; :hook (go-mode . eglot-ensure))

(use-package fsharp-mode
  :mode "\\.fsx?\\'"
  :config
  (require 'eglot-fsharp)
  (add-hook 'fsharp-mode-hook 'eglot-ensure)
  (add-hook 'fsharp-mode-hook (lambda () (set (make-local-variable 'compile-command) "./build.sh")))
)

(use-package elm-mode
  :mode "\\.elm\\'"
)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package powershell
  :mode ("\\.ps1\\'" . powershell-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
)

(use-package web-mode
  :mode "\\.tsx\\'"
)

(defun setup-tide-mode ()
  (tide-setup)
  (define-key evil-normal-state-local-map "gd" 'tide-jump-to-definition)
  (flycheck-mode 1)
  (eldoc-mode 1))

(use-package tide
  :init
  (add-hook 'typescript-mode-hook 'setup-tide-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (and buffer-file-name (string-equal "tsx" (file-name-extension buffer-file-name))) (setup-tide-mode))))
)

(use-package elpy
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package elfeed
  :init
  (setq elfeed-feeds
        '(("https://forums.aws.amazon.com/rss/rssannounce.jspa?forumID=24" aws s3)  ;; S3 announcements
          ("https://forums.aws.amazon.com/rss/rssannounce.jspa?forumID=30" aws ec2)  ;; EC2 announcements
          ("https://forums.aws.amazon.com/rss/rssannounce.jspa?forumID=155" aws redshift) ;; Redshift announcements
          ("https://forums.aws.amazon.com/rss/rssannounce.jspa?forumID=87" aws route53)  ;; Route 53 announcements
          ("https://forums.aws.amazon.com/rss/rssannounce.jspa?forumID=206" aws acm) ;; Cert Mgr announcements
          ))
  )

(use-package elpher)

(use-package php-mode
  :mode "\\.php\\'")

(use-package lsp-mode
  :disabled    ;; using eglot
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (go-mode . lsp-deferred)))

(use-package go-mode
  :mode "\\.go\\'")

(use-package csharp-mode
  :mode "\\.cs\\'")

(use-package omnisharp
  ;; :hook csharp-mode
  :config
  (add-to-list 'company-backends 'company-omnisharp))


(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
   same directory as the org-buffer and insert a link to this file."  
   (interactive)
   (setq filename (concat (make-temp-name (concat (buffer-file-name) "_" (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
   (shell-command "snippingtool /clip")
   (sleep-for 0.5)
   (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
   (insert (concat "[[file:" filename "]]"))
   (org-display-inline-images))

(let ((local-config "~/.emacs.d/local.el"))
  (when (file-exists-p local-config)
      (load local-config)))
