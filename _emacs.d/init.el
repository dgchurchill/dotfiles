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

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(setq space-map (make-sparse-keymap))
(global-set-key (kbd "M-SPC") space-map)
(global-set-key (kbd "C-M-S-<f1>") space-map)

(use-package prescient)

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode))

(use-package vertico-prescient
  :init
  (vertico-prescient-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle)
         ;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action!
         ;;:map embark-general-map
         ;;     ("A" . marginalia-cycle)
        )

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  ;; (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
)

(use-package embark
  :bind
  ("C-M-." . embark-act)               ; pick some comfortable binding
)

(use-package consult
  :bind (("C-x M-:" . consult-complex-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-keep-lines)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-S-s" . consult-outline)
         ("C-s" . consult-line)
         ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
         ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
         ("M-g r" . consult-git-grep)      ;; or consult-grep, consult-ripgrep
         ("M-g f" . consult-find)          ;; or consult-locate, my-fdfind
         ("M-g i" . consult-project-imenu) ;; or consult-imenu
         ("M-g e" . consult-error)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)

         :map space-map
         ("g f" . consult-find))

  :init
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure register preview function.
  ;; This gives a consistent display for both `consult-register' and
  ;; the register preview when editing registers.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)

  :config
  ;; Configure preview. Note that the preview-key can also be configured on a
  ;; per-command basis via `consult-config'.
  ;; The default value is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-p"))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

  ;; Optionally configure narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

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
    (if (< (display-pixel-height) 1850)
      (set-face-font 'default "-*-Consolas-normal-normal-normal-*-14-*-*-*-m-0-iso8859-1")
      (set-face-font 'default "-*-Consolas-normal-normal-normal-*-24-*-*-*-m-0-iso8859-1"))

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


;; this is required so that the latest version will be pulled from a repo
;; instead of using the version that comes with Emacs.
;; eglot and some other packages need this
(use-package project)


(use-package deadgrep
  :bind (:map space-map
         ("g g" . deadgrep)))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)))

(use-package pdf-tools)


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

  :config
  (setq doom-modeline-minor-modes t)
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

  :bind (
    :map space-map
         ("b" . purpose-switch-buffer-overload)
         ("f" . purpose-find-file-overload))
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

(define-key space-map (kbd "SPC") 'execute-extended-command)
(define-key space-map (kbd "C-M-S-<f1>") 'execute-extended-command)
(define-key space-map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
(define-key space-map (kbd "c") 'calc-dispatch)
(define-key space-map (kbd "x") 'delete-window)
(define-key space-map (kbd "1") 'delete-other-windows)
(define-key space-map (kbd "-") (lambda () (interactive) (select-window (split-window-below))))
(define-key space-map (kbd "|") (lambda () (interactive) (select-window (split-window-right))))
(define-key space-map (kbd "e n") (lambda () (interactive) (if flycheck-mode (flycheck-next-error) (flymake-goto-next-error))))
(define-key space-map (kbd "e p") (lambda () (interactive) (if flycheck-mode (flycheck-previous-error) (flymake-goto-prev-error))))

(require 'org-protocol)

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "SPC") space-map))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "SPC") space-map))

(with-eval-after-load 'cus-edit
  (define-key custom-mode-map (kbd "SPC") space-map))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(define-key space-map (kbd "o l") 'org-store-link)
(define-key space-map (kbd "o a") 'org-agenda)
(define-key space-map (kbd "o c") 'org-capture)
(define-key space-map (kbd "o x i") 'org-clock-in)
(define-key space-map (kbd "o x x") 'org-clock-in-last)
(define-key space-map (kbd "o x z") 'org-resolve-clocks)
(define-key space-map (kbd "o x o") 'org-clock-out)
(define-key space-map (kbd "o x j") 'org-clock-goto)
(define-key space-map (kbd "o x q") 'org-clock-cancel)
(define-key space-map (kbd "o x d") 'org-clock-display)

(global-set-key "\C-x\C-b" 'ibuffer)

(global-set-key "\M-u" 'universal-argument)
(define-key universal-argument-map "\M-u" 'universal-argument-more)

(use-package evil
  :demand
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

;; (use-package smex)

(use-package magit
  :bind (("C-x C-g" . magit-status)
         :map space-map
         ("g s" . magit-status))
         ;; ("g f" . counsel-git)
         ;; ("g g" . counsel-git-grep))
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

(use-package orgit
  :after magit)

;; (use-package forge
;;   :after magit)

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

(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package flycheck)

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package eglot)
  ;; :config
  ;; (load "flycheck-eglot.el")
  ;; (require 'flycheck-eglot))
  ;; :hook (go-mode . (lambda ()
  ;;                    ;; (eglot-ensure)
  ;;                   (flycheck-add-next-checker 'eglot 'golangci-lint))))


(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode))


(use-package fsharp-mode
  :mode "\\.fsx?\\'"
  :config
  (add-hook 'fsharp-mode-hook 'eglot-ensure)
  (add-hook 'fsharp-mode-hook 'dap-auto-configure-mode)
)

(use-package eglot-fsharp
  :after (fsharp-mode eglot))

(use-package elm-mode
  :mode "\\.elm\\'"
)


(defun markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmpname (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmpname))
       (buffer-string)))
   (current-buffer)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
        (add-hook 'markdown-mode-hook (lambda () (imp-set-user-filter #'markdown-filter))))


(use-package powershell
  :mode ("\\.ps1\\'" . powershell-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
)

(use-package web-mode
  :mode "\\.tsx\\'"
)

(use-package yaml-mode
  :mode "\\.yaml\\'"
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
  ;; :disabled    ;; using eglot
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (go-mode . lsp-deferred)))

(use-package dap-mode
  :config
  (require 'dap-netcore))

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :hook ((go-mode . go-eldoc-setup)))

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package csharp-mode
  :mode "\\.cs\\'")

(use-package omnisharp
  ;; :hook csharp-mode
  :config
  (add-to-list 'company-backends 'company-omnisharp))


(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package company-terraform
  :config
  (company-terraform-init))

(use-package bicep-mode
  :straight (bicep-mode :type git :host github :repo "christiaan-janssen/bicep-mode")
  :mode "\\.bicep\\'")


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
