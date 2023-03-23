;;; flycheck-eglot --- Hacky eglot support in flycheck -*- lexical-binding: t; -*-
;;
;; From https://raw.githubusercontent.com/gagbo/doom-emacs/3e5b7cce3f12e87e2f88e3c7b95ab3c2da3a16ac/modules/tools/lsp/flycheck-eglot.el


(require 'flycheck)

;;; Code:
(defun flycheck-eglot--start (checker callback)
  "Clean up errors when done.

CHECKER is the checker (eglot).
CALLBACK is the function that we need to call when we are done, on all the errors."
  (cl-labels
      ((flymake-diag->flycheck-err
        (diag)
        (with-current-buffer (flymake--diag-buffer diag)
          (flycheck-error-new-at-pos
           (flymake--diag-beg diag)
           (pcase (flymake--diag-type diag)
             ('eglot-note 'info)
             ('eglot-warning 'warning)
             ('eglot-error 'error)
             (_ (error "Unknown diagnostic type, %S" diag)))
           (flymake--diag-text diag)
           :end-pos (flymake--diag-end diag)
           :checker checker
           :buffer (current-buffer)
           :filename (buffer-file-name)))))
    ;; NOTE: Setting up eglot to automatically create flycheck errors for the buffer.
    (eglot-flymake-backend (lambda (flymake-diags &rest _)
                             (funcall callback 'finished (mapcar #'flymake-diag->flycheck-err flymake-diags))))
    ;; NOTE: Forcefully trigger a check in the buffer (function name is confusing)
    (flycheck-buffer)))

(defun flycheck-eglot--available-p ()
  (bound-and-true-p eglot--managed-mode))

(flycheck-define-generic-checker 'eglot
  "Report `eglot' diagnostics using `flycheck'."
  :start #'flycheck-eglot--start
  :predicate #'flycheck-eglot--available-p
  :modes '(prog-mode text-mode))

(push 'eglot flycheck-checkers)

(defun +doom/eglot-prefer-flycheck-h ()
  (when eglot--managed-mode
    (when-let ((current-checker (flycheck-get-checker-for-buffer)))
      (unless (equal current-checker 'eglot)
        (flycheck-add-next-checker 'eglot current-checker)))
    (flycheck-add-mode 'eglot major-mode)
    (flycheck-mode 1)
    (flymake-mode -1)))

(add-hook 'eglot--managed-mode-hook #'+doom/eglot-prefer-flycheck-h)

(provide 'flycheck-eglot)
;;; flycheck-eglot.el ends here

