(setq package-enable-at-startup nil)

; gdi seems to render fonts more clearly
(when (eq system-type 'windows-nt)
  (add-to-list 'default-frame-alist '(font-backend . (gdi))))

