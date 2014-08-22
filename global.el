(require 'evil)
(evil-mode 1)


(setq speedbar-directory-unshown-regexp "^$")

(projectile-global-mode)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)

;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'monokai t)
