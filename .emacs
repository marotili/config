(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa") t)

(require 'evil)
(evil-mode 1)

(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(add-hook 'haskell-mode-hook 'inf-haskell-mode)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(autoload 'company-mode "company" nil t)
(add-hook 'after-init-hook 'global-company-mode)
(autoload 'company-ghc "company-ghc" nil t)
(setq company-backends '(company-ghc))
(custom-set-variables
 '(company-ghc-show-info t)
)

(load-theme 'monokai t)
