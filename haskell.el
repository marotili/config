
(setq-default indent-tabs-mode nil)

(require 'powerline)
(powerline-center-evil-theme)
(define-key evil-normal-state-map "\\" nil)
(define-key evil-normal-state-map "\\t" 'universal-argument)
(define-key evil-normal-state-map "\\i" 'universal-argument)
(define-key key-translation-map (kbd "\\t") (kbd "C-c C-t"))
(define-key key-translation-map (kbd "\\i") (kbd "C-c C-i"))
(define-key key-translation-map (kbd "\\h") (kbd "C-c p h"))
(define-key key-translation-map (kbd "\\m") (kbd "C-c h"))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata for Powerline" :foundry "unknown" :slant normal :weight normal :height 100 :width normal)))))

(load "haskell-mode-autoloads")

(require 'haskell-mode)
(require 'haskell-process)
(require 'haskell-simple-indent)
(require 'haskell-interactive-mode)
(require 'haskell-font-lock)
(require 'haskell-debug)
(require 'sgml-mode)
(require 'css-mode)
(require 'ghc)
(require 'company)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(add-to-list 'load-path "~/.emacs.d/elpa/company-20140731.944/")
(autoload 'company-mode "company" nil t)
(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends 'company-ghc)

(custom-set-variables
     '(haskell-process-type 'cabal-repl)
     '(haskell-notify-p t)
     '(haskell-tags-on-save t)
     '(haskell-interactive-popup-errors nil)
     '(haskell-process-suggest-haskell-docs-imports t))

(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

;; (define-key (kbd "C-c h") 'mini-helm)

(defun haskell-hook ()
  (turn-on-haskell-simple-indent)
  (define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)
  (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-restart)
  
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  (define-key haskell-mode-map (kbd "M-i") 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "M-f") 'haskell-mode-tag-find)
  )

(require 'linum-relative)

(global-linum-mode 1)
;; (linum-relative-toogle)
(call-interactively '(lambda ()
                       (interactive)
                       'linux-relative-toggle))


(add-hook 'haskell-mode-hook 'orgstruct-mode)
(setq orgstruct-heading-prefix-regexp "^-- ")
