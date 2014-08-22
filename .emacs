;; Standard libraries needed

(require 'cl)
(require 'package)
(setq package-archives '(
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ))
(package-initialize)


;; Packages and configs to load

;; (defvar packages
;;   '(haskell-mode
;;     smex
;;     notify
;;     goto-last-change
;;     goto-last-point
;;     ))

(defvar configs
  '("global"
    "haskell"
    ))


;; Load packages

(add-to-list 'load-path "~/.emacs.d/elpa/shm-20140804.151")
;; (add-to-list 'load-path
;;              (concat (file-name-directory load-file-name)
;;                      "packages/"
;;                      "ghc-server/elisp"))

(require 'shm)
(require 'ghc)
(require 'shm-case-split)
(require 'w3m-haddock)


;; Global/standard Emacs configuration

(loop for name in configs
      do (load (concat (file-name-directory load-file-name)
                       "config/"
                       name ".el")))


;; Mode initializations

(smex-initialize)
(turn-on-haskell-simple-indent)
(load "haskell-mode-autoloads.el")
