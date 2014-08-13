
(require 'package)
(setq package-archives '(
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ))
(package-initialize)

(require 'evil)
(evil-mode 1)

;; (require 'shm)
;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'monokai t)

;; (eval-after-load "haskell-mode"
;;  '(progn
;;     (define-key haskell-mode-map (kbd "C-x C-d") nil)
;;     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
;;     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;;     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;;     (define-key haskell-mode-map (kbd "C-c M-.") nil)
;;     (define-key haskell-mode-map (kbd "C-c C-d") nil)
;;     (define-key haskell-mode-map (kbd "M-p") nil)))

;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; (add-hook 'haskell-mode-hook 'haskell-indent-mode)

(autoload 'company-mode "company" nil t)
(add-hook 'after-init-hook 'global-company-mode)
(autoload 'company-ghc "company-ghc" nil t)
(setq company-backends '(company-ghc))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(haskell-process-log t)
 '(haskell-process-type (quote cabal-repl))
 '(inferior-haskell-wait-and-jump t))

(setq speedbar-directory-unshown-regexp "^$")

(projectile-global-mode)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)

(setq-default indent-tabs-mode nil)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata for Powerline" :foundry "unknown" :slant normal :weight normal :height 100 :width normal)))))


(require 'flymake-json)
(add-hook 'json-mode 'flymake-json-load)
(require 'haskell-mode)
(require 'haskell-process)
(require 'haskell-simple-indent)
(require 'haskell-interactive-mode)
(require 'haskell-font-lock)
(require 'haskell-debug)
(require 'sgml-mode)
(require 'css-mode)
(require 'ghc)


;; Functions

(defun haskell-interactive-toggle-print-mode ()
  (interactive)
  (setq haskell-interactive-mode-eval-mode
        (intern
         (ido-completing-read "Eval result mode: "
                              '("fundamental-mode"
                                "haskell-mode"
                                "espresso-mode"
                                "ghc-core-mode"
                                "org-mode")))))

(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (insert "-- | "))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (if (and (boundp 'structured-haskell-mode)
           structured-haskell-mode)
      (shm-insert-string "undefined")
    (insert "undefined")))

(defun haskell-move-right ()
  (interactive)
  (haskell-move-nested 1))

(defun haskell-move-left ()
  (interactive)
  (haskell-move-nested -1))

(defvar haskell-process-use-ghci nil)

(defun haskell-process-cabal-build-and-restart ()
  "Build and restart the Cabal project."
  (interactive)
  (cond
   (haskell-process-use-ghci
    (when (buffer-file-name)
      (save-buffer))
    ;; Reload main module where `main' function is
    (haskell-process-reload-devel-main))
   (t
    (haskell-process-cabal-build)
    (haskell-process-queue-without-filters
     (haskell-process)
     (format ":!cd %s && scripts/restart\n" (haskell-session-cabal-dir (haskell-session)))))
   (t (turbo-devel-reload))))

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (cond
     ;; Use grep
     (nil (let ((buffer
                 (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                                    (haskell-session-current-dir (haskell-session))
                                    sym))))
            (with-current-buffer buffer
              (rename-buffer "*who-calls*")
              (switch-to-buffer-other-window buffer))))
     ;; Use ag
     (t (ag-files sym
                  "\\.hs$"
                  (haskell-session-current-dir (haskell-session)))))))

(defun haskell-auto-insert-module-template ()
  "Insert a module template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (insert
     "-- | "
     "\n"
     "\n"
     "module "
     )
    (let ((name (haskell-guess-module-name)))
      (if (string= name "")
          (progn (insert "Main")
                 (shm-evaporate (- (point) 5)
                                (point)))
        (insert name)))
    (insert " where"
            "\n"
            "\n")
    (goto-char (point-min))
    (forward-char 4)
    (god-mode)))

(defun shm-contextual-space ()
  "Do contextual space first, and run shm/space if no change in
the cursor position happened."
  (interactive)
  (if god-local-mode
      (call-interactively 'god-mode-self-insert)
    (if (looking-back "import")
        (call-interactively 'haskell-mode-contextual-space)
      (progn
        (let ((ident (haskell-ident-at-point)))
          (when ident
            (haskell-process-do-try-type ident)))
        (call-interactively 'shm/space)))))

(defun shm/insert-putstrln ()
  "Insert a putStrLn."
  (interactive)
  (let ((name
         (save-excursion
           (goto-char (car (shm-decl-points)))
           (buffer-substring-no-properties
            (point)
            (1- (search-forward " "))))))
    (insert
     (format "putStrLn \"%s:%s:%d\""
             (file-name-nondirectory (buffer-file-name))
             name
             (line-number-at-pos)))))

(defun haskell-switch-mode (mode)
  "Switch the interaction mode."
  (interactive
   (list (ido-completing-read "Mode: " '("interactive-mode" "ghc-mode"))))
  (cond
   ((string= mode "interactive-mode")
    (remove-hook 'haskell-mode-hook 'ghc-mode)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (loop for buffer
          in (buffer-list)
          do (with-current-buffer buffer
               (when (eq major-mode 'haskell-mode)
                 (ghc-mode -1)
                 (interactive-haskell-mode 1)))))
   ((string= mode "ghc-mode")
    (add-hook 'haskell-mode-hook 'ghc-mode)
    (remove-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (loop for buffer
          in (buffer-list)
          do (with-current-buffer buffer
               (when (eq major-mode 'haskell-mode)
                 (ghc-mode 1)
                 (interactive-haskell-mode -1)))))))

(custom-set-variables
    '(haskell-process-type 'cabal-repl)
    '(haskell-notify-p t)
    '(haskell-stylish-on-save t)
    '(haskell-tags-on-save t)
    '(haskell-process-suggest-remove-import-lines t)
    '(haskell-process-auto-import-loaded-modules t)
    '(haskell-process-log t)
    '(haskell-process-reload-with-fbytecode nil)
    '(haskell-process-use-presentation-mode t)
    '(haskell-interactive-mode-include-file-name nil)
    '(haskell-interactive-mode-eval-pretty nil)
    '(shm-use-hdevtools t)
    '(shm-use-presentation-mode t)
    '(shm-auto-insert-skeletons t)
    '(shm-auto-insert-bangs t)
    '(haskell-process-show-debug-tips nil)
    '(haskell-process-suggest-hoogle-imports nil)
    '(haskell-process-suggest-haskell-docs-imports t))

(setq haskell-interactive-mode-eval-mode 'haskell-mode)

(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
(add-hook 'w3m-display-hook 'w3m-haddock-display)
(add-hook 'haskell-mode-hook 'relative-line-number-mode)

(setq haskell-i-mode 'interactive-mode)
(case haskell-i-mode
    (interactive-mode
        (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
    (ghc-mode
      (add-hook 'haskell-mode-hook 'ghc-mode)))
(setq haskell-interactive-mode-eval-mode 'haskell-mode)
(define-key haskell-mode-map [kbd "C-c C-l"] 'ghc/load)

(define-key interactive-haskell-mode-map [kbd "C-c C-l"] 'haskell-process-load-or-reload)
(define-key interactive-haskell-mode-map [f12] 'turbo-devel-reload)
(define-key interactive-haskell-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key interactive-haskell-mode-map [kbd "M-,"] 'haskell-who-calls)
(define-key interactive-haskell-mode-map [kbd "C-`"] 'haskell-interactive-bring)
(define-key interactive-haskell-mode-map [kbd "C-c C-k"] 'haskell-interactive-mode-clear)
(define-key interactive-haskell-mode-map [kbd "C-c C-c"] 'haskell-process-cabal-build)
(define-key interactive-haskell-mode-map [kbd "C-c c"] 'haskell-process-cabal)
(define-key interactive-haskell-mode-map [kbd "M-."] 'haskell-mode-jump-to-def-or-tag)


(define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)
(define-key haskell-mode-map [f8] 'haskell-navigate-imports)
(define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)
(define-key haskell-mode-map (kbd "C-c C-a") 'haskell-insert-doc)
(define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)
(define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)
(define-key haskell-mode-map (kbd "C-<right>") 'haskell-move-right)
(define-key haskell-mode-map (kbd "C-<left>") 'haskell-move-left)
(define-key haskell-mode-map (kbd "<space>") 'haskell-mode-contextual-space)

(define-key haskell-cabal-mode-map [f9] 'haskell-interactive-mode-visit-error)
(define-key haskell-cabal-mode-map [f11] 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)

(define-key haskell-interactive-mode-map (kbd "C-c C-v") 'haskell-interactive-toggle-print-mode)
(define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-interactive-mode-map [f9] 'haskell-interactive-mode-visit-error)
(define-key haskell-interactive-mode-map [f11] 'haskell-process-cabal-build)
(define-key haskell-interactive-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key haskell-interactive-mode-map (kbd "C-<left>") 'haskell-interactive-mode-error-backward)
(define-key haskell-interactive-mode-map (kbd "C-<right>") 'haskell-interactive-mode-error-forward)
(define-key haskell-interactive-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; (define-key shm-map (kbd "C-c C-p") 'shm/expand-pattern)
;; (define-key shm-map (kbd "C-c C-s") 'shm/case-split)
;; (define-key shm-map (kbd "SPC") 'shm-contextual-space)
;; (define-key shm-map (kbd "C-\\") 'shm/goto-last-point)

(require 'linum-relative)
