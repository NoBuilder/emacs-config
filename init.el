 (when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(tooltip-mode  -1)
(menu-bar-mode -1)

(global-hl-line-mode +1)

(which-function-mode +1)

(use-package spacemacs-theme
  :config
  (load-theme 'spacemacs-dark t))

;; Don't wait for terminal after opening a client.
;; This allows `emacsclient -t` to immedietly open requested file.
(setq xterm-query-timeout nil)

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)

; disable beeping in GUI
(setq ring-bell-function 'ignore)

(set-frame-font "monaco")

; no tabs please!
(setq-default indent-tabs-mode nil)

(setq-default buffer-save-without-query nil)

(setq inhibit-startup-screen t)

; increase amount of data read from the process (1M)
(setq read-process-output-max (* 1024 1024))

(add-hook 'text-mode-hook 'auto-fill-mode)

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(if (file-exists-p custom-file)
    (load custom-file))

; Pro prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable mouse support
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

; Show column next to line in modeline
(setq column-number-mode t)

(global-set-key (kbd "C-k") 'soji/kill-line-or-region)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c c o") 'soji/find-emacs-config)
(global-set-key (kbd "<C-return>") 'save-buffer)
(global-set-key (kbd "<M-return>") 'open-line)
(global-set-key (kbd "C-y") 'soji/yank-and-indent)
(global-set-key (kbd "C-x y") 'yank)


(global-set-key (kbd "C-c =") 'soji/resize-window)

;; My International keyboard layout uses right alt for accents I don't use.
;; Bind those characters to proper bindings. Hacky but easy.
(global-set-key "æ" (kbd "M-f"))
(global-set-key "”" (kbd "M-b"))
(global-set-key "ŋ" (kbd "M-g g"))
(global-set-key "œ" (kbd "M-w"))
(global-set-key (kbd " ") " ") ; M-SPC -> nbsp. Often mistyped.

(setq use-dialog-box nil)

(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-c c c") 'comment-or-uncomment-region)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


; Better terminal scrolling
(xterm-mouse-mode)

; Delete original selection on typing
(delete-selection-mode)

;; Save places in file

(save-place-mode +1)

;; Backups et al.

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq auto-save-default nil)
(setq create-lockfiles nil)

;;; Packages

(require 'package)


(setq package-native-compile t)

(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))

; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq show-paren-delay 0)
(show-paren-mode +1)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(use-package diminish)

(use-package xclip
  :config
  (xclip-mode))

(use-package ripgrep)

(use-package smartparens
  :hook
  ((tide-mode) . smartparens-mode))


(use-package use-package-chords
  :config
  (key-chord-mode 1))

(use-package ivy
  :diminish

  :init
  (ivy-mode)

  :bind (("C-s" . swiper)
	 ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x))

  :chords ((";s" . swiper)
           (";r" . ivy-resume)
           (";v" . counsel-imenu))

  :config
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil))


(use-package smex
  :init
  (smex-initialize))

;(require 'apheleia)

;(apheleia-global-mode +1)

(use-package emmet-mode
  :hook ((css-mode web-mode) . emmet-mode))

(use-package avy
  :bind (("C-l" . avy-goto-word-1))
  :config
  :chords ((";e" . avy-goto-word-1)))

(use-package paredit
  :diminish
  :hook ((clojure-mode emacs-lisp-mode typescript-mode) . paredit-mode)
  :bind (:map paredit-mode-map
	      (("C-k" . soji/kill-line-or-region)))
  :config)

(use-package projectile
  :diminish
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(use-package counsel-projectile
  :requires (ivy projectile)
  :chords
  ((";f" . counsel-projectile-find-file)
   (";g" . counsel-projectile-rg))
  :init
  (counsel-projectile-mode +1))

(use-package dap-mode)

(use-package ein
  :config
  ;; ein doesn't expose map until it is used
  ;; (progn
  ;;   (with-eval-after-load 'ein-notebook
  ;;     (key-chord-define ein:notebook-mode-map ",t" 'ein:tb-show)
  ;;     (key-chord-define ein:notebook-mode-map ",c" 'ein:worksheet-execute-cell)))
  )

(use-package jupyter
  :config
  (key-chord-define jupyter-repl-interaction-mode-map ",c" 'soji/jupyter-eval-defun-body))

(use-package expand-region
  :bind (("C-o" . er/expand-region)
         ("M-o" . er/expand-region)))

(use-package flycheck)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode +1))


;(use-package company-anaconda)

(use-package company
  :diminish
  :init
  (global-company-mode +1)
  :config
  (setq company-idle-delay 0))

(use-package tide
  :requires (company)
  :commands (tide-setup tide-mode)
  :init
  (add-hook 'typescript-mode-hook #'soji/setup-tide-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . typescript-mode))
  :config
  (key-chord-define tide-mode-map ",r" 'tide-rename-symbol)
  (key-chord-define tide-mode-map ",u" 'tide-references)
  (key-chord-define tide-mode-map ",f" 'tide-fix)
  (key-chord-define tide-mode-map ",d" 'tide-jump-to-definition))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

  (if (package-installed-p 'tide)
      (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(soji/setup-tide-mode))))))

;; (use-package cider
;;   :ensure nil
;;   :config
;;   (setq cider-repl-display-help-banner nil)
;;   (key-chord-define cider-repl-mode-map ",e" 'cider-switch-to-last-clojure-buffer)
;;   (key-chord-define cider-mode-map ",c" 'cider-switch-to-repl-buffer)
;;   (key-chord-define cider-mode-map ",r" 'cider-inspect-last-result)
;;   (key-chord-define cider-mode-map ",e" 'cider-eval-defun-at-point)
;;   (key-chord-define cider-mode-map ",f" 'cider-read-and-eval-defun-at-point))

(use-package magit
  :bind
  (("C-c g s" . magit-status)
   ("C-c g f" . magit-find-file-other-window))
  :config
  ; Disable built-in VC handling.
  (setq vc-handled-backends nil)

  ; Don't try save associated visited files before doing any magit op.
  (setq magit-save-repository-buffers nil))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode +1)
  :hook ((clojure-mode emacs-lisp-mode tide-mode web-mode) . yas-minor-mode))

(use-package restclient)

;; (use-package god-mode
;;   :diminish god-local-mode
;;   :init
;;   (require 'god-mode)
;;   (add-hook 'god-local-mode-hook (lambda ()
;; 				   (if (bound-and-true-p god-local-mode)
;; 				       (hl-line-mode +1)
;; 				     (hl-line-mode -1))))

;;   :chords (("fj" . god-mode)))

;; (use-package go-mode
;;   :ensure nil)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (key-chord-define lsp-mode-map ",r" 'lsp-rename)
  (key-chord-define lsp-mode-map ",u" 'lsp-find-references)
  :hook ((go-mode . lsp)
         (python-mode . lsp)
         ))


(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; (use-package anaconda-mode
;;   :hook ((python-mode . anaconda-mode)
;;          (python-mode . anaconda-eldoc-mode)))

;; (use-package tree-sitter
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :requires (tree-sitter))

(use-package lsp-ui
  :requires (lsp-mode)
  :commands lsp-ui-mode)


(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
	      (("N" . make-directory)))
  :config
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package org-download)

(use-package org-mode
  :ensure nil
  :hook ((org-mode . auto-revert-mode)
         (org-mode . org-indent-mode))
  :config
  (require 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((eshell . t)
     (shell . t)
     (python . t))))

(use-package terraform-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode)))

;; (use-package docker
;;   :bind ("C-c d" . docker))

;; (use-package ob-async
;;   :requires (org-mode))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package rgtk
  :ensure nil
  :no-require t
  :chords (("/r" . revert-buffer)
           (";c" . soji/find-corresponding-file)
           (";w" . save-buffer)
           (";q" . kill-this-buffer)
           (";a" . other-window)
           (";b" . switch-to-buffer)
           (";1" . delete-other-window)
           (";2" . split-window-below)
           (";3" . split-window-right)
           (";d" . find-file)
           (";4" . goto-line)
           (";z" . pop-global-mark))

  :config
  (key-chord-define minibuffer-local-map ";g" 'keyboard-quit))



(defun soji/save-and-kill-buffer ()
  (interactive)
  (call-interactively 'save-buffer)
  (call-interactively 'kill-this-buffer))

(defun soji/window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun soji/resize-window (&optional arg)    ; Hirose Yuuji and Bob Wiener
  "*Resize window interactively."
  (interactive "p")
  (if (one-window-p)
      (error "Cannot resize sole window"))
  (or arg (setq arg 9))
  (let (c)
    (catch 'done
      (while t
	(message "[n]arrow [w]iden [h]eighten [s]hrink [1-9] unit [q]uit" arg)
	(setq c (read-char))
	(condition-case ()
	    (cond
	     ((= c ?h) (enlarge-window arg))
	     ((= c ?s) (shrink-window arg))
	     ((= c ?w) (enlarge-window-horizontally arg))
	     ((= c ?n) (shrink-window-horizontally arg))
	     ((= c ?\^G) (keyboard-quit))
	     ((= c ?q) (throw 'done t))
	     ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
	     (t (beep)))
	  (error (beep)))))
    (message "Done.")))

(defun soji/save-cursor-location ()
  "Saves FILENAME:LINE of a current cursor position to a kill-ring"
  (interactive)
  (let ((path (buffer-file-name))
        (line (line-number-at-pos)))
    (if path
        (progn
          (kill-new (concat
                     (buffer-file-name)
                     ":"
                     (number-to-string (line-number-at-pos))))
          (message "Location saved to kill-ring."))
      (message "Location not copied. Buffer doesn't have file open."))))

(defun soji/clip-file-name-nondirectory ()
  (interactive)
  (kill-new
   (file-name-nondirectory buffer-file-name)))

(defun soji/clip-file-name ()
  (interactive)
  (kill-new
   (buffer-file-name)))

(defun soji/clip ()
  "Copies kill ring to system clipboard"
  (interactive)
  (if (display-graphic-p)
      (call-interactively 'clipboard-kill-ring-save)
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "wl-copy")
          (deactivate-mark)))))

(defun soji/setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(defun soji/kill-line-or-region ()
  "kill region if active only or kill line"
  (interactive)
  (if (region-active-p)
      (call-interactively 'delete-region)
    (cond
     ((bound-and-true-p paredit-mode) (call-interactively 'paredit-kill))
     (t (call-interactively 'kill-line)))))

(defun soji/yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(defun soji/find-emacs-config ()
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun soji/find-corresponding-file ()
  (interactive)
  (let* ((base (file-name-base (buffer-file-name)))
         (extension (file-name-extension (buffer-file-name)))
         (match (seq-find
                 (lambda (ext)
                   (and
                    (not (equalp ext extension))
                    (file-readable-p (concat base "." ext))
                    ))
                 '("less" "tsx" "js" "css"))))
    (if match
        (find-file (concat base "." match))
      (message "No corresponding file"))))


(defun soji/jupyter-eval-defun-body ()
  (interactive)
  (when (bounds-of-thing-at-point 'defun)
    (save-excursion
      (let ((beg (progn
                   (python-nav-beginning-of-defun)
                   (python-nav-forward-statement)
                   (point)))
            (end (progn
                   (python-nav-end-of-defun)
                   (point))))
        (jupyter-eval-string
         (replace-regexp-in-string "^\s\s\s\s"
                                   ""
                                   (buffer-substring-no-properties beg end) beg end))))))

(defun soji/magit-copy-region-hunk (&optional no-column)
  (interactive "P")
  (when (magit-section-internal-region-p)
    (magit-section-when hunk
      (deactivate-mark)
      (let ((text (buffer-substring-no-properties
                   (region-beginning) (region-end))))
        (kill-new (if no-column
                      (replace-regexp-in-string "^[ \\+\\-]" "" text)
                    text))))))
