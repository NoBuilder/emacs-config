(setq gc-cons-threshold 100000000)

(pixel-scroll-precision-mode)


;; Unify all the things.
(cua-mode +1)

(global-hl-line-mode +1)

;(which-function-mode +1)

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

(setq org-babel-python-command "python3")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) ;; replace this with python if you want regular python
   ;; other languages..
   ))

;; Backups et al.

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq auto-save-default nil)
(setq create-lockfiles nil)

(when (string-equal system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq recentf-max-saved-items 2048
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        "recentf$"
                        "company-statistics-cache\\.el$"
                        ;; ctags
                        "/TAGS$"
                        ;; global
                        "/GTAGS$"
                        "/GRAGS$"
                        "/GPATH$"
                        ;; binary
                        "\\.mkv$"
                        "\\.mp[34]$"
                        "\\.avi$"
                        "\\.pdf$"
                        "\\.docx?$"
                        "\\.xlsx?$"
                        ;; sub-titles
                        "\\.sub$"
                        "\\.srt$"
                        "\\.ass$"
                        ;; ~/.emacs.d/**/*.el included
                        ;; "/home/[a-z]\+/\\.[a-df-z]" ; configuration file should not be excluded
                        ))


;; load only if file exists
(if (file-exists-p (concat user-emacs-directory "local.el"))
    (load-library (concat user-emacs-directory "local.el")))


;;; Packages

(require 'package)

(package-initialize)

(setq package-native-compile t)

(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))

(unless package-archive-contents
  (package-refresh-contents))

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(require 'use-package)

;(require 'use-package-ensure)
(setq use-package-always-ensure t)


(use-package spacemacs-theme
  :config
  (load-theme 'spacemacs-dark t))


(setq show-paren-delay 0)
(show-paren-mode +1)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(use-package key-chord
  :init
  (key-chord-mode +1))

(use-package use-package-chords
  :config
  (key-chord-mode 1))


(use-package diminish)

(use-package ripgrep)

(use-package smartparens
  :hook
  ((tide-mode) . smartparens-mode))

;; (use-package helm
;;   :config
;;   (setq helm-mode-fuzzy-match t)
;;   (setq helm-completion-in-region-fuzzy-match t)

;;   :bind (("C-x C-f" . helm-find-files)
;;          ("M-x" . helm-M-x)
;;          ("C-x b" . helm-mini))
;;   :chords ((";r" . helm-resume)
;;            (";v" . helm-imenu)))

(use-package ivy
  :diminish

  :init
  (ivy-mode +1)

  :bind (("C-s" . swiper)
	 ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x))

  :chords ((";s" . swiper)
           (";r" . ivy-resume)
           (";v" . counsel-imenu))

  :config
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history))))


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

;(use-package dap-mode)

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
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
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

(use-package treesit
  :ensure nil
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css "https://github.com/tree-sitter/tree-sitter-css")
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "master" "src"))
               (python "https://github.com/tree-sitter/tree-sitter-python")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
                                        ;(treesit-install-language-grammar (car grammar))
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars))

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



(use-package eglot
  :chords ((",r" . eglot-rename)
           (",u" . eglot-find-references)))

(use-package ace-window
  :chords ((",," . ace-window))
  :init
  (ace-window-display-mode +1))


(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

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
         (org-mode . org-indent-mode)))

(use-package terraform-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode)))

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

(use-package editorconfig
  :ensure t)

(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(use-package copilot
  :load-path "/Users/soji/.emacs.d/pkgs/copilot.el"
  :config
  ;;(define-key copilot-mode-map (kbd "C-M-<return>") 'copilot-complete)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
                                        (add-to-list 'copilot-major-mode-alist '("go-ts" . "go"))

  ;(add-hook 'prog-mode-hook 'copilot-mode)
                                        )

(use-package zoom
  :ensure t
  :config
  (zoom-mode +1)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))

(use-package zoom-window
  :ensure t
  :bind (("C-x C-z" . zoom-window-zoom)))

(use-package gptel
  :init
  (setq gptel-api-key my-openai-api-key))

(use-package ein)


(use-package ob-ipython
  :ensure t
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t) ;; replace this with python if you want regular python
     ;; other languages..
     )))

(use-package eldoc-box
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))


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


(defun soji/copy-path ()
  "Copy the file or directory of the current buffer."
  (interactive)
  (let ((path (or (buffer-file-name) default-directory)))
    (kill-new path)
    (message "Copied path '%s' to the clipboard" path)))

(global-set-key (kbd "C-c C-c p") 'soji/copy-path)

;; (defun soji/clip ()
;;   "Copies kill ring to system clipboard"
;;   (interactive)
;;   (if (display-graphic-p)
;;       (call-interactively 'clipboard-kill-ring-save)
;;     (if (region-active-p)
;;         (progn
;;           (shell-command-on-region (region-beginning) (region-end) "wl-copy")
;;           (deactivate-mark)))))

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
