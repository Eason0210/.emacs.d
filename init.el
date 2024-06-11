;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Early birds

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(progn ; `startup'
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq ring-bell-function #'ignore))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(eval-and-compile ; `use-package'
  (setq use-package-enable-imenu-support t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (require  'use-package))

(use-package no-littering)

(use-package custom
  :no-require t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :hook (after-init . (lambda ()
                        (unless (server-running-p)
                          (server-start)))))

(progn ; `startup'
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

(use-package exec-path-from-shell
  :when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
  :custom (exec-path-from-shell-arguments '("-l"))
  :config
  (dolist (var '("GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;; Long tail

;;; Theme

(use-package color-theme-sanityinc-tomorrow
  :custom
  (custom-safe-themes t)
  (custom-enabled-themes '(sanityinc-tomorrow-day))
  :hook (after-init . reapply-themes)
  :config
  (defun reapply-themes ()
    "Forcibly load the themes listed in `custom-enabled-themes'."
    (dolist (theme custom-enabled-themes)
      (unless (custom-theme-p theme)
        (load-theme theme)))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

  (defun light ()
    "Activate a light color theme."
    (interactive)
    (setq custom-enabled-themes '(sanityinc-tomorrow-day))
    (reapply-themes))

  (defun dark ()
    "Activate a dark color theme."
    (interactive)
    (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
    (reapply-themes)))

(use-package ns-auto-titlebar
  :when *is-a-mac*
  :config (ns-auto-titlebar-mode))

;;; Dired mode

(use-package dired
  :demand t
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alGhv --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-kill-when-opening-new-dired-buffer t))

(use-package diredfl
  :hook (dired-mode . diredfl-global-mode))

;;; Isearch settings

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (isearch-allow-motion t)
  (isearch-motion-changes-direction t))

;;; Configure uniquification of buffer names

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;;; Operate on buffers like dired

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom (ibuffer-filter-group-name-face 'font-lock-doc-face))

;;; A universal on-the-fly syntax checker

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :hook (flymake-mode . (lambda ()
                          (setq eldoc-documentation-functions
                                (cons 'flymake-eldoc-function
                                      (delq 'flymake-eldoc-function
                                            eldoc-documentation-functions)))))
  :init (setq elisp-flymake-byte-compile-load-path (cons "./" load-path)))

;;; Minibuffer and completion

(use-package minibuffer
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (inhibit-message-regexps '("^Saving file" "^Wrote"))
  (set-message-functions '(inhibit-message))
  :init (minibuffer-depth-indicate-mode))

(use-package fussy
  :custom
  (completion-styles '(fussy basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles fussy basic))))
  (fussy-filter-fn 'fussy-filter-default)
  (fussy-use-cache t)
  :config
  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)
  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local fussy-max-candidate-limit 5000
                          fussy-default-regex-fn 'fussy-pattern-first-letter
                          fussy-prefer-prefix nil))))

(use-package orderless
  :disabled
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles orderless))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package pinyinlib
  :disabled
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(use-package vertico
  :demand t
  :bind (:map vertico-map
              ([tab] . vertico-next)
              ([backtab] . vertico-previous))
  :custom (vertico-cycle t)
  :config (vertico-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :defer 0.5
  :bind (([remap repeat-complex-command] . consult-complex-command)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap yank-pop] . consult-yank-pop)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-s d" . consult-fd)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         :map minibuffer-local-map
         ("M-s" . consult-history))
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (consult-narrow-key "<")
  (xref-search-program 'ripgrep)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :commands consult--customize-put
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-."))

(use-package embark
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  ("C-h b" . embark-bindings)
  ("C-h B" . embark-bindings-at-point)
  ("M-n" . embark-next-symbol)
  ("M-p" . embark-previous-symbol)
  (:map dired-mode-map ("e" . dired-open-externally))
  :custom
  (embark-quit-after-action nil)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ".")
  (embark-help-key "?")
  :commands embark-open-externally
  :config
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (defun dired-open-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks (embark-open-externally (dired-get-filename)) arg)))

(use-package avy
  :bind ("C-;" . avy-goto-char-timer)
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
              ([tab] . corfu-next)
              ([backtab] . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET" . nil)
              ([remap move-end-of-line] . nil))
  :hook (eshell-mode . (lambda () (setq-local corfu-auto nil)))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :after corfu
  :bind (("C-c p p" . completion-at-point)
         ("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (when (eq system-type 'windows-nt)
    (plist-put kind-icon-default-style :height 0.8))
  (when (eq system-type 'gnu/linux)
    (plist-put kind-icon-default-style :height 0.7))
  (when (fboundp 'reapply-themes)
    (advice-add 'reapply-themes :after 'kind-icon-reset-cache)))

(use-package yasnippet
  :diminish yas-minor-mode
  :custom (yas-keymap-disable-hook
           (lambda () (and (frame-live-p corfu--frame)
                           (frame-visible-p corfu--frame))))
  :hook (after-init . yas-global-mode))

(use-package consult-yasnippet
  :bind ("M-*" . consult-yasnippet)
  :config
  (with-eval-after-load 'embark
    (defvar-keymap embark-yasnippet-completion-actions
      :doc "Keymap for actions for yasnippets."
      :parent embark-general-map
      "v" #'consult-yasnippet-visit-snippet-file)
    (push '(yasnippet . embark-yasnippet-completion-actions)
          embark-keymap-alist)))

;;; Settings for hippie-expand

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :custom (hippie-expand-try-functions-list
           '(try-complete-file-name-partially
             try-complete-file-name
             try-expand-dabbrev
             try-expand-dabbrev-all-buffers
             try-expand-dabbrev-from-kill)))

;;; Working with Windows within frames

(use-package window
  :bind (([f7] . sanityinc/split-window)
         ("C-c <down>". sanityinc/toggle-current-window-dedication)
         :map ctl-x-4-map
         ("s" . window-swap-states)
         ("t" . toggle-window-split))
  :config
  (bind-key "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically))
  (bind-key "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally))
  :preface
  (defun split-window-func-with-other-buffer (split-function)
    "Use SPLIT-FUNCTION to split window."
    (lambda (&optional arg)
      "Split this window and switch to the new window unless ARG is provided."
      (interactive "P")
      (funcall split-function)
      (let ((target-window (next-window)))
        (set-window-buffer target-window (other-buffer))
        (unless arg
          (select-window target-window)))))

  (defun toggle-window-split ()
    "Toggle window split from vertical to horizontal."
    (interactive)
    (if (> (length (window-list)) 2)
        (error "Can't toggle with more than 2 windows")
      (let ((was-full-height (window-full-height-p)))
        (delete-other-windows)
        (if was-full-height
            (split-window-vertically)
          (split-window-horizontally))
        (save-selected-window
          (other-window 1)
          (switch-to-buffer (other-buffer))))))

  (defun sanityinc/split-window()
    "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
    (interactive)
    (if (eq last-command 'sanityinc/split-window)
        (progn
          (jump-to-register :sanityinc/split-window)
          (setq this-command 'sanityinc/unsplit-window))
      (window-configuration-to-register :sanityinc/split-window)
      (switch-to-buffer-other-window nil)))

  (defun sanityinc/toggle-current-window-dedication ()
    "Toggle whether the current window is dedicated to its current buffer."
    (interactive)
    (let* ((window (selected-window))
           (was-dedicated (window-dedicated-p window)))
      (set-window-dedicated-p window (not was-dedicated))
      (message "Window %sdedicated to %s"
               (if was-dedicated "no longer " "")
               (buffer-name)))))

(use-package winner
  :defer 0.5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config (winner-mode 1))

;;; Settings for tracking recent files

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/tmp/" "/ssh:" ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'")))
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode))

;;; Save and restore editor sessions between restarts

(use-package desktop
  :custom
  (desktop-load-locked-desktop 'check-pid)
  (desktop-globals-to-save nil)
  :config (desktop-save-mode 1))

(use-package savehist
  :custom (savehist-additional-variables
           '(last-kbd-macro
             register-alist
             (comint-input-ring        . 50)
             (dired-regexp-history     . 20)
             (face-name-history        . 20)
             (kill-ring                . 20)
             (regexp-search-ring       . 20)
             (search-ring              . 20)))
  :config (savehist-mode))

(use-package saveplace
  :config (save-place-mode))

;;; Editing utils

(use-package emacs
  :custom
  (blink-cursor-interval 0.4)
  (mouse-yank-at-point t)
  (scroll-preserve-screen-position 'always)
  (truncate-partial-width-windows nil)
  (tooltip-delay 1.5)
  (use-short-answers t)
  (frame-resize-pixelwise t)
  (x-underline-at-descent-line t)
  (create-lockfiles nil)
  (auto-save-default nil)
  (make-backup-files nil)
  (auto-save-visited-interval 1.1)
  (auto-save-visited-predicate
   (lambda () (and (not (buffer-live-p (get-buffer " *vundo tree*")))
                   (not (string-suffix-p "gpg" (file-name-extension (buffer-name)) t))
                   (not (eq (buffer-base-buffer (get-buffer (concat "CAPTURE-" (buffer-name))))
                            (current-buffer)))
                   (or (not (boundp 'corfu--total)) (zerop corfu--total))
                   (or (not (boundp 'yas--active-snippets)) (not yas--active-snippets)))))
  (display-fill-column-indicator-character ?\u254e)
  :hook ((prog-mode . display-fill-column-indicator-mode)
         ((prog-mode text-mode) . indicate-buffer-boundaries-left)
         (after-init . auto-save-visited-mode))
  :config
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left)))

(use-package simple
  :bind
  ("M-j" . join-line) ; M-^ is inconvenient, so also bind M-j
  ("C-x x p" . pop-to-mark-command)
  ("C-x C-." . pop-global-mark)
  ([remap capitalize-word] . capitalize-dwim)
  ("<f8>" . scratch-buffer)
  :custom
  (save-interprogram-paste-before-kill t)
  (set-mark-command-repeat-pop t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (column-number-mode t))

(use-package pixel-scroll
  :config (pixel-scroll-precision-mode t))

(use-package delsel
  :config (delete-selection-mode))

(use-package elec-pair
  :config (electric-pair-mode))

(use-package electric
  :config (electric-indent-mode))

(use-package autorevert
  :diminish
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode))

(use-package beacon
  :custom
  (beacon-lighter "")
  (beacon-size 20)
  (beacon-blink-when-window-scrolls nil)
  :config (beacon-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

(use-package move-dup
  :bind
  ("C-c d" . move-dup-duplicate-down)
  ("C-c u" . move-dup-duplicate-up)
  ([M-up] . move-dup-move-lines-up)
  ([M-down] . move-dup-move-lines-down))

;;; Version control

(use-package diff-hl
  :bind (:map diff-hl-mode-map
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk))
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine-toggle))

(use-package git-link
  :bind (("C-c g l" . git-link)
         ("C-c g h" . git-link-homepage)
         ("C-c g c" . git-link-commit)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-diff-refine-hunk t)
  (magit-module-sections-nested nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :commands magit-add-section-hook
  :config
  (put 'magit-clean 'disabled nil)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (with-eval-after-load "magit-submodule"
    (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
    (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
    (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote)))

;;; Text editing

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c i" . org-capture-inbox)
         :map org-mode-map
         ("C-c I" . org-id-get-create)
         ("C-c e d" . org-pandoc-convert-to-docx)
         :map sanityinc/org-global-prefix-map
         ("j" . org-clock-goto)
         ("l" . org-clock-in-last)
         ("i" . org-clock-in)
         ("o" . org-clock-out)
         ("b" . org-mark-ring-goto)
         :map org-src-mode-map
         ("C-c C-c" . org-edit-src-exit))
  :bind-keymap ("C-c o" . sanityinc/org-global-prefix-map)
  :hook (org-mode . variable-pitch-mode)
  :custom
  (org-modules nil) ; Faster loading
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-fontify-done-headline nil)
  (org-edit-timestamp-down-means-later t)
  (org-export-coding-system 'utf-8)
  (org-fast-tag-selection-single-key 'expert)
  (org-html-validation-link nil)
  (org-export-kill-product-buffer-when-displayed t)
  (org-tags-column 80)
  (org-hide-emphasis-markers t)
  (org-confirm-babel-evaluate nil)
  (org-link-elisp-confirm-function nil)
  (org-src-preserve-indentation t)
  (org-directory "~/agenda")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-agenda-files (list "inbox.org" "agenda.org" "notes.org" "projects.org"))
  (org-capture-templates
   `(("i" "Inbox" entry  (file "") ; "" => `org-default-notes-file'
      ,(concat "* TODO %?\n"
               "/Entered on/ %U"))
     ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
      ,(concat "* %? :meeting:\n"
               "<%<%Y-%m-%d %a %H:00>>"))
     ("n" "Note" entry  (file "notes.org")
      ,(concat "* Note (%a)\n"
               "/Entered on/ %U\n" "\n" "%?"))))
  ;; (org-capture-mode-hook 'delete-other-windows)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d!/!)")))
  (org-todo-repeat-to-state "NEXT")
  (org-todo-keyword-faces '(("NEXT" :inherit warning)))
  (org-archive-location "%s_archive::* Archive")
  :custom-face
  (org-block ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit (shadow fixed-pitch)))))
  (org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-property-value ((t (:inherit fixed-pitch))) t)
  (org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-table ((t (:inherit fixed-pitch))))
  (org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 1.0))))
  (org-verbatim ((t (:inherit (shadow fixed-pitch)))))
  :commands (org-get-todo-state org-entry-get org-entry-put)
  :config
  (advice-add 'org-babel-execute-src-block :before #'my/org-babel-execute-src-block)
  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
  (defun org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i"))
  :preface
  (defun log-todo-next-creation-date ()
    "Log NEXT creation time in the property drawer under the key \\='ACTIVATED\\='"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  
  (defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")

  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language when needed"
    (let* ((lang (nth 0 info))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages))
      (unless (assoc sym backup-languages)
        (condition-case err
            (progn
              (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
              (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
          (file-missing
           (setq-default org-babel-load-languages backup-languages)
           err)))))

  (defun org-pandoc-convert-to-docx ()
    "Convert current buffer file to docx format by Pandoc."
    (interactive)
    (let ((command "pandoc")
          (refdoc (list "--reference-doc"
                        (expand-file-name "var/reference.docx" user-emacs-directory))))
      (cond ((not buffer-file-name) (user-error "Must be visiting a file"))
            (t (let* ((buffer (generate-new-buffer " *Pandoc output*"))
                      (filename (list buffer-file-name))
                      (output (list "-o" (concat (file-name-sans-extension (buffer-file-name)) ".docx")))
                      (arguments (nconc filename refdoc output))
                      (exit-code (apply #'call-process command nil buffer nil arguments)))
                 (cond ((eql 0 exit-code)
                        (kill-buffer buffer)
                        (message "Convert finished: %s" (cadr output)))
                       (t (with-current-buffer buffer
                            (goto-char (point-min))
                            (insert (format "%s\n%s\n\n" (make-string 50 ?=) (current-time-string)))
                            (insert (format "Calling pandoc with:\n\n%s\n\nFailed with error:\n\n"
                                            (mapconcat #'identity (cons command arguments) " ")))
                            (special-mode))
                          (pop-to-buffer buffer)
                          (error "Convert failed with exit code %s" exit-code)))))))))

(use-package org-refile
  :after org
  :custom
  (org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm))

(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :hook (org-agenda-mode . (lambda ()
                             (add-hook
                              'window-configuration-change-hook 'org-agenda-align-tags nil t)))
  :hook (org-agenda-mode . hl-line-mode)
  :custom
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  (org-agenda-compact-blocks t)
  (org-agenda-sticky t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 'day)
  (org-agenda-window-setup 'current-window)
  (org-agenda-custom-commands
   '(("n" "Notes" tags "notes"
      ((org-agenda-overriding-header "Notes")
       (org-tags-match-list-sublevels t)))
     ("g" "Get Things Done (GTD)"
      ((agenda ""
               ((org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'deadline))
                (org-deadline-warning-days 0)))
       (todo "NEXT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-prefix-format " %i %-2:c [%e] ")
              (org-agenda-overriding-header "Tasks")))
       (agenda nil
               ((org-agenda-entry-types '(:deadline))
                (org-agenda-format-date "")
                (org-deadline-warning-days 7)
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                (org-agenda-overriding-header "Deadlines")))
       (tags-todo "inbox"
                  ((org-agenda-prefix-format "  %?-12t% s")
                   (org-agenda-overriding-header "Inbox")))
       (tags "CLOSED>=\"<today>\""
             ((org-agenda-overriding-header "Completed today"))))))))

;; Writing mode similar to the famous Writeroom editor for macOS
(use-package writeroom-mode
  :hook (org-mode . prose-mode)
  :custom
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-mode-line-toggle-position 'mode-line-format)
  :preface
  (define-minor-mode prose-mode
    "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
    :init-value nil :lighter " Prose" :keymap nil
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq word-wrap-by-category t)
          (setq cursor-type 'bar)
          (when (eq major-mode 'org)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          (setq-local blink-cursor-interval 0.6)
          (setq-local show-trailing-whitespace nil)
          ;; (setq-local line-spacing 0.2)
          (setq-local electric-pair-mode nil)
          (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'word-wrap-by-category)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      ;; (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      (flyspell-mode -1)
      (visual-line-mode -1)
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0)))))

(use-package org-roam
  :if (file-exists-p "~/.org/org-roam")
  :diminish
  :bind (("C-c n a" . org-id-get-create)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n r" . org-roam-ref-find)
         ("C-c n R" . org-roam-ref-add)
         ("C-c n s" . org-roam-db-sync))
  :custom
  (org-roam-directory (file-truename "~/.org/org-roam"))
  (org-roam-db-gc-threshold most-positive-fixnum)
  :config
  (org-roam-db-autosync-enable)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(use-package markdown-mode
  :mode (("\\.md\\.html\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package ox-gfm
  :after org
  :custom (org-export-with-toc nil))

(use-package verb
  :after org
  :bind (:map verb-response-body-mode-map
              ("q" . kill-buffer-and-window))
  :bind-keymap ("C-c C-r" . verb-command-map)
  :init (unbind-key "C-c C-r" org-mode-map))

;;; Programming languages support

(use-package elisp-mode
  :hook ((emacs-lisp-mode . (lambda () (setq mode-name "ELisp")))
         (emacs-lisp-mode . sanityinc/maybe-set-bundled-elisp-readonly))
  :config
  (setq initial-scratch-message
        (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))
  :preface
  (defun sanityinc/maybe-set-bundled-elisp-readonly ()
    "If this elisp appears to be part of Emacs, then disallow editing."
    (when (and (buffer-file-name)
               (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
      (setq buffer-read-only t)
      (view-mode 1))))

(use-package paredit
  :diminish paredit-mode " Par"
  :hook (emacs-lisp-mode . enable-paredit-mode)
  :config
  (dolist (binding '("C-<left>" "C-<right>" "M-s" "M-?"))
    (define-key paredit-mode-map (read-kbd-macro binding) nil)))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package haskell-mode
  :defer t
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-auto-insert-module-template)))

(use-package python
  :defer t
  :custom (python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :config
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter
                      (concat pyvenv-virtual-env
                              (if (eq system-type 'windows-nt)
                                  "scripts/python"
                                "bin/python")))))))

(use-package rust-mode
  :defer t
  :custom (rust-mode-treesitter-derive t))

(use-package envrc
  :when (executable-find "direnv")
  :bind-keymap ("C-c e" . envrc-command-map)
  :hook (after-init . envrc-global-mode))

(use-package reformatter
  :config
  (reformatter-define black :program "black" :args '("-") :group 'reformatter)
  (reformatter-define blue :program "blue" :args '("-") :group 'reformatter)
  (reformatter-define js-beautify :program "js-beautify" :group 'reformatter)
  (reformatter-define html-beautify :program "html-beautify" :group 'reformatter)
  (reformatter-define css-beautify :program "css-beautify" :group 'reformatter)
  (reformatter-define hindent :program "hindent" :lighter " Hin" :group 'reformatter)
  (reformatter-define ormolu :program "ormolu" :lighter " Orm"
    :args `("--stdin-input-file" ,buffer-file-name) :group 'reformatter))

(use-package apheleia
  :bind ("C-c f" . apheleia-format-buffer)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

;;; Tree-sitter support
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29
(use-package treesit
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :custom
  (major-mode-remap-alist
   '((c-mode          . c-ts-mode)
     (c++-mode        . c++-ts-mode)
     (csharp-mode     . csharp-ts-mode)
     (conf-toml-mode  . toml-ts-mode)
     (css-mode        . css-ts-mode)
     (java-mode       . java-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-json-mode    . json-ts-mode)
     (python-mode     . python-ts-mode)
     (ruby-mode       . ruby-ts-mode)
     (sh-mode         . bash-ts-mode)))
  (c-ts-mode-indent-style 'linux)
  (c-ts-mode-indent-offset 8)
  :config
  (add-to-list 'auto-mode-alist '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))

;;; Languages Server Protocol(LSP)

(use-package eglot
  :defer t
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc))
  :custom (eglot-report-progress nil)
  :config
  (setq read-process-output-max (* 1024 1024)))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map ("M-g s" . consult-eglot-symbols)))

(use-package breadcrumb
  :config (breadcrumb-mode))

;;; Helpers for M-x compile

(use-package compile
  :bind ([f6] . recompile)
  :custom (compilation-scroll-output 'first-error))

;;; Miscellaneous config

(use-package vundo
  :bind ("C-x u" . vundo)
  :custom (vundo-roll-back-on-quit nil))

(use-package go-translate
  :bind ("C-c t" . gts-do-translate)
  :custom (gts-translate-list '(("en" "zh"))))

(use-package flyspell
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
         ((git-commit-mode markdown-mode) . flyspell-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "enchant-2")
  (ispell-dictionary "english")
  (text-mode-ispell-word-completion nil))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-," . flyspell-correct-wrapper)))

(use-package sis
  :demand t
  :bind ("<f9>" . sis-switch)
  :config
  (add-to-list 'sis-prefix-override-keys "M-s")
  (add-to-list 'sis-prefix-override-keys "M-g")
  (when *is-a-mac*
    (sis-ism-lazyman-config "com.apple.keylayout.ABC" "im.rime.inputmethod.Squirrel.Hans"))
  (when (eq system-type 'gnu/linux)
    (sis-ism-lazyman-config "1" "2" 'fcitx5))
  (setq sis-other-cursor-color "orange")
  (sis-global-cursor-color-mode t)
  (sis-global-respect-mode t))

;;; Built-in packages

(use-package eldoc
  :custom (eldoc-documentation-strategy #'eldoc-documentation-compose)
  :config (eldoc-add-command-completions "paredit-"))

(use-package help
  :defer t
  :custom (help-window-select t)
  :config (temp-buffer-resize-mode))

(use-package info
  :hook ((Info-mode . variable-pitch-mode)
         (Info-mode . writeroom-mode))
  :custom-face (Info-quoted ((t (:inherit fixed-pitch)))))

;;; Tequila worms

(progn ; `startup'
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ; personalize
  (let ((file (expand-file-name "private.el" user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))


;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: nil
;; End:
;;; init.el ends here
