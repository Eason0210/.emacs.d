;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Early birds

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
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
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
  (menu-bar-mode 0))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(progn ; `use-package'
  (setq use-package-enable-imenu-support t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (require 'use-package))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(progn ; `startup'
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))
(progn ; `ns-win'
  (when *is-a-mac*
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'none)))

;; Set up exec-path to help Emacs find programs
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

;;; Dired mode

(use-package dired
  :bind (:map dired-mode-map
              ("e" . dired-open-externally))
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alGh")
  (dired-recursive-copies 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (defun dired-open-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks
     (consult-file-externally (dired-get-filename))
     arg)))

;;; Settings for tracking recent files

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/tmp/" "/ssh:" ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'")))
  :config (recentf-mode))

;;; Save and restore editor sessions between restarts

;; Save a list of open files in ~/.emacs.d/.emacs.desktop
(use-package desktop
  :custom
  (desktop-path (list user-emacs-directory))
  (desktop-auto-save-timeout 600)
  (desktop-load-locked-desktop 'check-pid)
  ;; Save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  (desktop-globals-to-save
   '((comint-input-ring        . 50)
     (compile-history          . 30)
     desktop-missing-file-warning
     (dired-regexp-history     . 20)
     (extended-command-history . 30)
     (face-name-history        . 20)
     (file-name-history        . 100)
     (grep-find-history        . 30)
     (grep-history             . 30)
     (magit-revision-history   . 50)
     (minibuffer-history       . 50)
     (org-clock-history        . 50)
     (org-refile-history       . 50)
     (org-tags-history         . 50)
     (query-replace-history    . 60)
     (read-expression-history  . 60)
     (regexp-history           . 60)
     (regexp-search-ring       . 20)
     register-alist
     (search-ring              . 20)
     (kill-ring                . 20)
     (shell-command-history    . 50)
     tags-file-name
     tags-table-list))
  :config
  (advice-add 'desktop-read :around 'sanityinc/desktop-time-restore)
  (advice-add 'desktop-create-buffer :around 'sanityinc/desktop-time-buffer-create)
  (desktop-save-mode 1)
  :preface
  (defun sanityinc/time-subtract-millis (b a)
    (* 1000.0 (float-time (time-subtract b a))))

  (defun sanityinc/desktop-time-restore (orig &rest args)
    (let ((start-time (current-time)))
      (prog1
          (apply orig args)
        (message "Desktop restored in %.2fms"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)))))

  (defun sanityinc/desktop-time-buffer-create (orig ver filename &rest args)
    (let ((start-time (current-time)))
      (prog1
          (apply orig ver filename args)
        (message "Desktop: %.2fms to restore %s"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)
                 (when filename
                   (abbreviate-file-name filename)))))))

;; Restore histories and registers after saving
(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :config (save-place-mode))

;;; Editing utils

(progn ; favorite default
  (setq-default
   use-short-answers t
   blink-cursor-interval 0.4
   bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
   column-number-mode t
   indent-tabs-mode nil
   create-lockfiles nil
   auto-save-default nil
   make-backup-files nil
   mouse-yank-at-point t
   save-interprogram-paste-before-kill t
   scroll-preserve-screen-position 'always
   set-mark-command-repeat-pop t
   truncate-partial-width-windows nil
   tooltip-delay 1.5))

(progn ; `pixel-scroll'
  (if (boundp 'pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode t)))

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

(bind-keys
 ("C-x k" . kill-current-buffer)
 ("C-x x p" . pop-to-mark-command)
 ("C-x C-." . pop-global-mark)
 ;; M-^ is inconvenient, so also bind M-j
 ("M-j" . join-line)
 ;; Zap *up* to char is a handy pair for zap-to-char
 ("M-Z" . zap-up-to-char))

;;; Version control

(use-package diff-hl
  :bind (:map diff-hl-mode-map
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk))
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)))

;;; Built-in packages

(use-package help
  :defer t
  :custom (help-window-select t)
  :config (temp-buffer-resize-mode))

;;; Configure default locale

(progn ; `charset'
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq system-time-locale "C")
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8)))

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
;; no-byte-compile: t
;; End:
;;; init.el ends here
