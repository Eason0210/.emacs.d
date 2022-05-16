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
