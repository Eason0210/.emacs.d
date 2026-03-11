;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(setq load-prefer-newer t)

;; Tell Emacs to initialize Borg instead of Package
(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

(setq package-enable-at-startup nil)

;; Prevent unwanted runtime compilation for Emacs with native-comp
(setq native-comp-jit-compilation nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(push '(fullscreen . maximized) initial-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Configure keys specific to macOS
(when (featurep 'ns)
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super))

;;; Encoding
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")

;;; Font setting
(defvar aquamacs-font-size (cond ((eq system-type 'darwin) 15)
                                 ((eq system-type 'windows-nt) 13.5)
                                 (t 16))
  "Current font size.")

(defvar aquamacs-font-weight "regular"
  "Current font weight.")

(defvar aquamacs-fonts
  `((mono . ,(cond ((eq system-type 'darwin) "Monaco")
                   ((eq system-type 'windows-nt) "Consolas")
                   (t "SF Mono")))
    (sans . ,(cond ((eq system-type 'darwin) "Helvetica Neue")
                   ((eq system-type 'windows-nt) "Helvetica Neue")
                   (t "SF Pro")))
    (serif . ,(cond ((eq system-type 'darwin) "New York")
                    ((eq system-type 'windows-nt) "Consolas")
                    (t "SF Mono")))
    (cjk . ,(cond ((eq system-type 'darwin) "FZLiuGongQuanKaiShuJF")
                  ((eq system-type 'windows-nt) "方正柳公权楷书 简繁")
                  (t "方正柳公权楷书 简繁")))
    (symbol . ,(cond ((eq system-type 'darwin) "Apple Color Emoji")
                     ((eq system-type 'windows-nt) "Segoe UI Emoji")
                     (t "Noto Color Emoji"))))
  "Fonts to use.")

(defun aquamacs--get-font-family (key)
  "Get font family with KEY."
  (let ((font (alist-get key aquamacs-fonts)))
    (if (string-empty-p font)
        (alist-get 'mono aquamacs-fonts)
      font)))

(defun aquamacs-load-default-font ()
  "Load default font configuration."
  (let ((default-font (format "%s-%s:%s"
                              (aquamacs--get-font-family 'mono)
                              aquamacs-font-size aquamacs-font-weight)))
    (add-to-list 'default-frame-alist (cons 'font default-font))))

(defun aquamacs-load-face-font ()
  "Load face font configuration."
  (let ((sans (format "%s-%s:%s" (aquamacs--get-font-family 'sans)
                      aquamacs-font-size aquamacs-font-weight))
        (mono (format "%s-%s:%s" (aquamacs--get-font-family 'mono)
                      aquamacs-font-size aquamacs-font-weight))
        (serif (format "%s-%s:%s" (aquamacs--get-font-family 'serif)
                       aquamacs-font-size aquamacs-font-weight)))
    (set-face-attribute 'variable-pitch nil :font sans)
    (set-face-attribute 'variable-pitch-text nil :family serif)
    (set-face-attribute 'fixed-pitch nil :font mono)
    (set-face-attribute 'fixed-pitch-serif nil :font mono)
    (set-face-attribute 'mode-line-active nil :font sans)
    (set-face-attribute 'mode-line-inactive nil :font sans)))

(defun aquamacs-load-charset-font (&optional font)
  "Load charset FONT configuration."
  (let ((default-font (or font (format "%s-%s:%s"
                                       (aquamacs--get-font-family 'mono)
                                       aquamacs-font-size aquamacs-font-weight)))
        (cjk-font (aquamacs--get-font-family 'cjk))
        (symbol-font (aquamacs--get-font-family 'symbol))
        (scale-factor (if (eq system-type 'windows-nt) 1.1 1.2)))
    (set-frame-font default-font)
    (add-to-list 'face-font-rescale-alist `(,cjk-font . ,scale-factor))
    (dolist (charset '(kana han hangul cjk-misc bopomofo))
      (set-fontset-font t charset cjk-font))
    (set-fontset-font t 'symbol symbol-font)
    (set-fontset-font t 'unicode symbol-font nil 'append)))

(aquamacs-load-default-font)

;; Run after startup
(add-hook 'after-init-hook (lambda ()
                             (when (display-graphic-p)
                               (aquamacs-load-face-font)
                               (aquamacs-load-charset-font))))

;; Local Variables:
;; no-byte-compile: nil
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
