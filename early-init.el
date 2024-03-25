;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(setq load-prefer-newer t)

(setq package-enable-at-startup nil)

;; Prevent unwanted runtime compilation for Emacs with native-comp
(setq native-comp-jit-compilation nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(setq tool-bar-mode nil)
(setq menu-bar-mode nil)
(set-scroll-bar-mode nil)
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
(defvar aquamacs-font-size (cond ((eq system-type 'darwin) 13)
                                 ((eq system-type 'windows-nt) 12)
                                 (t 12))
  "Current font size.")

(defvar aquamacs-font-weight "medium"
  "Current font weight.")

(defvar aquamacs-fonts
  `((default . ,(cond ((eq system-type 'darwin) "IBM Plex Mono")
                      ((eq system-type 'windows-nt) "IBM Plex Mono Medm")
                      (t "IBM Plex Mono")))
    (fixed . ,(cond ((eq system-type 'darwin) "IBM Plex Mono")
                    ((eq system-type 'windows-nt) "IBM Plex Mono Medm")
                    (t "IBM Plex Mono")))
    (fixed-serif . ,(cond ((eq system-type 'darwin) "IBM Plex Mono")
                          ((eq system-type 'windows-nt) "IBM Plex Mono Medm")
                          (t "IBM Plex Mono")))
    (variable . ,(cond ((eq system-type 'darwin) "IBM Plex Sans")
                       ((eq system-type 'windows-nt) "IBM Plex Sans Medm")
                       (t "IBM Plex Sans")))
    (cjk . ,(cond ((eq system-type 'darwin) "PingFang SC")
                  ((eq system-type 'windows-nt) "Microsoft Yahei")
                  (t "Noto Sans CJK SC")))
    (symbol . ,(cond ((eq system-type 'darwin) "Apple Color Emoji")
                     ((eq system-type 'windows-nt) "Segoe UI Emoji")
                     (t "Noto Color Emoji"))))
  "Fonts to use.")

(defun aquamacs--get-font-family (key)
  "Get font family with KEY."
  (let ((font (alist-get key aquamacs-fonts)))
    (if (string-empty-p font)
        (alist-get 'default aquamacs-fonts)
      font)))

(defun aquamacs-load-default-font ()
  "Load default font configuration."
  (let ((default-font (format "%s-%s:%s"
                              (aquamacs--get-font-family 'default)
                              aquamacs-font-size aquamacs-font-weight)))
    (add-to-list 'default-frame-alist (cons 'font default-font))))

(defun aquamacs-load-face-font ()
  "Load face font configuration."
  (let ((variable-font (format "%s-%s:%s" (aquamacs--get-font-family 'variable)
                               aquamacs-font-size aquamacs-font-weight))
        (fixed-font (format "%s-%s:%s" (aquamacs--get-font-family 'fixed)
                            aquamacs-font-size aquamacs-font-weight))
        (fixed-serif-font (format "%s-%s:%s" (aquamacs--get-font-family 'fixed-serif)
                                  aquamacs-font-size aquamacs-font-weight)))
    (set-face-attribute 'variable-pitch nil :font variable-font )
    (set-face-attribute 'fixed-pitch nil :font fixed-font)
    (set-face-attribute 'fixed-pitch-serif nil :font fixed-serif-font)
    (set-face-attribute 'mode-line-active nil :font variable-font)
    (set-face-attribute 'mode-line-inactive nil :font variable-font)))

(defun aquamacs-load-charset-font (&optional font)
  "Load charset FONT configuration."
  (let ((default-font (or font (format "%s-%s:%s"
                                       (aquamacs--get-font-family 'default)
                                       aquamacs-font-size aquamacs-font-weight)))
        (cjk-font (aquamacs--get-font-family 'cjk))
        (symbol-font (aquamacs--get-font-family 'symbol)))
    (set-frame-font default-font)
    (dolist (charset '(kana han hangul cjk-misc bopomofo))
      (set-fontset-font t charset cjk-font))
    (set-fontset-font t 'symbol symbol-font)))

(aquamacs-load-default-font)

;; Run after startup
(add-hook 'after-init-hook (lambda ()
                             (when window-system
                               (aquamacs-load-face-font)
                               (aquamacs-load-charset-font))))

;; Local Variables:
;; no-byte-compile: nil
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
