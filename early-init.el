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
                                 ((eq system-type 'windows-nt) 12)
                                 (t 16))
  "Current font size.")

(defvar aquamacs-font-weight "regular"
  "Current font weight.")

(defvar aquamacs-fonts
  `((default . ,(cond ((eq system-type 'darwin) "Triplicate T4c")
                      ((eq system-type 'windows-nt) "Triplicate T4c")
                      (t "Triplicate T4c")))
    (fixed . ,(cond ((eq system-type 'darwin) "Triplicate T4c")
                    ((eq system-type 'windows-nt) "Triplicate T4c")
                    (t "Triplicate T4c")))
    (fixed-serif . ,(cond ((eq system-type 'darwin) "Triplicate T4c")
                          ((eq system-type 'windows-nt) "Triplicate T4c")
                          (t "Triplicate T4c")))
    (variable . ,(cond ((eq system-type 'darwin) "Bookerly")
                       ((eq system-type 'windows-nt) "Bookerly")
                       (t "Bookerly")))
    (han . ,(cond ((eq system-type 'darwin) "FZLiuGongQuanKaiShuJF")
                  ((eq system-type 'windows-nt) "方正柳公权楷书 简繁")
                  (t "方正柳公权楷书 简繁")))
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
        (han-font (aquamacs--get-font-family 'han))
        (cjk-font (aquamacs--get-font-family 'cjk))
        (symbol-font (aquamacs--get-font-family 'symbol)))
    (set-frame-font default-font)
    (add-to-list 'face-font-rescale-alist `(,han-font . 1.2))
    (add-to-list 'face-font-rescale-alist `(,cjk-font . 1.2))
    (set-fontset-font t 'han han-font)
    (dolist (charset '(kana hangul cjk-misc bopomofo))
      (set-fontset-font t charset cjk-font))
    (set-fontset-font t 'symbol symbol-font)))

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
