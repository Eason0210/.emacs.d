;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(setq load-prefer-newer t)

(setq package-enable-at-startup nil)

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

;;; Font setting

(defvar font-list
  (cond
   ((eq system-type 'darwin)
    '(("SF Mono" . 13) ("Monaco" . 13) ("Menlo" . 13)))
   ((eq system-type 'windows-nt)
    '(("Cascadia Mono" . 11) ("SF Mono" . 11) ("Consolas" . 12)))
   (t
    '(("SF Mono" . 11) ("Consolas" . 12) ("Cascadia Mono" . 11))))
  "List of fonts and sizes.  The first one available will be used.")

;; Set default font before frame creation to make sure the first frame have the correct size
(add-to-list 'default-frame-alist (cons 'font (format "%s-%d" (caar font-list) (cdar font-list))))

(defun change-font ()
  "Change the font of frame from an available `font-list'."
  (interactive)
  (let* (available-fonts font-name font-size font-set)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
      (when (x-list-fonts (car font))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t)
                                       available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-set (format "%s-%d" font-name font-size))
      (set-frame-font font-set nil t)
      (add-to-list 'default-frame-alist (cons 'font font-set)))))

(defun change-unicode-font ()
  "Setup the Unicode font."
  (when (display-graphic-p)
    (cl-loop for font in '("Microsoft Yahei" "PingFang SC" "Noto Sans Mono CJK SC")
             when (x-list-fonts font)
             return (dolist (charset '(kana han hangul cjk-misc bopomofo))
                      (set-fontset-font t charset font)))
    (cl-loop for font in '("Segoe UI Emoji" "Apple Color Emoji" "Noto Color Emoji")
             when (x-list-fonts font)
             return (set-fontset-font t 'emoji font))
    (dolist (font '("HanaMinA" "HanaMinB"))
      (when (x-list-fonts font)
        (set-fontset-font t 'unicode font nil 'append)))))

;; Run after startup
(dolist (fn '(change-font change-unicode-font))
  (add-hook 'after-init-hook fn))


;; Local Variables:
;; no-byte-compile: nil
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
