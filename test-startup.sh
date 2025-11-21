#!/bin/sh -e
echo "Setting up Borg..."
make bootstrap-borg
make bootstrap

echo "Attempting startup..."
${EMACS:=emacs} -nw --batch \
                --eval '(progn
                        (defvar url-show-status)
                        (let ((debug-on-error t)
                              (url-show-status nil)
                              (user-emacs-directory default-directory)
                              (user-init-file (expand-file-name "init.el"))
                              (user-early-init-file (expand-file-name "early-init.el"))
                              (load-path (delq default-directory load-path)))                           
                           (load-file user-early-init-file)
			   (load-file user-init-file)
                           (run-hooks (quote after-init-hook))))'
echo "Startup successful"
