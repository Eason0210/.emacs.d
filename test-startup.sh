#!/bin/sh -e
echo "Setting up Borg"
git config --global url.https://github.com/.insteadOf git@github.com:
git config --global url.https://gitlab.com/.insteadOf git@gitlab.com:
git config --global url.https://git.sr.ht/.insteadOf git@git.sr.ht:
git config --global url.https://codeberg.org/.insteadOf git@codeberg.org:

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
                              (load-path (delq default-directory load-path)))
                           (load-file user-init-file)
                           (run-hooks (quote after-init-hook))))'
echo "Startup successful"
