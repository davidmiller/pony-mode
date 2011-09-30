;; This file contains a select subsection of the .emacs at
;; https://github.com/davidmiller/mydotfiles in order to contain the
;; level of cognitive dissonance incurred when firing up configuration-lite
;; instances of Emacs for testing purposes

(add-to-list 'load-path "~/emacs/site-packages/pony-mode")
(require 'pony-mode)

(global-set-key [M-left] 'windmove-left) ; move to left windnow
(global-set-key [M-right] 'windmove-right) ; move to right window
(global-set-key [M-up] 'windmove-up) ; move to upper window
(global-set-key [M-down] 'windmove-down) ; move to downer window
(defalias 'yes-or-no-p 'y-or-n-p)
(cd "~/")