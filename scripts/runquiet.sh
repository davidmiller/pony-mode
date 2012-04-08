#!/bin/sh

MYPATH=$(dirname $(readlink -f $0))
emacs --no-desktop -q -Q --load "$MYPATH"/../tests/resources/init.el --debug-init $@

