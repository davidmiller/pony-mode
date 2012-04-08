#!/bin/sh

EMACS="emacs"
ARGS=""
MYPATH=$(dirname $(readlink -f $0))

while [ $# -gt 0 ] ; do
    case $1 in

    --emacs)
            # The Emacs executable name - useful for x-version testing
        EMACS=$2
        shift 2
        ;;

    *)
        ARGS="$ARGS $1"
        shift 1
        ;;
    esac
done

$EMACS --no-desktop -q -Q --load "$MYPATH"/../tests/resources/init.el --debug-init $ARGS

