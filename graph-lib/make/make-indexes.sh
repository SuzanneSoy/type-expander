#!/bin/bash
find "$@" -type d -print0 | xargs -n 1 -r0 bash -c '(shopt -s nullglob; cd "$0"; if (! test -e index.html) || test -e .make-index-html; then touch .make-index-html; echo "<html><ul>" > index.html; for i in */ *.html; do if test "$i" != "index.html"; then echo "<li><a href=\"$i\">$i</a></li>" >> index.html; fi; done; echo "</ul></html>" >> index.html; fi)'
