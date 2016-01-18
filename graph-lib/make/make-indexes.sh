#!/bin/bash
find "$@" -type d -print0 | xargs -n 1 -r0 bash -c '(shopt -s nullglob; cd "$0"; if (! test -e index.html) || test -e .make-index-html; then touch .make-index-html; echo "<html>" > index.html; for i in */ *.html; do if test "$i" != "index.htm"; then echo "<a href=\"$i\">$i</a>" >> index.html; fi; done; echo "</html>" >> index.html; fi)'
