#!/bin/bash

INTERFACE=$1
SOLUTION=$2
TESTSUITE=$3

TMP="foo"
GLUE="$TMP.rkt"

trap "rm -f $GLUE $TMP" EXIT

cat > $GLUE <<EOF
  #lang plai
  (require plai/private/command-line plai/private/test)

  (disable-tests true)
  (require (only-in "$SOLUTION" $INTERFACE))
  (disable-tests false)
  (print-only-errors true)
  (halt-on-errors false)
  (abridged-test-output false)
  (plai-ignore-exn-strings true)
EOF

cat $TESTSUITE >> $GLUE

cat >> $GLUE <<EOF
(let ([results plai-all-test-results]
      [out (open-output-file "$TMP" #:exists 'append)])

(define (display-results r)
  (cond
    [(string? r) (display r out)]
    [(list? r)
     (for-each (lambda (e) (fprintf  out  "~v~n" e)) r)]
    [else (print r out)]))

  (fprintf out "~a~n" (count-errors results))
  (display-results results)
  (close-output-port out))
EOF

ulimit -t 5
ulimit -v 131072 # 128MB
ulimit -u 8


touch $TMP
chmod go+r $SOLUTION $TESTSUITE
chmod go+w $TMP

sudo -u student racket $GLUE &> /dev/null

gawk '{ ix = index($0, "at line"); \
        lineno = substr($0,ix+8); \
        if (ix < 1) { print $0 }
        else { print substr($0, 1, ix+7) strtonum(lineno)-10 "\")" } \
      }' $TMP | \
gawk '{ line[NR] = $0 } END { i = NR; while (i > 0) { print line[i--] } }'

if [[  `head -n1 $TMP` -gt 0 ]]; then
  exit 1
fi

