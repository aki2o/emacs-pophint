EMACS ?= emacs
CASK ?= cask

all:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	${CASK} exec ${EMACS} -Q --batch -L . --eval "(batch-byte-compile)" pophint.el
	${CASK} exec ${EMACS} -Q --batch -L . --eval "(batch-byte-compile)" pophint-config.el

test:
	ret=0 ; \
	outfile=/tmp/.elisp-test-result ; \
	for f in $$(find test -type f -name "*.el"); do \
	    test -f $$outfile && rm -f $$outfile ; \
		${CASK} exec ${EMACS} -Q --batch -L . -l $$f -f batch-expectations $$outfile || ret=1 ; \
	    test -f $$outfile && cat $$outfile ; \
	done ; \
	test $$ret -eq 0

clean:
	rm -f pophint.elc
	rm -f pophint-config.elc

.PHONY: all compile test clean
