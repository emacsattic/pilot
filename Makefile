### change these to suit your needs

## The place for Elisp files
ELISP_DIR=/usr/lib/xemacs/site-packages/lisp/

## The place for the shell scripts
SCRIPT_DIR=/usr/local/bin

## the Emacs executable
EMACS=xemacs


### no need to change anything below this line
ELISP_CC=${EMACS} -batch -f batch-byte-compile
ELISP_SRC=pilot.el bbdb-pilot.el diary-pilot.el memo-pilot.el
ELISP_OBJ=pilot.elc bbdb-pilot.elc diary-pilot.elc memo-pilot.elc

all: install

%.elc: %.el
	${ELISP_CC} $<

compile: ${ELISP_OBJ}

install: ${ELISP_SRC} ${ELISP_OBJ} pilot-wait pilot-wait-end
	cp pilot-wait pilot-wait-end ${SCRIPT_DIR}
	chmod +x ${SCRIPT_DIR}/pilot-wait ${SCRIPT_DIR}/pilot-wait-end 
	cp ${ELISP_SRC} ${ELISP_OBJ} ${ELISP_DIR}

clean: 
	rm -f *.elc
