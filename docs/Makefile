.PHONY: all clean

all: cl-odata-client.info cl-odata-client.html index.html cl-odata-client.pdf ../README.md

cl-odata-client.info: cl-odata-client.texi
	makeinfo --no-validate cl-odata-client.texi

cl-odata-client.pdf: cl-odata-client.texi
	texi2any --force --no-validate --pdf cl-odata-client.texi -o cl-odata-client.pdf

cl-odata-client.html: cl-odata-client.texi
	texi2any --no-validate --html cl-odata-client.texi --css-ref texinfo-klare.css --css-ref lisp-coloring.css --no-split -o cl-odata-client.html
	sbcl --eval '(require :hyperspec)' --load colorize-lisp-examples.lisp cl-odata-client.html

index.html: cl-odata-client.texi
	texi2any --no-validate --html cl-odata-client.texi --css-ref texinfo-klare.css --css-ref lisp-coloring.css --no-split -o index.html
	sbcl --eval '(require :hyperspec)' --load colorize-lisp-examples.lisp index.html

cl-odata-client.texi:
	sbcl --eval '(ql:quickload :odata-client)' --eval '(ql:quickload :msgraph)' --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "cl-odata-client.source.texi" "cl-odata-client.texi" :docsystem :texinfo)' --quit

cl-odata-client-internals.info: cl-odata-client-internals.weaved.texi
	makeinfo --no-validate cl-odata-client-internals.weaved.texi

cl-odata-client-internals.pdf: cl-odata-client-internals.weaved.texi
	texi2any --force --no-validate --pdf cl-odata-client-internals.weaved.texi -o cl-odata-client-internals.pdf

cl-odata-client-internals.html: cl-odata-client-internals.weaved.texi
	texi2any --no-validate --html cl-odata-client-internals.weaved.texi --css-ref texinfo-klare.css --no-split -o cl-odata-client-internals.html

cl-odata-client-internals.weaved.texi:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "cl-odata-client-internals.texi" "cl-odata-client-internals.weaved.texi" :docsystem :texinfo)' --quit

clean:
	rm -f *.pdf
	rm -f *.html
	rm cl-odata-client.texi
