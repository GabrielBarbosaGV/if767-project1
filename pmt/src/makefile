makepmt: *.lisp
	sbcl --load main.lisp --eval "(sb-ext:save-lisp-and-die \"pmt\" :toplevel 'main :executable t)"
	mkdir -p ../bin
	mv pmt ../bin
