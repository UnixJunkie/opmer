.PHONY: build clean edit install uninstall reinstall

build:
	jbuilder build opmer.exe
	ln -sf _build/default/opmer.exe ./opmer

clean:
	rm -rf _build opmer.exe

edit:
	emacs src/*.ml &

install:
	jbuilder build @install
	jbuilder install

uninstall:
	jbuilder uninstall

reinstall: uninstall install
