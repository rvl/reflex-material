JSEXE := $(shell stack path --local-install-root)/bin/reflex-material-exe.jsexe
export JSEXE

build-resources:
	stack build
	npm install

	./build.sh assets_for_stack
	cp -r static/* ${JSEXE}

	rm -rf docs
	cp -r ${JSEXE} docs
	touch docs/.nojekyll
