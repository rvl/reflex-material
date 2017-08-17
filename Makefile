JSEXE := $(shell stack path --local-install-root)/bin/reflex-material-example.jsexe
export JSEXE

_shake/build: Build.hs
	mkdir -p _shake
	ghc --make Build.hs -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build

build-resources: _shake/build
	stack build
	npm install

	_shake/build assets_for_stack
	cp -r static/* ${JSEXE}

	rm -rf docs
	cp -r ${JSEXE} docs
	touch docs/.nojekyll
