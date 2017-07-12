
SHELL=/bin/bash

sdm.tar.gz: source/Decoder.elm
	rm -rf sdm
	mkdir -p sdm
	elm-make source/Main.elm --yes --warn --output=sdm/elm.js
	cp index.html main.js fixes.css sdm
	tar -zcvf sdm.tar.gz sdm

source/Decoder.elm: swagger.json source/Decoder.elm.patch
	cat swagger.json | swagger-to-elm | elm-format --stdin > source/Decoder.elm.generated
	patch -o source/Decoder.elm -i source/Decoder.elm.patch source/Decoder.elm.generated
	rm -f source/Decoder.elm.generated


.PHONY: sdm.tar.gz



