ELMFLAGS = --yes --warn

all: sdm.tar.gz

debug: ELMFLAGS += --debug
debug: sdm.tar.gz

sdm.tar.gz: sdm/elm.js sdm/*
	tar -zcvf sdm.tar.gz sdm

sdm/elm.js: source/Decoder.elm source/*
	elm-make source/Main.elm $(ELMFLAGS) --output=sdm/elm.js

source/Decoder.elm: swagger.json source/Decoder.elm.patch
	cat swagger.json | swagger-to-elm | elm-format --stdin > source/Decoder.elm.generated
	patch -o source/Decoder.elm -i source/Decoder.elm.patch source/Decoder.elm.generated
	rm -f source/Decoder.elm.generated

clean:
	rm -f sdm.tar.gz sdm/elm.js source/Decoder.elm

.PHONY: all clean
