ELMFLAGS = --yes --warn

all: sdm.tar.gz mcpa.tar.gz

debug: ELMFLAGS += --debug
debug: sdm.tar.gz mcpa.tar.gz

sdm.tar.gz: sdm/elm.js sdm/*
	tar -zcvf sdm.tar.gz --exclude=sdmFlagsOverride.js sdm

mcpa.tar.gz: mcpa/elmTree.js mcpa/elmStats.js mcpa/*
	tar -zcvf mcpa.tar.gz mcpa

sdm/elm.js: source/Decoder.elm source/*
	elm-make source/Main.elm $(ELMFLAGS) --output=sdm/elm.js

mcpa/elmStats.js: source/Decoder.elm source/*
	elm-make source/StatsMain.elm $(ELMFLAGS) --output=mcpa/elmStats.js

mcpa/elmTree.js: source/Decoder.elm source/*
	elm-make source/McpaMain.elm $(ELMFLAGS) --output=mcpa/elmTree.js

source/Decoder.elm: swagger.json source/Decoder.elm.patch
	cat swagger.json | swagger-to-elm | elm-format --stdin > source/Decoder.elm.generated
	patch -o source/Decoder.elm -i source/Decoder.elm.patch source/Decoder.elm.generated
	rm -f source/Decoder.elm.generated

clean:
	rm -f source/Decoder.elm
	rm -f sdm.tar.gz sdm/elm.js
	rm -f mcpa.tar.gz mcpa/elm.js

.PHONY: all clean
