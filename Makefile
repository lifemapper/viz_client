ELMFLAGS = --yes --warn

all: sdm.tar.gz mcpa.tar.gz mcpa/elm/subsetpam.js

debug: ELMFLAGS += --debug
debug: all

sdm.tar.gz: sdm/elm.js sdm/*
	tar -zcvf sdm.tar.gz --exclude=sdmFlagsOverride.js sdm

mcpa.tar.gz: mcpa/elm/AncState.js mcpa/elm/Mcpa.js mcpa/elm/Stats.js mcpa/elm/FractalTree.js mcpa/*
	tar -zcvf mcpa.tar.gz mcpa

sdm/elm.js: source/Decoder.elm source/*
	elm-make source/Main.elm $(ELMFLAGS) --output=sdm/elm.js

mcpa/elm/Stats.js: source/Decoder.elm source/*
	elm-make source/StatsMain.elm $(ELMFLAGS) --output=mcpa/elm/Stats.js

mcpa/elm/Mcpa.js: source/Decoder.elm source/*
	elm-make source/McpaMain.elm $(ELMFLAGS) --output=mcpa/elm/Mcpa.js

mcpa/elm/AncState.js: source/Decoder.elm source/*
	elm-make source/AncStateMain.elm $(ELMFLAGS) --output=mcpa/elm/AncState.js

mcpa/elm/FractalTree.js: source/Decoder.elm source/*
	elm-make source/FractalTreeMain.elm $(ELMFLAGS) --output=mcpa/elm/FractalTree.js

mcpa/elm/subsetpam.js:  source/Decoder.elm source/*
	elm-make source/SubsetPam.elm $(ELMFLAGS) --output=mcpa/elm/subsetpam.js

source/Decoder.elm: swagger.json source/Decoder.elm.patch
	cat swagger.json | swagger-to-elm | elm-format --stdin > source/Decoder.elm.generated
	patch -o source/Decoder.elm -i source/Decoder.elm.patch source/Decoder.elm.generated
	rm -f source/Decoder.elm.generated

clean:
	rm -f source/Decoder.elm
	rm -f sdm.tar.gz sdm/elm.js
	rm -f mcpa.tar.gz mcpa/elm/*.js

test: source/Decoder.elm source/* tests/*.elm
	elm test

.PHONY: all debug clean test
