ELMFLAGS = --yes --warn

all: boom.tar.gz mcpa.tar.gz global-pam.tar.gz

debug: ELMFLAGS += --debug
debug: all

boom.tar.gz: boom/elm.js boom/*
	git describe --tags > boom/VERSION
	tar -zcvf boom.tar.gz --exclude=sdmFlagsOverride.js boom

mcpa.tar.gz: mcpa/elm/AncState.js mcpa/elm/Mcpa.js mcpa/elm/Stats.js mcpa/elm/StatsHeatMap.js mcpa/elm/FractalTree.js mcpa/*
	git describe --tags > mcpa/VERSION
	tar -zcvf mcpa.tar.gz mcpa

global-pam.tar.gz: global-pam/elm/subsetpam.js
	git describe --tags > global-pam/VERSION
	tar -zcvf global-pam.tar.gz global-pam

boom/elm.js: source/Decoder.elm source/*
	elm-make source/Main.elm $(ELMFLAGS) --output=boom/elm.js

mcpa/elm/Stats.js: source/Decoder.elm source/*
	elm-make source/StatsMain.elm $(ELMFLAGS) --output=mcpa/elm/Stats.js

mcpa/elm/StatsHeatMap.js: source/Decoder.elm source/*
	elm-make source/StatsHeatMap.elm $(ELMFLAGS) --output=mcpa/elm/StatsHeatMap.js

mcpa/elm/Mcpa.js: source/Decoder.elm source/*
	elm-make source/McpaMain.elm $(ELMFLAGS) --output=mcpa/elm/Mcpa.js

mcpa/elm/AncState.js: source/Decoder.elm source/*
	elm-make source/AncStateMain.elm $(ELMFLAGS) --output=mcpa/elm/AncState.js

mcpa/elm/FractalTree.js: source/Decoder.elm source/*
	elm-make source/FractalTreeMain.elm $(ELMFLAGS) --output=mcpa/elm/FractalTree.js

global-pam/elm/subsetpam.js:  source/Decoder.elm source/*
	elm-make source/SubsetPam.elm $(ELMFLAGS) --output=global-pam/elm/subsetpam.js

source/Decoder.elm: swagger.json source/Decoder.elm.patch
	cat swagger.json | swagger-to-elm | elm-format --stdin > source/Decoder.elm.generated
	patch -o source/Decoder.elm -i source/Decoder.elm.patch source/Decoder.elm.generated
	rm -f source/Decoder.elm.generated

clean:
	rm -f source/Decoder.elm
	rm -f boom.tar.gz boom/elm.js
	rm -f mcpa.tar.gz mcpa/elm/*.js
	rm -f global-pam.tar.gz global-pam/elm/*.js

test: source/Decoder.elm source/* tests/*.elm
	elm test

.PHONY: all debug clean test
