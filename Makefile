

sdm.tar.gz:
	rm -rf sdm
	mkdir -p sdm
	elm-make source/Main.elm --yes --warn --output=sdm/elm.js
	cp index.html main.js fixes.css sdm
	tar -zcvf sdm.tar.gz sdm

.PHONY: sdm.tar.gz



