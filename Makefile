.PHONY: minesweeper.js

all: minesweeper.js

minesweeper.js: Main.hs
	hastec --start=onload -Wall --out=$@ $<

clean:
	-rm -rf main *.hi *.o *.js
