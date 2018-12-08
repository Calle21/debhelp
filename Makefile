clean: Main.hs
	ghc -o debhelp Main.hs
	rm *.o
	rm *.hi
	cp -u debhelp.txt ~/.debhelp.text
