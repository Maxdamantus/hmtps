all: hmtpsdb hmtpsup hmtpspl

hmtpsup: hmtpsup.hs SendFFI.hs send.c
	ghc --make hmtpsup.hs send.c -lmtp -ltag_c `taglib-config --cflags`

hmtpsdb: hmtpsdb.hs
	ghc --make hmtpsdb.hs

hmtpspl: hmtpspl.hs
	ghc --make hmtpspl.hs send.c -lmtp -ltag_c `taglib-config --cflags`

clean:
	rm -f hmtpsdb hmtpsup *.o *.hi
