all: hmtps

hmtps: hmtps.hs SendFFI.hs send.c
	ghc --make hmtps.hs send.c -lmtp -ltag_c `taglib-config --cflags`

clean:
	rm -f hmtps *.o *.hi
