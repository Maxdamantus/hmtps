hmtpsup: hmtpsup.hs send.c
	ghc --make hmtpsup.hs send.c -lmtp -ltag_c `taglib-config --cflags`

hmtpsdb: hmtpsdb.hs
	ghc --make hmtpsdb.hs

all: hmtpsdb hmtpsup
