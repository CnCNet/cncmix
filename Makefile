CC             ?= gcc
CFLAGS_mixer    = -m32 -pedantic -O2 -Wall

mixerdir       ?= .

mixer: $(mixerdir)/cncmix$(EXT)

$(mixerdir)/cncmix$(EXT): $(mixerdir)/cncmix.c
	$(CC) $(CFLAGS_patcher) -o $(mixerdir)/cncmix$(EXT) $(mixerdir)/cncmix.c

clean_makefile:
	rm -rf $(mixerdir)/cncmix$(EXT)