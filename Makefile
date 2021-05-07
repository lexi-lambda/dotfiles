CC      := cc
CC_OPTS := -std=c11 -Wall -Werror -pedantic -O3

build: bin/fish-nix-locale

bin/fish-nix-locale: bin/fish-nix-locale.c
	$(CC) $(CC_OPTS) bin/fish-nix-locale.c -o bin/fish-nix-locale

install-xkb:
	ln -sf xkb/us_typography /usr/share/X11/xkb/symbols/us_typography

.PHONY: build install-xkb
