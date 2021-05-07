CC      := cc
CC_OPTS := -std=c11 -Wall -Werror -pedantic -O3

build: bin/fish-nix-locale

bin/fish-nix-locale: bin/fish-nix-locale.c
	$(CC) $(CC_OPTS) bin/fish-nix-locale.c -o bin/fish-nix-locale

install-atom-packages:
	apm install --packages-file atom/packages.txt

install-xkb:
	ln -sf "$$(pwd)/xkb/us_typography" /usr/share/X11/xkb/symbols/us_typography

.PHONY: build install-atom-packages install-xkb
