CC      := cc
CC_OPTS := -std=c11 -Wall -Werror -pedantic -O3

all: bin/fish-nix-locale

bin/fish-nix-locale: bin/fish-nix-locale.c
	$(CC) $(CC_OPTS) bin/fish-nix-locale.c -o bin/fish-nix-locale

.PHONY: all
