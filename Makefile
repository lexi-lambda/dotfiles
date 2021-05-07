JOBS    := 1

CC      := cc
CC_OPTS := -std=c11 -Wall -Werror -pedantic -O3

build: build-racket-bin bin/fish-nix-locale

build-racket-bin:
	raco pkg install --skip-installed --auto gregor threading
	cd bin && raco make -vj $(JOBS) \
		batch-args exec-in-pty git-diffstats-histogram \
		hoogle-generate-from-ghc-environment-file racket-regexp \
		raco-pkg-show-local-paths timestamp-lines watch-exec

bin/fish-nix-locale: bin/fish-nix-locale.c
	$(CC) $(CC_OPTS) bin/fish-nix-locale.c -o bin/fish-nix-locale

install-atom-packages:
	apm install --packages-file atom/packages.txt

install-xkb:
	ln -sf "$$(pwd)/xkb/us_typography" /usr/share/X11/xkb/symbols/us_typography

.PHONY: build build-racket-bin install-atom-packages install-xkb
