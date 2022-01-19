SHELL   := /bin/bash

JOBS    := 1

CC      := cc
CC_OPTS := -std=c11 -Wall -Werror -pedantic -O3

.PHONY: build build-racket-bin install-atom-config install-xkb

build: build-racket-bin bin/fish-nix-locale bookmarklets/firefox/dotfiles.zip

build-racket-bin:
	raco pkg install --skip-installed --auto gregor threading
	cd bin && raco make -vj $(JOBS) \
		batch-args exec-in-pty git-diffstats-histogram \
		hoogle-generate-from-ghc-environment-file racket-regexp \
		raco-pkg-show-local-paths timestamp-lines watch-exec

bin/fish-nix-locale: bin/fish-nix-locale.c
	$(CC) $(CC_OPTS) bin/fish-nix-locale.c -o bin/fish-nix-locale

# ------------------------------------------------------------------------------
# bookmarklets

# Packages bookmarklets as a Firefox extension, which allows them to be invoked
# using keyboard shortcuts. Note that most versions of Firefox require the
# extension to be signed by Mozilla to install, which this Makefile does not do.
bookmarkets_firefox_deps := background-script.js manifest.json $(wildcard bookmarklets/*.js)
bookmarklets/firefox/dotfiles.zip: $(addprefix bookmarklets/firefox/,$(bookmarkets_firefox_deps))
	cd bookmarklets/firefox && zip -FS dotfiles.zip $(bookmarkets_firefox_deps)

# Copies all bookmarklets into the firefox/ subdirectory, since Firefox
# extensions aren’t allowed to inject scripts outside of their root directory.
bookmarklets/firefox/bookmarklets/%.js: bookmarklets/%.js
	@mkdir -p bookmarklets/firefox/bookmarklets
	cp '$<' '$@'

# ------------------------------------------------------------------------------

install-atom-config:
	apm install --packages-file atom/packages.txt
	ln -s "$$(pwd)"/atom/{config,keymap}.cson ~/.atom/

install-xkb:
	ln -sf "$$(pwd)/xkb/us_typography" /usr/share/X11/xkb/symbols/us_typography
