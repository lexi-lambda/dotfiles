#define _POSIX_C_SOURCE 200809L

/* Nix does not automatically set up locale information in such a way that glibc
can find it, as described here:

    https://github.com/NixOS/nixpkgs/issues/38991

Instead, nixpkgs patches glibc to accept a path to the locale information via
the LOCALE_ARCHIVE environment variable. Normally, this would be just fine: we'd
set the environment variable at login, and everything would work okay.

The wrinkle is that my login shell (fish) is installed via nix, and it cares
about the current locale. If I set LOCALE_ARCHIVE in my config.fish, it's
already too late: fish needs to load the locale information to start
interpreting commands, so setting the environment variable will only affect
subprocesses.

Fortunately, this is simple to work around with a tiny wrapper shell script. In
theory, the following ought to be enough to do the job:

    #!/bin/bash
    . ~/.nix-profile/etc/profile.d/nix.sh
    export LOCALE_ARCHIVE_2_27="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"
    exec -l -fish fish "$@"

There's just one problem: we really only want to do all this if our script is
invoked as a login shell, and there's no way to detect that from within a shell
script as far as I can tell. Normally this is communicated by prefixing argv[0]
with '-', but $0 within the shell script never includes the prefix.

This tiny C program is our workaround. It does essentially the same thing as the
shell script, except in C, and it just execs fish normally when invoked as a
non-login shell. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

static char *get_locale_archive_path(void);

int main(int argc, char *argv[]) {
  if (argc > 0 && argv[0][0] == '-') {
    // this is a login shell, set up LOCALE_ARCHIVE
    char *locale_archive_path = get_locale_archive_path();
    setenv("LOCALE_ARCHIVE_2_27", locale_archive_path, 1);
    free(locale_archive_path);
    argv[0] = "-fish";
  } else {
    argv[0] = "fish";
  }

  execv("/home/alexis/.nix-profile/bin/fish", argv);
}

/* Looks up the appropriate value for LOCALE_ARCHIVE_2_27 by shelling out. There
   isn't really any way to avoid the sh process here, because the only way to
   reliably set up the environment to invoke nix-build is to source nix.sh. */
static char *get_locale_archive_path(void) {
  size_t path_buf_len = 1024;
  size_t path_len = 0;
  char *path_buf = malloc(path_buf_len);

  const char path_command[] =
    ". ~/.nix-profile/etc/profile.d/nix.sh && "
    "printf \"%s\" \"$(nix-build --no-out-link \"<nixpkgs>\" -A glibcLocales)/lib/locale/locale-archive\"";
  FILE *path_file = popen(path_command, "r");

  while (fgets(path_buf + path_len, path_buf_len - path_len, path_file)) {
    path_len += strlen(path_buf + path_len);
    if (path_len + 1 == path_buf_len) {
      path_buf_len *= 2;
      path_buf = realloc(path_buf, path_buf_len);
    }
  }

  int exit_status = pclose(path_file);
  if (exit_status == -1) {
    perror("pclose");
    exit(EXIT_FAILURE);
  }
  if (exit_status != 0) {
    fprintf(stderr, "fish-nix-locale: command failed with status %d\n", exit_status);
    exit(EXIT_FAILURE);
  }

  return path_buf;
}
