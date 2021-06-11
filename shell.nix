{ pkgs ? import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/8ea1ce75929f6d852911061e20717522e00ea47c.tar.gz) {}
, compiler ? "ghc8104"
# , compiler ? "ghc901"
}:

with pkgs;

mkShell rec {
  name = "bech32-env";
  meta.platforms = lib.platforms.unix;

  ghc = haskell.compiler.${compiler};

  buildInputs = [
    ghc
    cabal-install
    stack
    nix
  ]
  ++ ghc.buildInputs
  ++ lib.optional (stdenv.hostPlatform.libc == "glibc") glibcLocales
  ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa CoreServices libcxx libiconv
  ]);

  # Ensure that libz.so and other libraries are available to TH splices.
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;

  # Force a UTF-8 locale because many Haskell programs and tests
  # assume this.
  LANG = "en_US.UTF-8";

  # Make the shell suitable for the stack nix integration
  # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
  GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  STACK_IN_NIX_SHELL = "true";
}
