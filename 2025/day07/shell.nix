{
  pkgs ? import <nixpkgs> { },
}:

let
  guile = with pkgs; [
    guile_3_0
  ];
  guileDeps = with pkgs; [
    guile-lib
    guile-quickcheck
  ];
  packages = with pkgs; [
    pkg-config
    mise
    watchexec
  ];
  mkGuilePath =
    prefix: version: deps:
    pkgs.lib.concatMapStringsSep ":" (pkg: "${pkg}${prefix}${version}") deps;
in
pkgs.mkShell {
  name = "aoc-guile-env";
  buildInputs = guile ++ guileDeps ++ packages;
  shellHook = ''
    mise trust
    export MISE_EXPERIMENTAL=1
    export GUILE_LOAD_PATH="${mkGuilePath "/share/guile/site" "3.0" guileDeps}:$GUILE_LOAD_PATH"
    export GUILE_LOAD_COMPILED_PATH="${
      mkGuilePath "/lib/guile/3.0/site-ccache" "3.0" guileDeps
    }:$GUILE_LOAD_COMPILED_PATH"
    alias guile="guile -l .guile"
    eval "$(mise activate bash)"
  '';
}
