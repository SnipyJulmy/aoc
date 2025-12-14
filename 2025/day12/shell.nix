{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {

  name = "aoc-scala-shell";

  packages = with pkgs; [
    scala-cli
    bloop
  ];

  shellHook = ''
    mise trust
    eval "$(mise activate bash)"
  '';
}
