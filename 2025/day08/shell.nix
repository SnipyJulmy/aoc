{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  name = "aoc-nim-env";
  buildInputs = with pkgs; [
    nim
    nimble
  ];
  shellHook = ''
    eval "$(mise activate bash)"
  '';
}
