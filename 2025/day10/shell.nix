{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {

  name = "ortools-solver-env";

  packages = [
    pkgs.python3
    pkgs.python3Packages.pulp
    pkgs.cbc
  ];

  shellHook = ''
    mise trust
    eval "$(mise activate bash)"
  '';
}
