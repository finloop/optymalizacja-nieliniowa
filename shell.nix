{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.R
    pkgs.rPackages.rmarkdown
    pkgs.rPackages.knitr
    pkgs.rstudioWrapper
    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
