{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.R
    pkgs.rPackages.rmarkdown
    pkgs.rPackages.knitr
    pkgs.rstudioWrapper
    pkgs.rPackages.magick
    pkgs.rPackages.tidyverse
    # keep this line if you use bash
    pkgs.bashInteractive
    pkgs.ffmpeg
  ];
}
