{ pkgs, lib, config, inputs, ... }:

{
  packages = with pkgs; [
    haskellPackages.doctest
  ];

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc910;
  };
}
