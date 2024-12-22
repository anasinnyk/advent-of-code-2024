{ pkgs, lib, config, inputs, ... }:

{
  packages = with pkgs; [
    haskellPackages.doctest
  ];

  languages.haskell.enable = true;
}
