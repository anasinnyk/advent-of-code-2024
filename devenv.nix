{ pkgs, lib, config, inputs, ... }:

{
  packages = with pkgs; [
    haskellPackages.hoogle
    haskellPackages.doctest
    haskellPackages.haskell-dap
  ];

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc910;
  };
}
