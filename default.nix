{ compiler ? "ghc844"

, rev    ? "61c3169a0e17d789c566d5b241bfe309ce4a6275"
, sha256 ? "0qbycg7wkb71v20rchlkafrjfpbk2fnlvvbh3ai9pyfisci5wxvq"

, pkgs   ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "hnix requires at least nix 2.0"
    else import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
           inherit sha256; }) {
           config.allowUnfree = true;
           config.allowBroken = false;
         }

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation   ? null
}:

let haskellPackages = pkgs.haskell.packages.${compiler};

drv = haskellPackages.developPackage {
  name = "hnix";
  root = ./.;

  overrides = with pkgs.haskell.lib; self: super: {
  };

  source-overrides = {};

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    passthru = {
      nixpkgs = pkgs;
    };
  });

  inherit returnShellEnv;
};

in drv
