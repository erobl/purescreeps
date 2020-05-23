{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
   name = "ghcjs-shell";
   buildInputs = with pkgs; [ 
     purescript
     spago
     nodePackages.npm
   ];
   shellHook = ''
   '';          
}
