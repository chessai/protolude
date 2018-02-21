{ package ? "protolude", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).protolude
