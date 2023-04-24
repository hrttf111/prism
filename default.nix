{ callCabal2nix, haskell, cabal-install, hpack, yasm }:

with haskell.lib;

let
  blukOrigin = justStaticExecutables (callCabal2nix "prism" ./. {});
in
  overrideCabal blukOrigin (old: {
    pname = "prism";
    buildDepends = [ cabal-install hpack yasm ];
  })
