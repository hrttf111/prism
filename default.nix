{ callCabal2nix, haskell, cabal-install, hpack, zlib1, yasm }:

with haskell.lib;

let
  prismOrigin = justStaticExecutables (callCabal2nix "prism" ./. {});
in
  (overrideCabal prismOrigin (old: {
    pname = "prism";
    buildDepends = [ cabal-install hpack zlib1 yasm ];
  })).overrideAttrs (finalAttrs: previousAttrs: {
    #propagatedBuildInputs = [ zlib1 ];
    LD_LIBRARY_PATH = "${zlib1}/lib:$LD_LIBRARY_PATH";
  })
