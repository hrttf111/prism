{
  description = "Prism x86 emulator";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlay ];
      };
    in rec {
      overlay = final: prev: let
      in {
        prism = prev.haskellPackages.callPackage ./default.nix {zlib1 = pkgs.zlib;};
      };

      packages.x86_64-linux.prism = pkgs.prism;
      defaultPackage.x86_64-linux = pkgs.prism;
    };
}
