let pkgs = import <nixpkgs> {
  overlays = [
    (import ./nixpkgs-mozilla/rust-overlay.nix)
  ];
};
in with pkgs; callPackage ./default.nix {
  rust = rustChannels.nightly.rust.override {
    targets = [ "asmjs-unknown-emscripten" ];
  };
}
