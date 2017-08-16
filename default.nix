{ stdenv, rust, emscripten, nodejs }:

stdenv.mkDerivation rec {
  name = "time-steward-${version}";
  version = "0.0";

  src = ./src;

  buildInputs = [ emscripten nodejs rust ];
}
