{ mkDerivation, base, containers, stdenv }:
mkDerivation {
  pname = "LambdaCalculus";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
