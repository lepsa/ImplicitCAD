with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "ImplicitCAD";
  buildInputs = [
    zlib
    pkgconfig
    # match the ghc version from stack
    haskell.compiler.ghc844
    gmp
  ];
  # copied from docker-compose-localdb.yml
  shellHook = ''
    export SENSORCLOUD_USERNAME=grains-app@csiro.au
    # export SENSORCLOUD_PASSWORD=hjgpy2g1G69iHD3q
    export SENSORCLOUD_PASSWORD=ghn8jGG43C14
    # export PADDOCK_DB_HOST=db
    export PADDOCK_DB_HOST=localhost
    export PADDOCK_DB_PORT=5432
    export PADDOCK_DB_USER=postgres
    export PADDOCK_DB_PASSWORD=password
    export PADDOCK_DB_NAME=paddockDb
    export PRELOAD_DISABLE_SENAPS=True
    export PRELOAD_CREATE_NEW_PP_IF_EXIST=False
    export PRELOAD_CREATE_IDEMPOTENT=True
    export PRELOAD_ENV_NAME=Development
  '';
}
