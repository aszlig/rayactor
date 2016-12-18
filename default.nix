{ debug ? valgrind
, fullDebug ? false
, valgrind ? false
, valgrindFlags ? [
    "--leak-check=full"
    "--track-origins=yes"
    "--log-file=beam.valgrind.%p"
  ]
}:

let
  mkErlang = pkgs: let
    erlang = pkgs.erlang.override {
      enableDebugInfo = debug;
    };
    drv = erlang.drvAttrs;
  in if valgrind then pkgs.stdenv.mkDerivation (drv // {
    buildInputs = (drv.buildInputs) ++ [ pkgs.valgrind ];
    postBuild = let
      flags = "\${enableParallelBuilding:+"
            + "-j\${NIX_BUILD_CORES} -l\${NIX_BUILD_CORES}}";
    in (drv.postBuild or "") + ''
      ERL_TOP="$(pwd)" make -C erts/emulator ${flags} valgrind FLAVOR=smp
      ERL_TOP="$(pwd)" make -C erts/emulator ${flags} valgrind FLAVOR=plain
    '';
    debugInfo = true;
    enableParallelBuilding = true;
    postInstall = (drv.postInstall or "") + ''
      ertsBin="$(echo "$out"/lib/erlang/erts-*/bin)"
      bindir="bin/$(make target_configured)"

      for valbeam in "$bindir"/beam.valgrind*; do
        fileName="$(basename "$valbeam")"
        origName="''${fileName//.valgrind}"
        install -v "$valbeam" "$ertsBin/$fileName"

        origNameDot="$origName."
        valgrindInserted="''${origNameDot/./.novalgrind.}"
        noValgrindName="''${valgrindInserted/%.}"

        mv "$ertsBin/$origName" "$ertsBin/$noValgrindName"

        ${''
          cat > "$ertsBin/$origName" <<EOF
          #!${pkgs.stdenv.shell}
          if [ -n "\$ENABLE_VALGRIND" ]; then
            exec "${pkgs.valgrind}/bin/valgrind" \$VALGRIND_FLAGS \
              "$ertsBin/$fileName" "\$@"
          else
            exec "$ertsBin/$noValgrindName" "\$@"
          fi
          EOF
        ''}
        chmod +x "$ertsBin/$origName"
      done
      cp -a bin/x86_64*/child_setup.* "$ertsBin/"
    '';
    inherit (erlang) meta;
  }) else erlang;

in

with import <nixpkgs> (if valgrind || debug || fullDebug then {
  config.packageOverrides = (pkgs: {
    erlang = mkErlang pkgs;
  } // (if fullDebug then {
    stdenv = pkgs.stdenvAdapters.keepDebugInfo pkgs.stdenv;
  } else {}));
} else {});

beamPackages.buildRebar3 (rec {
  name = "rayactor-${version}";
  version = "0.0.1";

  src = ./.;

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ libftdi1 ];
} // lib.optionalAttrs valgrind {
  ENABLE_VALGRIND = valgrind;
  VALGRIND_FLAGS = toString valgrindFlags;
} // lib.optionalAttrs debug {
  debugInfo = true;
})
