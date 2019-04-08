{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
# with import ./.obelisk/impl {
with import /home/rodney/src/obsidiansystems/obelisk {
  inherit system iosSdkVersion;
  # You must accept the Android Software Development Kit License Agreement at
  # https://developer.android.com/studio/terms in order to build Android apps.
  # Uncomment and set this to `true` to indicate your acceptance:
  config.android_sdk.accept_license = true;
};
let
  pkgs = reflex-platform.nixpkgs;

  ob = project ./. ({ ... }: {
    android.applicationId = "rvl.reflexmaterial.example";
    android.displayName = "Reflex Material";
    ios.bundleIdentifier = "rvl.reflexmaterial.example";
    ios.bundleName = "Reflex Material";

    # withHoogle = true;

    packages = {
      reflex-material = ./reflex-material;
    };

    staticFiles = pkgs.runCommand "static" {} ''
      mkdir -p $out/{js,css,fonts,images}
      cd ${nodePackages}/lib/node_modules/reflex-material/node_modules

      cd material-components-web/dist
      cp material-components-web.min.js* $out/js
      cp material-components-web.min.css* $out/css
      cd ../..

      cp material-design-icons/iconfont/material-icons.css $out/css
      cp roboto-fontface/css/roboto/roboto-fontface.css $out/css
      cp roboto-fontface/fonts/{roboto,roboto-condensed,roboto-slab}/* $out/fonts
      cp material-design-icons/iconfont/MaterialIcons-Regular.* $out/fonts

      cp -Rv ${./static/images}/* $out/images
      cp -Rv ${./static/css}/* $out/css
    '';

  });

  # GHCJS compile fixes
  ghcjsBuildFixes = self: super: with pkgs.haskell.lib; {
    Glob = dontCheck super.Glob; # tests failing on ghcjs
    SHA1 = dontCheck super.SHA1; # tests failing on ghcjs
    http-date = doJailbreak (dontCheck super.http-date); # doctest
    iproute = doJailbreak (dontCheck super.iproute); # doctest
    unix-time = doJailbreak (dontCheck super.unix-time); # doctest
    bsb-http-chunked = doJailbreak (dontCheck super.unix-time); # doctest
  };

  nodePackages = (import ./mdc.nix { inherit pkgs; }).package.override {
    src = pkgs.lib.cleanSourceWith {
      src = ./.;
      filter = name: _: baseNameOf name == "package.json";
    };
  } // {
    regenerate = pkgs.writeScript "node-packages-regenerate.sh" ''
      exec ${pkgs.nodePackages.node2nix}/bin/node2nix \
          --input package.json --output node-packages.nix \
          --composition mdc.nix --node-env node-env.nix
    '';
  };

in
  ob // {
    inherit nodePackages;
  }
