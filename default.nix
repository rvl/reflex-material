{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
let
  pkgs = reflex-platform.nixpkgs;

  ob = project ./. ({ ... }: {
    android.applicationId = "rvl.reflexmaterial.examples";
    android.displayName = "Reflex Material Catalogue";
    ios.bundleIdentifier = "rvl.reflexmaterial.examples";
    ios.bundleName = "Reflex Material Catalogue";

    withHoogle = true;

    packages = {
      reflex-material = pkgs.lib.cleanSource ./reflex-material;
    };

    # overrides = self: super: with pkgs.haskell.lib; {
    # };

    # shellToolOverrides = ghc: super: { };

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

  mkShell = haskellPackages: addNodeModules (haskellPackages.shellFor {
    packages = p: [ p.reflex-material ];
    withHoogle = true;
  });

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

  addNodeModules = shellDrv: shellDrv.overrideAttrs (oldAttrs: {
    # Provide NODE_MODULES in the nix-shell.
    # This will be used by the shake build script.
    shellHook = (oldAttrs.shellHook or "") + ''
      export NODE_MODULES=${nodePackages}/lib/node_modules/reflex-material/node_modules
    '';
  });

in
  ob // {
    inherit nodePackages;
  }
