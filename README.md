## Reflex Material

A Haskell [Reflex-Dom](https://hackage.haskell.org/package/reflex-dom) wrapper for the [Material Components Web](https://github.com/material-components/material-components-web/) project.

This is a work in progress, forked from [alasconnect/reflex-material](https://github.com/alasconnect/reflex-material) to add more widgets and javascript effects.

The style of API follows [reflex-dom-semui](https://github.com/reflex-frp/reflex-dom-semui).
Widgets are prefixed with `md` to differentiate between e.g. `checkbox` and `mdCheckbox`.

Initial goal is to support most of the
[Material Components Examples](https://material-components-web.appspot.com/).


### Try the demo

https://rvl.github.io/reflex-material/

### API Documentation

https://rvl.github.io/reflex-material/doc/


### Building with nix

    ./build.sh

This will instantiate [reflex-platform](https://github.com/reflex-frp/reflex-platform)
and download Material Components Web, fonts, and icons, so may take a little while.

### How to use in your project

Ensure that you have `js/material-components-web.min.js` set up
because it's needed by `Reflex.Material.Basic.mdcScript`.

### TODOs

- [x] Update Select HTML to match new usage
- [x] Pin version of material-components-web in package.json
- [x] Update for material-design-components 1.1.0
- [ ] Add radio component and example
- [ ] Port example app to obelisk
- [ ] Clean up CSS class name functions
- [ ] Lots more...
