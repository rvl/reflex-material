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
