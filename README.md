ghc-core-html
=============

ghc-core-html makes core output that is easier to read by adding colors, filtering
unnecessary information, adding clickable links, and foldable index and bindings.

This is similar to the [ghc-core](http://hackage.haskell.org/package/ghc-core) program,
except that it's to use in a browser.

usage
-----

Typical usage:

    ghc-core-html Program.hs > my-output.html

Just like ghc-core you have the following options:

~~~
    --ghc <ghcprog>: specify which ghc to use (optional)
~~~

improving
---------

Better CSS/HTML, improved Javascript, more core parsing, multiple pages, more
popups: send your pull requests. don't be shy.
