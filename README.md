# depdocs

A Leiningen plugin to generate dependency documentation

## Usage

Put `[depdocs "0.1.0-SNAPSHOT"]` into the `:plugins` vector of your `:user`
profile.

The plugin outputs markdown, so you can use it as follows (using [cmark-gfm](https://github.com/github/cmark/)).

    $ lein depdocs | cmark-gfm > docs/usage_docs.html

## License

Copyright © 2017 James Cash

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
