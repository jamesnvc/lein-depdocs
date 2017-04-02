# depdocs

A Leiningen plugin to generate dependency documentation

[![Clojars Project](https://img.shields.io/clojars/v/lein-depdocs.svg)](https://clojars.org/lein-depdocs)

## Usage

Put `[lein-depdocs "1.0.0"]` into the `:plugins` vector of your `:user`
profile.

The plugin outputs markdown, so you can use it as follows (using [cmark-gfm](https://github.com/github/cmark/)).

    $ lein depdocs | cmark-gfm > docs/usage_docs.html

Example output:

<hr>

rookie.common.util
==========

## Uses of  `rookie.common.util/ensure-coll`

Defined on line 11

### Uses in  `rookie.server.db`

At line 124 column 15

```clojure
(map ensure-coll props)
```
## Uses of  `rookie.common.util/nice-count`

Defined on line 3

## Uses of  `rookie.common.util/if?`

Defined on line 7

### Uses in  `rookie.common.util`

At line 11 column 18

```clojure
(if? coll? identity list)
```
<hr>

## License

Copyright © 2017 James Cash

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
