# DataFlow

Generate Graphviz documents from a Haskell representation.

## Getting Started

```
cabal install dataflow@0.6.0.0
```

## Usage

![Legend](https://rawgit.com/owickstrom/dataflow/master/examples/legend.svg)

The objects supported by DataFlow is:

* `boundary`
* `io`
* `function`
* `database`
* `->`

These are composed in a `diagram` to get something printable.

## Example

```
diagram 'Webapp' {
  boundary 'Browser' {
    function client 'Client'
  }
  boundary 'Amazon AWS' {
    function server 'Web Server'
    database logs 'Logs'
  }
  io analytics 'Google<br/>Analytics'

  client -> server 'Request /' ''
  server -> logs 'Log' 'User IP'
  server -> client 'Response' 'User Profile'
  client -> analytics 'Log' 'Page Navigation'
}
```

Then generate your output with dot.

```bash
dataflow dfd webapp.flow | dot -Tsvg > webapp.svg
```

That should generate something like the following.

![Example Output](https://rawgit.com/owickstrom/dataflow/master/examples/webapp.svg)

## Build

```
cabal sandbox init
cabal configure --enable-tests
cabal install --only-dependencies --enable-tests
```

## Building the Examples

```bash
make -C examples
```
## Haskell Docs

See [the Hackage site](https://hackage.haskell.org/package/dataflow).

## Release

```bash
cabal clean && cabal build && cabal sdist && cabal upload dist/dataflow-*.tar.gz
```
