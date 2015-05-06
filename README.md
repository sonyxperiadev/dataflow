# DataFlow

Render graphs using a declarative markup. Currently supports DFD
(http://en.wikipedia.org/wiki/Data_flow_diagram) and sequence diagrams
(http://plantuml.sourceforge.net/sequence.html).

![DFD Output](examples/webapp.dfd.png)

## Usage

So you want to you use DataFlow? Then please read [Usage](USAGE.md).

## Setup

```bash
cabal sandbox init # optional
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
```

## Build

```bash
cabal build
```

## Install

If you initialized a sandbox the executable will end up in the sandbox, i.e.
`.cabal-sandbox/bin/dataflow`. If you have no sandbox it will end up in
`~/.cabal/bin/dataflow`. If you get any stange errors during install try a `cabal clean`

```bash
cabal install
```

## Tests

```bash
./run-tests.sh
# or...
./watch-tests.sh
```

## Building the Examples

```bash
make -C examples
```

## License

BSD-3, see [LICENSE](LICENSE).
