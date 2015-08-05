# Build Instructions

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
