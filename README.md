# DataFlow

Generate Graphviz documents from a Haskell representation.

## Getting Started

```
cabal sandbox init
cabal configure --enable-tests
cabal install --only-dependencies --enable-tests
```

## Usage

![Legend](https://rawgit.com/owickstrom/dataflow/master/examples/legend.svg)

The objects supported by DataFlow is:

* `TrustBoundary`
* `InputOutput`
* `Function`
* `Database`
* `Flow`

These are composed in a `Diagram` to get something printable.

For more on Haskell data types, see [the Hackage site](https://hackage.haskell.org/package/dataflow).

## Example

```haskell
module Main where

import DataFlow.Core
import DataFlow.Graphviz.Renderer
import DataFlow.DFD

main :: IO ()
main = putStr $ renderGraphviz $ asDFD  $
  Diagram (Just "Webapp") [
    TrustBoundary "browser" "Browser" [
      Function "client" "Client"
    ],
    TrustBoundary "aws" "Amazon AWS" [
      Function "server" "Web Server",
      Database "logs" "Logs"
    ],
    InputOutput "analytics" "Google Analytics",

    Flow "client" "server" "Request /" "",
    Flow "server" "logs" "Log" "User IP",
    Flow "server" "client" "Response" "User Profile",

    Flow "client" "analytics" "Log" "Page Navigation"
  ]
```

Then generate your output with dot.

```bash
runhaskell example.hs | dot -Tsvg > example.svg
```

That should generate something like the following.

![Example Output](https://rawgit.com/owickstrom/dataflow/master/examples/webapp.svg)

## Building the Examples

```bash
make -C examples
```

## Release

```bash
cabal clean && cabal build && cabal sdist && cabal upload dist/dataflow-*.tar.gz
```
