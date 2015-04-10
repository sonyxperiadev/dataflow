# DataFlow

Generate Graphviz documents from a Haskell representation.

```
cabal configure
cabal build
```

## Usage

![Legend](https://rawgit.com/owickstrom/dataflow/master/example/legend.svg)

The objects supported by DataFlow is:

* `TrustBoundary`
* `InputOutput`
* `Function`
* `Flow`

These are composed in a `Diagram` to get something printable.

For more information see the [Hackage site](https://hackage.haskell.org/package/dataflow).

## Example

```haskell
module Main where

import DataFlow.Core
import DataFlow.DFD

main :: IO ()
main = printDfd $
  Diagram "My Diagram" [
    TrustBoundary "browser" "Browser" [
      Function "webapp" "Webapp"
    ],
    TrustBoundary "aws" "Amazon AWS" [
      Function "server" "Web Server",
      Database "logs" "Logs"
    ],
    InputOutput "analytics" "Google Analytics",

    Flow "webapp" "server" "Request /" "",
    Flow "server" "logs" "Log" "User IP",
    Flow "server" "webapp" "Response" "User Profile",

    Flow "webapp" "analytics" "Log" "Page Navigation"
  ]
```

Then generate your output with dot.

```bash
runhaskell example.hs | dot -Tsvg > example.svg
```

That should generate something like the following.

![Example Output](https://rawgit.com/owickstrom/dataflow/master/example/example.svg)

## Building the Examples

```bash
cabal install
make -C example
```

## Release

```bash
cabal clean && cabal build && cabal sdist && cabal upload dist/dataflow-*.tar.gz
```
