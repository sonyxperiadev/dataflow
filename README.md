# DataFlow

Generate Graphviz documents from a Haskell representation.

```
cabal configure
cabal build
```

## Usage

The objects supported by DataFlow is:

* `TrustBoundary`
* `External`
* `Process`
* `Edge`

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
      Process "webapp" "Webapp"
    ],
    TrustBoundary "aws" "Amazon AWS" [
      Process "server" "Web Server",
      Database "logs" "Logs"
    ],
    External "analytics" "Google Analytics",

    Edge "webapp" "server" "Request /" "",
    Edge "server" "logs" "Log" "User IP",
    Edge "server" "webapp" "Response" "User Profile",

    Edge "webapp" "analytics" "Log" "Page Navigation"
  ]
```

Then generate your output with dot.

```bash
runhaskell example.hs | dot -Tsvg > output.svg
```

That should generate something like the following.

![Example Output](https://rawgit.com/owickstrom/dataflow/master/example/output.svg)

