module DataFlow.PrettyRenderer where

import Control.Monad.State
import Control.Monad.Writer

type Indent = Int
type IndentNext = Bool
data RendererState = RendererState Indent IndentNext

-- | The Renderer represents some output generator that runs on a 'Diagram'.
type Renderer t = WriterT [String] (State RendererState) t

-- | Write a string to the output (no linefeed).
write :: String -> Renderer ()
write s = do
  (RendererState n indentNext) <- lift get
  if indentNext
    then tell [replicate n ' ' ++ s]
    else tell [s]
  put $ RendererState n False

-- | Write a string to the output (with linefeed).
writeln :: String -> Renderer ()
writeln s = do
  write s
  write "\n"
  modify $ \(RendererState n _) -> RendererState n True

-- | Increase indent with 2 spaces.
indent :: Renderer ()
indent = modify $ \(RendererState n indentNext) -> RendererState (n + 2) indentNext

-- | Decrease indent with 2 spaces.
dedent :: Renderer ()
dedent = modify $ \(RendererState n indentNext) -> RendererState (n - 2) indentNext

-- | Indent the output of gen with 2 spaces.
withIndent :: Renderer () -> Renderer ()
withIndent gen = do
  indent
  gen
  dedent

renderWithIndent :: Renderer () -> String
renderWithIndent r =
  concat $ evalState (execWriterT r) (RendererState 0 False)
