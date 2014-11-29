module Quoridor.Cmdline
where

import Quoridor.Parse
import Quoridor

cmdlineMain :: IO ()
cmdlineMain = play initialGameState

play :: GameState -> IO ()
play gs =
  print $ parseTurn "m 3 4 "

{-render :: GameState -> IO ()-}
{-render gs = go -}

