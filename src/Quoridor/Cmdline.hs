module Quoridor.Cmdline
where

import Quoridor.Cmdline.Render (render)
import Quoridor.Cmdline.Parse (parseTurn)
import Quoridor

cmdlineMain :: IO ()
cmdlineMain = play initialGameState

play :: GameState -> IO ()
play gs =
  print $ parseTurn "m 3 4 "

