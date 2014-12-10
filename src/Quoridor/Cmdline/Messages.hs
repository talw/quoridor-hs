module Quoridor.Cmdline.Messages where

import Quoridor

msgAwaitingTurn :: Color -> String
msgAwaitingTurn c = "Waiting for " ++ show c ++ " to make a move."

msgGameEnd :: Color -> String
msgGameEnd c = show c ++ " won!"

msgValidTurn :: Color -> Turn -> String
msgValidTurn c t = show c ++ " : " ++ show t

msgInvalidTurn, msgInitialTurn :: String
msgInvalidTurn = "Last Turn was invalid"
msgInitialTurn = "Good luck!"

