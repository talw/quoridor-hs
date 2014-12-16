-- | Messages to be printed to the player
module Quoridor.Cmdline.Messages
where

import           Quoridor (Color, Turn)

msgAwaitingTurn :: Color -> String
msgAwaitingTurn c = "Waiting for " ++ show c ++ " to make a move."

msgGameEnd :: Color -> String
msgGameEnd c = show c ++ " won!"

msgValidTurn :: Color -> Turn -> String
msgValidTurn c t = "Previous turn was: " ++ show c ++ " - " ++ show t

msgInvalidTurn, msgInitialTurn :: String
msgInvalidTurn = "Attempted Turn was invalid"
msgInitialTurn = "Good luck!"

validMovesChars :: String
validMovesChars = "!@#$%^&*"

msgInputInstr :: String
msgInputInstr = unlines
  [ "type: g y x [h/v]   to place horizontal/vertical gate,"
  , "                    across a 2x2 square, whose top left is at y,x"
  , "type: m y x         to move."
  , "type one of: " ++ validMovesChars ++ " to move to where that character is on the board."
  ]
