module Main
 ( main
 ) where

import Data.List  (find, sort)
import Data.Maybe (fromJust)

import Test.HUnit

import Quoridor

-- helper functions

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO Counts
main = runTestTT $ TestList accumulateTests
{-main = runTestTT $ accumulateTests !! 7-}



-- A gamestate to test
-- Black's turn
--    2 3 4 5
--  2 E E E|E
--      - -
--  3 E B W|E
--
--  4 E E E E
--      E = empty tile, B = Black, W = White, |,- = Gates
someGameState :: GameState
someGameState = initgs
  { halfGates  = halfGates'
  , playerList = playerList'
  }
  where initgs = initialGameState defaultGameConfig
        halfGates' = foldr insertGate (halfGates initgs)
          [ gateUpperLeft (2,3) H
          , gateUpperLeft (2,4) V
          ]
        playerList' =
          [ Player { color     = Black
                   , pos       = (3,3)
                   , gatesLeft = 1
                   }
          , Player { color     = White
                   , pos       = (3,4)
                   , gatesLeft = 0
                   }
          ]

runGameTest :: Game m a -> GameState -> m (a, GameState)
runGameTest g gs = runGameWithGameState g gs defaultGameConfig

execGame :: Functor m => Game m a -> GameState -> m GameState
execGame = (fmap snd .) . runGameTest

evalGame :: Functor m => Game m a -> GameState -> m a
evalGame = (fmap fst .) . runGameTest

accumulateTests :: [Test]
accumulateTests =
  [
    testCase "changeCurrPlayer" $ do
      let gs = someGameState
      gs' <- execGame changeCurrPlayer gs
      True @=? currP gs' /= currP gs
      head (tail $ playerList gs) @=? currP gs'
      currP gs @=? last (playerList gs')

  , testCase "getValidMoves" $ do
      let
        getPlayer thisColor = fromJust $
          find ((==) thisColor . color) (playerList someGameState)
        validForColor c =
          getValidMoves (pos $ getPlayer c)
          (boardSize defaultGameConfig) someGameState
      [(3,2),(4,3),(4,4)] @=? sort (validForColor Black)
      [(3,2),(4,4)] @=? sort (validForColor White)

  , testCase "isValidTurn-1-move-valid" $ do
      (True @=?) =<< evalGame (isValidTurn $ Move (3,2)) someGameState
      (True @=?) =<< evalGame (isValidTurn $ Move (4,3)) someGameState

  , testCase "isValidTurn-1-move-invalid" $ do
      (False @=?) =<< evalGame (isValidTurn $ Move (2,3)) someGameState
      (False @=?) =<< evalGame (isValidTurn $ Move (3,4)) someGameState

  , testCase "isValidTurn-2-move-valid" $
      (True @=?) =<< evalGame (isValidTurn $ Move (4,4)) someGameState

  , testCase "isValidTurn-2-move-invalid" $ do
      let gs = someGameState
          halfGates' = insertGate (gateUpperLeft (3,4) H) $ halfGates gs
          gs' = gs { halfGates = halfGates' }
      (False @=?) =<< evalGame (isValidTurn $ Move (4,4)) gs'

  , testCase "isValidTurn-putGate-valid" $
      (True @=?) =<< evalGame (isValidTurn $ PutGate $ gateUpperLeft (2,3) V)
                 someGameState

  , testCase "isValidTurn-putGate-invalid-overlap" $
      (False @=?) =<< evalGame (isValidTurn $ PutGate $ gateUpperLeft (2,2) H)
                 someGameState

  , testCase "isValidTurn-putGate-invalid-willBlock" $ do
      let gs = someGameState
          halfGates' = insertGate (gateUpperLeft (3,3) V) $ halfGates gs
          gs' = gs { halfGates = halfGates' }
      (False @=?) =<< evalGame
        (isValidTurn $ PutGate $ gateUpperLeft (3,3) H) gs'

  , testCase "makeTurn-move-valid" $ do
      let gs = someGameState
      (succeed, gs') <- runGameTest (makeTurn $ Move (4,4)) gs
      True @=? succeed
      let p' = last $ playerList gs'
      color (currP gs) @=? color p'
      (4,4) @=? pos p'

  , testCase "makeTurn-move-invalid" $ do
      let gs = someGameState
      (succeed, gs') <- runGameTest (makeTurn $ Move  (3,5)) gs
      False @=? succeed
      color (currP gs) @=? color (currP gs')
      (3,3) @=? pos (currP gs')

  , testCase "makeTurn-putGate-valid" $ do
      let gs = someGameState
          ggs = halfGates gs
          gateToInsert = gateUpperLeft (3,3) V
      (succeed, gs') <- runGameTest (makeTurn $ PutGate gateToInsert) gs
      True @=? succeed
      insertGate gateToInsert ggs @=? halfGates gs'

  , testCase "checkAndSetWinner-nothing" $
      (Nothing @=?) =<< evalGame checkAndSetWinner someGameState

  , testCase "checkAndSetWinner-black-won" $ do
      let gs = someGameState
          gs' = modifyCurrP (\p -> p { pos = (0,3) }) gs
      (Just (color $ currP gs) @=?) =<< evalGame checkAndSetWinner gs'
  ]
