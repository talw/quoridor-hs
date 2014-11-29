import Test.HUnit
import Quoridor
import Control.Monad.State

-- helper functions

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO Counts
main = runTestTT $ TestList accumulateTests
{-main = runTestTT $ accumulateTests !! 10-}

-- A gamestate to test
-- Black's turn
--      E E|E
--      - -
--      B W|E
--
--      E = empty tile, B = Black, W = White, |,- = Gates
--      B is at 3,3 direction of y,x axis >,v
someGameState :: GameState
someGameState = initialGameState {
                  halfGates = halfGates',
                  playerLoop = concat $ repeat playerList'
                }
  where halfGates' = foldr insertGate (halfGates initialGameState) [
            gateUpperLeft (2,3) H
          , gateUpperLeft (2,4) V
          ]
        playerList' = [
            Player { color = Black,
                     pos = (3,3),
                     gatesLeft = 1 }
          , Player { color = White,
                     pos = (3,4),
                     gatesLeft = 0 }
          ]

accumulateTests :: [Test]
accumulateTests =
  [
    testCase "changeCurrPlayer" $ do
      let gs = someGameState
          gs' = execState changeCurrPlayer gs
      True @=? currP gs' /= currP gs
      head (tail $ playerList gs) @=? currP gs'
      currP gs @=? last (playerList gs')
  , testCase "isValidTurn-1-move-valid" $ do
      True @=? evalState (isValidTurn $ Move (3,2)) someGameState
      True @=? evalState (isValidTurn $ Move (4,3)) someGameState
  , testCase "isValidTurn-1-move-invalid" $ do
      False @=? evalState (isValidTurn $ Move (2,3)) someGameState
      False @=? evalState (isValidTurn $ Move (3,4)) someGameState
  , testCase "isValidTurn-2-move-valid" $
      True @=? evalState (isValidTurn $ Move (4,4)) someGameState
  , testCase "isValidTurn-2-move-invalid" $ do
      let gs = someGameState
          halfGates' = insertGate (gateUpperLeft (3,4) H) $ halfGates gs
          gs' = gs { halfGates = halfGates' }
      False @=? evalState (isValidTurn $ Move (4,4)) gs'
  , testCase "isValidTurn-putGate-valid" $
      True @=? evalState (isValidTurn $ PutGate $ gateUpperLeft (2,3) V)
                 someGameState
  , testCase "isValidTurn-putGate-invalid-overlap" $
      False @=? evalState (isValidTurn $ PutGate $ gateUpperLeft (2,2) H)
                 someGameState
  , testCase "isValidTurn-putGate-invalid-willBlock" $ do
      let gs = someGameState
          halfGates' = insertGate (gateUpperLeft (3,3) V) $ halfGates gs
          gs' = gs { halfGates = halfGates' }
      False @=? evalState (isValidTurn $ PutGate $ gateUpperLeft (3,3) H) gs'
  , testCase "makeTurn-move-valid" $ do
      let gs = someGameState
          (succeed, gs') = runState (makeTurn $ Move (4,4)) gs
      True @=? succeed
      let p' = last $ playerList gs'
      color (currP gs) @=? color p'
      (4,4) @=? pos p'
  , testCase "makeTurn-move-invalid" $ do
      let gs = someGameState
          (succeed, gs') = runState (makeTurn $ Move  (3,5)) gs
      False @=? succeed
      color (currP gs) @=? color (currP gs')
      (3,3) @=? pos (currP gs')
  , testCase "makeTurn-putGate-valid" $ do
      let gs = someGameState
          ggs = halfGates gs
          gateToInsert = gateUpperLeft (3,3) V
          (succeed, gs') = runState (makeTurn $ PutGate gateToInsert) gs
      True @=? succeed
      insertGate gateToInsert ggs @=? halfGates gs'
  , testCase "getWinner-nothing" $
      Nothing @=? evalState getWinner someGameState
  , testCase "getWinner-black-won" $ do
      let gs = someGameState
          pl = playerList gs
          pl' = (head pl) { pos = (0,3) } : tail pl
          gs' = gs {playerLoop = concat $ repeat pl'}
      Just (color $ currP gs) @=? evalState getWinner gs'
  ]
