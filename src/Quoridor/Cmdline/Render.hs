module Quoridor.Cmdline.Render
  ( runRender
  , runRenderColor
  , putColoredBoardTerm
  , putColoredStrJson
  , putColoredBoardHtml
  , putChatMessageJson
  ) where

import           Control.Monad.Reader      (ReaderT, reader, runReaderT)
import           Control.Monad.State       (StateT, get, gets, modify,
                                            runStateT)
import           Control.Monad.Writer      (Writer, runWriter, tell, void)
import           Data.List                 (partition, sort, sortBy)
import qualified Data.Set                  as S (toAscList)
import           Text.Printf               (printf)

import qualified Data.DList                as D
import qualified System.Console.ANSI       as CA

import           Quoridor
import           Quoridor.Cmdline.Messages (msgInputInstr, validMovesChars)



-- | Monad stack used for rendering the board.
-- ReaderT with game configuration
-- Writer for accumulating the String which represents the board
-- StateT for a state of sorted lists o what is left to
-- render on the board (e.g. players, gates).
type Render = ReaderT GameConfig
                (StateT RenderState
                (Writer (D.DList Char)))

-- | Sorted lists of what is left to render on the board.
-- The rendering of the board is linear, and at every tile
-- or in between tiles, whether something should be rendered there
-- or not is checked. To avoid going over all the lists every turn,
-- only the head is checked (but for that to be correct the lists
-- must be sorted).
data RenderState = RenderState
  { players             :: [Player]
  , vertHalfGates       :: [HalfGate]
  , horizHalfGates      :: [HalfGate]
  , validMoves          :: [Cell]
  , leftValidMovesChars :: String
  }


--- exported functions

-- | Returns a String of the game board along with some basic info
runRender :: GameState -> GameConfig -> ValidMoves -> String
runRender gs gc vms = D.toList w
  where (_,w) =
          runWriter (runStateT (runReaderT (render cp) gc) initialRenderState)
        initialRenderState = RenderState
          psSorted vhgs hhgs vmSorted validMovesChars
        psSorted = sortPlayers $ playerList gs
        vmSorted = sort vms
        (hhgs, vhgs) = partitionHalfGates $ S.toAscList $ halfGates gs
        cp = currP gs

-- | Returns a String of the game board along with some basic info,
-- and a series of IO () actions, one per character, which describe how
-- to set the terminal color. putColoredStr can be used to apply
-- those actions automatically
runRenderColor :: GameState -> GameConfig -> ValidMoves -> (String, [CA.Color])
runRenderColor x y z = addColor $ runRender x y z

-- | Given an input such as the output of runRenderColor, writes the
-- game board along with some basic info, to the screen, applying
-- the IO actions to colorize the output.
putColoredBoardTerm :: (String, [CA.Color]) -> IO ()
putColoredBoardTerm (str, colors) = mapM_ putColoredChar $ zip str colors
 where putColoredChar (ch, col) = colorToAction col >> putChar ch
       colorToAction col =
         CA.setSGR [CA.SetColor CA.Foreground CA.Vivid col]


-- | This is wasteful compared to having this logic in the browser's javascript.
-- However this is still amounts to very little data being transferred, and that way
-- I can avoid duplicating the coloring logic"
putColoredBoardHtml :: (String, [CA.Color]) -> IO ()
putColoredBoardHtml (str, colors) = putStr $ concatMap addColorProp $ zip str colors
  where
    addColorProp (ch, CA.White) = [ch]
    addColorProp (ch, col) = printf "<font class=\"%s\">%c</font>" (show col) ch

putColoredStrJson :: String -> String -> IO ()
putColoredStrJson msgType text = putStrLn $ printf "{ \
  \ \"msgType\": \"%s\", \
  \ \"text\": \"%s\" \
  \ }" msgType text

putChatMessageJson :: Color -> String -> IO ()
putChatMessageJson col msg = putColoredStrJson "chat" inner
 where inner = printf "%s : %s" (show col) msg :: String

--- helper functions

render ::  Player -> Render ()
render cp = do
  renderBoard
  tellLine msgInputInstr
  tellLine $ "It's " ++ show (color cp) ++ "'s Turn."
        ++ " " ++ show (gatesLeft cp) ++ " gates left."
  tellNewLine

tellStr :: String -> Render ()
tellStr str = tell $ D.fromList str

tellLine :: String -> Render ()
tellLine str = tellStr str >> tellNewLine

tellNewLine :: Render ()
tellNewLine = tellStr "\n"

-- | Actually specifies how to render the board.
-- Using 'renderTileRow' and 'renderBetweenRow'
renderBoard :: Render ()
renderBoard = do
  bs <- reader boardSize
  let go y
        | y == bs = return ()
        | otherwise = do
            let lineRuler = show y ++ tail linePadding
            tellStr lineRuler >> renderTileRow y
            tellStr linePadding >> renderBetweenRow y
            go $ y+1
      tellRulerLine = tellLine $
        linePadding ++ unwords (map show [0..bs-1])
  tellRulerLine
  tellNewLine
  go 0
  tellRulerLine
  tellNewLine

-- | Rendering of a tile row
-- (i.e. a row with players and/or vertical gates potentially on it)
renderTileRow :: Int -> Render ()
renderTileRow row = do
  bs <- reader boardSize
  let go y x
        | x == bs = void $ tellStr "\n"
        | otherwise = do
            RenderState ps vhgs _ vms vmcs <- get
            let (cp, ps') = getCharAndList
                  ps ((==) (y,x) . pos) noP (colorLetter $ color $ head ps)
                (cg, vhgs') = getCharAndList vhgs (== ((y,x),(y,x+1))) noG vgc
                (cvm, vms') = getCharAndList vms (== (y,x)) noP (head vmcs)
                vmcs' = if cvm /= noP then tail vmcs else vmcs
                cTile | cp /= noP = cp
                      | cvm /= noP = cvm
                      | otherwise = noP
            modify $ \s -> s { players = ps'
                             , vertHalfGates = vhgs'
                             , validMoves = vms'
                             , leftValidMovesChars = vmcs'
                             }
            tellStr [cTile,cg]
            go y (x+1)
  go row 0

-- | Rendering of a between row
-- (i.e. a row with horizontal gates potentially on it)
renderBetweenRow :: Int -> Render ()
renderBetweenRow row = do
  bs <- reader boardSize
  let go y x
        | x == bs = void $ tellStr "\n"
        | otherwise = do
            hhgs <- gets horizHalfGates
            let (c, hhgs') = getCharAndList hhgs (== ((y,x),(y+1,x))) noG hgc
            modify $ \s -> s { horizHalfGates = hhgs' }
            tellStr (c:" ")
            go y $ x+1
  go row 0

-- | This function, checks if the first item in the sorted list
-- (of players/gates/valid moves) satisfies a predicate.
-- The predicate is basically, whether or not that item should be printed
-- in the current position of the board render.
-- It returns the according character that should be printed in that
-- potential tile, and returns the tail of the list if there's a match.
getCharAndList :: [a] -> (a -> Bool) -> Char -> Char -> (Char, [a])
getCharAndList [] _ cFalse _ = (cFalse, [])
getCharAndList (x:xs) predicate cFalse cTrue
  | predicate x = (cTrue, xs)
  | otherwise = (cFalse, x:xs)

-- | Partition 'HalfGate's into horizontal and vertical.
partitionHalfGates :: [HalfGate] -> ([HalfGate],[HalfGate])
partitionHalfGates = partition $ \((_,x),(_,x')) -> x == x'

sortPlayers :: [Player] -> [Player]
sortPlayers = sortBy func
  where func p1 p2
          | pos p1 < pos p2 = LT
          | pos p1 > pos p2 = GT
          | otherwise = EQ

colorLetter :: Color -> Char
colorLetter = head . show

-- | Given a board render, attaches a color per character
addColor :: String -> (String, [CA.Color])
addColor str = (str, map addColorChar str)
  where
    addColorChar ch | ch == noP = CA.Yellow
                    | ch == hgc || ch == vgc = CA.Magenta
                    | ch `elem` validMovesChars = CA.Cyan
                    | ch == 'W' = CA.White
                    | ch == 'B' = CA.Blue
                    | ch == 'R' = CA.Red
                    | ch == 'G' = CA.Green
                    | otherwise = CA.White



-- constants

noP, noG, hgc, vgc :: Char
noP = 'E'
noG = ' '
hgc = '-'
vgc = '|'

linePadding :: String
linePadding = replicate 2 ' '
