module Quoridor.Helpers
where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M



andP :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andP = liftM2 (&&)

rotateList :: [a] -> [a]
rotateList [] = []
rotateList (x:xs) = xs ++ [x]

unsafeLookup :: Ord k => k -> M.Map k a -> a
unsafeLookup = (fromJust .) . M.lookup
