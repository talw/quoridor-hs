module Quoridor.Helpers
where

import           Control.Monad (liftM2)
import qualified Data.Map      as M
import           Data.Maybe    (fromJust)



andP :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andP = liftM2 (&&)

rotateList :: [a] -> [a]
rotateList [] = []
rotateList (x:xs) = xs ++ [x]

unsafeLookup :: Ord k => k -> M.Map k a -> a
unsafeLookup = (fromJust .) . M.lookup
