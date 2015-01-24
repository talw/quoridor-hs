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
unsafeLookup = fromJust .: M.lookup

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = \x y -> f $ g x y

safeAt :: [a] -> Int -> Maybe a
safeAt list targetInd = go list targetInd 0
 where
  go [] _ _ = Nothing
  go (x:xs) i n
    | i == n = Just x
    | otherwise = go xs i (n+1)
