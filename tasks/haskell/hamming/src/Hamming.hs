module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys 
  | null xs || null ys = Just 0
  | length xs /= length ys = Nothing
  | otherwise = Just (sum [1 | pair <- zip xs ys, fst pair /= snd pair])pair])