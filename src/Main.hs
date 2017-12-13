{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe

import Grammar


reverseTaps :: DaPhone -> Char -> [(Digit, Press)]
reverseTaps (DaPhone daPhone) c
  | isUpper c = mapMaybe convert ['^', toLower c]
  | otherwise = mapMaybe convert [c]
  where convert :: Char -> Maybe (Digit, Press)
        convert x                   = daCharToTuple <$> find (isWantedSymbol x) daPhone 
        daCharToTuple DaChar{..}    = (button, presses)
        isWantedSymbol :: Char -> DaChar -> Bool
        isWantedSymbol x DaChar{..} = x == symbol

cellPhonesDead :: DaPhone -> String -> [(Digit, Press)]
cellPhonesDead daPhone = concatMap (reverseTaps daPhone)

fingerTaps :: [(Digit, Press)] -> Press
fingerTaps = foldl' (\b a -> b + snd a) 0

-- statistics

mostPopular :: (Ord a) => [a] -> a
mostPopular = head 
            . maximumBy (compare `on` length)
            . group 
            . sort

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopular . filter (/= ' ')

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopular . words . concat

main :: IO ()
main = do
  inputLine <- getLine
  let encoded = cellPhonesDead daPhone inputLine
  let mpletter = mostPopularLetter inputLine
  let mpword = coolestWord [inputLine]
  print ("Encoded: " ++ show encoded)
  print ("Most popular letter is: " ++ show mpletter)
  print ("Most pupular word is: " ++ mpword)

