module LiveJournal.Post.EventProps (
    LineEndings(..),
    Security(..),
    Property(..),
    leToStr,
    secToStr,
    strToLE,
    strToSec
) where

import Data.Array
import Data.Maybe

data LineEndings = UNIX | PC | MAC deriving (Enum,Bounded,Eq,Ord,Ix)
leMapping :: Array LineEndings String
leMapping = listArray (minBound,maxBound) ["unix","pc","mac"]

leToStr :: LineEndings -> String
leToStr = (leMapping !)

strToLE :: String -> Maybe LineEndings
strToLE name = listToMaybe $ foldr (findPair name) [] pairs
    where
        pairs = assocs leMapping

data Security = PUBLIC  | PRIVATE | USEMASK deriving (Enum,Bounded,Eq,Ord,Ix)
secMapping :: Array Security String
secMapping = listArray (minBound,maxBound) ["public","private","usemask"]

secToStr :: Security -> String
secToStr = (secMapping !)

strToSec :: String -> Maybe Security
strToSec name = listToMaybe $ foldr (findPair name) [] pairs
    where
        pairs = assocs secMapping

findPair :: String -> (a, String) -> [a] -> [a]
findPair name (idx,value) acc | name == value = [idx]
                         | otherwise = acc

data Property = Property { name, value :: String }
