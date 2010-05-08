module LiveJournal.Pair where

import Data.ByteString.Char8 as BStr
import Data.Maybe
import Prelude as P

data Pair = Pair { name, value :: BStr.ByteString }

makePair :: String -> String -> Pair
makePair strName strValue = Pair strName' strValue'
    where
        strName' = BStr.pack strName
        strValue' = BStr.pack strValue

makePairBSName :: BStr.ByteString -> String -> Pair
makePairBSName strName strValue = Pair strName strValue'
    where
        strValue' = BStr.pack strValue

makePairBSValue :: String -> BStr.ByteString -> Pair
makePairBSValue strName strValue = Pair strName' strValue
    where
        strName' = BStr.pack strName

findPair :: String -> [Pair] -> Maybe BStr.ByteString
findPair pName = listToMaybe . P.map value . P.filter ( ( == pName') . name )
    where
        pName' = BStr.pack pName

instance Show Pair where
    show (Pair name' value') = BStr.unpack name' ++ "=" ++ BStr.unpack value'
