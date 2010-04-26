module LiveJournal.Common where

import Data.ByteString.Char8 as BStr
import LiveJournal.Error
import LiveJournal.Transport
import Data.Maybe
import Prelude as P

type Result a = Either a Error

statusOk = BStr.pack "OK"

findPair :: String -> [Pair] -> Maybe BStr.ByteString
findPair _ [] = Nothing
findPair pName val = listToMaybe . P.map value . P.filter ( ( == pName') . name ) $ val
    where
        pName' = BStr.pack pName

getErrorMsgFromResponse :: [Pair] -> Error
getErrorMsgFromResponse response = errorState errMsg
    where
        errMsg = findPair "errmsg" response
        errorState Nothing = WrongResponseFormat
        errorState (Just str) = SimpleError (BStr.unpack str)

responseStatus :: [Pair] -> Maybe BStr.ByteString
responseStatus = findPair "success"

statusOk :: ByteString
statusOK = BStr.pack "OK"
