module LiveJournal.Common where

import Data.ByteString.UTF8 as BStr
import LiveJournal.Pair
import LiveJournal.Error
import LiveJournal.Session
import LiveJournal.Transport
import Data.Maybe

type Result a = Either LJError a

type LJResponseHandler a = [Pair] -> Result a

statusOk = BStr.fromString "OK"

getErrorMsgFromResponse :: [Pair] -> LJError
getErrorMsgFromResponse response = errorState errMsg
    where
        errMsg = findPair "errmsg" response
        errorState Nothing = WrongResponseFormat
        errorState (Just str) = SimpleError (BStr.toString str)

responseStatus :: [Pair] -> Maybe BStr.ByteString
responseStatus = findPair "success"

makeLJCall :: Session -> [Pair] -> LJResponseHandler a -> IO (Result a)
makeLJCall Anonymous _ _ = return $ Left AuthRequired
makeLJCall session params handler =
    fmap processResponse $ runRequestSession session params'
    where
        params' = makePair "ver" "1" : params
        processResponse response | responseStatus response == Nothing = Left WrongResponseFormat
                                 | responseStatus response == Just statusOk = handler response
                                 | otherwise = Left $ getErrorMsgFromResponse response
