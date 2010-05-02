module LiveJournal.Common where

import Data.ByteString.Char8 as BStr
import LiveJournal.Error
import LiveJournal.Transport
import Data.Maybe
import Prelude as P

type Result a = Either Error a

type LJResponseHandler a = [Pair] -> Result a

statusOk = BStr.pack "OK"


getErrorMsgFromResponse :: [Pair] -> Error
getErrorMsgFromResponse response = errorState errMsg
    where
        errMsg = findPair "errmsg" response
        errorState Nothing = WrongResponseFormat
        errorState (Just str) = SimpleError (BStr.unpack str)

responseStatus :: [Pair] -> Maybe BStr.ByteString
responseStatus = findPair "success"

makeLJCall :: Session -> [Pair] -> LJResponseHandler a -> IO (Result a)
makeLJCall Anonymous _ _ = return $ Left AuthRequired
makeLJCall session params handler = do
    fmap processResponse $ runRequestSession session params
    where
        processResponse response | responseStatus response == Nothing = Left WrongResponseFormat
                                 | responseStatus response == (Just statusOk) = handler response
                                 | otherwise = Left $ getErrorMsgFromResponse response
