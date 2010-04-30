module LiveJournal.Auth (
    login
)
where 

import LiveJournal.Common
import LiveJournal.Error
import LiveJournal.Transport
import Data.ByteString.Char8 as BStr
import Data.ByteString.Lazy.Char8 as BStrL
import Data.Digest.Pure.MD5
import Prelude as P

login :: String -> String -> IO ( Result Session )
login username password = do
    response <- prepareChallenge password
    login' response
    where
        login' Nothing = return $ Left NoChallenge
        login' (Just (chal, auth_response)) = do
            response <- runRequest [makePair "mode" "login", 
                                    makePair "user" username, 
                                    makePair "auth_method" "challenge", 
                                    makePairBSValue "auth_challenge" chal, 
                                    makePairBSValue "auth_response" auth_response]
            return $ createSession chal auth_response ( responseStatus response ) response
        createSession _ _ Nothing _ = Left WrongResponseFormat
        createSession chal auth_response (Just status) response | status == statusOk = Right (Authenticated chal auth_response)
                                                                | otherwise          = Left $ getErrorMsgFromResponse response
            
prepareChallenge :: String -> IO (Maybe (BStr.ByteString, BStr.ByteString))
prepareChallenge password = do
    response <- runRequest [makePair "mode" "getchallenge"]
    return . fmap (result) $ findPair "challenge" response
    where
        md5Pass = BStrL.pack . show . md5 $ BStrL.pack password -- have no idea how to force MD5Digest to be converted to lazy bytestring
        repack = BStrL.fromChunks . (:[])
        hashcode chal = md5 $ BStrL.concat [chal, md5Pass]
        result chal = (chal, BStr.pack . show . hashcode . repack $ chal )
    
