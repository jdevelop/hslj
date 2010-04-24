module LiveJournal.Auth (
    Session(..),
    login
)
where 

import Maybe

import LiveJournal.Transport
import Data.ByteString.Char8 as BStr
import Data.ByteString.Lazy.Char8 as BStrL
import Data.Digest.Pure.MD5
import Prelude as P

data Session = Anonymous | Authenticated { auth_challenge, auth_response :: BStr.ByteString }

instance Show Session where
    show Anonymous = "Anonymous session"
    show (Authenticated auth_challenge auth_response) = BStr.unpack auth_challenge ++ " :: " ++ BStr.unpack auth_response

login :: String -> String -> IO ( Maybe Session )
login username password = 
    prepareChallenge password >>= login'
    where
        login' Nothing = return Nothing
        login' (Just (chal, auth_response)) = do
            response <- runRequest [makePair1 modeStr "login", 
                                    makePair "user" username, 
                                    makePair "auth_method" "challenge", 
                                    makePair2 "auth_challenge" chal, 
                                    makePair2 "auth_response" auth_response]
            return $ findPair "success" response >>= createSession chal auth_response
        createSession chal auth_response status  | status == statusOK = Just (Authenticated chal auth_response)
                                                 | otherwise          = Nothing
            

modeStr = BStr.pack "mode"
statusOK = BStr.pack "OK"

prepareChallenge :: String -> IO (Maybe (BStr.ByteString, BStr.ByteString))
prepareChallenge password = do
    response <- runRequest [Pair modeStr getChallengeStr]
    return . fmap (result) $ findPair "challenge" response
    where
        getChallengeStr = BStr.pack "getchallenge"
        md5Pass = BStrL.pack . show . md5 $ BStrL.pack password -- have no idea how to force MD5Digest to be converted to lazy bytestring
        repack = BStrL.fromChunks . (:[])
        hashcode chal = md5 $ BStrL.concat [chal, md5Pass]
        result chal = (chal, BStr.pack . show . hashcode . repack $ chal )
    

findPair :: String -> [Pair] -> Maybe BStr.ByteString
findPair _ [] = Nothing
findPair pName val = listToMaybe . P.map value . P.filter ( ( == pName') . name ) $ val
    where
        pName' = BStr.pack pName
