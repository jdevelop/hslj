{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts #-}
module LiveJournal.Transport where

import Control.Applicative
import Data.Maybe as DM
import Data.Map as DMP
import Data.List as DL
import Network.Curl as CU
import Data.ByteString.UTF8 as BStrU
import Data.ByteString.Char8 as BStr
import Data.Digest.OpenSSL.MD5 as MD5

import LiveJournal.Session as LJS
import LiveJournal.Request as LJR
import LiveJournal.ResponseParser as LJRP
import LiveJournal.Error

data CustomResponseParser a b = CRP { 
    customObjectFactory :: ObjectFactory b,
    customObjectDAO :: ObjectUpdater b
    }

extractResponse :: CurlResponse_ [(String,String)] BStrU.ByteString -> BStrU.ByteString
extractResponse = respBody

runRequest :: LJRequest -> CustomResponseParser String b -> IO ( Either LJError (ParseResult String b) )
runRequest request responseParser = do
    curl <- CU.initialize
    parseResponse _customObjectFactory _customObjectDAO .  extractResponse <$> 
        CU.do_curl_ curl "http://www.livejournal.com/interface/flat" curlOptions
    where
        _customObjectFactory = customObjectFactory responseParser
        _customObjectDAO = customObjectDAO responseParser
        curlOptions = makeRequest request : CU.method_POST
        makeRequest = CurlPostFields . DL.map makeParamNV . params
        makeParamNV (RequestParam name value) = name ++ "=" ++ value

runRequestSession :: Session -> LJRequest -> CustomResponseParser String b -> IO ( Either LJError (ParseResult String b) )
runRequestSession Anonymous request responseParser = runRequest request responseParser
runRequestSession (Authenticated password) request responseParser =
    prepareChallenge password >>= DM.maybe emptyParser runRequest'
    where
        emptyParser = return $ Left WrongResponseFormat
        runRequest' ( auth_challenge, auth_response ) =
            runRequest newRequest responseParser
            where 
                newRequest = Request $ LJR.makeRequestParams [
                    ("auth_challenge", auth_challenge),
                    ("auth_response", auth_response),
                    ("auth_method","challenge")] ++ LJR.params request

prepareChallenge :: String -> IO (Maybe (String, String))
prepareChallenge password = do
    getChallenge <$> runRequest request (CRP noFactory noUpdate)
    where
        request = makeRequest [("mode","getchallenge")]
        result chal = ( chal, hashcode chal )
        md5Pass = MD5.md5sum $ BStr.pack password
        hashcode chal = MD5.md5sum . BStr.pack $ chal ++ md5Pass
        getChallenge (Left _) = Nothing
        getChallenge (Right (simpleMap, _, _)) = do 
            chlg <- DMP.lookup "challenge" simpleMap
            return $ result chlg
        noFactory = \_ -> Nothing
        noUpdate = \_ _ _ -> Nothing

