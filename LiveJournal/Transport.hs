{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts,
             TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module LiveJournal.Transport (
    runRequest,
    runRequestSession,
    prepareChallenge,
    CustomResponseParser (CRP),
    ResponseTransformer(transform)
) where

import Control.Applicative
import Data.Maybe as DM
import Data.Map as DMP
import Data.List as DL
import Network.Curl as CU
import Data.ByteString.UTF8 as BStrU
import Data.ByteString.Char8 as BStr
import Data.Digest.OpenSSL.MD5 as MD5

import LiveJournal.Entity
import LiveJournal.Session as LJS
import LiveJournal.Request as LJR
import LiveJournal.ResponseParser as LJRP
import LiveJournal.Error

data CustomResponseParser a b = CRP { 
    customObjectFactory :: ObjectFactory b,
    customObjectDAO :: ObjectUpdater b
    }

class ResponseTransformer a b where
    transform :: ParseResult String a -> Result b

extractResponse :: CurlResponse_ [(String,String)] BStrU.ByteString -> BStrU.ByteString
extractResponse = respBody

applyResultP :: (ResponseTransformer a b) => Result (ParseResult String a) -> Result b
applyResultP = applyResultP' . getLJResult
    where
        applyResultP' (Left err) = makeError err
        applyResultP' (Right s) = checkResultState s ( transform )
        checkResultState s@(simpleMap,_,_) f = maybe ( f s ) makeErrorStr $ DMP.lookup "errmsg" simpleMap

runRequest :: (ResponseTransformer a b) => LJRequest -> CustomResponseParser String a -> IO ( Result b )
runRequest request responseParser = do
    curl <- CU.initialize
    applyResultP <$> parseResponse _customObjectFactory _customObjectDAO .  extractResponse <$> 
        CU.do_curl_ curl "http://www.livejournal.com/interface/flat" curlOptions
    where
        _customObjectFactory = customObjectFactory responseParser
        _customObjectDAO = customObjectDAO responseParser
        curlOptions = makeRequest request : CU.method_POST
        makeRequest = CurlPostFields . DL.map makeParamNV . params
        makeParamNV (RequestParam name value) = name ++ "=" ++ value

runRequestSession :: (ResponseTransformer a b) => Session -> LJRequest -> CustomResponseParser String a -> IO ( Result b )
runRequestSession Anonymous request responseParser = runRequest request responseParser
runRequestSession (Authenticated password) request responseParser =
    prepareChallenge password >>= DM.maybe emptyParser runRequest'
    where
        emptyParser = return . makeError $ WrongResponseFormat "no challenge"
        runRequest' ( auth_challenge, auth_response ) =
            runRequest newRequest responseParser
            where 
                newRequest = Request $ LJR.makeRequestParams [
                    ("auth_challenge", auth_challenge),
                    ("auth_response", auth_response),
                    ("auth_method","challenge")] ++ LJR.params request

newtype ChalString a = ChalString { getChStr :: a }

instance ResponseTransformer ( ChalString String ) (Maybe String) where
    transform (simpleMap, _, _) = makeResult $ DMP.lookup "challenge" simpleMap

prepareChallenge :: String -> IO (Maybe (String, String))
prepareChallenge password = do
    handleResponse <$> (runRequest request (CRP noFactory noUpdate) :: IO (Result (Maybe String)))
    where
        handleResponse src = handleResponse' $ getLJResult src
        handleResponse' (Left err) = Nothing
        handleResponse' (Right res) = makeChallengePair <$> res
        request = makeRequest [("mode","getchallenge")]
        noFactory :: String -> Maybe ( ChalString String )
        noFactory = \_ -> Nothing
        noUpdate = \_ _ _ _ -> Nothing
        makeChallengePair chal = ( chal, hashcode chal )
        md5Pass = MD5.md5sum $ BStr.pack password
        hashcode chal = MD5.md5sum . BStr.pack $ chal ++ md5Pass

