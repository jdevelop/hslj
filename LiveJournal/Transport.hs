module LiveJournal.Transport where

import Control.Applicative
import Data.Maybe as DM
import Data.List as DL
import Network.Curl as CU
import Data.ByteString.UTF8 as BStrU
import Data.ByteString.Char8 as BStr
import Data.Digest.OpenSSL.MD5 as MD5

import LiveJournal.Session as LJS
import LiveJournal.Request as LJR
import LiveJournal.ResponseParser as LJRP

extractResponse :: CurlResponse_ [(String,String)] BStrU.ByteString -> BStrU.ByteString
extractResponse = respBody

runRequest :: LJRequest -> ResponseParser a -> IO a
runRequest request responseParser = do
    curl <- CU.initialize
    parser responseParser . Just . extractResponse <$> 
        CU.do_curl_ curl "http://www.livejournal.com/interface/flat" curlOptions
    where
        curlOptions = makeRequest request : CU.method_POST
        makeRequest = CurlPostFields . DL.map makeParamNV . params
        makeParamNV (RequestParam name value) = name ++ "=" ++ value

runRequestSession :: Session -> LJRequest -> ResponseParser a -> IO a
runRequestSession Anonymous request responseParser = runRequest request responseParser
runRequestSession (Authenticated password) request responseParser =
    prepareChallenge password >>= DM.maybe emptyParser runRequest'
    where
        emptyParser = return $ parser responseParser Nothing
        runRequest' ( auth_challenge, auth_response ) =
            runRequest newRequest responseParser
            where 
                newRequest = Request $ LJR.makeRequestParams [
                    ("auth_challenge", auth_challenge),
                    ("auth_response", auth_response),
                    ("auth_method","challenge")] ++ LJR.params request

prepareChallenge :: String -> IO (Maybe (String, String))
prepareChallenge password = 
    fmap result <$> runRequest request ( LJRP.findParameter "challenge" )
    where
        request = makeRequest [("mode","getchallenge")]
        result chal = ( chal, hashcode chal )
        md5Pass = MD5.md5sum $ BStr.pack password
        hashcode chal = MD5.md5sum . BStr.pack $ chal ++ md5Pass

