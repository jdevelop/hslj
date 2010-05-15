module LiveJournal.Transport(
    Session(..),
    runRequest,
    runRequestSession,
    prepareChallenge
)
where

import Maybe
import Prelude as P
import Network.Curl
import Data.ByteString.UTF8 as BStrU
import Data.ByteString.Lazy.Char8 as BStrL
import Data.Digest.Pure.MD5
import LiveJournal.Error
import LiveJournal.Session
import LiveJournal.Pair

runRequest :: [Pair] -> IO ( [Pair] )
runRequest input = do
    curl <- initialize
    fmap ( parseResponse . extractResponse ) $ do_curl_ curl "http://www.livejournal.com/interface/flat" curlOptions
    where
        curlOptions = joinPairs input:method_POST
        joinPairs = CurlPostFields . P.map joinPair
        joinPair = show  -- may be some sort of escaping?

runRequestSession :: Session -> [Pair] -> IO ( [Pair] )
runRequestSession Anonymous pairs = runRequest pairs
runRequestSession (Authenticated password) pairs = do 
    challenge <- prepareChallenge password
    runRequest' pairs challenge
    where
        runRequest' pairs Nothing = return []
        runRequest' pairs (Just (auth_challenge,auth_response) ) =
            runRequest newPairs
            where 
                newPairs = makePairBSValue "auth_challenge" auth_challenge:
                    makePairBSValue "auth_response" auth_response:
                    makePair "auth_method" "challenge":
                    pairs

prepareChallenge :: String -> IO (Maybe (BStrU.ByteString, BStrU.ByteString))
prepareChallenge password = do
    response <- runRequest [makePair "mode" "getchallenge"]
    return . fmap (result) $ findPair "challenge" response
    where
        md5Pass = BStrL.pack . show . md5 $ BStrL.pack password -- have no idea how to force MD5Digest to be converted to lazy bytestring
        repack = BStrL.fromChunks . (:[])
        hashcode chal = md5 $ BStrL.concat [chal, md5Pass]
        result chal = (chal, BStrU.fromString . show . hashcode . repack $ chal )

extractResponse :: CurlResponse_ [(String,String)] BStrU.ByteString -> BStrU.ByteString
extractResponse = respBody

emptyString :: BStrU.ByteString
emptyString = BStrU.fromString "" 

parseResponse :: BStrU.ByteString -> [Pair]
parseResponse = buildPairs . clearEmpty
    where
        clearEmpty = P.dropWhile ( == emptyString ) . BStrU.lines
        buildPairs (name:value:pairs) = Pair name value:buildPairs pairs
        buildPairs _ = []
