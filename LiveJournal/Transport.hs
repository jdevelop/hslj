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
import Data.ByteString.Char8 as BStr
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

prepareChallenge :: String -> IO (Maybe (BStr.ByteString, BStr.ByteString))
prepareChallenge password = do
    response <- runRequest [makePair "mode" "getchallenge"]
    return . fmap (result) $ findPair "challenge" response
    where
        md5Pass = BStrL.pack . show . md5 $ BStrL.pack password -- have no idea how to force MD5Digest to be converted to lazy bytestring
        repack = BStrL.fromChunks . (:[])
        hashcode chal = md5 $ BStrL.concat [chal, md5Pass]
        result chal = (chal, BStr.pack . show . hashcode . repack $ chal )

extractResponse :: CurlResponse_ [(String,String)] BStr.ByteString -> BStr.ByteString
extractResponse = respBody

parseResponse :: BStr.ByteString -> [Pair]
parseResponse = buildPairs . clearEmpty
    where
        clearEmpty = P.dropWhile ( == BStr.empty ) . BStr.lines
        buildPairs (name:value:pairs) = Pair name value:buildPairs pairs
        buildPairs _ = []
