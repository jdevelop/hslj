module LiveJournal.Transport(
    Pair(..),
    Session(..),
    makePair,
    makePairBSName,
    makePairBSValue,
    runRequest,
    runRequestSession,
    findPair
)
where

import Maybe
import Prelude as P
import Network.Curl
import Data.ByteString.Char8 as BStr

data Pair = Pair { name, value :: ByteString }

data Session = Anonymous | Authenticated { auth_challenge, auth_response :: BStr.ByteString }

makePair :: String -> String -> Pair
makePair strName strValue = Pair strName' strValue'
    where
        strName' = BStr.pack strName
        strValue' = BStr.pack strValue

makePairBSName :: ByteString -> String -> Pair
makePairBSName strName strValue = Pair strName strValue'
    where
        strValue' = BStr.pack strValue

makePairBSValue :: String -> ByteString -> Pair
makePairBSValue strName strValue = Pair strName' strValue
    where
        strName' = BStr.pack strName

instance Show Pair where
    show (Pair name' value') = BStr.unpack name' ++ "=" ++ BStr.unpack value'

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
runRequestSession (Authenticated auth_challenge auth_response) pairs = runRequest newPairs
    where
        newPairs = makePairBSValue "auth_challenge" auth_challenge:
            makePairBSValue "auth_response" auth_response:
            makePair "auth_method" "challenge":
            pairs

extractResponse :: CurlResponse_ [(String,String)] ByteString -> ByteString
extractResponse = respBody

parseResponse :: ByteString -> [Pair]
parseResponse = buildPairs . clearEmpty
    where
        clearEmpty = P.dropWhile ( == BStr.empty ) . BStr.lines
        buildPairs (name:value:pairs) = Pair name value:buildPairs pairs
        buildPairs _ = []

findPair :: String -> [Pair] -> Maybe BStr.ByteString
findPair _ [] = Nothing
findPair pName val = listToMaybe . P.map value . P.filter ( ( == pName') . name ) $ val
    where
        pName' = BStr.pack pName
