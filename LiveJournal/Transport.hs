module LiveJournal.Transport(
    Pair(..),
    makePair,
    makePairBSName,
    makePairBSValue,
    runRequest
)
where

import Prelude as P
import Network.Curl
import Data.ByteString.Char8 as BStr

data Pair = Pair { name, value :: ByteString }

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

extractResponse :: CurlResponse_ [(String,String)] ByteString -> ByteString
extractResponse = respBody

parseResponse :: ByteString -> [Pair]
parseResponse = buildPairs . clearEmpty
    where
        clearEmpty = P.dropWhile ( == BStr.empty ) . BStr.lines
        buildPairs (name:value:pairs) = Pair name value:buildPairs pairs
        buildPairs _ = []
