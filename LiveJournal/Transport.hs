module LiveJournal.Transport where

import Network.Curl

data Pair = Pair { name, value :: String }

instance Show Pair where
    show (Pair name' value') = name' ++ "=" ++ value'

runRequest :: [Pair] -> IO ( [Pair] )
runRequest input = do
    curl <- initialize
    fmap ( parseResponse . extractResponse ) $ do_curl_ curl "http://www.livejournal.com/interface/flat" curlOptions
    where
        curlOptions = joinPairs input:method_POST
        joinPairs = CurlPostFields . map joinPair
        joinPair = show  -- may be some sort of escaping?

extractResponse :: CurlResponse_ [(String,String)] String -> String
extractResponse = respBody

parseResponse :: String -> [Pair]
parseResponse = buildPairs . clearEmpty
    where
        clearEmpty = dropWhile ( == "" ) . lines
        buildPairs (name:value:pairs) = Pair name value:buildPairs pairs
        buildPairs _ = []
