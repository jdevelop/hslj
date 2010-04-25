module LiveJournal.CheckFriends(
    LastUpdate(..),
    lastUpdate
)
where

import LiveJournal.Transport
import LiveJournal.Auth
import Data.ByteString.Char8 as BStr

data LastUpdate = LastUpdate { lastUpdateStr :: String, hasNew, interval :: Int }

lastUpdate :: Session -> String -> String -> Int -> IO (Maybe LastUpdate)
lastUpdate Anonymous _ _ _ = return Nothing
lastUpdate session username lastupdate mask = do
    response <- runRequestSession session [ 
                                makePair "mode" "checkfriends",
                                makePair "user" username,
                                makePair "lastupdate" lastupdate,
                                makePair "mask" (show mask)]
    return $ findPair "success" response >>= lastUpdateSuccess response
    where
        lastUpdateSuccess response state | state == BStr.pack "OK" = createLastUpdate response
                                         | otherwise = Nothing
        createLastUpdate response = do
            lastUpdateStr <- findPair "lastupdate" response
            new <- findPair "new" response
            interval <- findPair "interval" response
            return $ LastUpdate (BStr.unpack lastUpdateStr) (readBSInt new) (readBSInt interval)
        readBSInt = read . BStr.unpack
