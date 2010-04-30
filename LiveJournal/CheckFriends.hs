module LiveJournal.CheckFriends(
    LastUpdate(..),
    lastUpdate
)
where

import LiveJournal.Common
import LiveJournal.Transport
import LiveJournal.Error
import LiveJournal.Auth
import Data.ByteString.Char8 as BStr

data LastUpdate = LastUpdate { lastUpdateStr :: String, hasNew, interval :: Int }

lastUpdate :: Session -> String -> String -> Int -> IO (Result LastUpdate)
lastUpdate Anonymous _ _ _ = return $ Left AuthRequired
lastUpdate session username lastupdate mask = do
    makeLJCall session [makePair "mode" "checkfriends",
                        makePair "user" username,
                        makePair "lastupdate" lastupdate,
                        makePair "mask" (show mask)] createLastUpdate
    where
        createLastUpdate  = hasLastUpdate . newLastUpdate
        newLastUpdate response = do
            lastUpdateStr <- findPair "lastupdate" response
            new <- findPair "new" response
            interval <- findPair "interval" response
            return $ LastUpdate (BStr.unpack lastUpdateStr) (readBSInt new) (readBSInt interval)
        hasLastUpdate Nothing = Left WrongResponseFormat
        hasLastUpdate (Just lastUpdate) = Right lastUpdate
        readBSInt = read . BStr.unpack

