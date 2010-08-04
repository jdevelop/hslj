{-# LANGUAGE FlexibleContexts,TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module LiveJournal.CheckFriends ( 
    checkFriendStatus,
    LJFriendsStatus(..)
)
where

import LiveJournal.Request
import LiveJournal.Error
import LiveJournal.Session
import LiveJournal.Transport
import LiveJournal.Entity
import LiveJournal.ResponseParser
import Data.Int
import Data.Map as DM
import Data.Maybe
import Control.Monad
import Data.List as DL


data LJFriendsStatus = FriendsStatus {
                            ljLastUpdate :: String,
                            ljHasNewEntries :: Bool,
                            ljDesiredDelaySec :: Int
                        } deriving (Show)

emptyCreate :: ObjectFactory ()
emptyCreate _ = Nothing

emptyUpdate :: ObjectUpdater ()
emptyUpdate _ _ _ _ = Nothing

checkFriendStatus :: Session -> String -> String -> Maybe Int32 -> IOResult LJFriendsStatus
checkFriendStatus session pUsername pLastUpdate pMask = 
    runRequestSession session request (CRP emptyCreate emptyUpdate) :: IOResult LJFriendsStatus
    where
        params = [ ("mode","checkfriends"),
                   ("user", pUsername),
                   ("lastupdate", pLastUpdate) ] `mplus`
                   maybe mzero ( (:[]) . (,) "mask" . show ) pMask
        request = makeRequest params


instance ResponseTransformer () LJFriendsStatus where
    transform (simpleMap, enumMap, objectMap) = makeResult $ FriendsStatus lastUpdate hasNewEntries desiredDelay
        where
            lastUpdate = maybe "" id $ DM.lookup "lastupdate" simpleMap
            hasNewEntries = maybe False ( "1" == ) $ DM.lookup "new" simpleMap
            desiredDelay = maybe 0 read $ DM.lookup "interval" simpleMap
