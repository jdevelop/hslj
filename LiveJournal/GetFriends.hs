{-# LANGUAGE FlexibleContexts,TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module LiveJournal.GetFriends where

import LiveJournal.Request
import LiveJournal.Error
import LiveJournal.Session
import LiveJournal.Transport
import LiveJournal.Entity
import LiveJournal.ResponseParser
import LiveJournal.Login ( loginObjectFactory, loginObjectUpdater )
import Data.Int
import Data.DateTime
import Data.Map as DM
import Data.Maybe
import Control.Monad
import Data.List as DL

typeMap = DM.fromList [
    ("community",Community),
    ("syndicated",Syndicated),
    ("news",News),
    ("shared",Shared),
    ("identity",Identity)
    ]

statusMap = DM.fromList [
    ("deleted",Deleted),
    ("suspended",Suspended),
    ("purged",Purged)
    ]

data LJFriendsResponse = FriendsResponse {
    ljFriends :: [ResponseData],
    ljFriendsOf :: [ResponseData],
    ljGroups :: [ResponseData]
} deriving (Show)

friendObjectFactory :: ObjectFactory ResponseData
friendObjectFactory "friend" = Just $ Friend AFriend "" "" "" "" 0 User Nothing Nothing Nothing Active Nothing
friendObjectFactory "friendof" = Just $ Friend AFriendOf "" "" "" "" 0 User Nothing Nothing Nothing Active Nothing
friendObjectFactory "frgrp" = loginObjectFactory "frgrp"
friendObjectFactory _ = Nothing

friendObjectUpdater :: ObjectUpdater ResponseData
friendObjectUpdater "friend" "name" value obj = Just $ obj { friendName = value }
friendObjectUpdater "friend" "user" value obj = Just $ obj { friendUsername = value }
friendObjectUpdater "friend" "bg" value obj = Just $ obj { bgColor = value }
friendObjectUpdater "friend" "fg" value obj = Just $ obj { fgColor = value }
friendObjectUpdater "friend" "groupmask" value obj = Just $ obj { groupmask = read value }
friendObjectUpdater "friend" "type" value obj = Just $ obj { friendType = DM.findWithDefault User value typeMap }
friendObjectUpdater "friend" "identity_display" value obj = Just $ obj { identityDisplay = Just value }
friendObjectUpdater "friend" "identity_type" value obj = Just $ obj { identityType = Just value }
friendObjectUpdater "friend" "identity_value" value obj = Just $ obj { identityValue = Just value }
friendObjectUpdater "friend" "status" value obj = Just $ obj { friendStatus = DM.findWithDefault Active value statusMap }
friendObjectUpdater "friend" "birthday" value obj = Just $ obj { birthday = parseDateTime "%F" value }
friendObjectUpdater "friendof" x y z = friendObjectUpdater "friend" x y z
friendObjectUpdater "frgrp" x y z = loginObjectUpdater "frgrp" x y z

instance ResponseTransformer ResponseData LJFriendsResponse where
    transform (simpleMap, enumMap, objectMap) = makeResult $ FriendsResponse friends friendsOf groups
        where
            friends = DM.elems $ DM.findWithDefault DM.empty "friend" objectMap
            friendsOf = DM.elems $ DM.findWithDefault DM.empty "friendof" objectMap
            groups = DM.elems $ DM.findWithDefault DM.empty "frgrp" objectMap

getFriends :: Session -> String -> Bool -> Bool -> Bool -> Maybe Int -> IOResult LJFriendsResponse
getFriends session username incFriendOf incGroups incBdays limit = 
    runRequestSession session request (CRP friendObjectFactory friendObjectUpdater)
    where 
        request = Request . makeRequestParams $ [
            ("mode", "getfriends"),
            ("user", username) ]
            `mplus` makeBoolArr incBdays ("includebdays","1")
            `mplus` makeBoolArr incGroups ("includegroups","1")
            `mplus` makeBoolArr incFriendOf ("includefriendof","1")
        makeBoolArr True item = [item]
        makeBoolArr _ _ = []
