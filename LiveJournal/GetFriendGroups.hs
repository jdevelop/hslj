{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts,
             TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module LiveJournal.GetFriendGroups (
    getFriendGroups
) where

import LiveJournal.Entity
import LiveJournal.Request
import LiveJournal.Transport
import LiveJournal.Error
import LiveJournal.Login (loginObjectUpdater, loginObjectFactory)
import LiveJournal.Session

import Data.Map as DM
import Data.List as DL
import Data.Maybe

friendGroupObjectFactory "frgrp" = loginObjectFactory "frgrp"
friendGroupObjectFactory _ = Nothing

friendGroupObjectUpdater "frgrp" x y z = loginObjectUpdater "frgrp" x y z
friendGroupObjectUpdater _ _ _ obj = Just obj

instance ResponseTransformer ResponseData [ResponseData] where
    transform (simpleMap, enumMap, objectMap) = makeResult $ fromMaybe [] $ do
        groups <- DM.lookup "frgrp" objectMap
        Just $ DM.elems groups


getFriendGroups :: Session -> String -> IOResult [ResponseData]
getFriendGroups session username = 
    runRequestSession session params (CRP friendGroupObjectFactory friendGroupObjectUpdater)
    where
        params = makeRequest [
            ("mode", "getfriendgroups"),
            ("user", username)
            ]
