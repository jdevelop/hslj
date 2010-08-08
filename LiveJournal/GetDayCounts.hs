{-# LANGUAGE FlexibleContexts,TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module LiveJournal.GetDayCounts where

import Data.DateTime
import Data.Map
import Data.Maybe
import LiveJournal.Transport
import LiveJournal.Error
import LiveJournal.Entity
import LiveJournal.Session
import LiveJournal.Request

data LJDailyPost = DailyPost {
    ljPostDate :: DateTime,
    posts :: Int } deriving (Show)

instance ResponseTransformer () [LJDailyPost] where
    transform (simpleMap, _, _) = makeResult $ foldWithKey folder [] simpleMap
        where
            folder key value arr = maybe arr (:arr) $ flip DailyPost ( read value ) `fmap` parseDateTime "%F" key

getDayCounts :: Session -> String -> IOResult [LJDailyPost]
getDayCounts session username = 
    runRequestSession session request (CRP noFactory noUpdater)
    where
        request = Request $ makeRequestParams params
        params = [
            ("mode","getdaycounts"),
            ("user",username)
            ]
        noFactory _ = Nothing :: Maybe ()
        noUpdater _ _ _ _ = Nothing :: Maybe ()
