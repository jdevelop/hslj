{-# LANGUAGE FlexibleContexts,TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module LiveJournal.GetEvents where

import LiveJournal.Transport
import LiveJournal.ResponseParser
import LiveJournal.Session
import LiveJournal.Entity
import LiveJournal.Error
import LiveJournal.Request

import Data.DateTime
import Data.Maybe
import Data.Map as DM
import Data.Array as DA
import Data.Word

import Control.Monad

import Codec.Binary.Url as U
import Codec.Binary.UTF8.String as SU

data SelectType = Day { year, month, day :: Int } |
                  Sync { lastSync :: String } |
                  LastN { howMany :: Int, 
                          beforeDate :: Maybe String } |
                  One { itemId :: String }

data LJLineEndings = Unix | PC | Mac | Space | Dots deriving ( Show, Eq, Ord, Enum, Bounded, Ix )
lineEndingMapping = listArray ( minBound, maxBound ) ["unix", "pc", "mac", "space", "dots"] :: Array LJLineEndings String

data LJEventSecurity = Public | Private | UserMask { userMask :: Int } deriving ( Show )

data LJEventProperty = EventProperty { eventPropName, eventPropValue :: String } deriving ( Show )

data LJEvent = Event {  eventId :: String, 
                        eventTime :: DateTime,
                        eventText :: String,
                        eventSecurity :: LJEventSecurity,
                        eventSubject :: String,
                        eventPoster :: String,
                        eventANum :: String,
                        eventUrl :: String, 
                        eventProperties :: [LJEventProperty]
                      } |
                PropertyContainer { pItemId, pName, pValue :: String } deriving ( Show )

epoch = fromSeconds 0

eventObjectFactory :: ObjectFactory LJEvent
eventObjectFactory "events" = Just $ Event "" epoch "" Public "" "" "" "" []
eventObjectFactory "prop" = Just $ PropertyContainer "" "" ""
eventObjectFactory _ = Nothing

eventObjectUpdater :: ObjectUpdater LJEvent
eventObjectUpdater "events" "itemid" value obj = Just $ obj { eventId = value }
eventObjectUpdater "events" "eventtime" value obj = Just $ obj { eventTime = fromMaybe epoch (parseDateTime "" value) }
eventObjectUpdater "events" "event" value obj = Just $ obj { eventText = decodeUTF8 value }
eventObjectUpdater "events" "security" "private" obj = Just $ obj { eventSecurity = Private }
eventObjectUpdater "events" "security" "public" obj = Just $ obj { eventSecurity = Public }
eventObjectUpdater "events" "security" _ obj = Just obj
eventObjectUpdater "events" "allowmask" value obj = Just $ obj { eventSecurity = UserMask $ read value }
eventObjectUpdater "events" "subject" value obj = Just $ obj { eventSubject = decodeUTF8 value }
eventObjectUpdater "events" "poster" value obj = Just $ obj { eventPoster = value }
eventObjectUpdater "events" "anum" value obj = Just $ obj { eventANum = value }
eventObjectUpdater "events" "url" value obj = Just $ obj { eventUrl = value }

eventObjectUpdater "prop" "itemid" value obj = Just $ obj { pItemId = value }
eventObjectUpdater "prop" "name" value obj = Just $ obj { pName = value }
eventObjectUpdater "prop" "value" value obj = Just $ obj { pValue = value }


decodeUTF8 :: String -> String
decodeUTF8 = maybe "Encoding error" SU.decode . U.decode

instance ResponseTransformer LJEvent [LJEvent] where
    transform (simpleMap, enumMap, objectMap) = makeResult $ maybe [] createList (DM.lookup "events" objectMap)
        where
            propsMap = DM.fold makeProps DM.empty $ fromMaybe DM.empty $ DM.lookup "prop" objectMap
            makeProps propItem = DM.alter ( alterProp propItem ) ( pItemId propItem )
            alterProp propItem Nothing = Just [ makeLJEventProperty propItem ]
            alterProp propItem (Just arr) = Just $ makeLJEventProperty propItem : arr
            makeLJEventProperty ( PropertyContainer pId pName pValue ) = EventProperty pName pValue
            createList = DM.fold traverse []
            traverse ljEvt res = update ljEvt : res
            update ljEvt = fromMaybe ljEvt $ do 
                                let ljEvtItemId = eventId ljEvt
                                props <- DM.lookup ljEvtItemId propsMap
                                return $ ljEvt { eventProperties = props }

getEvents :: Session -> String -> Maybe Int -> Bool -> Bool -> SelectType -> LJLineEndings -> IOResult [LJEvent]
getEvents session username truncate preferSubject noMetadata selectType lineEndings = 
    runRequestSession session request (CRP eventObjectFactory eventObjectUpdater)
    where
        request = makeRequest $ [ ("mode", "getevents"),
                    ( "user", username ),
                    ( "ver", "1" ) ] ++
                    paramFromMaybe "truncate" truncate ++
                    [ makeBool "prefersubject" preferSubject,
                      makeBool "noprops" noMetadata ] ++ 
                    makeSelectType selectType ++
                    [ ("lineendings", lineEndingMapping DA.! lineEndings),
                      ("usejournal", username) ]
        makeBool name val = ("name", if val then "1" else "0")
        makeSelectType (Day yy mm dd) = [ ("selecttype","day") ,
                                          ("year", show yy) ,
                                          ("month", show mm) ,
                                          ("day", show dd)
                                        ]
        makeSelectType ( LastN howMany beforeDate ) = [ ("selecttype", "lastn") ,
                                                        ("howmany", show howMany ) ] ++
                                                      paramFromMaybe "beforedate" beforeDate
        makeSelectType ( One itemId ) = [ ("selecttype", "one") ,
                                        ("itemid", itemId ) ]
        makeSelectType ( Sync lastSyncP ) = [ ("selecttype","syncitems") ,
                                              ("lastsync", lastSyncP)
                                            ]
        paramFromMaybe pName = maybe [] ( (:[]) . (,) pName . show )
