module LiveJournal.Post.ListEvent (
    ListEvent(..),
    SelectType(..),
    MbDateTime,
    listEvents
)
where

import Data.ByteString.Char8 as BStr
import Text.ParserCombinators.ReadP
import LiveJournal.Pair as PA
import LiveJournal.Session
import LiveJournal.Common
import LiveJournal.Post.EventProps as EP
import Data.DateTime
import Data.Maybe
import Data.Word
import Text.Printf
import Prelude as P
import Data.Map as M

data PairDescriptor = EventDescriptor { evtId :: Int, evtPropName, evtPropValue :: String} | 
                      PropertyDescriptor { propId :: Int, propName, propValue :: String }

type MbDateTime = Maybe DateTime

type PropertyContainer = ( Int, Property )

data SelectType =   Day DateTime | 
                    LastN Int MbDateTime | 
                    One String |
                    SyncItems DateTime

selectTypeToString :: SelectType -> String
selectTypeToString (Day _) = "day"
selectTypeToString (LastN _ _) = "lastn"
selectTypeToString (One _)= "one"
selectTypeToString (SyncItems _) = "syncitems"

data ListEvent = ListEvent { anum, event, itemid, url, subject :: String, 
                             eventtime :: DateTime, 
                             security :: Security, 
                             allowmask :: Maybe Word32,
                             poster :: Maybe String,
                             properties :: [Property]
                             }

instance Show ListEvent 
    where
        show ( ListEvent {  anum = anum', event = event', 
                        itemid = itemid', url = url', 
                        subject = subject',
                        eventtime = eventtime',
                        security = security',
                        allowmask = allowmask',
                        poster = poster',
                        properties = properties'
                        } ) = printf "[itemid=%s, anum=%s, url=%s, eventtime='%s', security=%s, allowmask=%d]\n<%s> : %s\n\n%s\nproperties:\n%s\n" 
                            itemid' anum' url' formattedDate (show security') allowmask'' (show poster') subject' event' props
            where
                allowmask'' | allowmask' == Nothing = 0
                            | otherwise = fromJust allowmask'
                formattedDate = formatDateTime "%Y-%m-%d %T" eventtime'
                props = P.unlines $ P.map (show) properties'

listEvents :: Session -> String -> Maybe Int -> Bool -> Bool -> SelectType -> LineEndings -> Maybe String -> IO (Result [ListEvent])
listEvents session username truncate preferSubject noProps selectType lineEnding journal = do
    
    makeLJCall session pairs (Right . parseResponse)
    where
        pairs = P.concat [ [  makePair "mode" "getevents",
                            makePair "user" username,
                            makePair "selecttype" selectType',
                            makePair "lineendings" (leToStr lineEnding)
                         ],
                         makePairBool1 "prefersubject" preferSubject,
                         makePairBool1 "noprops" noProps,
                         mbMakePairShowable "truncate" truncate,
                         mbMakePairShowable "usejournal" journal,
                         selectTypeData selectType
                        ]
        selectType' = selectTypeToString selectType


selectTypeData :: SelectType -> [Pair]
selectTypeData (Day wantedDay) = [ makePair "year" yearStr,
                             makePair "month" monthStr,
                             makePair "day" dayStr]
    where
        print2Digits = printf "%02d"
        (year, month, day, _, _, _) = toGregorian wantedDay
        yearStr = print2Digits $ fromIntegral year
        monthStr = print2Digits month
        dayStr = print2Digits day
selectTypeData (One itemId) = [makePair "itemid" itemId]
selectTypeData (LastN amount Nothing) = [ makePair "howmany" (show amount) ]
selectTypeData (LastN amount (Just beforedate)) = [ makePair "howmany" (show amount), 
                                                    makePair "beforedate" fmtDate]
    where
        fmtDate = formatDateTime "%Y-%m-%d %T" beforedate
selectTypeData (SyncItems datetime) = [ makePair "lastsync" fmtDate ]
    where
        fmtDate = formatDateTime "%Y-%m-%d %T" datetime
        

makePairBool1 :: String -> Bool -> [Pair]
makePairBool1 name True = [makePair name "1"]
makePairBool1 _ False = []

mbMakePairShowable :: ( Show a ) => String -> Maybe a -> [Pair]
mbMakePairShowable name = maybeToList . fmap ( makePair name . show )

parsePair :: Pair -> Maybe PairDescriptor
parsePair (Pair { PA.name=name', PA.value=value' }) = fmap (makeTriple) $ parseParamName $ BStr.unpack name'
    where
        makeTriple evt@(EventDescriptor _ _ _) = evt { evtPropValue = BStr.unpack value' }
        makeTriple evt@(PropertyDescriptor _ _ _) = evt { propValue = BStr.unpack value' }

parseParamName :: String -> Maybe PairDescriptor
parseParamName = makeDescriptor . readP_to_S ( eventPropertyParser +++ propertyParser )
    where
        makeDescriptor ((descr,""):[]) = Just descr
        makeDescriptor _ = Nothing
        eventPropertyParser :: ReadP PairDescriptor
        eventPropertyParser = do
            string "events_"
            paramId <- munch (flip P.elem ['0','1'..'9'])
            char '_'
            paramName <- munch (\_ -> True)
            return (EventDescriptor (read paramId) paramName "")
        propertyParser :: ReadP PairDescriptor
        propertyParser = do
            string "prop_"
            paramId <- munch (flip P.elem ['0','1'..'9'])
            char '_'
            paramName <- munch (\_ -> True)
            return (PropertyDescriptor (read paramId) paramName "")

type EventMap = M.Map Int ListEvent
type PropertyMap = M.Map Int PropertyContainer
type EventPropertyTuple = ( EventMap, PropertyMap )

parseResponse :: [Pair] -> [ListEvent]
parseResponse pairs = result
    where
        result = joinEventAndProperties $ P.foldl (makeListEvent) (M.empty, M.empty) pairs
        makeListEvent :: EventPropertyTuple -> Pair -> EventPropertyTuple
        makeListEvent resMap = updateMap resMap . parsePair
        updateMap resMap Nothing = resMap
        updateMap (evtMap, propMap) (Just (EventDescriptor idx key value)) = (updateMap' evtMap idx key value, propMap)
            where
                updateMap' resMap idx key value | idx `M.member` resMap = M.update (Just . updateListEvent key value) idx resMap
                                                | otherwise = M.insert idx (updateListEvent key value emptyListEvent) resMap
        updateMap (evtMap, propMap) (Just (PropertyDescriptor idx key value)) = (evtMap, updateMap' propMap idx key value)
            where
                updateMap' resMap idx key value | idx `M.member` resMap = M.update (Just . updateProperty key value) idx resMap
                                                | otherwise = M.insert idx (updateProperty key value emptyPropertyContainer) resMap
        emptyListEvent :: ListEvent
        emptyListEvent = ListEvent "" "" "" "" "" (fromSeconds 0) PUBLIC Nothing Nothing []
        emptyPropertyContainer :: PropertyContainer
        emptyPropertyContainer = (0 , Property "" "" )
        updateProperty :: String -> String -> PropertyContainer -> PropertyContainer
        updateProperty "name" val (idx, property) = (idx, property { EP.name = val } )
        updateProperty "value" val (idx, property) = (idx, property { EP.value = val } )
        updateProperty "itemid" val (idx, property) = (read val, property )
        updateListEvent :: String -> String -> ListEvent -> ListEvent
        updateListEvent "anum" value evt = evt { anum = value }
        updateListEvent "event" value evt = evt { event = value } -- TODO - decode it
        updateListEvent "itemid" value evt = evt { itemid = value }
        updateListEvent "url" value evt = evt { url = value }
        updateListEvent "eventtime" value evt = evt { 
                eventtime = fromJust $ P.head [val | val <- [parseDateTime "%Y-%m-%d %T" value, Just (fromSeconds 0)], val /= Nothing]
            }
        updateListEvent "security" value evt | security' == Nothing = evt
                                             | otherwise = evt { security = fromJust security' }
            where
                security' = strToSec value
        updateListEvent "allowmask" value evt = evt { allowmask = Just ( read value ) }
        updateListEvent "subject" value evt = evt { subject = value }
        updateListEvent "poster" value evt = evt { poster = Just value }
        updateListEvent _ _ evt = evt
        joinEventAndProperties :: EventPropertyTuple -> [ListEvent]
        joinEventAndProperties (evtMap, propsMap) = elems $ fold (findAddProperty) newEvtMap propsMap
            where
                -- probably bad idea - convert itemid of post to int value here
                newEvtMap = fold (\val nMap -> M.insert ( read $ itemid val ) val nMap) M.empty evtMap
                addProperty :: Property -> ListEvent -> ListEvent
                addProperty prop evt = evt { properties = prop:properties evt }
                findAddProperty :: PropertyContainer -> EventMap -> EventMap
                findAddProperty (evtId,prop) evtMap' | evtId `M.member` evtMap' = M.update ( Just . addProperty prop ) evtId evtMap'
                                                     | otherwise = evtMap'
