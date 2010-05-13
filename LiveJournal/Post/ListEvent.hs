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
import LiveJournal.Post.Event as PE hiding (event)
import Data.DateTime
import Data.Array as A
import Data.Maybe
import Text.Printf
import Prelude as P
import Data.Map as M

type ParamDescriptor = (Int, String)

type MbDateTime = Maybe DateTime

data SelectType =   Day DateTime | 
                    LastN Int MbDateTime | 
                    One String |
                    SyncItems DateTime

selectTypeToString :: SelectType -> String
selectTypeToString (Day _) = "day"
selectTypeToString (LastN _ _) = "lastn"
selectTypeToString (One _)= "one"
selectTypeToString (SyncItems _) = "syncitems"

data ListEvent = ListEvent { anum, event, itemid, url :: String, postedAt :: DateTime }

instance Show ListEvent 
    where
        show evt = printf "[itemid=%s, anum=%s, url=%s, posted='%s'] \n%s\n" (itemid evt) (anum evt) (url evt) (formatDateTime "%Y-%m-%d %T" (postedAt evt)) (event evt)

listEvents :: Session -> String -> Maybe Int -> Bool -> Bool -> SelectType -> LineEndings -> Maybe String -> IO (Result [ListEvent])
listEvents session username truncate preferSubject noProps selectType lineEnding journal = do
    
    makeLJCall session pairs (Right . parseResponse)
    where
        pairs = P.concat [ [  makePair "mode" "getevents",
                            makePair "user" username,
                            makePair "selecttype" selectType',
                            makePair "lineendings" (PE.leMapping A.! lineEnding)
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

parsePair :: Pair -> Maybe (Int, String, String)
parsePair (Pair { PA.name=name', PA.value=value' }) = fmap (makeTriple) $ parseParamName $ BStr.unpack name'
    where
        makeTriple (idx,paramName) = (idx,paramName, BStr.unpack value')


parseParamName :: String -> Maybe ParamDescriptor
parseParamName = makeDescriptor . readP_to_S pnameParser
    where
        makeDescriptor ((descr,""):[]) = Just descr
        makeDescriptor _ = Nothing

pnameParser :: ReadP ParamDescriptor
pnameParser = do
    string "events_"
    paramId <- munch (flip P.elem ['0','1'..'9'])
    char '_'
    paramName <- munch (\_ -> True)
    return (read paramId, paramName)

parseResponse :: [Pair] -> [ListEvent]
parseResponse pairs = M.elems resultMap
    where
        initMap :: M.Map Int ListEvent
        initMap = M.empty
        resultMap = P.foldr (makeListEvent) initMap pairs
        makeListEvent :: Pair -> M.Map Int ListEvent -> M.Map Int ListEvent 
        makeListEvent pair resMap = updateMap resMap $ parsePair pair
        updateMap resMap Nothing = resMap
        updateMap resMap (Just (idx, key, value)) = updateMap' resMap idx key value
        updateMap' resMap idx key value | idx `M.member` resMap = M.update (Just . updateListObject key value) idx resMap
                                        | otherwise = M.insert idx (updateListObject key value emptyListEvent) resMap
        emptyListEvent :: ListEvent
        emptyListEvent = ListEvent "" "" "" "" (fromSeconds 0)
        updateListObject :: String -> String -> ListEvent -> ListEvent
        updateListObject "anum" value evt = evt { anum = value }
        updateListObject "event" value evt = evt { event = value } -- TODO - decode it
        updateListObject "itemid" value evt = evt { itemid = value }
        updateListObject "url" value evt = evt { url = value }
        updateListObject "eventtime" value evt = evt { 
                postedAt = fromJust $ P.head [val | val <- [parseDateTime "%Y-%m-%d %T" value, Just (fromSeconds 0)], val /= Nothing]
            }
        updateListObject _ _ evt = evt
