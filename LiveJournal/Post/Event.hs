module LiveJournal.Post.Event (
    LineEndings(..),
    leMapping,
    Security(..),
    secMapping,
    Event(..),
    Property(..),
    event2pairs
) where

import Data.Word
import Data.Maybe
import Data.Array
import LiveJournal.Pair
import Data.DateTime

data LineEndings = UNIX | PC | MAC deriving (Enum,Bounded,Eq,Ord,Ix)
leMapping :: Array LineEndings String
leMapping = listArray (minBound,maxBound) ["unix","pc","mac"]

data Security = PUBLIC  | PRIVATE | USEMASK deriving (Enum,Bounded,Eq,Ord,Ix)
secMapping :: Array Security String
secMapping = listArray (minBound,maxBound) ["public","private","usemask"]

data Property = Property { name, value :: String }

data Event = Event { event, subject :: Maybe String, 
                     lineendings :: LineEndings,
                     security :: Maybe Security,
                     allowmask :: Word32,
                     date :: DateTime,
                     metadata :: Maybe [Property] }

type Entry2PairTransformer = Event -> [Maybe Pair]

transformers :: [Entry2PairTransformer]
transformers = [ mbEventPair "event" event ,
                 mbEventPair "subject" subject ,
                 trLineendings ,
                 mbSecurity ,
                 makeShowableEventPair "allowmask" allowmask ,
                 --TODO implement parsing of metadata here
                 datePairs ]
    where
        trLineendings = (:[]) . Just . makePair "lineendings" . (leMapping !) . lineendings
        mbSecurity = (:[]) . fmap ( makePair "security" . (secMapping !)) . security

mbEventPair :: String -> (Event -> Maybe String) -> Event -> [Maybe Pair]
mbEventPair name f post = (:[]) . fmap (makePair name) $ f post

makeShowableEventPair :: (Show a) => String -> (Event -> a) -> Event -> [Maybe Pair]
makeShowableEventPair name f = (:[]) . Just . makePair name . show . f

datePairs :: Event -> [ Maybe Pair]
datePairs = toPairs . toGregorian . date
    where
        toPairs (year,month,day,hours,minutes,_) = fmap Just [ makePair "year" $ show year,
                                                               makePair "mon" $ show month,
                                                               makePair "day" $ show day,
                                                               makePair "hour"$ show hours,
                                                               makePair "min" $ show minutes ]

event2pairs :: Event -> [Pair]
event2pairs post = concatMap ( concatMap maybeToList . ( $post ) ) transformers
