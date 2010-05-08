module LiveJournal.Post.Event where

import Data.ByteString.UTF8 as BStr
import Data.Word
import Data.Maybe
import Data.Array
import LiveJournal.Pair

data LineEndings = UNIX | PC | MAC deriving (Enum,Bounded)
leMapping = listArray (0,2) ["unix","pc","mac"]

data Security = PUBLIC  | PRIVATE | USEMASK deriving (Enum,Bounded)
secMapping = listArray (0,2) ["public","private","usemask"]

data Property = Property { name, value :: ByteString }

data Event = Event { event, subject :: Maybe BStr.ByteString, 
                     lineendings :: LineEndings,
                     security :: Maybe Security,
                     allowmask :: Word32,
                     year, mon, day, hour, minute :: Int,
                     metadata :: Maybe [Property] }

type Entry2PairTransformer = Event -> Maybe Pair
transformers :: [Entry2PairTransformer]
transformers = [ mbEventPair "event" event,
                 mbEventPair "subject" subject,
                 trLineendings,
                 mbSecurity,
                 makeShowableEventPair "allowmask" allowmask,
                 makeShowableEventPair "year" year,
                 makeShowableEventPair "mon" mon,
                 makeShowableEventPair "day" day,
                 makeShowableEventPair "hour" hour,
                 makeShowableEventPair "min" minute
                 --TODO implement parsing of metadata here
                 ]
    where
        trLineendings = Just . makePair "lineendings" . enum2String leMapping . lineendings
        mbSecurity = fmap ( makePair "security" . enum2String secMapping) . security
        enum2String arr = (arr !) . fromEnum

mbEventPair :: String -> (Event -> Maybe BStr.ByteString) -> Event -> Maybe Pair
mbEventPair name f post = fmap (makePairBSValue name) $ f post

makeShowableEventPair :: (Show a) => String -> (Event -> a) -> Event -> Maybe Pair
makeShowableEventPair name f = Just . makePair name . show . f

event2pairs :: Event -> [Pair]
event2pairs post = concatMap ( maybeToList . ( $post ) ) transformers
