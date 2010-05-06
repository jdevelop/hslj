module LiveJournal.Post.Event where

import Data.ByteString.UTF8 as BStr
import Data.Word
import Data.Maybe
import Data.Array
import LiveJournal.Transport

data LineEndings = UNIX | PC | MAC deriving Enum
leMapping = listArray (1,3) ["unix","pc","mac"]

data Security = PIBLIC  | PRIVATE | USEMASK deriving Enum
secMapping = listArray (1,3) ["public","private","usemask"]

data Property = Property { name, value :: ByteString }

data Event = Event { event, subject :: Maybe BStr.ByteString, 
                     lineendings :: LineEndings,
                     security :: Maybe Security,
                     allowmask :: Word32,
                     year, mon, day, hour, min :: Int,
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
                 makeShowableEventPair "min" LiveJournal.Post.Event.min
                 --TODO implement parsing of metadata here
                 ]
    where
        trLineendings = Just . makePair "lineendings" . fix leMapping . lineendings
        mbSecurity = fmap ( makePair "security" . fix secMapping) . security
        fix arr = (arr !) . fromEnum

mbEventPair :: String -> (Event -> Maybe BStr.ByteString) -> Event -> Maybe Pair
mbEventPair name f post = fmap (makePairBSValue name) $ f post

makeShowableEventPair :: (Show a) => String -> (Event -> a) -> Event -> Maybe Pair
makeShowableEventPair name f = Just . makePair name . show . f

event2pairs :: Event -> [Pair]
event2pairs post = concatMap ( maybeToList . ( $post ) ) transformers
