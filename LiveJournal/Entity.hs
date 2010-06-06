module Entity where

data LastUpdate = LastUpdate { lastUpdateStr :: String, hasNew, interval :: Int }

data Event = Event { event, subject :: Maybe String, 
                     lineendings :: LineEndings,
                     security :: Maybe Security,
                     allowmask :: Word32,
                     date :: DateTime,
                     metadata :: Maybe [Property] }

data ListEvent = ListEvent { anum, event, itemid, url, subject :: String, 
                             eventtime :: DateTime, 
                             security :: Security, 
                             allowmask :: Maybe Word32,
                             poster :: Maybe String,
                             properties :: [Property]
                             }
