module LiveJournal.Post.SyncItems (
    LJSyncItemType(..),
    LJSyncType(..),
    LJSyncItem(..),
    LJSyncResponse(..),
    getSyncItems
) where

import Text.ParserCombinators.ReadP as TPR
import Text.Printf as TP
import Data.DateTime as DT
import LiveJournal.Session as LS
import LiveJournal.Pair as LP
import LiveJournal.Common as LC
import LiveJournal.SimpleResponseParser as SRP
import Data.List as L
import Data.ByteString.UTF8 as BStr
import Data.Maybe as M

data LJSyncItemType = SyncItemType { typeStr :: String } deriving (Show)

data LJSyncType = Update | Create deriving (Show)

data LJSyncItem = SyncItem { itemType :: LJSyncItemType, syncType :: LJSyncType, changed :: DateTime }

instance Show LJSyncItem
    where
        show (SyncItem iType sType dateChanged) = TP.printf "type: %s, sync: %s, changed: %s" 
                                                    (show iType) (show sType) (DT.formatDateTime "%d-%m-%Y %T" dateChanged)

data LJSyncResponse = SyncResponse { items :: [LJSyncItem], newestDateTime :: DateTime }

instance Show LJSyncResponse
    where
        show (SyncResponse recItems newestDate) = TP.printf "Items:\n%s\n\n last date: %s" (L.unlines $ fmap show recItems) 
                                                (DT.formatDateTime "%d-%m-%Y %T" newestDate)

getSyncItems :: Session -> String -> Maybe DateTime -> IO (Result LJSyncResponse)
getSyncItems session username lastSync = 
    makeLJCall session pairs parseResponse
    where
        pairs = makePair "mode" "syncitems" :
                makePair "user" username :
                maybe [] ( (:[]) . makePair "lastsync" . formatDateTime "%Y-%m-%d %T") lastSync

parseResponse :: [Pair] -> Result LJSyncResponse
parseResponse pairs = Right $ SyncResponse records newestDateTime'
    where
        newestDateTime' = L.foldl' ( ( . changed ) . compareDates ) (DT.fromSeconds 0) records
        compareDates :: DateTime -> DateTime -> DateTime
        compareDates curMax newMax | toSeconds curMax < toSeconds newMax = newMax
                                   | otherwise = curMax
        records = convertPairsToRecords updateSyncItem parseSyncPair emptySyncItem pairs
        emptySyncItem = SyncItem ( SyncItemType "" ) Create (DT.fromSeconds 0)
        parseSyncPair (Pair name value) = extractCompleteParse ( TPR.readP_to_S parser $ BStr.toString name ) >>= createIxPair value
        extractCompleteParse ((pair,""):[]) = Just pair
        extractCompleteParse _ = Nothing
        createIxPair value (idx,name) = Just (idx,name,BStr.toString value)
        parser = SRP.simpleLJResponseNameParser "sync_"

updateSyncItem :: String -> String -> LJSyncItem -> LJSyncItem
updateSyncItem "item" value rec = rec { itemType = SyncItemType value }
updateSyncItem "action" "update" rec = rec { syncType = Update }
updateSyncItem "action" "create" rec = rec { syncType = Create }
updateSyncItem "action"  _  rec = rec { syncType = Create }
updateSyncItem "time" value rec = rec { changed = M.fromMaybe (fromSeconds 0) $ parseDateTime "%Y-%m-%d %T" value }
updateSyncItem _ _ rec = rec
