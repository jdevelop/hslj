module LiveJournal.Tag.ListTags 
where

import LiveJournal.Pair as LP
import LiveJournal.Session as LS
import LiveJournal.Common as LC
import LiveJournal.SimpleResponseParser as SRP
import Data.ByteString.UTF8 as BStr
import Text.ParserCombinators.ReadP as TPR
import Data.Char as C
import Data.List as L
import Text.Printf as TP

data LJTagSscurity = PUBLIC | PRIVATE | FRIENDS | GROUP deriving (Enum, Eq, Ord, Read, Show)

data LJTag = Tag { tagName :: String,
                   used, usedFriendsOnly, usedPublic, usedPrivate :: Int,
                   displayed :: Bool,
                   security :: LJTagSscurity,
                   groups :: [Int]
                  }

instance Show LJTag
    where show (Tag { tagName = tagName',
                      used = used', usedFriendsOnly = usedFriendsOnly', usedPublic = usedPublic', 
                      usedPrivate = usedPrivate', displayed = displayed', security = security',
                      groups = groups'} ) = TP.printf "%s=[used=%d, friend-only=%d, public=%d, private=%d, displayed=%s, security=%s]\ngroups=%s\n" tagName' used' usedFriendsOnly' usedPublic' usedPrivate' (show displayed') (show security') (show groups')



listTags :: Session -> String -> Maybe String -> IO (Result [LJTag])
listTags session username target = 
    makeLJCall session pairs responseParser
    where
        pairs = makePair "user" username :
                makePair "mode" "getusertags" : 
                maybe [] ( (:[]) . makePair "usejournal" ) target


responseParser :: [Pair] -> Result [LJTag]
responseParser = Right . convertPairsToRecords recordUpdater pairParser emptyTag
    where
        emptyTag = Tag "" 0 0 0 0 False PUBLIC []
        pairParser (Pair name value) = makeIxPair . TPR.readP_to_S (simpleLJResponseNameParser "tag_") $ BStr.toString name
            where
                makeIxPair (((idx, name'),""):[]) = Just (idx, name', BStr.toString value)
                makeIxPair _ = Nothing
        recordUpdater "name" value rec = rec { tagName = value }
        recordUpdater "uses" value rec = rec { used = read value }
        recordUpdater "security" value rec = rec { security = read $ map C.toUpper value }
        recordUpdater "display" value rec = rec { displayed = value == "1" }
        recordUpdater "sb_friends" value rec = rec { usedFriendsOnly = read value }
        recordUpdater "sb_public" value rec = rec { usedPublic = read value }
        recordUpdater "sb_private" value rec = rec { usedPrivate = read value }
        recordUpdater name value rec | "sb_group" `L.isPrefixOf` name = rec { groups = read ( L.drop 8 name ) : groups rec }
                                     | otherwise = rec
