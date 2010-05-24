module LiveJournal.Friends.GetFriends (
    LJFriend(..),
    LJIdentity(..),
    LJAcctType(..),
    LJAcctStatus(..),
    listFriends
)
where

import Text.Printf as TP
import Text.ParserCombinators.ReadP as TPR
import LiveJournal.Common as LJC
import LiveJournal.Pair as LJP
import LiveJournal.Session as LJS
import LiveJournal.SimpleResponseParser as SRP
import Data.DateTime as DT
import Data.Word
import Data.ByteString.UTF8 as BStr
import Data.List as L
import Data.Array as A
import Data.Map as M
import Data.Char as C

data LJIdentity = Identity { idtType, idtValue, idtDisplay :: String } 

instance Show LJIdentity 
    where
        show (Identity idType idValue idDisplay) = TP.printf "[%s : %s : %s]" idType idValue idDisplay

data LJAcctType = CommunityAcct | SyndicatedAcct | NewsAcct | SharedAcct | IdentityAcct | PersonalAcct
    deriving (Enum, Eq, Ord, Ix, Bounded)
acctMap :: Array LJAcctType String
acctMap = A.listArray (minBound, maxBound) ["community","syndicated","news","shared","identity","personal"]
acctRevMap = M.fromList . L.map (\(x,y) -> (y,x)) $ A.assocs acctMap

data LJAcctStatus = Active | Deleted | Suspended | Purged deriving (Enum, Eq, Ord, Ix, Bounded)
statusMap :: Array LJAcctStatus String
statusMap = A.listArray (minBound, maxBound) ["active","deleted","suspended","purged"]
statusRevMap = M.fromList . L.map (\(x,y) -> (y,x)) $ A.assocs statusMap

data LJFriend = Friend { username, fullname, bgColor, fgColor :: String,
                         birthday :: Maybe DateTime,
                         groupmask :: Word32,
                         accountType :: LJAcctType,
                         identity :: LJIdentity,
                         accountStatus :: LJAcctStatus }

instance Show LJAcctType
    where
        show = (acctMap A.!)

instance Show LJAcctStatus
    where
        show = (statusMap A.!)

instance Show LJFriend where
    show ( Friend { username = username', fullname = fullname', bgColor = bgColor',
                    fgColor = fgColor', birthday = birthday', accountType = accountType',
                    identity = identity', accountStatus = status' } ) =
                    TP.printf "{username=%s, fullname=%s, bgColor=%s, fgColor=%s, birthday=%s, accountType=%s, status=%s, identity=%s}"
                        username' fullname' bgColor' fgColor' 
                        (fmtBirthday birthday') (show accountType') (show status') 
                        (show identity')
        where
            fmtBirthday = maybe "" (DT.formatDateTime "%d-%m-%Y %T")

listFriends :: Session -> String -> Bool -> Bool -> Bool -> Maybe Int -> IO (Result [LJFriend]) 
-- TODO include friendof and groups parsing
listFriends session username incFriendOf incGroups incBirthDs howmuch =
    makeLJCall session request parseResponse
    where
        request = 
            makePair "mode" "getfriends" :
            makePair "user" username : 
            concat [incFriendOf',incGroups',incBirthDs',howmuch']
        incFriendOf' = mbBool1 "includefriendof" incFriendOf
        incGroups' = mbBool1 "includegroups" incGroups
        incBirthDs' = mbBool1 "includebdays" incBirthDs
        howmuch' = maybe [] ( (:[]) . makePair "friendlimit" . show) howmuch
        mbBool1 name bool | bool = [makePair name "1"]
                          | otherwise = []

parseResponse :: [Pair] -> Result [LJFriend]
parseResponse = Right . SRP.convertPairsToRecords updateFriendRecord parsePair emptyFriend
    where
        emptyIdentity = Identity "" "" ""
        emptyFriend = Friend "" "" "" "" Nothing 0 PersonalAcct emptyIdentity Active
        parsePair :: IxPairParser
        parsePair (Pair name value) = pair >>= (\(idx, name') -> Just (idx, name', BStr.toString value))
            where
                pair = parsePairName (BStr.toString name)

parsePairName :: String -> Maybe (Int, String)
parsePairName = handleParsedPair . TPR.readP_to_S nameParser
    where
        handleParsedPair [(parsedPair,"")] = Just parsedPair
        handleParsedPair _ = Nothing
        nameParser = SRP.simpleLJResponseNameParser "friend_"

updateFriendRecord :: RecordUpdateF LJFriend
updateFriendRecord "bg" value rec = rec { bgColor = value }
updateFriendRecord "fg" value rec = rec { fgColor = value }
updateFriendRecord "user" value rec = rec { username = value }
updateFriendRecord "name" value rec = rec { fullname = value }
updateFriendRecord "birthday" value rec = rec { birthday = parseDateTime "%Y-%m-%d" value }
updateFriendRecord "groupmask" value rec = rec { groupmask = read value }
updateFriendRecord "type" value rec = maybe rec (\acctType' ->  rec { accountType = acctType' } ) mapping
    where
        mapping = value `M.lookup` acctRevMap
updateFriendRecord "status" value rec = maybe rec (\acctStatus' ->  rec { accountStatus = acctStatus' } ) mapping
    where
        mapping = value `M.lookup` statusRevMap
updateFriendRecord "identity_display" value rec = rec { identity = newIdt } 
    where
        newIdt = identity' { idtDisplay = value  }
        identity' = identity rec
updateFriendRecord "identity_type" value rec = rec { identity = newIdt } 
    where
        newIdt = identity' { idtType = value  }
        identity' = identity rec
updateFriendRecord "identity_value" value rec = rec { identity = newIdt } 
    where
        newIdt = identity' { idtValue = value  }
        identity' = identity rec
updateFriendRecord _ _ rec = rec
