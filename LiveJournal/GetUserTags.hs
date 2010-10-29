{-# LANGUAGE FlexibleContexts,TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module LiveJournal.GetUserTags (
    LJUserTagGroup(..),
    LJUserTagSecurity(..),
    LJUserTag(..),
    getUserTags
)where

import LiveJournal.Entity as LJE
import LiveJournal.Request
import LiveJournal.Session
import LiveJournal.Error
import LiveJournal.Transport
import LiveJournal.ResponseParser
import Data.Map as DM

import Text.Parsec as TP
import Text.Parsec.Error
import Text.Parsec.ByteString

import Control.Monad

data LJUserTagGroup = UserTagGroup {
                        tagGroupId, tagGroupUsed :: Int
                        } deriving (Show)

data LJUserTagSecurity = TagPublic | TagPrivate | TagFriends | TagGroup deriving (Show, Eq, Ord)

data LJUserTag = UserTag {
                    tagName :: String,
                    tagGroups :: DM.Map Int LJUserTagGroup,
                    tagUsedTotal, tagUsedPrivate, tagUsedPublic, tagUsedFriendsOnly :: Int,
                    tagDisplay :: Bool,
                    tagSecurity :: LJUserTagSecurity } deriving (Show)


instance Read LJUserTagSecurity 
    where
        readsPrec v "public" = [(TagPublic,"")]
        readsPrec v "private" = [(TagPrivate,"")]
        readsPrec v "friends" = [(TagFriends,"")]
        readsPrec v "group" = [(TagGroup,"")]


instance ResponseTransformer LJUserTag [LJUserTag] where
    transform (simpleMap, enumMap, objectMap) = makeResult . DM.elems $ DM.findWithDefault DM.empty "tag" objectMap

userTagObjectFactory :: ObjectFactory LJUserTag
userTagObjectFactory "tag" = Just $ UserTag "" DM.empty 0 0 0 0 False TagPublic
userTagObjectFactory _ = Nothing

userTagObjectUpdater :: ObjectUpdater LJUserTag
userTagObjectUpdater "tag" = updateTagProps
    where 
        updateTagProps "name" value obj = Just $ obj { tagName = value }
        updateTagProps "uses" value obj = Just $ obj { tagUsedTotal = read value }
        updateTagProps "display" "1" obj = Just $ obj { tagDisplay = True }
        updateTagProps "security" value obj = Just $ obj { tagSecurity = read value }
        updateTagProps "sb_friends" value obj = Just $ obj { tagUsedFriendsOnly = read value }
        updateTagProps "sb_private" value obj = Just $ obj { tagUsedPrivate = read value }
        updateTagProps "sb_public" value obj = Just $ obj { tagUsedPublic = read value }
        updateTagProps paramName value obj = Just $ either (const obj) id $ runP (tagGroupParser value obj) () "" paramName

tagGroupParser :: (Stream s m Char) => String -> LJUserTag -> ParsecT s u m LJUserTag
tagGroupParser value userTag = do 
    TP.string "sb_group"
    TP.char '_'
    groupId <- liftM read $ TP.many1 TP.digit
    TP.char '_'
    paramName <- TP.many TP.letter
    return $ userTag { tagGroups = DM.alter (updateMap paramName) groupId tags }
    where
        updateMap "id" Nothing = Just $ UserTagGroup ( read value ) 0
        updateMap "id" (Just obj) = Just $ obj { tagGroupId = read value }
        updateMap "count" Nothing = Just $ UserTagGroup 0 ( read value )
        updateMap "count" (Just obj) = Just $ obj { tagGroupUsed = read value }
        tags = tagGroups userTag

getUserTags :: Session -> String -> IOResult [LJUserTag]
getUserTags session username = 
    runRequestSession session params (CRP userTagObjectFactory userTagObjectUpdater)
    where
        params = makeRequest [
            ("mode", "getusertags"),
            ("user", username)
            ]
