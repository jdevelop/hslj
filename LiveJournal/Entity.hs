module LiveJournal.Entity where

import LiveJournal.Error
import Control.Monad.Error
import Data.Int
import Data.DateTime
import Data.Map

type Result m a = ErrorT LJError m a

type IOResult a = Result IO a

type Community = String

makeErrorStr :: String -> IOResult a
makeErrorStr = ErrorT . return . Left . SimpleError

makeError :: (Monad m) =>  LJError -> Result m a
makeError = ErrorT . return . Left

makeResult :: (Monad m) => a -> Result m a
makeResult = ErrorT . return . Right

data LJFriendKind = AFriend | AFriendOf deriving (Show)

data LJFriendType = Community | Syndicated | News | Shared | Identity | User deriving (Show, Enum, Bounded)

data LJFriendStatus = Deleted | Suspended | Purged | Active deriving (Show, Enum, Bounded)

data ResponseData = Mood { moodId, moodParent :: Int, moodName :: String } |
                    Group { groupName :: String, groupSortOrder :: Int, groupPublic :: Bool } |
                    Menu { menuId :: Int, menuItems :: Map Int ResponseData } |
                    MenuItem { menuItem, menuSub :: Int, menuUrl, menuText :: String } | 
                    Friend { 
                             friendKind :: LJFriendKind,
                             friendUsername, friendName, bgColor, fgColor :: String,
                             groupmask :: Int32,
                             friendType :: LJFriendType,
                             identityDisplay, identityType, identityValue :: Maybe String,
                             friendStatus :: LJFriendStatus,
                             birthday :: Maybe DateTime } |
                    Pickw { pickwUrl :: String, pickwKeyword :: [String] }
                         deriving (Show)
