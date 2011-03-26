module LiveJournal.Entity where

import LiveJournal.Error
import Control.Monad.Error
import Data.Int
import Data.DateTime
import Data.Map
import Data.Array

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

data SelectType = Day { year, month, day :: Int } |
                  Sync { lastSync :: String } |
                  LastN { howMany :: Int, 
                          beforeDate :: Maybe String } |
                  One { itemId :: String }

data LJLineEndings = Unix | PC | Mac | Space | Dots deriving ( Show, Eq, Ord, Enum, Bounded, Ix )

lineEndingMapping = listArray ( minBound, maxBound ) ["unix", "pc", "mac", "space", "dots"] :: Array LJLineEndings String

data LJEventSecurity = Public | Private | UserMask { userMask :: Int } deriving ( Show )

data LJEventProperty = EventProperty { eventPropName, eventPropValue :: String } deriving ( Show )

data LJEvent = Event {  eventId :: String, 
                        eventTime :: DateTime,
                        eventText :: String,
                        eventSecurity :: LJEventSecurity,
                        eventSubject :: String,
                        eventPoster :: String,
                        eventANum :: String,
                        eventUrl :: String, 
                        eventProperties :: [LJEventProperty]
                      } |
                PropertyContainer { pItemId, pName, pValue :: String } deriving ( Show )

data LJMetaData = AdminContentFlag String |
                  AdultContent String |
                  CommentAlter Int |
                  CurrentCoordinates Double Double |
                  CurrentLocation String |
                  CurrentMood String | 
                  CurrentMoodId (Maybe Int) |
                  CurrentMusic String |
                  HasScreenedComments Bool |
                  BackDatedPost Bool |
                  NoComments Bool |
                  NoEmailComments Bool |
                  Preformatted Bool |
                  CommentScreening Char |
                  PersonifiLang String |
                  PersonifiTags String | 
                  PersonifiWordCount Int |
                  PictureKeyword String |
                  RevisionNum Int |
                  RevisionTime Int |
                  SMSMessageId String |
                  Visibility Char |
                  SyndicatedId String |
                  SyndicatedUrl String |
                  Tags [String] |
                  NonUTF8Text Bool |
                  UnsuspendRequestId Int |
                  ComposeInRTE Bool |
                  UserAgent String |
                  VerticalsList [String]
