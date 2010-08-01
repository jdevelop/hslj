{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts,
             TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module LiveJournal.Login (
    login,
    loginExt,
    LJLoginRequest(..),
    LJLoginResponse(..)
)
where 

import LiveJournal.Entity
import LiveJournal.Error
import LiveJournal.Session hiding (password)
import LiveJournal.Transport
import LiveJournal.Request
import LiveJournal.ResponseParser as LJRP

import Control.Applicative hiding ((<|>))
import Control.Monad
import Control.Monad.Trans

import Data.Maybe as DM
import Data.Map as DMP
import Data.List as DL

import Text.Parsec as TP
import Text.Parsec.ByteString

data LJLoginRequest = LoginRequest { 
                        user :: String,
                        password :: String, 
                        moods :: Maybe Int,
                        menus :: Bool,
                        pickws :: Bool,
                        pickwurls :: Bool
                      }

type Community = String

data LJLoginResponse = LoginResponse { 
                                       ljUsername :: String,
                                       ljSession :: Session, 
                                       ljCommunities :: [Community],
                                       ljMoods :: [LoginResponseData],
                                       ljGroups :: [LoginResponseData],
                                       ljMenus :: [LoginResponseData],
                                       ljPics :: [LoginResponseData],
                                       ljDefaultPicwUrl :: Maybe String,
                                       ljFastServer :: Bool
                                     } deriving (Show)

data LoginResponseData = Mood { moodId, moodParent :: Int, moodName :: String } |
                         Group { groupName :: String, groupSortOrder :: Int, groupPublic :: Bool } |
                         Menu { menuId :: Int, menuItems :: DMP.Map Int LoginResponseData } |
                         MenuItem { menuItem, menuSub :: Int, menuUrl, menuText :: String } | 
                         Pickw { pickwUrl :: String, pickwKeyword :: [String] } 
                         deriving (Show)

login :: String -> String -> IOResult LJLoginResponse
login username password = loginExt $ LoginRequest username password Nothing False False False

loginObjectFactory :: ObjectFactory LoginResponseData
loginObjectFactory "mood" = Just $ Mood 0 0 ""
loginObjectFactory "frgrp" = Just $ Group "" 0 False
loginObjectFactory "menu" = Just $ Menu 0 DMP.empty
loginObjectFactory _ = Nothing

loginObjectUpdater :: ObjectUpdater LoginResponseData
loginObjectUpdater "mood" "id" value obj = Just $ obj { moodId = read value }
loginObjectUpdater "mood" "name" value obj = Just $ obj { moodName = value }
loginObjectUpdater "mood" "parent" value obj = Just $ obj { moodParent = read value }
loginObjectUpdater "frgrp" "name" value obj = Just $ obj { groupName = value }
loginObjectUpdater "frgrp" "sortorder" value obj = Just $ obj { groupSortOrder = read value }
loginObjectUpdater "frgrp" "public" value obj = Just $ obj { groupPublic = "1" == value }
loginObjectUpdater "menu" menuParam value obj = 
    case parseResult of
        (Left err) -> Just obj
        (Right obj') -> Just obj'
    where
        parseResult = head $ TP.runPT parseMenuItem (obj, value) "" menuParam
loginObjectUpdater _ _ _ _ = Nothing

instance ResponseTransformer LoginResponseData LJLoginResponse where
    transform (simpleMap, enumMap, objectMap) = 
        maybe (makeErrorStr $ "Can't create response " ++ show simpleMap) (makeResult) $ do
        username <- DMP.lookup "name" simpleMap
        return $ LoginResponse username Anonymous communities moods groups menus pickws defaultPicwUrl fastServer
        where 
            communities = maybe [] (DL.concat . DMP.elems) $  DMP.lookup "access" enumMap
            moods = DM.maybe [] id $ DMP.elems <$> DMP.lookup "mood" objectMap
            groups = DM.maybe [] id $  DMP.elems <$> DMP.lookup "frgrp" objectMap
            menus = DM.maybe [] id $ DMP.elems <$> DMP.lookup "menu" objectMap
            pickws = foldWithKey makePickws [] pickwUrls
            pickwKeys = maybe DMP.empty id $ DMP.lookup "pickw" enumMap
            pickwUrls = maybe DMP.empty id $ DMP.lookup "pickwurl" enumMap
            makePickws idx [url] = let keywords = concat . maybeToList $ DMP.lookup idx pickwKeys
                                    in ( Pickw url keywords : )
            defaultPicwUrl = DMP.lookup "defaultpicurl" simpleMap
            fastServer = DM.maybe False (const True) $ DMP.lookup "fastserver" simpleMap


loginExt :: LJLoginRequest -> IOResult LJLoginResponse
loginExt request =
    prepareChallenge ( password request ) >>= DM.maybe emptyResponse login'
    where
        emptyResponse = makeError NoChallenge
        login' (chal, auth_response) = do
            result <- (runRequest request' (CRP loginObjectFactory loginObjectUpdater) :: IOResult LJLoginResponse)
            return $ result { ljSession = Authenticated ( password request ) }
            where
                params = DL.concat [
                        [
                            ("mode","login"), 
                            ("user",user request), 
                            ("auth_method","challenge"),
                            ("auth_challenge",chal),
                            ("auth_response",auth_response)
                         ],
                         DM.maybe [] ( makeTupleSArr "getmoods" .  show ) ( moods request ),
                         guard (menus request) >> makeTupleSArr "getmenus" "1",
                         guard (pickws request) >> makeTupleSArr "getpickws" "1",
                         guard (pickwurls request) >> makeTupleSArr "getpickwurls" "1"
                         ]
                request' = makeRequest params
                makeTupleSArr = ( return . ) . (,)

parseMenuItem =
    try (parseMenuItemProperty "_text" updateText) <|> 
        ( try (parseMenuItemProperty "_url" updateUrl) <|> parseMenuItemProperty "_sub" updateSub )
    where
        updateText txt menyItemP = menyItemP { menuText = txt }
        updateUrl txt menyItemP = menyItemP { menuUrl = txt }
        updateSub txt menyItemP = menyItemP { menuSub = read txt }

parseMenuItemProperty suffix f = do
    itemNum <- liftM (read) $ TP.many TP.digit
    TP.string suffix
    (menu, value) <- getState
    return $ updateMenuMap menu itemNum (f value)

updateMenuMap menu itemNum f  = 
    let newMap = DMP.alter updFunc itemNum ( menuItems menu )
    in menu { menuItems = newMap }
    where
        updFunc Nothing = updFunc . Just $ MenuItem itemNum 0 "" ""
        updFunc (Just menuItem) = Just (f menuItem)
