{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts,
             TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module LiveJournal.Login (
    login,
    loginExt,
    LJLoginRequest(..),
    LJLoginResponse(..),
    loginObjectUpdater,
    loginObjectFactory
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


data LJLoginResponse = LoginResponse { 
                                       ljUsername :: String,
                                       ljSession :: Session, 
                                       ljCommunities :: [Community],
                                       ljMoods :: [ResponseData],
                                       ljGroups :: [ResponseData],
                                       ljMenus :: [ResponseData],
                                       ljPics :: [ResponseData],
                                       ljDefaultPicwUrl :: Maybe String,
                                       ljFastServer :: Bool
                                     } deriving (Show)


login :: String -> String -> IOResult LJLoginResponse
login username password = loginExt $ LoginRequest username password Nothing False False False

loginObjectFactory :: ObjectFactory ResponseData
loginObjectFactory "mood" = Just $ Mood 0 0 ""
loginObjectFactory "frgrp" = Just $ Group "" 0 False
loginObjectFactory "menu" = Just $ Menu 0 DMP.empty
loginObjectFactory _ = Nothing

loginObjectUpdater :: ObjectUpdater ResponseData
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

instance ResponseTransformer ResponseData LJLoginResponse where
    transform (simpleMap, enumMap, objectMap) = 
        maybe (makeErrorStr $ "Can't create response " ++ show simpleMap) makeResult $ do
        username <- DMP.lookup "name" simpleMap
        return $ LoginResponse username Anonymous communities moods groups menus pickws defaultPicwUrl fastServer
        where 
            communities = maybe [] (DL.concat . DMP.elems) $  DMP.lookup "access" enumMap
            moods = fromMaybe [] $ DMP.elems <$> DMP.lookup "mood" objectMap
            groups = fromMaybe [] $  DMP.elems <$> DMP.lookup "frgrp" objectMap
            menus = fromMaybe [] $ DMP.elems <$> DMP.lookup "menu" objectMap
            pickws = foldWithKey makePickws [] pickwUrls
            pickwKeys = fromMaybe DMP.empty $ DMP.lookup "pickw" enumMap
            pickwUrls = fromMaybe DMP.empty $ DMP.lookup "pickwurl" enumMap
            makePickws idx [url] = let keywords = concat . maybeToList $ DMP.lookup idx pickwKeys
                                    in ( Pickw url keywords : )
            defaultPicwUrl = DMP.lookup "defaultpicurl" simpleMap
            fastServer = isJust $ DMP.lookup "fastserver" simpleMap


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
    itemNum <- liftM read $ TP.many TP.digit
    TP.string suffix
    (menu, value) <- getState
    return $ updateMenuMap menu itemNum (f value)

updateMenuMap menu itemNum f  = 
    let newMap = DMP.alter updFunc itemNum ( menuItems menu )
    in menu { menuItems = newMap }
    where
        updFunc Nothing = updFunc . Just $ MenuItem itemNum 0 "" ""
        updFunc (Just menuItem) = Just (f menuItem)
