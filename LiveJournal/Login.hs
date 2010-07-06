module LiveJournal.Login (
    login,
    loginExt,
    LJLoginRequest(..)
)
where 

import LiveJournal.Entity
import LiveJournal.Error
import LiveJournal.Session hiding (password)
import LiveJournal.Transport
import LiveJournal.Request
import LiveJournal.ResponseParser as LJRP

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.Maybe as DM
import Data.List as DL

import Text.Parsec as TP

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
                                       ljCommunities :: [Community]
                                     }

data LoginResponseData = Mood { moodId, moodParent :: Int, moodName :: String } |
                         Group { groupName :: String, groupSortOrder :: String } |
                         Menu { menuUrl, menuText :: String }

instance Show LJLoginResponse where
    show (LoginResponse uname session comms) = uname ++ show comms

login :: String -> String -> IO ( Result LJLoginResponse )
login username password = loginExt $ LoginRequest username password Nothing False False False

loginObjectFactory :: ObjectFactory LoginResponseData
loginObjectFactory "mood" = Just $ Mood 0 0 ""
loginObjectFactory _ = Nothing

loginObjectUpdater :: ObjectUpdater LoginResponseData
loginObjectUpdater "mood" "id" value obj = Just $ obj { moodId = read value }
loginObjectUpdater "mood" "name" value obj = Just $ obj { moodName = value }
loginObjectUpdater "mood" "parent" value obj = Just $ obj { moodParent = read value }
loginObjectUpdater _ _ _ _ = Nothing

loginExt :: LJLoginRequest -> IO ( Result LJLoginResponse )
loginExt request =
    prepareChallenge ( password request ) >>= DM.maybe emptyResponse login'
    where
        emptyResponse = return ( makeError NoChallenge )
        login' (chal, auth_response) = 
            handleResp . getLJResult <$> runRequest request' (CRP loginObjectFactory loginObjectUpdater)
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
                handleResp = undefined
