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

instance Show LJLoginResponse where
    show (LoginResponse uname session comms) = uname ++ show comms

login :: String -> String -> IO ( Result LJLoginResponse )
login username password = loginExt $ LoginRequest username password Nothing False False False

loginExt :: LJLoginRequest -> IO ( Result LJLoginResponse )
loginExt request =
    prepareChallenge ( password request ) >>= DM.maybe emptyResponse login'
    where
        emptyResponse = return ( makeError NoChallenge )
        login' (chal, auth_response) =
            runRequest request' (LJRP.simpleResponseParserWrapper sessionParser)
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
                         unwrapBool (menus request) $ makeTupleSArr "getmenus" "1",
                         unwrapBool (pickws request) $ makeTupleSArr "getpickws" "1",
                         unwrapBool (pickwurls request) $ makeTupleSArr "getpickwurls" "1"
                         ]
                request' = makeRequest params
                initialResponse = LoginResponse "" Anonymous []
                sessionParser = nameValueParser initialResponse $ loginResponseBuilder request
                makeTupleSArr name = (:[]) . (,) name
                unwrapBool cond f  = if cond
                                     then f
                                     else []

loginResponseBuilder :: LJLoginRequest -> ResultBuilder LJLoginResponse
loginResponseBuilder request = ResultBuilder builder
    where
        builder response
                (NameValue ("success","OK")) = response { ljSession = Authenticated $ password request }
        builder response
                (NameValue (name, value)) 
                    | matchIt ( TP.string "access_" >> TP.many TP.digit >> TP.eof ) name = 
                        response { ljCommunities = value:(ljCommunities response) }
                    | otherwise = response
       
matchIt parserSpec src = case parseResult of
                        Right _ -> True
                        Left _  -> False
    where 
        parseResult = TP.parse parserSpec "" src
