module LiveJournal.Login (
    login
)
where 

import LiveJournal.Entity
import LiveJournal.Error
import LiveJournal.Session
import LiveJournal.Transport
import LiveJournal.Request
import LiveJournal.ResponseParser as LJRP

import Control.Applicative

import Data.Maybe as DM

login :: String -> String -> IO ( Result Session )
login username password =
    prepareChallenge password >>= DM.maybe emptyResponse login'
    where
        emptyResponse = return ( makeError NoChallenge )
        login' (chal, auth_response) =
            runRequest request (LJRP.simpleResponseParserWrapper sessionParser)
            where
                request = makeRequest [
                                    ("mode","login"), 
                                    ("user",username), 
                                    ("auth_method","challenge"),
                                    ("auth_challenge",chal),
                                    ("auth_response",auth_response)
                                   ]
                sessionParser = nameValueParser Anonymous (ResultBuilder builder)
                builder _ (NameValue ("success","OK")) = Authenticated password
