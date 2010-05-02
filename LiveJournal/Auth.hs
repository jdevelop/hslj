module LiveJournal.Auth (
    login
)
where 

import LiveJournal.Common
import LiveJournal.Error
import LiveJournal.Transport

login :: String -> String -> IO ( Result Session )
login username password = do
    response <- prepareChallenge password
    login' response
    where
        login' Nothing = return $ Left NoChallenge
        login' (Just (chal, auth_response)) = do
            response <- runRequest [makePair "mode" "login", 
                                    makePair "user" username, 
                                    makePair "auth_method" "challenge", 
                                    makePairBSValue "auth_challenge" chal, 
                                    makePairBSValue "auth_response" auth_response]
            return $ createSession chal auth_response ( responseStatus response ) response
        createSession _ _ Nothing _ = Left WrongResponseFormat
        createSession chal auth_response (Just status) response | status == statusOk = Right (Authenticated password)
                                                                | otherwise          = Left $ getErrorMsgFromResponse response
