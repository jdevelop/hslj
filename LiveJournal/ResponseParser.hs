module LiveJournal.ResponseParser where

import Data.ByteString.UTF8 as BStrU
import Data.List as DL
import Data.Maybe as DM

newtype ResponseParser a = ResponseParser { runParser :: Maybe BStrU.ByteString -> a }

findParameter :: String -> ResponseParser (Maybe String)
findParameter paramName = ResponseParser parserImpl
    where
        parserImpl = DM.maybe Nothing (findMatch . dataLines)
        dataLines = DL.dropWhile ( /= BStrU.fromString paramName ) . BStrU.lines
        findMatch = DM.maybe Nothing (Just . BStrU.toString) . listToMaybe

data ParseStatus = ParseOk | ParseError { message :: String }

newtype ParserState a = ParserState ([ByteString], a, ParseStatus)

newtype NameValue = NameValue (String, String)

newtype ResultBuilder a = ResultBuilder { runNvParser :: a -> NameValue -> a }

errMsgParamName = BStrU.fromString "errmsg"

nameValueParser :: a -> ResultBuilder a -> ResponseParser (ParserState a)
nameValueParser obj builder = ResponseParser parserImpl
    where
        emptyParser = ParserState ([], obj, (ParseError "No input"))
        parserImpl = DM.maybe emptyParser (handleLines obj . BStrU.lines)
        handleLines target (name:value:rest) 
            | name == errMsgParamName =  ParserState (rest, target, (ParseError $ BStrU.toString value))
            | otherwise = handleLines updateTarget rest
            where
                updateTarget = runNvParser builder target (NameValue (BStrU.toString name, BStrU.toString value))
        handleLines target rest = ParserState (rest, target, ParseOk)
