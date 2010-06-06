module LiveJournal.ResponseParser where

import Data.ByteString.UTF8 as BStrU
import Data.List as DL
import Data.Maybe as DM
import LiveJournal.Entity
import LiveJournal.Error

newtype ResponseParser a = ResponseParser { runParser :: Maybe BStrU.ByteString -> a }

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

simpleResponseParserWrapper :: ResponseParser (ParserState a) -> ResponseParser (Result a)
simpleResponseParserWrapper oldParser = ResponseParser ( transformResponse . runParser oldParser )
    where
        transformResponse (ParserState (_,src,ParseOk)) = Result $ Right src
        transformResponse (ParserState (_,_,ParseError msg)) =Result . Left $ SimpleError msg
