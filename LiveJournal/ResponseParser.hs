{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts #-}

module LiveJournal.ResponseParser (
    parseResponse,
    ObjectUpdater,
    ObjectFactory,
    ParseResult
) 
where

import Text.Parsec as TP
import Text.Parsec.Error
import Text.Parsec.ByteString
import Control.Monad as CM
import Data.Map as DM
import Data.List as DL
import Data.ByteString.UTF8
import Control.Applicative hiding ((<|>))

import LiveJournal.Error
import LiveJournal.Entity

type ObjectFactory b = String -> Maybe b
type ObjectUpdater b = String -> String -> String -> b -> Maybe b
type ParseResult a b = (DM.Map String String, DM.Map String (DM.Map Int [a]), DM.Map String ( DM.Map Int b ) )

data ResponseParserState a b = RPS { simpleMap :: DM.Map String String,
                                     listMap :: DM.Map String (DM.Map Int [a]),
                                     objectMap :: DM.Map String (DM.Map Int b),
                                     newObjectF :: ObjectFactory b,
                                     updateObjectF :: ObjectUpdater b
                                   }

notNewlineP = TP.noneOf "\r\n"

enumeratedParser :: (Stream s m Char) => ParsecT s (ResponseParserState String b) m ()
enumeratedParser = do 
    paramName <- TP.many (TP.noneOf "_\r\n")
    TP.char '_'
    idx <- liftM read $ TP.many TP.digit
    TP.newline
    paramValue <- TP.many notNewlineP
    TP.optional TP.newline
    currentState <- TP.getState
    let listMap' = DM.alter ( updateMapKey idx paramValue ) paramName $ listMap currentState
    TP.putState $ currentState { listMap = listMap' }
    where
        updateMapKey idx value (Just curMap)= Just $ DM.alter (alterArray value) idx curMap
        updateMapKey idx value Nothing = Just $ singleton idx [value]
        alterArray value Nothing = Just [value]
        alterArray value (Just items) = Just (value:items)

primitiveParser :: (Stream s m Char) => ParsecT s (ResponseParserState a b) m ()
primitiveParser = do
    paramName <- TP.many notNewlineP
    TP.newline
    paramValue <- TP.many notNewlineP
    TP.optional TP.newline
    currentState <- TP.getState
    let simpleMap' = DM.insert paramName paramValue $ simpleMap currentState
    TP.putState $ currentState { simpleMap = simpleMap' }

objectParamParser = do
    objectType <- TP.many (TP.noneOf "_")
    TP.char '_'
    objectId <- CM.liftM ( read :: String -> Int ) (TP.many TP.digit)
    TP.char '_'
    propertyName <- TP.many notNewlineP
    TP.newline
    propertyValue <- TP.many notNewlineP
    currentState <- TP.getState
    TP.optional TP.newline
    let tmpMap = objectMap currentState
        objectMap' = DM.alter ( updateMapKey currentState objectType objectId propertyName propertyValue ) objectType tmpMap
    TP.putState $ currentState { objectMap = objectMap' }
    where
        updateMapKey (RPS _ _ _ newObject updateObject) 
                     objectType objectId propertyName propertyValue
                     Nothing = newObject objectType >>= 
                               updateObject objectType propertyName propertyValue >>= 
                               Just . DM.singleton objectId
        updateMapKey (RPS _ _ _ newObject updateObject) 
                     objectType objectId propertyName propertyValue 
                     (Just objTypeMap) = 
                        Just $ DM.alter ( updExistingObj ) 
                               objectId objTypeMap
                    where
                        newObjectInst = newObject objectType
                        updExistingObj Nothing = newObjectInst >>= updObjHint
                        updExistingObj (Just obj) =  updObjHint obj
                        updObjHint = updateObject objectType propertyName propertyValue

responseParser =
    try objectParamParser <|> (try enumeratedParser <|> primitiveParser)

finishData = do
    (RPS simpleMap listMap objectMap _ _ ) <- TP.getState
    return (simpleMap, listMap, objectMap)

parseResponse :: (Stream s [] Char) => ObjectFactory b -> ObjectUpdater b -> s -> Result (ParseResult String b)
parseResponse newObject updateObject = 
    handleParseResult . parsecResult
    where
        parsecResult = head . TP.runPT (TP.manyTill responseParser TP.eof >> finishData) 
            (RPS DM.empty DM.empty DM.empty newObject updateObject) ""
        handleParseResult (Left parseError) = makeError $ 
            WrongResponseFormat ( concatMap ( (++ "\n") . messageString ) $ errorMessages parseError )
        handleParseResult (Right result) = makeResult result
