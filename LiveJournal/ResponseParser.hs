{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts #-}

module LiveJournal.ResponseParser (
    parseResponse,
    ObjectUpdater,
    ObjectFactory,
    ParseResult
) 
where

import Text.Parsec as TP
import Text.Parsec.ByteString
import Control.Monad as CM
import Data.Map as DM
import Data.List as DL
import Data.ByteString.UTF8
import Control.Applicative hiding ((<|>))

import LiveJournal.Error

type ObjectFactory b = String -> Maybe b
type ObjectUpdater b = String -> String -> b -> Maybe b
type ParseResult a b = (DM.Map String String, DM.Map String [a], DM.Map String ( DM.Map Int b ) )

data ResponseParserState a b = RPS { simpleMap :: DM.Map String String,
                                     listMap :: DM.Map String [a],
                                     objectMap :: DM.Map String (DM.Map Int b),
                                     newObjectF :: ObjectFactory b,
                                     updateObjectF :: ObjectUpdater b
                                   }

notNewlineP = TP.noneOf "\r\n"

enumeratedParser :: (Stream s m Char) => ParsecT s (ResponseParserState String b) m ()
enumeratedParser = do 
    paramName <- TP.many (TP.noneOf "_")
    TP.char '_'
    TP.many TP.digit
    TP.newline
    paramValue <- TP.many notNewlineP
    currentState <- TP.getState
    let listMap' = DM.alter ( updateMapKey paramValue ) paramName $ listMap currentState
    TP.putState $ currentState { listMap = listMap' }
    where
        updateMapKey value (Just params) = Just (value:params)
        updateMapKey value Nothing = Just [value]

primitiveParser :: (Stream s m Char) => ParsecT s (ResponseParserState a b) m ()
primitiveParser = do
    paramName <- TP.many notNewlineP
    TP.newline
    paramValue <- TP.many notNewlineP
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
    let tmpMap = objectMap currentState
        objectMap' = DM.alter ( updateMapKey currentState objectType objectId propertyName propertyValue ) objectType tmpMap
    TP.putState $ currentState { objectMap = objectMap' }
    where
        updateMapKey (RPS _ _ _ newObject updateObject) 
                     objectType objectId propertyName propertyValue
                     Nothing = newObject objectType >>= 
                               updateObject propertyName propertyValue >>= 
                               Just . DM.singleton objectId
        updateMapKey (RPS _ _ _ newObject updateObject) 
                     objectType objectId propertyName propertyValue 
                     (Just objTypeMap) = 
                        Just $ DM.alter ( updExistingObj ) 
                               objectId objTypeMap
                    where
                        newObjectInst = newObject objectType
                        updExistingObj Nothing = newObjectInst >>= updateObject propertyName propertyValue
                        updExistingObj (Just obj) = updateObject propertyName propertyValue obj

responseParser =
    try ( objectParamParser <|> enumeratedParser <|> primitiveParser ) *> TP.optional TP.newline

finishData = do
    (RPS simpleMap listMap objectMap _ _ ) <- TP.getState
    return (simpleMap, DM.map DL.reverse listMap, objectMap)

parseResponse :: (Stream s [] Char) => ObjectFactory b -> ObjectUpdater b -> s -> Either LJError (ParseResult String b)
parseResponse newObject updateObject = 
    handleParseResult . parsecResult
    where
        parsecResult = head . TP.runPT (TP.manyTill responseParser TP.eof >> finishData) 
            (RPS DM.empty DM.empty DM.empty newObject updateObject) ""
        handleParseResult (Left parseError) = Left WrongResponseFormat
        handleParseResult (Right result) = Right result
