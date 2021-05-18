{-# LANGUAGE LambdaCase #-}
module Frontend.ParseAttribute (readAttribute) where

import Text.ParserCombinators.Parsec

import Attribute (Attribute(..))

readAttribute :: String -> Maybe Attribute
readAttribute = toMaybe . parse parseAttributeString "attribute"
    where
        toMaybe = either (const Nothing) Just

parseAttributeString :: Parser Attribute
parseAttributeString = do
    _ <- char '['
    spaces
    keywordCtr <- parseKeyword
    spaces
    _ <- char '='
    spaces
    value <- parseValue
    spaces
    _ <- char ']'
    return $ keywordCtr value

parseKeyword :: Parser (Int -> Attribute)
parseKeyword = many (letter <|> digit) >>= \case
    "location" -> return Location
    "binding"  -> return Binding
    _ -> fail "Unexpected keyword"

parseValue :: Parser Int
parseValue = read <$> many1 digit
