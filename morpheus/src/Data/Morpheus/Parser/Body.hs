{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Body
    ( body
    )
where

import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
import           Data.Attoparsec.Text           ( Parser
                                                , char
                                                , letter
                                                , sepBy
                                                , skipSpace
                                                , try
                                                , parseOnly
                                                , parse
                                                , IResult(Done)
                                                , string
                                                , endOfInput
                                                )
import           Control.Applicative            ( (<|>)
                                                , many
                                                , some
                                                )
import           Data.Morpheus.Types.Types      ( QuerySelection(..)
                                                , SelectionSet
                                                , Arguments(..)
                                                )
import           Data.Morpheus.Parser.Arguments ( arguments )
import           Data.Morpheus.Parser.Primitive ( token
                                                , separator
                                                , getPosition
                                                )


spread :: Parser (Text, QuerySelection)
spread = do
    skipSpace
    index <- getPosition
    string "..."
    key <- some (letter <|> char '_')
    return (pack key, Spread (pack key) index)

entry :: Parser (Text, QuerySelection)
entry = do
    skipSpace
    index <- getPosition
    key   <- token
    args  <- try arguments <|> pure []
    value <- (try $ body args) <|> (pure $ Field args key index)
    return (key, value)

seperated x = x `sepBy` separator

body :: Arguments -> Parser QuerySelection
body args = do
    skipSpace
    index <- getPosition
    char '{'
    skipSpace
    entries <- seperated $ entry <|> spread
    skipSpace
    char '}'
    return (SelectionSet args entries index)