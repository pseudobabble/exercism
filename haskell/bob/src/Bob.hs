{-# LANGUAGE ImportQualifiedPost #-}

module Bob (responseFor) where

import Data.Char (isAlpha, isDigit, isLetter, isPunctuation, isSpace, isUpper)
import Data.Text (Text, filter, isSuffixOf, pack, unpack)
import Data.Text qualified as T

charPresent :: (Char -> Bool) -> Text -> Bool
charPresent function text = any function (unpack text)

anyDigits :: Text -> Bool
anyDigits text = charPresent isDigit text

anyPunctuation :: Text -> Bool
anyPunctuation text = charPresent isPunctuation text

anyLetters :: Text -> Bool
anyLetters text = charPresent isLetter text

isQuestion :: Text -> Bool
isQuestion text = isSuffixOf (pack "?") text

isYell :: Text -> Bool
isYell text = T.all isUpper (T.filter (\c -> isAlpha c) text)

isYelledQuestion :: Text -> Bool
isYelledQuestion text = isYell text && isQuestion text

isNothing :: Text -> Bool
isNothing text = text == pack ""

isAnyWhitespace :: Text -> Bool
isAnyWhitespace text = all isSpace (unpack text)

isSilence :: Text -> Bool
isSilence text = isNothing text || isAnyWhitespace text

responseFor :: Text -> Text
responseFor statement
  | isQuestion statement = pack "Sure."
  | isYelledQuestion statement = pack "Calm down, I know what I'm doing!"
  | isYell statement = pack "Whoa, chill out!"
  | isSilence statement = pack "Fine. Be that way!"
  | otherwise = pack "Whatever."
