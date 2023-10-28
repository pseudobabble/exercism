{-# LANGUAGE OverloadedStrings #-}

module Pangram (isPangram) where

import Data.Char (isAscii, isDigit, isPunctuation)
import Data.List (nub, sort)
import Data.Text as T (Text, filter, pack, toLower, unpack)

-- remove spaces
-- lowercase
-- sort
-- compare to abc..xyz

letters :: Text
letters = "abcdefghijklmnopqrstuvwxyz" :: Text

removeChar :: Char -> Text -> Text
removeChar char text = T.filter (/= char) text

isNotDigit :: Char -> Bool
isNotDigit c = not (isDigit c)

removeDigit :: Text -> Text
removeDigit text = T.filter isNotDigit text

isNotPunctuation :: Char -> Bool
isNotPunctuation c = not (isPunctuation c)

removePunctuation :: Text -> Text
removePunctuation text = T.filter isNotPunctuation text

keepAscii :: Text -> Text
keepAscii text = T.filter isAscii text

stripUnwantedChars :: Text -> Text
stripUnwantedChars text =
  removeChar '.' (removeChar '\\' (removeChar '"' (removeChar '_' (removeChar ' ' (keepAscii (removePunctuation (removeDigit text)))))))

lowercase :: Text -> Text
lowercase text = toLower text

sortText :: Text -> Text
sortText text = pack (sort (unpack text))

uniqueText :: Text -> Text
uniqueText text = pack (nub (unpack text))

isPangram :: Text -> Bool
isPangram text = uniqueText (sortText (lowercase (stripUnwantedChars text))) == letters

-- isPangram text = nub (sort (lowercase (removeChar text))) == letters
