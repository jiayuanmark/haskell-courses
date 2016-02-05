{-# LANGUAGE OverloadedStrings #-}

module ParseIni
    ( INISectName (..)
    , INIKey
    , INIVal (..)
    , INISection
    , INIFile
    , parseIniFile
    , toSectName
    , toKey
    , lookupSection
    , lookupValue
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import Data.Char (toLower, isAlpha, isAlphaNum)

-- **** TYPES ****
-- These are the types you should use for the results of your parse.
-- Think carefully about what's going on here!

-- |INI files are separated into sections and subsections of key-value pairs.
-- We represent section and subsection identifiers with the INISectName type.
-- Section names are case insensitive strings; subsection names are case sensitive.
data INISectName = ISect    { iSect    :: B.ByteString }
                 | ISubsect { iSect    :: B.ByteString
                            , iSubsect :: B.ByteString }
    deriving (Eq, Ord, Show)

-- |Within each (sub)section, an INI file contains a set of keys and values.
-- Keys are case insensitive strings.
type INIKey = B.ByteString

-- |After parsing key-value pairs, each value should be assigned a type.
-- We represent these types via the @INIVal@ sum type.
data INIVal = IBool Bool
            | IInt Integer
            | IString B.ByteString
    deriving (Eq, Ord, Show)

-- |An @INISection@ is a map from @INIKey@s to @INIVal@s.
type INISection = M.Map INIKey [INIVal]

-- |An @INIFile@ is a map from @INISectName@s to @INISection@s.
type INIFile = M.Map INISectName INISection


-- **** INTERFACE ****
-- You need to implement these so that we can test your code!
--
-- Why? Because you shouldn't need to expose exactly the way that
-- you handle, e.g., case insensitive string matching in order for
-- someone to use your INI file parser.

-- |Given a section name and possibly a subsection name, return an
-- appropriate @INISectName@. This function accounts for the case
-- insensitivity of the section name.
toSectName :: String -> Maybe String -> INISectName
toSectName sec (Just sub) = ISubsect (C.pack (lowercase sec)) (C.pack sub)
toSectName sec Nothing    = ISect . C.pack . lowercase $ sec

-- |Given a key name, return an appropriate @INIKey@. This function
-- accounts for the case insensitivity of the key name.
toKey :: String -> INIKey
toKey = C.pack . lowercase

-- |Look up a section in an @INIFile@.
lookupSection :: INISectName -> INIFile -> Maybe INISection
lookupSection = M.lookup

-- |Look up a value in an @INISection@.
lookupSValue :: INIKey -> INISection -> Maybe [INIVal]
lookupSValue = M.lookup

-- |Look up a value in an @INIFile@.
lookupValue :: INIKey -> INISectName -> INIFile -> Maybe [INIVal]
lookupValue key sec file = lookupSection sec file >>= lookupSValue key

lowercase :: String -> String
lowercase = map toLower

-- **** PARSER ****

-- |Parse an INI file into an @INIFile@.
--
-- An INI file comprises a sequence of sections.
--
-- A section starts with a header, which declares the name of the section or subsection.
-- The header is followed by a sequence of key-value declarations.
--
-- Whitespace between and inside sections is ignored, as are comment lines, which
-- begin with @#@ or @;@.
parseIniFile :: B.ByteString -> Either String INIFile
parseIniFile = const $ Right M.empty

-- Your implementation goes here.
--
-- parseIniFile should return @Left errmsg@ on error,
-- or @Right parsedResult@ on success.

spaces :: Parser ()
spaces = P.skipWhile isHorizontalSpace

comments :: Parser ()
comments = (char '#' <|> char ';') *>
           P.skipWhile (not . isEndOfLine) *>
           endOfLine

sectionName :: Parser String
sectionName = many1 (AC.satisfy (\c -> isAlphaNum c || AC.inClass "-." c))

subsectionName :: Parser String
subsectionName = char '\"' *> many' (esp <|> tok) <* char '\"'
  where tok = AC.satisfy (AC.notInClass "\"\n")
        esp = char '\\' *> AC.satisfy (AC.inClass "\\\"")

iniSecName :: Parser INISectName
iniSecName = do
  sec <- char '[' *> spaces *> sectionName <* spaces
  c   <- peekChar
  case c of
    Just ']'  -> char ']' *> return (toSectName sec Nothing)
    Just '\"' -> do subsec <- subsectionName <* spaces <* char ']'
                    return $ toSectName sec (Just subsec)
    _         -> fail "cannot parse section!"

iniKey :: Parser INIKey
iniKey = spaces *> p <* spaces
  where p = C.cons <$> AC.satisfy isAlpha
                   <*> AC.takeWhile (\c -> isAlphaNum c || c == '-')

iniVal :: Parser INIVal
iniVal = undefined
