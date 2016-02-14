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
import Data.Char (isAlpha, isAlphaNum, ord, toLower)
import Data.Maybe (fromJust)

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
iniKey = (toKey . C.unpack) <$> p
  where p = C.cons <$> AC.satisfy isAlpha
                   <*> AC.takeWhile (\c -> isAlphaNum c || c == '-')

eofOrWs :: Parser ()
eofOrWs = AC.endOfInput <|> space *> return ()

iniVal :: Parser INIVal
iniVal = wsOrLn *> char '=' *> wsOrLn *> value <* wsOrLn
  where value  = IBool <$> bool <|> IInt <$> int <|> IString <$> bstring
        wsOrLn = let ws = space *> return ()
                     ln = string "\\\n" *> return()
                 in many (ws <|> ln)

bool :: Parser Bool
bool = posTok *> eofOrWs *> return True <|>
       negTok *> eofOrWs *> return False
  where posTok = stringCI "true" <|> stringCI "yes" <|> stringCI "on"
        negTok = stringCI "false" <|> stringCI "no" <|> stringCI "off"

int :: Parser Integer
int = do
  v <- signed decimal
  u <- pUnit
  return (v * u)
  where
    pUnit = eofOrWs *> return 1 <|> pUnit' <* eofOrWs
      where
        base   = (2 :: Integer) ^ (10 :: Integer)
        unit   = "kMGTPE"
        units  = M.fromList . zip unit $ iterate (base *) base
        pUnit' = do
          c <- AC.satisfy (AC.inClass unit)
          return $ fromJust (M.lookup c units)

bstring :: Parser B.ByteString
bstring = do
  content <- many (nquote <|> escape <|> quoted)
  return . fst . C.spanEnd isSpace . C.concat $ content
  where
    -- Fast byte version of @notInClass@
    notAny s w = all (w /=) $ map (fromIntegral . ord) s
    -- Content without quotes or escaping
    nquote = P.takeWhile1 (notAny ";#\"\\\n")
    -- Escaped sequence
    escape = char '\\' *> do
      c <- peekChar
      case c of
        Just '\n' -> char '\n' *> return C.empty
        _         -> do
          c' <- choice $ map char "\\\"ntb"
          return $ C.cons '\\' (C.singleton c')
    -- Quoted content
    quoted = do
      c <- char '\"' *> many' (ch <|> escape) <* char '\"'
      return $ C.concatMap unesp (C.concat c)
      where ch = P.takeWhile1 (notAny "\\\"")
    -- Unescape sequence
    unesp '\n' = "\\n"
    unesp '\t' = "\\t"
    unesp '\b' = "\\b"
    unesp x    = C.singleton x
