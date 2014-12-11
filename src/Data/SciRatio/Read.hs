{-| Stability: experimental

The functions here parse numbers in a variety of formats.  Examples:

>>> readSciRational "-0.0e+3"         -- result: Just ((0 % 1) .^ 0)
>>> readSciRational "0.25e+2"         -- result: Just ((25 % 1) .^ 0)
>>> readSciRational "-1.0e-1"         -- result: Just (((-1) % 1) .^ (-1))
>>> readSciRational "5.0e+20/6.e0"    -- result: Just ((25 % 3) .^ 19)
>>> readSciRational "0xfeedface"      -- result: Just ((4277009102 % 1) .^ 0)
>>> readSciRational "1e99999999"      -- result: Just ((1 % 1) .^ 99999999)

-}
module Data.SciRatio.Read
       (

         -- * Simple parsers
         readNumber
       , readSciRational
       , readSign

         -- * @'ReadP'@ parsers
       , readNumberP
       , readSciRationalP
       , readScientificP
       , readDecimalP
       , readIntegerP
       , readUnsignedP
       , readHexP
       , readDecP
       , readOctP
       , readBinP

         -- * Character predicates
       , isBinDigit
       , isDecimalExponentSymbol
       , isFractionSlash

       ) where
import Control.Monad (ap, mzero)
import Data.Char (isDigit, isHexDigit, isOctDigit, toLower)
import Data.SciRatio (SciRational)
import Text.ParserCombinators.ReadP (ReadP, (<++))
import Text.Read.Lex (readIntP)
import qualified Text.ParserCombinators.ReadP as P

-- | Read a number (see @'readNumberP'@).
readNumber :: Fractional a => String -> Maybe a
readNumber = runReadP readNumberP

-- | Read a number (see @'readNumberP'@).
readSciRational :: String -> Maybe SciRational
readSciRational = runReadP readSciRationalP

-- | Interpret a sign:
--
--   > sign = [-+]
--
--   Note: @U+2212@ (MINUS SIGN) is also accepted.
readSign :: Num a => Char -> Maybe a
readSign c = case c of
  '+'      -> Just   1
  '-'      -> Just (-1)
  '\x2212' -> Just (-1)
  _        -> Nothing

-- | Read a rational number in scientific notation:
--
--   > number = [0] [bB] [0-1]+
--   >        | [0] [oO] [0-7]+
--   >        | [0] [xX] [0-9a-fA-F]+
--   >        | scientific ( fraction_slash scientific )?
--
readNumberP :: Fractional a => ReadP a
readNumberP = readUnsignedP' <++ readRatioP readScientificP

-- | Read a number (see @'readNumberP'@).
readSciRationalP :: ReadP SciRational
readSciRationalP = readNumberP

-- | Read a ratio of two numbers, each read with the given parser.
readRatioP :: Fractional a =>
              ReadP a           -- ^ Parser for the numerator and denominator.
           -> ReadP a
readRatioP p = fmap (/) p `ap` ((P.satisfy isFractionSlash >> p) <++ return 1)

-- | Read a decimal fraction in scientific notation:
--
--   > scientific = decimal (decimal_exponent_symbol sign? dec)?
--
readScientificP :: Fractional a => ReadP a
readScientificP = readSignedP $ do
  r <- readDecimalP
  e <- (P.satisfy isDecimalExponentSymbol >> readIntegerP) <++ return 0
  return (r * pow10 e)
  where pow10 = (10 ^^) :: Fractional a => Integer -> a

-- | Read a decimal fraction:
--
--   > decimal = sign? ( [0-9]+ [.]? [0-9]*
--   >                 | [.] [0-9]+ )
--
readDecimalP :: Fractional a => ReadP a
readDecimalP = readSignedP $ do
  intPart  <- P.munch isDigit
  string   <- P.look
  case string of
    '.' : _ -> do
      _ <- P.get
      fracPart <- P.munch isDigit
      if length intPart + length fracPart == 0
        then mzero
        else return $ readInt' intPart +
                      readInt' fracPart / 10 ^^ length fracPart
    _ ->
      case intPart of
        "" -> mzero
        _  -> return $ readInt' intPart
  where readInt' "" = 0
        readInt' s  = fromInteger (read s)

-- | Read a signed integer in base 10.
--
--   > integer = sign? [0-9]+
--
readIntegerP :: Num a => ReadP a
readIntegerP = readSignedP readDecP

-- | Read a unsigned number in either binary (@0b@), octal (@0o@), decimal,
--   or hexadecimal (@0x@) format:
--
--   > unsigned = [0] [bB] [0-1]+
--   >          | [0] [oO] [0-7]+
--   >          | [0] [xX] [0-9a-fA-F]+
--   >          | dec
--
readUnsignedP :: Num a => ReadP a
readUnsignedP = readUnsignedP' <++ readDecP

-- | Read a unsigned number in either binary (@0b@), octal (@0o@), or
--   hexadecimal (@0x@) format, but /not/ in decimal format.  The prefix is
--   not case-sensitive.
readUnsignedP' :: Num a => ReadP a
readUnsignedP' = do
  _          <- P.char '0'
  prefixChar <- P.get
  fromInteger `fmap`
    case toLower prefixChar of
      'b' -> readBinP
      'o' -> readOctP
      'x' -> readHexP
      _   -> mzero

-- | Read a number preceded by an optional sign.
readSignedP :: Num a => ReadP a -> ReadP a
readSignedP p = do
  sign <- (<++ return 1) $ do
    c <- P.get
    case readSign c of
      Just x  -> return x
      Nothing -> mzero
  num <- p
  return (sign * num)

-- | Read an unsigned integer in hexadecimal notation:
--
--   > hex = [0-9A-Fa-f]+
--
readHexP :: Num a => ReadP a
readHexP = readIntP 16 isHexDigit digitToInt
  where digitToInt c = case fromEnum c of
          c' | c <= '9'  -> c' -  fromEnum '0'
             | c <= 'F'  -> c' - (fromEnum 'A' - 10)
             | otherwise -> c' - (fromEnum 'a' - 10)

-- | Read an unsigned integer in base 10.
--
--   > dec = [0-9]+
--
--   Note: Although similar functions exist in @'Text.Read.Lex'@, the versions
--         here do not require @'Eq'@.
readDecP :: Num a => ReadP a
readDecP = readIntP 10 isDigit digitToInt
  where digitToInt c = fromEnum c - fromEnum '0'

-- | Read an unsigned integer in octal notation:
--
--   > oct = [0-7]+
--
readOctP :: Num a => ReadP a
readOctP = readIntP 8 isOctDigit digitToInt
  where digitToInt c = fromEnum c - fromEnum '0'

-- | Read an unsigned integer in binary notation:
--
--   > bin = [01]+
--
readBinP :: Num a => ReadP a
readBinP = readIntP 2 isBinDigit digitToInt
  where digitToInt c = fromEnum c - fromEnum '0'

-- | Run a @'ReadP'@ parser to the very end.  Only a single, unique parse is
--   accepted and it must consume the entire input.
runReadP :: ReadP a -> String -> Maybe a
runReadP p s = case P.readP_to_S p s of
  [(x, [])] -> Just x
  _         -> Nothing

-- | Whether the character is a binary digit:
--
--   > bin_digit = [01]
--
isBinDigit :: Char -> Bool
isBinDigit c = fromEnum '0' <= x && x <= fromEnum '1' where x = fromEnum c

-- | Whether the character can be used as an exponent symbol in scientific
--   notation:
--
--   > decimal_exponent_symbol = [eE]
--
--   Note: @U+23E8@ (DECIMAL EXPONENT SYMBOL) is also accepted.
isDecimalExponentSymbol :: Char -> Bool
isDecimalExponentSymbol = (`elem` "eE\x23e8")

-- | Whether the character is a fraction slash:
--
--   > fraction_slash = [/]
--
--   Note: @U+2044@ (FRACTION SLASH) and @U+2215@ (DIVISION SLASH) are also
--         accepted.
isFractionSlash :: Char -> Bool
isFractionSlash = (`elem` "/\x2044\x2215")
