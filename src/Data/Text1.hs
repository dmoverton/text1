{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Text1
  (
  -- * Types
    Text1

  -- * Creation and elimination
  , unpack
  , singleton
  , text1ToText
  , textToText1
  , textToText1M

  -- * Basic interface
  , cons
  , cons'
  , snoc
  , snoc'
  , append
  , uncons
  , unsnoc
  , head
  , last
  , tail
  , init
  , length
  , compareLength

  -- * Transformations
  , map
  , intercalate
  , intersperse
  , transpose
  , reverse
  , replace

  -- ** Case conversion
  -- $case
  , toCaseFold
  , toLower
  , toUpper
  , toTitle

  -- ** Justification
  , justifyLeft
  , justifyRight
  , centre
  , center

  -- * Folds
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr1

  -- ** Special folds
  , concat
  , concatMap
  , any
  , all
  , maximum
  , minimum

  -- ** Scans
  , scanl
  , scanl1
  , scanr
  , scanr1

  -- ** Accumulating maps
  , mapAccumL
  , mapAccumR

  -- ** Generation and unfolding
  , replicate
  , replicate'
  , unfoldr
  , unfoldrN

  -- ** Breaking strings
  , take
  , take'
  , takeEnd
  , takeEnd'
  , drop
  , dropEnd
  , takeWhile
  , takeWhileEnd
  , dropWhile
  , dropWhileEnd
  , dropAround
  , strip
  , stripStart
  , stripEnd
  , splitAt
  -- , breakOn
  -- , breakOnEnd
  , break
  , span
  -- , group
  -- , groupBy
  -- , inits
  -- , tails

  -- ** Breaking into many substrings
  -- $split
  , splitOn
  -- , split
  -- , chunksOf

  -- ** Breaking into lines and words
  , lines
  , words
  , unlines
  , unwords

  -- * Predicates
  , isPrefixOf
  , isSuffixOf
  , isInfixOf

  -- -- ** View patterns
  -- , stripPrefix
  -- , stripSuffix
  -- , commonPrefixes

  -- * Searching
  , filter
  -- , breakOnAll
  , find
  , partition

  -- * Indexing
  -- $index
  , index
  , index'
  , findIndex
  , count

  -- * Zipping
  , zip
  , zipWith

-- existing stuff
  , ToText1(..)
  , toText
  , toString
  , FromText1(..)
  , fromText1
  , fromText1M
  , fromText
  , fromTextM
  , fromString
  , fromStringM
  , takeText1
  , takeCIText1
  , CI(..)
  , fromText1Error
  , unsafeFromText
  , Text
  , asText1
  , _Text1
  , AsText1(..)
  ) where

import           Control.DeepSeq (NFData(..))
import           Control.Lens (Iso', Prism', iso, prism')
import           Control.Monad ((<=<))
import           Control.Monad.Fail (MonadFail(..))
import           Data.Aeson
                  (FromJSON(..), FromJSONKey(..), FromJSONKeyFunction(..), ToJSON(..),
                  ToJSONKey(..), Value(..), withText)
import           Data.Aeson.Types (toJSONKeyText)
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import           Data.Bifunctor (first, second)
import           Data.Bifunctor (bimap)
import           Data.CaseInsensitive (CI(..), FoldCase(..))
import qualified Data.CaseInsensitive as CI
import           Data.Coerce (coerce)
import           Data.Csv (FromField(..), ToField(..))
import           Data.Data (Data)
import           Data.Fixed (Fixed, HasResolution)
import           Data.Hashable (Hashable(..))
import           Data.Int (Int64)
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import           Data.Scientific (Scientific)
import           Data.Semigroup (sconcat)
import           Data.String (IsString)
import qualified Data.String as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
import           Data.These (These(..))
import           Data.Typeable (Typeable)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           GHC.Generics (Generic)
import           GHC.Read (Read(..), lexP, parens)
import           GHC.Stack (HasCallStack)
import           Network.AWS.Data.Numeric (Nat)
import           Network.AWS.Data.Text (FromText(..))
import qualified Network.AWS.Data.Text as AWS
import           Numeric.Natural (Natural)
import           Numeric.Positive (Positive)
import           Prelude
                  (Bool(..), Char, Double, Either(..), Eq(..), Int, Integer, Maybe(..), Ord(..),
                  Ordering(..), Semigroup(..), Show(..), String, either, error, fmap, fromIntegral,
                  id, maybe, not, otherwise, pure, ($), (.), (<$>), (<*), (<*>), (=<<))
import           Servant.API (FromHttpApiData(..), ToHttpApiData(..))
import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Instances ()
import           Text.Read.Lex (Lexeme(Ident))

newtype Text1 = UnsafeText1 Text
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Hashable Text1 where
  hashWithSalt salt = hashWithSalt salt . text1ToText

instance NFData Text1 where
  rnf !_ = ()

instance Show Text1 where
  show = show . text1ToText

instance Read Text1 where
  readPrec = parens $ do
    Ident s <- lexP
    textToText1M $ T.pack s

instance IsString Text1 where
  fromString = \case
    ""  -> error "IsString: Text1 from empty string literal."
    str -> coerce T.pack str
  {-# INLINE fromString #-}

instance Semigroup Text1 where
  (<>) = coerce ((<>) @Text)
  {-# INLINE (<>) #-}

instance Arbitrary Text1 where
  arbitrary =
    cons'
      <$> arbitrary
      <*> arbitrary
  shrink = mapMaybe textToText1M . shrink . text1ToText

instance ToField Text1 where
  toField = encodeUtf8 . text1ToText

instance FromField Text1 where
  parseField = textToText1M <=< either (fail . show) pure . decodeUtf8'

instance {-# OVERLAPPING #-} FromField (Maybe Text1) where
  parseField = fmap textToText1M . parseField
  {-# INLINE parseField #-}

instance ToJSONKey Text1 where
  toJSONKey = toJSONKeyText text1ToText

instance FromJSONKey Text1 where
  fromJSONKey = FromJSONKeyTextParser fromTextM

instance ToJSON Text1 where
  toJSON = String . text1ToText

instance FromJSON Text1 where
  parseJSON = withText "Text1" textToText1M

instance ToHttpApiData Text1 where
  toUrlPiece = text1ToText

instance FromHttpApiData Text1 where
  parseUrlPiece = first T.pack . fromText

instance FoldCase Text1 where
  foldCase = toCaseFold

-- | /O(n)/ Convert a 'Text1' into a 'String'.
unpack :: Text1 -> String
unpack = coerce T.unpack
{-# INLINE unpack #-}

-- | /O(1)/ Convert a character into a Text1.
-- Performs replacement on invalid scalar values.
singleton :: Char -> Text1
singleton = coerce T.singleton
{-# INLINE singleton #-}

-- | /O(1)/ Convert a 'Text1' into a 'Text'.
text1ToText :: Text1 -> Text
text1ToText = coerce
{-# INLINE text1ToText #-}

-- | /O(1)/ Convert a 'Text' into a 'Text1'.
textToText1 :: Text -> Either String Text1
textToText1 text
  | T.null text = Left "Failure parsing Text1 from empty text."
  | otherwise = Right $ coerce text
{-# INLINE textToText1 #-}

-- | /O(1)/ Convert a 'Text' into a 'Text1'.
textToText1M :: MonadFail m => Text -> m Text1
textToText1M = either fail pure . textToText1
{-# INLINE textToText1M #-}

-- | /O(n)/ Adds a character to the front of a 'Text1'.  This function
-- is more costly than its 'List' counterpart because it requires
-- copying a new array.  Performs replacement on invalid scalar values.
cons :: Char -> Text1 -> Text1
cons = coerce T.cons
{-# INLINE cons #-}

infixr 5 `cons`

-- | /O(n)/ Adds a character to the front of a 'Text'.  This function
-- is more costly than its 'List' counterpart because it requires
-- copying a new array.  Performs replacement on invalid scalar values.
cons' :: Char -> Text -> Text1
cons' = coerce T.cons
{-# INLINE cons' #-}

infixr 5 `cons'`

-- | /O(n)/ Adds a character to the end of a 'Text'.  This copies the
-- entire array in the process.
-- Performs replacement on invalid scalar values.
snoc :: Text1 -> Char -> Text1
snoc text1 char = coerce (`T.snoc` char) text1
{-# INLINE snoc #-}

-- | /O(n)/ Adds a character to the end of a 'Text'.  This copies the
-- entire array in the process.
-- Performs replacement on invalid scalar values.
snoc' :: Text1 -> Char -> Text1
snoc' text char = coerce (`T.snoc` char) text
{-# INLINE snoc' #-}

-- | /O(n)/ Appends one 'Text1' to the other by copying both of them
-- into a new 'Text1'.
append :: Text1 -> Text1 -> Text1
append text1 = coerce (T.append (text1ToText text1))

-- | Returns the first character and rest of a 'Text1'.
uncons :: Text1 -> (Char, Maybe Text1)
uncons = maybe (error errorMessage) (second textToText1M) . coerce T.uncons
  where errorMessage = "Text1.uncons was empty - this should be impossible"
{-# INLINE uncons #-}

-- | /O(1)/ Returns all but the last character and the last character of a
-- 'Text1'.
unsnoc :: Text1 -> (Maybe Text1, Char)
unsnoc = maybe (error errorMessage) (first textToText1M) . coerce T.unsnoc
  where errorMessage = "Text1.unsnoc was empty - this should be impossible"
{-# INLINE unsnoc #-}

-- | /O(1)/ Returns the first character of a 'Text1'.
head :: Text1 -> Char
head = coerce T.head
{-# INLINE head #-}

-- | /O(1)/ Returns the last character of a 'Text1'.
last :: Text1 -> Char
last = coerce T.last
{-# INLINE last #-}

-- | /O(1)/ Returns all characters after the head of a 'Text1'.
tail :: Text1 -> Maybe Text1
tail = textToText1M . coerce T.tail
{-# INLINE tail #-}

-- | /O(1)/ Returns all but the last character of a 'Text1'.
init :: Text1 -> Maybe Text1
init = textToText1M . coerce T.init
{-# INLINE init #-}

-- | /O(n)/ Returns the number of characters in a 'Text1'.
length :: Text1 -> Int
length = coerce T.length
{-# INLINE length #-}

-- | /O(n)/ Compare the count of characters in a 'Text1' to a number.
--
-- This function gives the same answer as comparing against the result
-- of 'length', but can short circuit if the count of characters is
-- greater than the number, and hence be more efficient.
compareLength :: Text1 -> Int -> Ordering
compareLength text1 n = coerce (`T.compareLength` n) text1
{-# INLINE compareLength #-}

-- | /O(n)/ 'map' @f@ @t@ is the 'Text1' obtained by applying @f@ to
-- each element of @t@.
--
-- Example:
--
-- >>> let message = "I am not angry. Not at all."
-- >>> T.map (\c -> if c == '.' then '!' else c) message
-- Just "I am not angry! Not at all!"
--
-- Performs replacement on invalid scalar values.
map :: (Char -> Char) -> Text1 -> Text1
map f = coerce (T.map f)
{-# INLINE map #-}


-- | /O(n)/ The 'intercalate' function takes a 'Text1' and a list of
-- 'Text1's and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- Example:
--
-- >>> T.intercalate "NI!" $ "We" :| ["seek", "the", "Holy", "Grail"]
-- "WeNI!seekNI!theNI!HolyNI!Grail"
intercalate :: Text1 -> NonEmpty Text1 -> Text1
intercalate t = concat . NE.intersperse t
{-# INLINE intercalate #-}

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'Text1'.
--
-- Example:
--
-- >>> T.intersperse '.' "SHIELD"
-- "S.H.I.E.L.D"
--
-- Performs replacement on invalid scalar values.
intersperse :: Char -> Text1 -> Text1
intersperse char = coerce (T.intersperse char)
{-# INLINE intersperse #-}

-- | /O(n)/ The 'transpose' function transposes the rows and columns
-- of its 'Text1' argument.  Note that this function uses 'pack',
-- 'unpack', and the list version of transpose, and is thus not very
-- efficient.
--
-- Examples:
--
-- >>> transpose ["green","orange"]
-- ["go","rr","ea","en","ng","e"]
--
-- >>> transpose ["blue","red"]
-- ["br","le","ud","e"]
transpose :: [Text1] -> [Text1]
transpose = coerce T.transpose

-- | /O(n)/ Reverse the characters of a `Text1`.
--
-- Example:
--
-- >>> T.reverse "desrever"
-- "reversed"
reverse :: Text1 -> Text1
reverse = coerce T.reverse
{-# INLINE reverse #-}

-- | /O(m+n)/ Replace every non-overlapping occurrence of @needle@ in
-- @haystack@ with @replacement@.
--
-- This function behaves as though it was defined as follows:
--
-- @
-- replace needle replacement haystack =
--   'intercalate' replacement ('splitOn' needle haystack)
-- @
--
-- As this suggests, each occurrence is replaced exactly once.  So if
-- @needle@ occurs in @replacement@, that occurrence will /not/ itself
-- be replaced recursively:
--
-- >>> replace "oo" "foo" "oo"
-- "foo"
--
-- In cases where several instances of @needle@ overlap, only the
-- first one will be replaced:
--
-- >>> replace "ofo" "bar" "ofofo"
-- "barfo"
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
replace ::
  Text1 -- ^ @needle@ to search for.
  -> Text1 -- ^ @replacement@ to replace @needle@ with.
  -> Text1 -- ^ @haystack@ in which to search.
  -> Text1
replace needle replacement haystack =
  let needle' = text1ToText needle
      replacement' = text1ToText replacement
  in coerce (T.replace needle' replacement') haystack

-- | /O(n)/ Convert a `Text1` to folded case.
--
-- This function is mainly useful for performing caseless (also known
-- as case insensitive) string comparisons.
--
-- A string @x@ is a caseless match for a string @y@ if and only if:
--
-- @toCaseFold x == toCaseFold y@
--
-- The result string may be longer than the input string, and may
-- differ from applying 'toLower' to the input string.  For instance,
-- the Armenian small ligature \"&#xfb13;\" (men now, U+FB13) is case
-- folded to the sequence \"&#x574;\" (men, U+0574) followed by
-- \"&#x576;\" (now, U+0576), while the Greek \"&#xb5;\" (micro sign,
-- U+00B5) is case folded to \"&#x3bc;\" (small letter mu, U+03BC)
-- instead of itself.
toCaseFold :: Text1 -> Text1
toCaseFold = coerce T.toCaseFold
{-# INLINE toCaseFold #-}

-- | /O(n)/ Convert a `Text1` to lower case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, \"&#x130;\" (Latin capital letter I with dot above,
-- U+0130) maps to the sequence \"i\" (Latin small letter i, U+0069)
-- followed by \" &#x307;\" (combining dot above, U+0307).
toLower :: Text1 -> Text1
toLower = coerce T.toLower
{-# INLINE toLower #-}

-- | /O(n)/ Convert a `Text1` to upper case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, the German \"&#xdf;\" (eszett, U+00DF) maps to the
-- two-letter sequence \"SS\".
toUpper :: Text1 -> Text1
toUpper = coerce T.toUpper
{-# INLINE toUpper #-}

-- | /O(n)/ Convert a `Text1` to title case, using simple case
-- conversion.
--
-- The first letter of the input is converted to title case, as is
-- every subsequent letter that immediately follows a non-letter.
-- Every letter that immediately follows another letter is converted
-- to lower case.
--
-- The result string may be longer than the input string. For example,
-- the Latin small ligature &#xfb02; (U+FB02) is converted to the
-- sequence Latin capital letter F (U+0046) followed by Latin small
-- letter l (U+006C).
--
-- /Note/: this function does not take language or culture specific
-- rules into account. For instance, in English, different style
-- guides disagree on whether the book name \"The Hill of the Red
-- Fox\" is correctly title cased&#x2014;but this function will
-- capitalize /every/ word.
toTitle :: Text1 -> Text1
toTitle = coerce T.toTitle
{-# INLINE toTitle #-}

-- | /O(n)/ Left-justify a `Text1` to the given length, using the
-- specified fill character on the right.
-- Performs replacement on invalid scalar values.
--
-- Examples:
--
-- >>> justifyLeft 7 'x' "foo"
-- "fooxxxx"
--
-- >>> justifyLeft 3 'x' "foobar"
-- "foobar"
justifyLeft :: Int -> Char -> Text1 -> Text1
justifyLeft k c = coerce (T.justifyLeft k c)
{-# INLINE justifyLeft #-}

-- | /O(n)/ Right-justify a `Text1` to the given length, using the
-- specified fill character on the left.  Performs replacement on
-- invalid scalar values.
--
-- Examples:
--
-- >>> justifyRight 7 'x' "bar"
-- "xxxxbar"
--
-- >>> justifyRight 3 'x' "foobar"
-- "foobar"
justifyRight :: Int -> Char -> Text1 -> Text1
justifyRight k c = coerce (T.justifyRight k c)
{-# INLINE justifyRight #-}

-- | /O(n)/ Centre a `Text1` to the given length, using the specified
-- fill character on either side.  Performs replacement on invalid
-- scalar values.
--
-- Examples:
--
-- >>> centre 8 'x' "HS"
-- "xxxHSxxx"
centre :: Int -> Char -> Text1 -> Text1
centre k c = coerce (T.center k c)
{-# INLINE centre #-}

-- Alias for 'centre' for our US friends.
center :: Int -> Char -> Text1 -> Text1
center = centre

-- | /O(n)/ 'foldl', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'Text1',
-- reduces the 'Text1' using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> Text1 -> a
foldl f a = coerce (T.foldl f a)
{-# INLINE foldl #-}

-- | /O(n)/ A strict version of 'foldl'.
foldl' :: (a -> Char -> a) -> a -> Text1 -> a
foldl' f a = coerce (T.foldl' f a)
{-# INLINE foldl' #-}

-- | /O(n)/ A variant of 'foldl' that has no starting value argument,
-- and thus must be applied to a 'Text1' which is inherently non-empty.
foldl1 :: (Char -> Char -> Char) -> Text1 -> Char
foldl1 f = coerce (T.foldl1 f)
{-# INLINE foldl1 #-}

-- | /O(n)/ A strict version of 'foldl1'.
foldl1' :: (Char -> Char -> Char) -> Text1 -> Char
foldl1' f = coerce (T.foldl1' f)
{-# INLINE foldl1' #-}

-- | /O(n)/ 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'Text1',
-- reduces the 'Text1' using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> Text1 -> a
foldr f a = coerce (T.foldr f a)
{-# INLINE foldr #-}

-- | /O(n)/ A variant of 'foldr' that has no starting value argument,
-- and thus must be applied to a 'Text1' which is inherently non-empty.
foldr1 :: (Char -> Char -> Char) -> Text1 -> Char
foldr1 f = coerce (T.foldr1 f)
{-# INLINE foldr1 #-}

-- | Concatenate a non-empty list of 'Text1's.
concat :: NonEmpty Text1 -> Text1
concat = sconcat
{-# INLINE concat #-}

-- | /O(n)/ Map a function over a 'Text1' that results in a 'Text1', and
-- concatenate the results.
concatMap :: (Char -> Text1) -> Text1 -> Text1
concatMap f = coerce (T.concatMap (text1ToText . f))
{-# INLINE concatMap #-}

-- | /O(n)/ 'any' @p@ @t@ determines whether any character in the
-- 'Text1' @t@ satisfies the predicate @p@.
any :: (Char -> Bool) -> Text1 -> Bool
any p = coerce (T.any p)
{-# INLINE any #-}

-- | /O(n)/ 'all' @p@ @t@ determines whether all characters in the
-- 'Text1' @t@ satisfy the predicate @p@.
all :: (Char -> Bool) -> Text1 -> Bool
all p = coerce (T.all p)
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'Text1'.
maximum :: Text1 -> Char
maximum = coerce T.maximum
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'Text1'.
minimum :: Text1 -> Char
minimum = coerce T.minimum
{-# INLINE minimum #-}

-- | /O(n)/ 'scanl' is similar to 'foldl', but returns a list of
-- successive reduced values from the left. Subject to fusion.
-- Performs replacement on invalid scalar values.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl :: (Char -> Char -> Char) -> Char -> Text1 -> Text1
scanl f a = coerce (T.scanl f a)
{-# INLINE scanl #-}

-- | /O(n)/ 'scanl1' is a variant of 'scanl' that has no starting
-- value argument. Performs replacement on invalid scalar values.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Char -> Char -> Char) -> Text1 -> Text1
scanl1 f = coerce (T.scanl1 f)
{-# INLINE scanl1 #-}

-- | /O(n)/ 'scanr' is the right-to-left dual of 'scanl'.  Performs
-- replacement on invalid scalar values.
--
-- > scanr f v == reverse . scanl (flip f) v . reverse
scanr :: (Char -> Char -> Char) -> Char -> Text1 -> Text1
scanr f a = coerce (T.scanr f a)
{-# INLINE scanr #-}

-- | /O(n)/ 'scanr1' is a variant of 'scanr' that has no starting
-- value argument. Performs replacement on invalid scalar values.
scanr1 :: (Char -> Char -> Char) -> Text1 -> Text1
scanr1 f = coerce (T.scanr1 f)
{-# INLINE scanr1 #-}

-- | /O(n)/ Like a combination of 'map' and 'foldl''. Applies a
-- function to each element of a 'Text1', passing an accumulating
-- parameter from left to right, and returns a final 'Text1'.  Performs
-- replacement on invalid scalar values.
mapAccumL :: (a -> Char -> (a, Char)) -> a -> Text1 -> (a, Text1)
mapAccumL f a = coerce (T.mapAccumL f a)
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- a strict 'foldr'; it applies a function to each element of a
-- 'Text1', passing an accumulating parameter from right to left, and
-- returning a final value of this accumulator together with the new
-- 'Text1'.
-- Performs replacement on invalid scalar values.
mapAccumR :: (a -> Char -> (a, Char)) -> a -> Text1 -> (a, Text1)
mapAccumR f a = coerce (T.mapAccumR f a)
{-# INLINE mapAccumR #-}

-- | /O(n*m)/ 'replicate' @n@ @t@ is a 'Text1' consisting of the input
-- @t@ repeated @n@ times.
-- Since @n@ is positive you are guanateed a `Text1` to be returned.
replicate :: Positive -> Text1 -> Text1
replicate n = coerce (T.replicate $ fromIntegral n)
{-# INLINE replicate #-}

-- | /O(n*m)/ 'replicate' @n@ @t@ is a 'Text1' consisting of the input
-- @t@ repeated @n@ times.
-- The number of times to repeat the `Text1` must be greater than 0,
-- otherwise `Nothing` is returned.
replicate' :: Int -> Text1 -> Maybe Text1
replicate' n t
  | n <= 0     = Nothing
  | n == 1    = Just t
  | otherwise = Just $ coerce (T.replicate n) t
{-# INLINE replicate' #-}

-- | /O(n)/, where @n@ is the length of the result. The 'unfoldr'
-- function is analogous to the List 'L.unfoldr'. 'unfoldr' builds a
-- 'Text' from a seed value. The function takes the element and
-- returns 'Nothing' if it is done producing the 'Text1', otherwise
-- 'Just' @(a,b)@.  In this case, @a@ is the next 'Char' in the
-- string, and @b@ is the seed value for further production. Performs
-- replacement on invalid scalar values.
unfoldr :: (a -> Maybe (Char, a)) -> a -> Maybe Text1
unfoldr f a = textToText1M $ coerce (T.unfoldr f a)
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a 'Text1' from a seed
-- value. However, the length of the result should be limited by the
-- first argument to 'unfoldrN'. This function is more efficient than
-- 'unfoldr' when the maximum length of the result is known and
-- correct, otherwise its performance is similar to 'unfoldr'.
-- Performs replacement on invalid scalar values.
unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> Maybe Text1
unfoldrN n f a = textToText1M $ coerce (T.unfoldrN n f a)
{-# INLINE unfoldrN #-}

-- | /O(n)/ 'take' @n@, applied to a 'Text1', returns the prefix of the
-- 'Text1' of length @n@, or the 'Text1' itself if @n@ is greater than
-- the length of the Text.
take :: Positive -> Text1 -> Text1
take n = coerce (T.take $ fromIntegral n)
{-# INLINE take #-}

-- | /O(n)/ 'take' @n@, applied to a 'Text1', returns the prefix of the
-- 'Text1' of length @n@, or the 'Text1' itself if @n@ is greater than
-- the length of the Text. If @n@ is less than 1, `Nothing` is returned.
take' :: Int -> Text1 -> Maybe Text1
take' n t
  | n <= 0    = Nothing
  | otherwise = Just $ coerce (T.take n) t
{-# INLINE take' #-}

-- | /O(n)/ 'takeEnd' @n@ @t@ returns the suffix remaining after
-- taking @n@ characters from the end of @t@.
--
-- Examples:
--
-- >>> takeEnd 3 "foobar"
-- "bar"
takeEnd :: Positive -> Text1 -> Text1
takeEnd n = coerce (T.takeEnd $ fromIntegral n)

-- | /O(n)/ 'takeEnd'' @n@ @t@ returns the suffix remaining after
-- taking @n@ characters from the end of @t@. If @n@ is less than
-- 1 then `Nothing` is returned.
--
-- Examples:
--
-- >>> takeEnd' 3 "foobar"
-- Just "bar"
takeEnd' :: Int -> Text1 -> Maybe Text1
takeEnd' n t
  | n <= 0    = Nothing
  | otherwise = Just $ coerce (T.takeEnd n) t

-- | /O(n)/ 'drop' @n@, applied to a 'Text1', returns the suffix of the
-- 'Text1' after the first @n@ characters, or `Nothing` if @n@
-- is greater than the length of the 'Text1'.
drop :: Int -> Text1 -> Maybe Text1
drop n t
  | n <= 0    = Just t
  | otherwise = textToText1M $ coerce (T.drop n) t

-- | /O(n)/ 'dropEnd' @n@ @t@ returns the prefix remaining after
-- dropping @n@ characters from the end of @t@, or `Nothing` if @n@
-- is greater than the length of the 'Text1'.
--
-- Examples:
--
-- >>> dropEnd 3 "foobar"
-- Just "foo"
dropEnd :: Int -> Text1 -> Maybe Text1
dropEnd n t
  | n <= 0    = Just t
  | otherwise = textToText1M $ coerce (T.dropEnd n) t

-- | /O(n)/ 'takeWhile', applied to a predicate @p@ and a 'Text1',
-- returns the longest prefix (possibly empty) of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> Text1 -> Maybe Text1
takeWhile p = textToText1M . coerce (T.takeWhile p)
{-# INLINE takeWhile #-}

-- | /O(n)/ 'takeWhileEnd', applied to a predicate @p@ and a 'Text1',
-- returns the longest suffix (possibly empty) of elements that
-- satisfy @p@.
-- Examples:
--
-- >>> takeWhileEnd (=='o') "foo"
-- Just "oo"
takeWhileEnd :: (Char -> Bool) -> Text1 -> Maybe Text1
takeWhileEnd p = textToText1M . coerce (T.takeWhileEnd p)
{-# INLINE takeWhileEnd #-}

-- | /O(n)/ 'dropWhile' @p@ @t@ returns the suffix remaining after
-- 'takeWhile' @p@ @t@.
dropWhile :: (Char -> Bool) -> Text1 -> Maybe Text1
dropWhile p = textToText1M . coerce (T.dropWhile p)
{-# INLINE dropWhile #-}

-- | /O(n)/ 'dropWhileEnd' @p@ @t@ returns the prefix remaining after
-- dropping characters that satisfy the predicate @p@ from the end of
-- @t@.
--
-- Examples:
--
-- >>> dropWhileEnd (=='.') "foo..."
-- Just "foo"
dropWhileEnd :: (Char -> Bool) -> Text1 -> Maybe Text1
dropWhileEnd p = textToText1M . coerce (T.dropWhileEnd p)
{-# INLINE dropWhileEnd #-}

-- | /O(n)/ 'dropAround' @p@ @t@ returns the substring remaining after
-- dropping characters that satisfy the predicate @p@ from both the
-- beginning and end of @t@.
dropAround :: (Char -> Bool) -> Text1 -> Maybe Text1
dropAround p = textToText1M . coerce (T.dropAround p)
{-# INLINE dropAround #-}

-- | /O(n)/ Remove leading white space from a string.  Equivalent to:
--
-- > dropWhile isSpace
stripStart :: Text1 -> Maybe Text1
stripStart = textToText1M . coerce T.stripStart
{-# INLINE stripStart #-}

-- | /O(n)/ Remove trailing white space from a string.  If the string
-- only contains white space `Nothing` is returned.  Equivalent to:
--
-- > dropWhileEnd isSpace
stripEnd :: Text1 -> Maybe Text1
stripEnd = textToText1M . coerce T.stripEnd
{-# INLINE stripEnd #-}

-- | /O(n)/ Remove leading and trailing white space from a string.
-- If the string only contains white space `Nothing` is returned.
-- Equivalent to:
--
-- > dropAround isSpace
strip :: Text1 -> Maybe Text1
strip = textToText1M . coerce T.strip
{-# INLINE strip #-}

-- | /O(n)/ 'splitAt' @n t@ returns a pair whose first element is a
-- prefix of @t@ of length @n@, and whose second is the remainder of
-- the string. It is equivalent to @('take' n t, 'drop' n t)@.
splitAt :: Int -> Text1 -> (Maybe Text1, Maybe Text1)
splitAt n t
  | n <= 0    = (Nothing, Just t)
  | otherwise = bimap textToText1M textToText1M $ coerce (T.splitAt n) t

-- | /O(n)/ 'span', applied to a predicate @p@ and text @t@, returns
-- a pair whose first element is the longest prefix (possibly empty)
-- of @t@ of elements that satisfy @p@, and whose second is the
-- remainder of the list.
span :: (Char -> Bool) -> Text1 -> (Maybe Text1, Maybe Text1)
span p t = bimap textToText1M textToText1M $ coerce (T.span p) t
{-# INLINE span #-}

-- | /O(n)/ 'break' is like 'span', but the prefix returned is
-- over elements that fail the predicate @p@.
break :: (Char -> Bool) -> Text1 -> (Maybe Text1, Maybe Text1)
break p = span (not . p)
{-# INLINE break #-}

splitOn :: Text1 -> Text1 -> [Text1]
splitOn pat = mapMaybe textToText1M . T.splitOn (text1ToText pat) . text1ToText

-- | /O(n)/ Breaks a 'Text1' up into a list of words, delimited by 'Char's
-- representing white space.
words :: Text1 -> [Text1]
words = mapMaybe textToText1M . coerce T.words
{-# INLINE words #-}

-- | /O(n)/ Breaks a 'Text1' up into a list of 'Text1's at
-- newline 'Char's. The resulting strings do not contain newlines.
lines :: Text1 -> [Text1]
lines = mapMaybe textToText1M . coerce T.lines
{-# INLINE lines #-}

-- | /O(n)/ Joins lines, after appending a terminating newline to
-- each.
unlines :: NonEmpty Text1 -> Text1
unlines = sconcat . fmap (`snoc` '\n')
{-# INLINE unlines #-}

-- | /O(n)/ Joins words using single space characters.
unwords :: NonEmpty Text1 -> Text1
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

-- | /O(n)/ The 'isPrefixOf' function takes two 'Text1's and returns
-- 'True' iff the first is a prefix of the second.
isPrefixOf :: Text1 -> Text1 -> Bool
isPrefixOf = coerce T.isPrefixOf
{-# INLINE isPrefixOf #-}

-- | /O(n)/ The 'isSuffixOf' function takes two 'Text1's and returns
-- 'True' iff the first is a suffix of the second.
isSuffixOf :: Text1 -> Text1 -> Bool
isSuffixOf = coerce T.isSuffixOf
{-# INLINE isSuffixOf #-}

-- | /O(n+m)/ The 'isInfixOf' function takes two 'Text1's and returns
-- 'True' iff the first is contained, wholly and intact, anywhere
-- within the second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
isInfixOf :: Text1 -> Text1 -> Bool
isInfixOf = coerce T.isInfixOf
{-# INLINE isInfixOf #-}

-- | /O(n)/ 'filter', applied to a predicate and a 'Text1',
-- returns a 'Text1' containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Text1 -> Maybe Text1
filter predicate = textToText1M . coerce (T.filter predicate)
{-# INLINE filter #-}

-- | /O(n)/ The 'find' function takes a predicate and a 'Text', and
-- returns the first element matching the predicate, or 'Nothing' if
-- there is no such element. Subject to fusion.
find :: (Char -> Bool) -> Text1 -> Maybe Char
find p = coerce $ T.find p
{-# INLINE find #-}

-- | /O(n)/ The 'partition' function takes a predicate and a 'Text1',
-- and returns a 'These' of 'Text1's with elements which do and do not
-- satisfy the predicate, respectively.
partition :: (Char -> Bool) -> Text1 -> These Text1 Text1
partition p text1 = case unsnoc text1 of
  (Nothing, char)
    | p char    -> That text1
    | otherwise -> This text1
  (Just init', char)
    | p char    -> foldr folder (That $ singleton char) init'
    | otherwise -> foldr folder (This $ singleton char) init'
  where
    folder :: Char -> These Text1 Text1 -> These Text1 Text1
    folder char = \case
      This ts     | p char    -> This $ cons char ts
                  | otherwise -> These ts $ singleton char
      That fs     | p char    -> These (singleton char) fs
                  | otherwise -> That $ cons char fs
      These ts fs | p char    -> These (cons char ts) fs
                  | otherwise -> These ts $ cons char fs
{-# INLINE partition #-}

-- | /O(n)/ 'Text1' index (subscript) operator, starting from 0.
index :: Text1 -> Natural -> Char
index t n = coerce (`T.index` fromIntegral n) t
{-# INLINE index #-}

-- | /O(n)/ 'Text1' index (subscript) operator, starting from 0.
index' :: Text1 -> Int -> Maybe Char
index' t n
  | n < 0 = Nothing
  | otherwise = Just $ coerce (`T.index` n) t
{-# INLINE index' #-}

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'Text1'
-- and returns the index of the first element in the 'Text' satisfying
-- the predicate.
findIndex :: (Char -> Bool) -> Text1 -> Maybe Int
findIndex p = coerce $ T.findIndex p
{-# INLINE findIndex #-}

-- | /O(n+m)/ The 'count' function returns the number of times the
-- query string appears in the given 'Text1'.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
count :: Text1 -> Text1 -> Int
count = coerce T.count
{-# INLINE count #-}

-- | /O(n)/ 'zip' takes two 'Text1's and returns a list of
-- corresponding pairs of bytes. If one input 'Text1' is short,
-- excess elements of the longer 'Text1' are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: Text1 -> Text1 -> NonEmpty (Char, Char)
zip textA textB =
  let (charA, restA) = uncons textA
      (charB, restB) = uncons textB
  in case (restA, restB) of
    (Just a', Just b') -> (charA, charB) <| zip a' b'
    _                  -> pure (charA, charB)
{-# INLINE zip #-}

-- | /O(n)/ 'zipWith' generalises 'zip' by zipping with the function
-- given as the first argument, instead of a tupling function.
-- Performs replacement on invalid scalar values.
zipWith :: (Char -> Char -> Char) -> Text1 -> Text1 -> Text1
zipWith f textA textB =
  let (charA, restA) = uncons textA
      (charB, restB) = uncons textB
  in case (restA, restB) of
    (Just a', Just b') -> cons (f charA charB) $ zipWith f a' b'
    _                  -> singleton $ f charA charB
{-# INLINE zipWith #-}

fromText1 :: FromText1 a => Text1 -> Either String a
fromText1 = A.parseOnly parser1 . text1ToText

fromText1M :: (MonadFail m, FromText1 a) => Text1 -> m a
fromText1M = either fail pure . fromText1

fromText :: FromText1 a => Text -> Either String a
fromText = fromText1 <=< textToText1

fromTextM :: (MonadFail m, FromText1 a) => Text -> m a
fromTextM = either fail pure . fromText

fromString :: FromText1 a => String -> Either String a
fromString = fromText . T.pack

fromStringM :: (MonadFail m, FromText1 a) => String -> m a
fromStringM = fromTextM . T.pack

toText :: ToText1 a => a -> Text
toText = text1ToText . toText1

toString :: ToText1 a => a -> String
toString = T.unpack . toText

takeText1 :: Parser Text1
takeText1 = textToText1M =<< A.takeText

takeCIText1 :: Parser (CI Text1)
takeCIText1 = CI.mk <$> takeText1

-- | Fail parsing with a 'Text1' error.
--
-- Constrained to the actual attoparsec monad to avoid
-- exposing 'fail' usage directly.
fromText1Error :: Text1 -> Parser a
fromText1Error = fail . unpack

unsafeFromText :: HasCallStack => Text -> Text1
unsafeFromText = either error id . textToText1

asText1 :: Iso' Text (Maybe Text1)
asText1 = iso textToText1M (maybe "" text1ToText)

_Text1 :: Prism' Text Text1
_Text1 = prism' text1ToText textToText1M

class FromText1 a where
  parser1 :: Parser a

instance FromText1 Double where
  parser1 = parser

instance FromText1 Int where
  parser1 = parser

instance FromText1 Int64 where
  parser1 = parser

instance FromText1 Integer where
  parser1 = parser

instance FromText1 Natural where
  parser1 = parser

instance FromText1 Nat where
  parser1 = parser

instance FromText1 Scientific where
  parser1 = parser

instance HasResolution a => FromText1 (Fixed a) where
  parser1 = A.signed A.rational <* A.endOfInput

instance FromText1 Text1 where
  parser1 = takeText1

instance FromText1 Text where
  parser1 = parser

instance FromText1 String where
  parser1 = parser

instance FromText1 Bool where
  parser1 = parser

instance FromText1 UUID where
  parser1 = tryParse . text1ToText =<< takeText1
    where
      tryParse text =
        maybe (fail $ "Cannot parse UUID from '" <> T.unpack text <> "'.") pure $
          UUID.fromText text

class ToText1 a where
  toText1 :: a -> Text1

instance ToText1 Double where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Int where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Int64 where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Integer where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Natural where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Nat where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Scientific where
  toText1 = unsafeFromText . AWS.toText

instance HasResolution a => ToText1 (Fixed a) where
  toText1 = unsafeFromText . T.pack . show

instance ToText1 Text1 where
  toText1 = id

instance ToText1 String where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Bool where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 UUID where
  toText1 = unsafeFromText . UUID.toText

-- | 'AsText' allows you to derive instances for your own types utilising their
-- 'ToText1' and 'FromText1' instances using 'deriving via'.
-- You could derive 'FromJSON' via 'AsText1' and this would use the 'FromText1'
-- instance, giving you greater control over your type instances without all the boiler plate
-- for each instance.
--
-- > data Jedi
-- >   = JediObiWanKenobi
-- >   | JediLuke
-- >   | JediYoda
-- >   deriving (Eq, Ord, Generic, Bounded, Enum)
-- >   deriving (Show, Read, ToJSON, FromJSON) via AsText1 Jedi
-- >
-- > instance FromText1 Jedi where
-- >   parser1 = takeCIText1 >>= \case
-- >     "ObiWanKenobi" -> pure JediObiWanKenobi
-- >     "Luke"         -> pure JediLuke
-- >     "Yoda"         -> pure JediYoda
-- >     e              -> fromText1Error $ "The force is not strong with you: " <> original e
-- >
-- > instance ToText1 Jedi where
-- >   toText1 = \case
-- >     JediObiWanKenobi -> "ObiWanKenobi"
-- >     JediLuke         -> "Luke"
-- >     JediYoda         -> "Yoda"
newtype AsText1 a = AsText1
  { _asText1 :: a }
  deriving (Eq)
  deriving newtype (ToText1, FromText1) -- 🤯

instance ToText1 a => Show (AsText1 a) where
  show = toString

instance FromText1 a => Read (AsText1 a) where
  readPrec = fromText1M =<< readPrec

instance ToText1 a => Hashable (AsText1 a) where
  hashWithSalt salt = hashWithSalt salt . toText1

instance FromText1 a => IsString (AsText1 a) where
  fromString ""  = error "IsString: Text1 from empty string literal."
  fromString str = either error id $ fromString str
  {-# INLINE fromString #-}

instance ToText1 a => ToField (AsText1 a) where
  toField = toField . toText1

instance FromText1 a => FromField (AsText1 a) where
  parseField = fromText1M <=< parseField

instance ToText1 a => ToJSONKey (AsText1 a) where
  toJSONKey = toJSONKeyText toText

instance FromText1 a => FromJSONKey (AsText1 a) where
  fromJSONKey = FromJSONKeyTextParser fromTextM

instance ToText1 a => ToJSON (AsText1 a) where
  toJSON = toJSON . toText1

instance FromText1 a => FromJSON (AsText1 a) where
  parseJSON = fromText1M <=< parseJSON

instance ToText1 a => ToHttpApiData (AsText1 a) where
  toUrlPiece = toUrlPiece . toText1

instance FromText1 a => FromHttpApiData (AsText1 a) where
  parseUrlPiece = first T.pack . fromText1 <=< parseUrlPiece
