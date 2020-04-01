{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Text1Spec (spec) where

import           Control.Exception (evaluate)
import           Control.Monad (forM_)
import           Data.Aeson
                  (FromJSON, FromJSONKey, Result(..), ToJSON(..), ToJSONKey, Value(..), fromJSON)
import           Data.Char (isLower, isUpper)
import           Data.Csv (parseField, runParser)
import           Data.Csv (FromField(..), ToField(..))
import           Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.String (IsString)
import qualified Data.String as S
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Servant.API (FromHttpApiData(..), ToHttpApiData(..))
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck.Arbitrary.Generic (Arbitrary(..), genericArbitrary, genericShrink)
import           Text.Read (readMaybe)

import           Data.Text1 as Text1

spec :: Spec
spec = parallel $ do

  context "Text1" $ parallel $ do

    prop "Arbitrary Should generate only valid Text1" $ \text1 ->
      textToText1M (text1ToText text1) `shouldBe` Just text1

    it "Should throw an error in IsString instance for empty strings" $
      evaluate (S.fromString "" :: Text1) `shouldThrow` anyErrorCall

    it "Should throw an error in IsString instance for empty string literals" $
      evaluate ("" :: Text1) `shouldThrow` anyErrorCall

    it "Should allow whitespace in IsString instance" $ do
      text1ToText (S.fromString " ") `shouldBe` (" " :: T.Text)
      text1ToText (S.fromString "\t") `shouldBe` ("\t" :: T.Text)

    it "Should disallow empty in textToText1M" $ do
      textToText1M "" `shouldBe` Nothing

    prop "Should always produce non-empty Texts with textToText1M" $ \text ->
      textToText1M text `shouldSatisfy` maybe True (not . T.null . text1ToText)

    it "Should parse FromField with empty as Maybe Text1" $
      runParser (parseField "") `shouldBe` Right (Nothing :: Maybe Text1)

  context "uncons" $

    prop "Should split Text1 into head and tail" $ \text1 ->
      uncons text1 `shouldBe` (Text1.head text1, Text1.tail text1)

  context "unsnoc" $

    prop "Should split Text1 into init and last" $ \text1 ->
      unsnoc text1 `shouldBe` (Text1.init text1, Text1.last text1)

  context "length" $ do

    prop "Should always have the same length as the underlying Text" $ \text ->
      textToText1M text `shouldSatisfy`
        maybe True (\text1 -> Text1.length text1 == T.length (text1ToText text1))

    prop "Should always produce equal length Text1 from Text" $ \text ->
      textToText1M text `shouldSatisfy` maybe True (\text1 -> Text1.length text1 == T.length text)

  context "map" $ do

    prop "Using 'id' should produce same Text1" $ \text1 ->
      Text1.map id text1 `shouldBe` text1

    prop "Should always produce valid Text1 whatever the returned Char" $ \text1 char ->
      Text1.map (const char) text1
        `shouldBe` Text1.replicate (fromIntegral $ Text1.length text1) (singleton char)

  context "intercalate" $

    it "Should intercalate!" $
      intercalate "NI!" ("We" :| ["seek", "the", "Holy", "Grail"])
        `shouldBe` "WeNI!seekNI!theNI!HolyNI!Grail"

  context "intersperse" $

    it "Should intersperse!" $
      intersperse '.' "SHIELD" `shouldBe` "S.H.I.E.L.D"

  context "transpose" $

    it "Should transpose!" $ do
      transpose ["green","orange"] `shouldBe` ["go","rr","ea","en","ng","e"]
      transpose ["blue","red"] `shouldBe` ["br","le","ud","e"]

  context "reverse" $

    it "Should reverse!" $
      Text1.reverse "desrever" `shouldBe` "reversed"

  context "toLower" $

    prop "Shouldn't any be lower" $ \text1 ->
      toLower text1 `shouldSatisfy` Text1.all (not . isUpper)

  context "toUpper" $

    prop "Should any be upper" $ \text1 ->
      toUpper text1 `shouldSatisfy` Text1.all (not . isLower)

  context "splitOn" $ parallel $ do

    it "Should remove empty elements when all empty" $
      splitOn "," ",,," `shouldBe` []

    it "Should remove empty elements when some empty" $
      splitOn "," "foo," `shouldBe` ["foo"]

    it "Should remove empty elements" $
      splitOn "," "foo, ,,bar" `shouldBe` ["foo", " ", "bar"]

  context "AsText" $ parallel $ do

    it "Should correctly use IsString instance" $ do
      ["ObiWanKenobi", "obiwankenobi", "OBIWANKENOBI", "ObIwAnKeNoBi"]
       `shouldBe` [ObiWanKenobi, ObiWanKenobi, ObiWanKenobi, ObiWanKenobi]

    forM_ [minBound..maxBound @Jedi] $ \jedi -> do

      prop "Should use Hashable instance for Text1" $ \salt ->
        hashWithSalt salt (toText1 jedi) `shouldBe` hashWithSalt salt jedi

      it "Should round trip Show/Read" $
        readMaybe (show jedi) `shouldBe` Just jedi

      it "Should round trip To/FromJSON" $
        fromJSON (toJSON jedi) `shouldBe` Success jedi

      it "Should round trip To/FromJSONKey" $
        let hashMap = HM.singleton jedi $ Number 1
        in fromJSON (toJSON hashMap) `shouldBe` Success hashMap

      it "Should round trip To/FromField" $
        runParser (parseField $ toField jedi) `shouldBe` Right jedi

      it "Should round trip To/FromHttpApiData" $
        parseUrlPiece (toUrlPiece jedi) `shouldBe` Right jedi

data Jedi
  = ObiWanKenobi
  | Luke
  | Yoda
  deriving (Eq, Ord, Generic, Bounded, Enum)
  deriving ( Show, Read, Hashable, IsString, ToField, FromField, ToJSONKey, FromJSONKey
           , ToJSON, FromJSON, ToHttpApiData, FromHttpApiData ) via AsText1 Jedi

instance Arbitrary Jedi where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance FromText1 Jedi where
  parser1 = takeCIText1 >>= \case
    "ObiWanKenobi" -> pure ObiWanKenobi
    "Luke"         -> pure Luke
    "Yoda"         -> pure Yoda
    e              -> fromText1Error $ "The force is not strong with you: '" <> original e <> "'."

instance ToText1 Jedi where
  toText1 = \case
    ObiWanKenobi -> "ObiWanKenobi"
    Luke         -> "Luke"
    Yoda         -> "Yoda"
