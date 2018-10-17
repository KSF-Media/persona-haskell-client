{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Persona.Types (
  ActiveDays (..),
  Address (..),
  Campaign (..),
  DescriptionFrequency (..),
  LoginData (..),
  LoginDataSSO (..),
  LoginDataSoMe (..),
  LoginResponse (..),
  Package (..),
  PackageDescription (..),
  PackageOffer (..),
  Paper (..),
  Product (..),
  Subscription (..),
  SubscriptionDates (..),
  User (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data ActiveDays = ActiveDays
  { activeDaysMon :: Bool -- ^ 
  , activeDaysTue :: Bool -- ^ 
  , activeDaysWed :: Bool -- ^ 
  , activeDaysThu :: Bool -- ^ 
  , activeDaysFri :: Bool -- ^ 
  , activeDaysSat :: Bool -- ^ 
  , activeDaysSun :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ActiveDays where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "activeDays")
instance ToJSON ActiveDays where
  toJSON = genericToJSON (removeFieldLabelPrefix False "activeDays")
instance ToSchema ActiveDays where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "activeDays"


-- | 
data Address = Address
  { addressCountryCode :: Text -- ^ 
  , addressZipCode :: Maybe Text -- ^ 
  , addressCity :: Maybe Text -- ^ 
  , addressStreetAddress :: Text -- ^ 
  , addressStreetName :: Maybe Text -- ^ 
  , addressHouseNo :: Maybe Text -- ^ 
  , addressStaircase :: Maybe Text -- ^ 
  , addressApartment :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Address where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "address")
instance ToJSON Address where
  toJSON = genericToJSON (removeFieldLabelPrefix False "address")
instance ToSchema Address where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "address"


-- | 
data Campaign = Campaign
  { campaignNo :: Int -- ^ 
  , campaignId :: Text -- ^ 
  , campaignName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Campaign where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "campaign")
instance ToJSON Campaign where
  toJSON = genericToJSON (removeFieldLabelPrefix False "campaign")
instance ToSchema Campaign where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "campaign"


-- | 
data DescriptionFrequency = DescriptionFrequency
  { descriptionFrequencyAmount :: Int -- ^ 
  , descriptionFrequencyUnit :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DescriptionFrequency where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "descriptionFrequency")
instance ToJSON DescriptionFrequency where
  toJSON = genericToJSON (removeFieldLabelPrefix False "descriptionFrequency")
instance ToSchema DescriptionFrequency where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "descriptionFrequency"


-- | 
data LoginData = LoginData
  { loginDataUsername :: Text -- ^ 
  , loginDataPassword :: Text -- ^ 
  , loginDataMergeToken :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LoginData where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "loginData")
instance ToJSON LoginData where
  toJSON = genericToJSON (removeFieldLabelPrefix False "loginData")
instance ToSchema LoginData where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "loginData"


-- | 
data LoginDataSSO = LoginDataSSO
  { loginDataSSOUuid :: UUID -- ^ 
  , loginDataSSOAccessToken :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LoginDataSSO where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "loginDataSSO")
instance ToJSON LoginDataSSO where
  toJSON = genericToJSON (removeFieldLabelPrefix False "loginDataSSO")
instance ToSchema LoginDataSSO where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "loginDataSSO"


-- | 
data LoginDataSoMe = LoginDataSoMe
  { loginDataSoMeProvider :: Text -- ^ 
  , loginDataSoMeSomeToken :: Text -- ^ 
  , loginDataSoMeMergeToken :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LoginDataSoMe where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "loginDataSoMe")
instance ToJSON LoginDataSoMe where
  toJSON = genericToJSON (removeFieldLabelPrefix False "loginDataSoMe")
instance ToSchema LoginDataSoMe where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "loginDataSoMe"


-- | 
data LoginResponse = LoginResponse
  { loginResponseToken :: Text -- ^ 
  , loginResponseSsoCode :: Maybe Text -- ^ 
  , loginResponseUuid :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LoginResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "loginResponse")
instance ToJSON LoginResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "loginResponse")
instance ToSchema LoginResponse where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "loginResponse"


-- | 
data Package = Package
  { packageId :: Text -- ^ 
  , packageName :: Text -- ^ 
  , packagePaper :: Paper -- ^ 
  , packageProducts :: [Product] -- ^ 
  , packageOffers :: [PackageOffer] -- ^ 
  , packageCampaigns :: [Campaign] -- ^ 
  , packageNextDelivery :: Maybe Day -- ^ 
  , packageDescription :: Maybe PackageDescription -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Package where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "package")
instance ToJSON Package where
  toJSON = genericToJSON (removeFieldLabelPrefix False "package")
instance ToSchema Package where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "package"


-- | 
data PackageDescription = PackageDescription
  { packageDescriptionBrand :: Text -- ^ 
  , packageDescriptionBrandLong :: Text -- ^ 
  , packageDescriptionDescShort :: Text -- ^ 
  , packageDescriptionDescLong :: Text -- ^ 
  , packageDescriptionUrl :: Text -- ^ 
  , packageDescriptionDays :: Text -- ^ 
  , packageDescriptionWeekdays :: Text -- ^ 
  , packageDescriptionFrequency :: DescriptionFrequency -- ^ 
  , packageDescriptionIncludes :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PackageDescription where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "packageDescription")
instance ToJSON PackageDescription where
  toJSON = genericToJSON (removeFieldLabelPrefix False "packageDescription")
instance ToSchema PackageDescription where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "packageDescription"


-- | 
data PackageOffer = PackageOffer
  { packageOfferMonths :: Int -- ^ 
  , packageOfferTotalPrice :: Int -- ^ 
  , packageOfferMonthlyPrice :: Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PackageOffer where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "packageOffer")
instance ToJSON PackageOffer where
  toJSON = genericToJSON (removeFieldLabelPrefix False "packageOffer")
instance ToSchema PackageOffer where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "packageOffer"


-- | 
data Paper = Paper
  { paperCode :: Text -- ^ 
  , paperName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Paper where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "paper")
instance ToJSON Paper where
  toJSON = genericToJSON (removeFieldLabelPrefix False "paper")
instance ToSchema Paper where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "paper"


-- | 
data Product = Product
  { productId :: Text -- ^ 
  , productName :: Text -- ^ 
  , productActive :: ActiveDays -- ^ 
  , productNextDelivery :: Maybe Day -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Product where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "product")
instance ToJSON Product where
  toJSON = genericToJSON (removeFieldLabelPrefix False "product")
instance ToSchema Product where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "product"


-- | 
data Subscription = Subscription
  { subscriptionSubsno :: Int -- ^ 
  , subscriptionExtno :: Int -- ^ 
  , subscriptionCusno :: Int -- ^ 
  , subscriptionPaycusno :: Int -- ^ 
  , subscriptionKind :: Text -- ^ 
  , subscriptionState :: Text -- ^ 
  , subscriptionPricegroup :: Maybe Text -- ^ 
  , subscriptionPackage :: Package -- ^ 
  , subscriptionDates :: SubscriptionDates -- ^ 
  , subscriptionExtsubsexists :: Bool -- ^ 
  , subscriptionCampaign :: Maybe Campaign -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Subscription where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscription")
instance ToJSON Subscription where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscription")
instance ToSchema Subscription where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscription"


-- | 
data SubscriptionDates = SubscriptionDates
  { subscriptionDatesLenMonths :: Maybe Int -- ^ 
  , subscriptionDatesLenDays :: Maybe Int -- ^ 
  , subscriptionDatesStart :: Day -- ^ 
  , subscriptionDatesEnd :: Maybe Day -- ^ 
  , subscriptionDatesUnpaidBreak :: Maybe Day -- ^ 
  , subscriptionDatesInvoicingStart :: Maybe Day -- ^ 
  , subscriptionDatesPaidUntil :: Maybe Day -- ^ 
  , subscriptionDatesSuspend :: Maybe Day -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionDates where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionDates")
instance ToJSON SubscriptionDates where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionDates")
instance ToSchema SubscriptionDates where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionDates"


-- | 
data User = User
  { userUuid :: UUID -- ^ 
  , userEmail :: Text -- ^ 
  , userFirstName :: Maybe Text -- ^ 
  , userLastName :: Maybe Text -- ^ 
  , userAddress :: Maybe Address -- ^ 
  , userCusno :: Text -- ^ 
  , userSubs :: [Subscription] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON User where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "user")
instance ToJSON User where
  toJSON = genericToJSON (removeFieldLabelPrefix False "user")
instance ToSchema User where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "user"



uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  { omitNothingFields  = True
  , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
  }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
