{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Persona.Types (
  ActiveDays (..),
  Address (..),
  Campaign (..),
  DescriptionFrequency (..),
  GdprConsent (..),
  Inline_response_400 (..),
  Inline_response_400_invalid_request_body (..),
  Inline_response_403 (..),
  Inline_response_403_1 (..),
  Inline_response_403_1_access_token_expired (..),
  Inline_response_403_2 (..),
  Inline_response_403_2_email_address_in_use (..),
  Inline_response_403_2_email_not_authorized (..),
  Inline_response_403_2_oauth_failed (..),
  Inline_response_403_invalid_credentials (..),
  Inline_response_415 (..),
  Inline_response_415_unsupported_media_type (..),
  Inline_response_500 (..),
  Inline_response_500_internal_server_error (..),
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
data GdprConsent = GdprConsent
  { gdprConsentKey :: Text -- ^ 
  , gdprConsentVal :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GdprConsent where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "gdprConsent")
instance ToJSON GdprConsent where
  toJSON = genericToJSON (removeFieldLabelPrefix False "gdprConsent")
instance ToSchema GdprConsent where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "gdprConsent"


-- | 
data Inline_response_400 = Inline_response_400
  { inlineResponse400HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse400InvalidUnderscorerequestUnderscorebody :: Maybe Inline_response_400_invalid_request_body -- ^ 
  , inlineResponse400HttpUnderscorestatus :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_400 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse400")
instance ToJSON Inline_response_400 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse400")
instance ToSchema Inline_response_400 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse400"


-- | 
data Inline_response_400_invalid_request_body = Inline_response_400_invalid_request_body
  { inlineResponse400InvalidRequestBodyDescription :: Maybe Text -- ^ 
  , inlineResponse400InvalidRequestBodyMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_400_invalid_request_body where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse400InvalidRequestBody")
instance ToJSON Inline_response_400_invalid_request_body where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse400InvalidRequestBody")
instance ToSchema Inline_response_400_invalid_request_body where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse400InvalidRequestBody"


-- | 
data Inline_response_403 = Inline_response_403
  { inlineResponse403HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse403HttpUnderscorestatus :: Maybe Text -- ^ 
  , inlineResponse403InvalidUnderscorecredentials :: Maybe Inline_response_403_invalid_credentials -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_403 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse403")
instance ToJSON Inline_response_403 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse403")
instance ToSchema Inline_response_403 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse403"


-- | 
data Inline_response_403_1 = Inline_response_403_1
  { inlineResponse4031HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse4031AccessUnderscoretokenUnderscoreexpired :: Maybe Inline_response_403_1_access_token_expired -- ^ 
  , inlineResponse4031HttpUnderscorestatus :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_403_1 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4031")
instance ToJSON Inline_response_403_1 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4031")
instance ToSchema Inline_response_403_1 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4031"


-- | 
data Inline_response_403_1_access_token_expired = Inline_response_403_1_access_token_expired
  { inlineResponse4031AccessTokenExpiredDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_403_1_access_token_expired where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4031AccessTokenExpired")
instance ToJSON Inline_response_403_1_access_token_expired where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4031AccessTokenExpired")
instance ToSchema Inline_response_403_1_access_token_expired where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4031AccessTokenExpired"


-- | 
data Inline_response_403_2 = Inline_response_403_2
  { inlineResponse4032EmailUnderscoreaddressUnderscoreinUnderscoreuse :: Maybe Inline_response_403_2_email_address_in_use -- ^ 
  , inlineResponse4032HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse4032OauthUnderscorefailed :: Maybe Inline_response_403_2_oauth_failed -- ^ 
  , inlineResponse4032HttpUnderscorestatus :: Maybe Text -- ^ 
  , inlineResponse4032EmailUnderscorenotUnderscoreauthorized :: Maybe Inline_response_403_2_email_not_authorized -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_403_2 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4032")
instance ToJSON Inline_response_403_2 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4032")
instance ToSchema Inline_response_403_2 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4032"


-- | 
data Inline_response_403_2_email_address_in_use = Inline_response_403_2_email_address_in_use
  { inlineResponse4032EmailAddressInUseMergeUnderscoretoken :: Maybe Text -- ^ 
  , inlineResponse4032EmailAddressInUseDescription :: Maybe Text -- ^ 
  , inlineResponse4032EmailAddressInUseExistingUnderscoreprovider :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_403_2_email_address_in_use where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4032EmailAddressInUse")
instance ToJSON Inline_response_403_2_email_address_in_use where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4032EmailAddressInUse")
instance ToSchema Inline_response_403_2_email_address_in_use where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4032EmailAddressInUse"


-- | 
data Inline_response_403_2_email_not_authorized = Inline_response_403_2_email_not_authorized
  { inlineResponse4032EmailNotAuthorizedDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_403_2_email_not_authorized where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4032EmailNotAuthorized")
instance ToJSON Inline_response_403_2_email_not_authorized where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4032EmailNotAuthorized")
instance ToSchema Inline_response_403_2_email_not_authorized where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4032EmailNotAuthorized"


-- | 
data Inline_response_403_2_oauth_failed = Inline_response_403_2_oauth_failed
  { inlineResponse4032OauthFailedDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_403_2_oauth_failed where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4032OauthFailed")
instance ToJSON Inline_response_403_2_oauth_failed where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4032OauthFailed")
instance ToSchema Inline_response_403_2_oauth_failed where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4032OauthFailed"


-- | 
data Inline_response_403_invalid_credentials = Inline_response_403_invalid_credentials
  { inlineResponse403InvalidCredentialsDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_403_invalid_credentials where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse403InvalidCredentials")
instance ToJSON Inline_response_403_invalid_credentials where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse403InvalidCredentials")
instance ToSchema Inline_response_403_invalid_credentials where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse403InvalidCredentials"


-- | 
data Inline_response_415 = Inline_response_415
  { inlineResponse415HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse415UnsupportedUnderscoremediaUnderscoretype :: Maybe Inline_response_415_unsupported_media_type -- ^ 
  , inlineResponse415HttpUnderscorestatus :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_415 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse415")
instance ToJSON Inline_response_415 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse415")
instance ToSchema Inline_response_415 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse415"


-- | 
data Inline_response_415_unsupported_media_type = Inline_response_415_unsupported_media_type
  { inlineResponse415UnsupportedMediaTypeDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_415_unsupported_media_type where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse415UnsupportedMediaType")
instance ToJSON Inline_response_415_unsupported_media_type where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse415UnsupportedMediaType")
instance ToSchema Inline_response_415_unsupported_media_type where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse415UnsupportedMediaType"


-- | 
data Inline_response_500 = Inline_response_500
  { inlineResponse500HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse500HttpUnderscorestatus :: Maybe Text -- ^ 
  , inlineResponse500InternalUnderscoreserverUnderscoreerror :: Maybe Inline_response_500_internal_server_error -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_500 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse500")
instance ToJSON Inline_response_500 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse500")
instance ToSchema Inline_response_500 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse500"


-- | 
data Inline_response_500_internal_server_error = Inline_response_500_internal_server_error
  { inlineResponse500InternalServerErrorDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inline_response_500_internal_server_error where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse500InternalServerError")
instance ToJSON Inline_response_500_internal_server_error where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse500InternalServerError")
instance ToSchema Inline_response_500_internal_server_error where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse500InternalServerError"


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
