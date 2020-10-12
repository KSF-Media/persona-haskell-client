{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Persona.Types (
  ActiveDays (..),
  Address (..),
  Campaign (..),
  CodeForTokenData (..),
  DeleteTempAddressChangeDates (..),
  DeliveryAddress (..),
  DeliveryReclamation (..),
  DeliveryReclamationClaim (..),
  DescriptionFrequency (..),
  EntitlementAccess (..),
  ForgotPasswordData (..),
  GdprConsent (..),
  InlineResponse400 (..),
  InlineResponse400InvalidRequestBody (..),
  InlineResponse403 (..),
  InlineResponse4031 (..),
  InlineResponse4031AccessTokenExpired (..),
  InlineResponse4032 (..),
  InlineResponse4032EmailAddressInUse (..),
  InlineResponse4032EmailNotAuthorized (..),
  InlineResponse4032OauthFailed (..),
  InlineResponse403InvalidCredentials (..),
  InlineResponse415 (..),
  InlineResponse415UnsupportedMediaType (..),
  InlineResponse500 (..),
  InlineResponse500InternalServerError (..),
  LegalConsent (..),
  LoginData (..),
  LoginDataSSO (..),
  LoginDataSoMe (..),
  LoginResponse (..),
  NewDeliveryReclamation (..),
  NewTemporaryUser (..),
  NewUser (..),
  Package (..),
  PackageDescription (..),
  PackageOffer (..),
  Paper (..),
  PausedSubscription (..),
  PendingAddressChange (..),
  Product (..),
  Subscription (..),
  SubscriptionDates (..),
  SubscriptionPauseDates (..),
  TemporaryAddressChange (..),
  TokenResponse (..),
  UpdatePasswordData (..),
  User (..),
  UserUpdate (..),
  UserUpdateAddress (..),
  UserUpdatePassword (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
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
data CodeForTokenData = CodeForTokenData
  { codeForTokenDataCode :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CodeForTokenData where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "codeForTokenData")
instance ToJSON CodeForTokenData where
  toJSON = genericToJSON (removeFieldLabelPrefix False "codeForTokenData")
instance ToSchema CodeForTokenData where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "codeForTokenData"


-- | 
data DeleteTempAddressChangeDates = DeleteTempAddressChangeDates
  { deleteTempAddressChangeDatesStartDate :: Day -- ^ 
  , deleteTempAddressChangeDatesEndDate :: Day -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeleteTempAddressChangeDates where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "deleteTempAddressChangeDates")
instance ToJSON DeleteTempAddressChangeDates where
  toJSON = genericToJSON (removeFieldLabelPrefix False "deleteTempAddressChangeDates")
instance ToSchema DeleteTempAddressChangeDates where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "deleteTempAddressChangeDates"


-- | 
data DeliveryAddress = DeliveryAddress
  { deliveryAddressStreetAddress :: Maybe Text -- ^ 
  , deliveryAddressZipcode :: Text -- ^ 
  , deliveryAddressCity :: Maybe Text -- ^ 
  , deliveryAddressTemporaryName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeliveryAddress where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "deliveryAddress")
instance ToJSON DeliveryAddress where
  toJSON = genericToJSON (removeFieldLabelPrefix False "deliveryAddress")
instance ToSchema DeliveryAddress where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "deliveryAddress"


-- | 
data DeliveryReclamation = DeliveryReclamation
  { deliveryReclamationNumber :: Int -- ^ 
  , deliveryReclamationCustomerNumber :: Int -- ^ 
  , deliveryReclamationSubscriptionNumber :: Int -- ^ 
  , deliveryReclamationDate :: Day -- ^ 
  , deliveryReclamationPublicationDate :: Day -- ^ 
  , deliveryReclamationClaim :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeliveryReclamation where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "deliveryReclamation")
instance ToJSON DeliveryReclamation where
  toJSON = genericToJSON (removeFieldLabelPrefix False "deliveryReclamation")
instance ToSchema DeliveryReclamation where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "deliveryReclamation"


-- | 
data DeliveryReclamationClaim = DeliveryReclamationClaim
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeliveryReclamationClaim where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "deliveryReclamationClaim")
instance ToJSON DeliveryReclamationClaim where
  toJSON = genericToJSON (removeFieldLabelPrefix False "deliveryReclamationClaim")
instance ToSchema DeliveryReclamationClaim where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "deliveryReclamationClaim"


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
data EntitlementAccess = EntitlementAccess
  { entitlementAccessStartAt :: Text -- ^ 
  , entitlementAccessEndAt :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EntitlementAccess where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entitlementAccess")
instance ToJSON EntitlementAccess where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entitlementAccess")
instance ToSchema EntitlementAccess where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "entitlementAccess"


-- | 
data ForgotPasswordData = ForgotPasswordData
  { forgotPasswordDataEmail :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ForgotPasswordData where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "forgotPasswordData")
instance ToJSON ForgotPasswordData where
  toJSON = genericToJSON (removeFieldLabelPrefix False "forgotPasswordData")
instance ToSchema ForgotPasswordData where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "forgotPasswordData"


-- | 
data GdprConsent = GdprConsent
  { gdprConsentBrand :: Text -- ^ 
  , gdprConsentConsentKey :: Text -- ^ 
  , gdprConsentValue :: Bool -- ^ 
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
data InlineResponse400 = InlineResponse400
  { inlineResponse400HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse400InvalidUnderscorerequestUnderscorebody :: Maybe InlineResponse400InvalidRequestBody -- ^ 
  , inlineResponse400HttpUnderscorestatus :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse400 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse400")
instance ToJSON InlineResponse400 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse400")
instance ToSchema InlineResponse400 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse400"


-- | 
data InlineResponse400InvalidRequestBody = InlineResponse400InvalidRequestBody
  { inlineResponse400InvalidRequestBodyDescription :: Maybe Text -- ^ 
  , inlineResponse400InvalidRequestBodyMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse400InvalidRequestBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse400InvalidRequestBody")
instance ToJSON InlineResponse400InvalidRequestBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse400InvalidRequestBody")
instance ToSchema InlineResponse400InvalidRequestBody where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse400InvalidRequestBody"


-- | 
data InlineResponse403 = InlineResponse403
  { inlineResponse403HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse403HttpUnderscorestatus :: Maybe Text -- ^ 
  , inlineResponse403InvalidUnderscorecredentials :: Maybe InlineResponse403InvalidCredentials -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse403 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse403")
instance ToJSON InlineResponse403 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse403")
instance ToSchema InlineResponse403 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse403"


-- | 
data InlineResponse4031 = InlineResponse4031
  { inlineResponse4031HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse4031AccessUnderscoretokenUnderscoreexpired :: Maybe InlineResponse4031AccessTokenExpired -- ^ 
  , inlineResponse4031HttpUnderscorestatus :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse4031 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4031")
instance ToJSON InlineResponse4031 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4031")
instance ToSchema InlineResponse4031 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4031"


-- | 
data InlineResponse4031AccessTokenExpired = InlineResponse4031AccessTokenExpired
  { inlineResponse4031AccessTokenExpiredDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse4031AccessTokenExpired where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4031AccessTokenExpired")
instance ToJSON InlineResponse4031AccessTokenExpired where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4031AccessTokenExpired")
instance ToSchema InlineResponse4031AccessTokenExpired where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4031AccessTokenExpired"


-- | 
data InlineResponse4032 = InlineResponse4032
  { inlineResponse4032EmailUnderscoreaddressUnderscoreinUnderscoreuse :: Maybe InlineResponse4032EmailAddressInUse -- ^ 
  , inlineResponse4032HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse4032OauthUnderscorefailed :: Maybe InlineResponse4032OauthFailed -- ^ 
  , inlineResponse4032HttpUnderscorestatus :: Maybe Text -- ^ 
  , inlineResponse4032EmailUnderscorenotUnderscoreauthorized :: Maybe InlineResponse4032EmailNotAuthorized -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse4032 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4032")
instance ToJSON InlineResponse4032 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4032")
instance ToSchema InlineResponse4032 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4032"


-- | 
data InlineResponse4032EmailAddressInUse = InlineResponse4032EmailAddressInUse
  { inlineResponse4032EmailAddressInUseMergeUnderscoretoken :: Maybe Text -- ^ 
  , inlineResponse4032EmailAddressInUseDescription :: Maybe Text -- ^ 
  , inlineResponse4032EmailAddressInUseExistingUnderscoreprovider :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse4032EmailAddressInUse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4032EmailAddressInUse")
instance ToJSON InlineResponse4032EmailAddressInUse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4032EmailAddressInUse")
instance ToSchema InlineResponse4032EmailAddressInUse where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4032EmailAddressInUse"


-- | 
data InlineResponse4032EmailNotAuthorized = InlineResponse4032EmailNotAuthorized
  { inlineResponse4032EmailNotAuthorizedDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse4032EmailNotAuthorized where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4032EmailNotAuthorized")
instance ToJSON InlineResponse4032EmailNotAuthorized where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4032EmailNotAuthorized")
instance ToSchema InlineResponse4032EmailNotAuthorized where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4032EmailNotAuthorized"


-- | 
data InlineResponse4032OauthFailed = InlineResponse4032OauthFailed
  { inlineResponse4032OauthFailedDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse4032OauthFailed where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4032OauthFailed")
instance ToJSON InlineResponse4032OauthFailed where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4032OauthFailed")
instance ToSchema InlineResponse4032OauthFailed where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse4032OauthFailed"


-- | 
data InlineResponse403InvalidCredentials = InlineResponse403InvalidCredentials
  { inlineResponse403InvalidCredentialsDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse403InvalidCredentials where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse403InvalidCredentials")
instance ToJSON InlineResponse403InvalidCredentials where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse403InvalidCredentials")
instance ToSchema InlineResponse403InvalidCredentials where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse403InvalidCredentials"


-- | 
data InlineResponse415 = InlineResponse415
  { inlineResponse415HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse415UnsupportedUnderscoremediaUnderscoretype :: Maybe InlineResponse415UnsupportedMediaType -- ^ 
  , inlineResponse415HttpUnderscorestatus :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse415 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse415")
instance ToJSON InlineResponse415 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse415")
instance ToSchema InlineResponse415 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse415"


-- | 
data InlineResponse415UnsupportedMediaType = InlineResponse415UnsupportedMediaType
  { inlineResponse415UnsupportedMediaTypeDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse415UnsupportedMediaType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse415UnsupportedMediaType")
instance ToJSON InlineResponse415UnsupportedMediaType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse415UnsupportedMediaType")
instance ToSchema InlineResponse415UnsupportedMediaType where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse415UnsupportedMediaType"


-- | 
data InlineResponse500 = InlineResponse500
  { inlineResponse500HttpUnderscorecode :: Maybe Int -- ^ 
  , inlineResponse500HttpUnderscorestatus :: Maybe Text -- ^ 
  , inlineResponse500InternalUnderscoreserverUnderscoreerror :: Maybe InlineResponse500InternalServerError -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse500 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse500")
instance ToJSON InlineResponse500 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse500")
instance ToSchema InlineResponse500 where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse500"


-- | 
data InlineResponse500InternalServerError = InlineResponse500InternalServerError
  { inlineResponse500InternalServerErrorDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse500InternalServerError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse500InternalServerError")
instance ToJSON InlineResponse500InternalServerError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse500InternalServerError")
instance ToSchema InlineResponse500InternalServerError where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inlineResponse500InternalServerError"


-- | 
data LegalConsent = LegalConsent
  { legalConsentScreenName :: Text -- ^ 
  , legalConsentConsentId :: Text -- ^ 
  , legalConsentDateAccepted :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LegalConsent where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "legalConsent")
instance ToJSON LegalConsent where
  toJSON = genericToJSON (removeFieldLabelPrefix False "legalConsent")
instance ToSchema LegalConsent where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "legalConsent"


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
data NewDeliveryReclamation = NewDeliveryReclamation
  { newDeliveryReclamationPublicationDate :: Day -- ^ 
  , newDeliveryReclamationClaim :: DeliveryReclamationClaim -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NewDeliveryReclamation where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newDeliveryReclamation")
instance ToJSON NewDeliveryReclamation where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newDeliveryReclamation")
instance ToSchema NewDeliveryReclamation where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "newDeliveryReclamation"


-- | 
data NewTemporaryUser = NewTemporaryUser
  { newTemporaryUserEmailAddress :: Text -- ^ 
  , newTemporaryUserLegalConsents :: [LegalConsent] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NewTemporaryUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newTemporaryUser")
instance ToJSON NewTemporaryUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newTemporaryUser")
instance ToSchema NewTemporaryUser where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "newTemporaryUser"


-- | 
data NewUser = NewUser
  { newUserFirstName :: Maybe Text -- ^ 
  , newUserLastName :: Maybe Text -- ^ 
  , newUserEmailAddress :: Text -- ^ 
  , newUserPassword :: Text -- ^ 
  , newUserConfirmPassword :: Text -- ^ 
  , newUserStreetAddress :: Maybe Text -- ^ 
  , newUserZipCode :: Maybe Text -- ^ 
  , newUserCity :: Maybe Text -- ^ 
  , newUserCountry :: Maybe Text -- ^ 
  , newUserPhone :: Maybe Text -- ^ 
  , newUserLegalConsents :: [LegalConsent] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NewUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newUser")
instance ToJSON NewUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newUser")
instance ToSchema NewUser where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "newUser"


-- | 
data Package = Package
  { packageId :: Text -- ^ 
  , packageName :: Text -- ^ 
  , packagePaper :: Paper -- ^ 
  , packageDigitalOnly :: Bool -- ^ 
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
data PausedSubscription = PausedSubscription
  { pausedSubscriptionStartDate :: Day -- ^ 
  , pausedSubscriptionEndDate :: Maybe Day -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PausedSubscription where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pausedSubscription")
instance ToJSON PausedSubscription where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pausedSubscription")
instance ToSchema PausedSubscription where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "pausedSubscription"


-- | 
data PendingAddressChange = PendingAddressChange
  { pendingAddressChangeAddress :: DeliveryAddress -- ^ 
  , pendingAddressChangeStartDate :: Day -- ^ 
  , pendingAddressChangeEndDate :: Day -- ^ 
  , pendingAddressChangeType :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PendingAddressChange where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pendingAddressChange")
instance ToJSON PendingAddressChange where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pendingAddressChange")
instance ToSchema PendingAddressChange where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "pendingAddressChange"


-- | 
data Product = Product
  { productId :: Text -- ^ 
  , productName :: Text -- ^ 
  , productActive :: ActiveDays -- ^ 
  , productNextDelivery :: Maybe Day -- ^ 
  , productPaper :: Paper -- ^ 
  , productDigital :: Bool -- ^ 
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
  , subscriptionPaused :: Maybe [PausedSubscription] -- ^ 
  , subscriptionDeliveryAddress :: Maybe DeliveryAddress -- ^ 
  , subscriptionPendingAddressChanges :: Maybe [PendingAddressChange] -- ^ 
  , subscriptionOrderNumber :: Maybe Text -- ^ 
  , subscriptionPaymentMethod :: Maybe Text -- ^ 
  , subscriptionPaymentMethodId :: Maybe Int -- ^ 
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
data SubscriptionPauseDates = SubscriptionPauseDates
  { subscriptionPauseDatesStartDate :: Day -- ^ 
  , subscriptionPauseDatesEndDate :: Day -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPauseDates where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPauseDates")
instance ToJSON SubscriptionPauseDates where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPauseDates")
instance ToSchema SubscriptionPauseDates where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionPauseDates"


-- | 
data TemporaryAddressChange = TemporaryAddressChange
  { temporaryAddressChangeZipCode :: Text -- ^ 
  , temporaryAddressChangeStreetAddress :: Text -- ^ 
  , temporaryAddressChangeCountryCode :: Text -- ^ 
  , temporaryAddressChangeStartDate :: Day -- ^ 
  , temporaryAddressChangeEndDate :: Maybe Day -- ^ 
  , temporaryAddressChangeTemporaryName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemporaryAddressChange where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "temporaryAddressChange")
instance ToJSON TemporaryAddressChange where
  toJSON = genericToJSON (removeFieldLabelPrefix False "temporaryAddressChange")
instance ToSchema TemporaryAddressChange where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "temporaryAddressChange"


-- | 
data TokenResponse = TokenResponse
  { tokenResponseAccessUnderscoretoken :: Text -- ^ 
  , tokenResponseStatus :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TokenResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "tokenResponse")
instance ToJSON TokenResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "tokenResponse")
instance ToSchema TokenResponse where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "tokenResponse"


-- | 
data UpdatePasswordData = UpdatePasswordData
  { updatePasswordDataPassword :: Text -- ^ 
  , updatePasswordDataPasswordUnderscoreconfirm :: Text -- ^ 
  , updatePasswordDataToken :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdatePasswordData where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updatePasswordData")
instance ToJSON UpdatePasswordData where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updatePasswordData")
instance ToSchema UpdatePasswordData where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "updatePasswordData"


-- | 
data User = User
  { userUuid :: UUID -- ^ 
  , userEmail :: Text -- ^ 
  , userFirstName :: Maybe Text -- ^ 
  , userLastName :: Maybe Text -- ^ 
  , userAddress :: Maybe Address -- ^ 
  , userCusno :: Int -- ^ 
  , userSubs :: [Subscription] -- ^ 
  , userConsent :: [GdprConsent] -- ^ 
  , userLegal :: [LegalConsent] -- ^ 
  , userPendingAddressChanges :: Maybe [PendingAddressChange] -- ^ 
  , userHasCompletedRegistration :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON User where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "user")
instance ToJSON User where
  toJSON = genericToJSON (removeFieldLabelPrefix False "user")
instance ToSchema User where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "user"


-- | 
data UserUpdate = UserUpdate
  { userUpdateFirstName :: Maybe Text -- ^ 
  , userUpdateLastName :: Maybe Text -- ^ 
  , userUpdateAddress :: Maybe UserUpdateAddress -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userUpdate")
instance ToJSON UserUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userUpdate")
instance ToSchema UserUpdate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "userUpdate"


-- | 
data UserUpdateAddress = UserUpdateAddress
  { userUpdateAddressCountryCode :: Text -- ^ 
  , userUpdateAddressZipCode :: Text -- ^ 
  , userUpdateAddressStreetAddress :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserUpdateAddress where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userUpdateAddress")
instance ToJSON UserUpdateAddress where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userUpdateAddress")
instance ToSchema UserUpdateAddress where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "userUpdateAddress"


-- | 
data UserUpdatePassword = UserUpdatePassword
  { userUpdatePasswordPassword :: Text -- ^ 
  , userUpdatePasswordConfirmPassword :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserUpdatePassword where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userUpdatePassword")
instance ToJSON UserUpdatePassword where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userUpdatePassword")
instance ToSchema UserUpdatePassword where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "userUpdatePassword"


uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do viceversa when sending data instead.
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
