{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Persona.Types (
  ActiveDays (..),
  Address (..),
  AdminNewUser (..),
  CancelSubscriptionReason (..),
  DeleteTempAddressChangeDates (..),
  DeliveryAddress (..),
  DeliveryReclamation (..),
  EntitlementAccess (..),
  FaroUser (..),
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
  JanrainUser (..),
  LegalConsent (..),
  LoginData (..),
  LoginDataSSO (..),
  LoginDataSoMe (..),
  LoginResponse (..),
  NewDeliveryReclamation (..),
  NewTemporaryUser (..),
  NewUser (..),
  Package (..),
  PackageCampaign (..),
  PackageOffer (..),
  Paper (..),
  PastTemporaryAddress (..),
  PausedSubscription (..),
  Payment (..),
  PendingAddressChange (..),
  Product (..),
  SearchQuery (..),
  SearchResult (..),
  Subscription (..),
  SubscriptionDates (..),
  SubscriptionPauseDates (..),
  SubscriptionPauseEdit (..),
  SubscriptionPayments (..),
  TemporaryAddressChange (..),
  TemporaryAddressChangeDates (..),
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
  { activeDaysMon :: Bool -- ^ Active on Monday
  , activeDaysTue :: Bool -- ^ Active on Tuesday
  , activeDaysWed :: Bool -- ^ Active on Wednedsday
  , activeDaysThu :: Bool -- ^ Active on Thursday
  , activeDaysFri :: Bool -- ^ Active on Friday
  , activeDaysSat :: Bool -- ^ Active on Saturday
  , activeDaysSun :: Bool -- ^ Active on Sunday
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ActiveDays where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "activeDays")
instance ToJSON ActiveDays where
  toJSON = genericToJSON (removeFieldLabelPrefix False "activeDays")


-- | Postal address for shipping the papers.
data Address = Address
  { addressCountryCode :: Text -- ^ Country code
  , addressZipCode :: Maybe Text -- ^ Zip code
  , addressCity :: Maybe Text -- ^ City
  , addressStreetAddress :: Text -- ^ Street address, containing all details
  , addressStreetName :: Maybe Text -- ^ Street name
  , addressHouseNo :: Maybe Text -- ^ House number
  , addressStaircase :: Maybe Text -- ^ Staircase letter
  , addressApartment :: Maybe Text -- ^ Apartment number
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Address where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "address")
instance ToJSON Address where
  toJSON = genericToJSON (removeFieldLabelPrefix False "address")


-- | 
data AdminNewUser = AdminNewUser
  { adminNewUserUser :: NewUser -- ^ 
  , adminNewUserCusno :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminNewUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminNewUser")
instance ToJSON AdminNewUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminNewUser")


-- | Cancel reason
data CancelSubscriptionReason = CancelSubscriptionReason
  { cancelSubscriptionReasonReason :: Text -- ^ Cancel reason code
  , cancelSubscriptionReasonNotes :: Maybe Text -- ^ Cancel reason explanation
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CancelSubscriptionReason where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "cancelSubscriptionReason")
instance ToJSON CancelSubscriptionReason where
  toJSON = genericToJSON (removeFieldLabelPrefix False "cancelSubscriptionReason")


-- | 
data DeleteTempAddressChangeDates = DeleteTempAddressChangeDates
  { deleteTempAddressChangeDatesStartDate :: Day -- ^ 
  , deleteTempAddressChangeDatesEndDate :: Maybe Day -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeleteTempAddressChangeDates where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "deleteTempAddressChangeDates")
instance ToJSON DeleteTempAddressChangeDates where
  toJSON = genericToJSON (removeFieldLabelPrefix False "deleteTempAddressChangeDates")


-- | 
data DeliveryAddress = DeliveryAddress
  { deliveryAddressStreetAddress :: Maybe Text -- ^ Street address, containing all details
  , deliveryAddressZipcode :: Text -- ^ 
  , deliveryAddressCity :: Maybe Text -- ^ 
  , deliveryAddressTemporaryName :: Maybe Text -- ^ Temporary name (or c/o) of delivery address
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeliveryAddress where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "deliveryAddress")
instance ToJSON DeliveryAddress where
  toJSON = genericToJSON (removeFieldLabelPrefix False "deliveryAddress")


-- | Data for a delivery reclamation.
data DeliveryReclamation = DeliveryReclamation
  { deliveryReclamationNumber :: Int -- ^ The reclamation identifier
  , deliveryReclamationCustomerNumber :: Int -- ^ The identifier of the customer that made reclamation
  , deliveryReclamationSubscriptionNumber :: Int -- ^ The identifier of the subscription for which reclamation was made
  , deliveryReclamationDate :: Day -- ^ 
  , deliveryReclamationPaper :: Maybe Text -- ^ 
  , deliveryReclamationPublicationDate :: Day -- ^ 
  , deliveryReclamationClaim :: Text -- ^ The type of claim for the reclamation
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeliveryReclamation where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "deliveryReclamation")
instance ToJSON DeliveryReclamation where
  toJSON = genericToJSON (removeFieldLabelPrefix False "deliveryReclamation")


-- | 
data EntitlementAccess = EntitlementAccess
  { entitlementAccessStartAt :: Text -- ^ 
  , entitlementAccessEndAt :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EntitlementAccess where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entitlementAccess")
instance ToJSON EntitlementAccess where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entitlementAccess")


-- | 
data FaroUser = FaroUser
  { faroUserCusno :: Int -- ^ 
  , faroUserName :: Text -- ^ 
  , faroUserAddress :: Maybe Address -- ^ 
  , faroUserEmail :: Maybe Text -- ^ 
  , faroUserSubscriptions :: Maybe [Subscription] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FaroUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "faroUser")
instance ToJSON FaroUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "faroUser")


-- | 
data ForgotPasswordData = ForgotPasswordData
  { forgotPasswordDataEmail :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ForgotPasswordData where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "forgotPasswordData")
instance ToJSON ForgotPasswordData where
  toJSON = genericToJSON (removeFieldLabelPrefix False "forgotPasswordData")


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


-- | 
data InlineResponse400InvalidRequestBody = InlineResponse400InvalidRequestBody
  { inlineResponse400InvalidRequestBodyDescription :: Maybe Text -- ^ 
  , inlineResponse400InvalidRequestBodyMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse400InvalidRequestBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse400InvalidRequestBody")
instance ToJSON InlineResponse400InvalidRequestBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse400InvalidRequestBody")


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


-- | 
data InlineResponse4031AccessTokenExpired = InlineResponse4031AccessTokenExpired
  { inlineResponse4031AccessTokenExpiredDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse4031AccessTokenExpired where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4031AccessTokenExpired")
instance ToJSON InlineResponse4031AccessTokenExpired where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4031AccessTokenExpired")


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


-- | 
data InlineResponse4032EmailNotAuthorized = InlineResponse4032EmailNotAuthorized
  { inlineResponse4032EmailNotAuthorizedDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse4032EmailNotAuthorized where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4032EmailNotAuthorized")
instance ToJSON InlineResponse4032EmailNotAuthorized where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4032EmailNotAuthorized")


-- | 
data InlineResponse4032OauthFailed = InlineResponse4032OauthFailed
  { inlineResponse4032OauthFailedDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse4032OauthFailed where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse4032OauthFailed")
instance ToJSON InlineResponse4032OauthFailed where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse4032OauthFailed")


-- | 
data InlineResponse403InvalidCredentials = InlineResponse403InvalidCredentials
  { inlineResponse403InvalidCredentialsDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse403InvalidCredentials where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse403InvalidCredentials")
instance ToJSON InlineResponse403InvalidCredentials where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse403InvalidCredentials")


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


-- | 
data InlineResponse415UnsupportedMediaType = InlineResponse415UnsupportedMediaType
  { inlineResponse415UnsupportedMediaTypeDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse415UnsupportedMediaType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse415UnsupportedMediaType")
instance ToJSON InlineResponse415UnsupportedMediaType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse415UnsupportedMediaType")


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


-- | 
data InlineResponse500InternalServerError = InlineResponse500InternalServerError
  { inlineResponse500InternalServerErrorDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineResponse500InternalServerError where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse500InternalServerError")
instance ToJSON InlineResponse500InternalServerError where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse500InternalServerError")


-- | 
data JanrainUser = JanrainUser
  { janrainUserUuid :: UUID -- ^ 
  , janrainUserEmail :: Maybe Text -- ^ 
  , janrainUserFirstName :: Maybe Text -- ^ 
  , janrainUserLastName :: Maybe Text -- ^ 
  , janrainUserMiddleName :: Maybe Text -- ^ 
  , janrainUserConsent :: [GdprConsent] -- ^ 
  , janrainUserLegal :: [LegalConsent] -- ^ 
  , janrainUserCusno :: Maybe Text -- ^ 
  , janrainUserOtherCusnos :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON JanrainUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "janrainUser")
instance ToJSON JanrainUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "janrainUser")


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


-- | 
data LoginDataSSO = LoginDataSSO
  { loginDataSSOUuid :: UUID -- ^ 
  , loginDataSSOAccessToken :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LoginDataSSO where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "loginDataSSO")
instance ToJSON LoginDataSSO where
  toJSON = genericToJSON (removeFieldLabelPrefix False "loginDataSSO")


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


-- | 
data LoginResponse = LoginResponse
  { loginResponseToken :: Text -- ^ 
  , loginResponseSsoCode :: Maybe Text -- ^ 
  , loginResponseUuid :: UUID -- ^ 
  , loginResponseIsAdmin :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LoginResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "loginResponse")
instance ToJSON LoginResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "loginResponse")


-- | Data for a delivery reclamation creation.
data NewDeliveryReclamation = NewDeliveryReclamation
  { newDeliveryReclamationPaper :: Maybe Text -- ^ 
  , newDeliveryReclamationPublicationDate :: Day -- ^ 
  , newDeliveryReclamationClaim :: Text -- ^ The type of claim for the reclamation
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NewDeliveryReclamation where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newDeliveryReclamation")
instance ToJSON NewDeliveryReclamation where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newDeliveryReclamation")


-- | 
data NewTemporaryUser = NewTemporaryUser
  { newTemporaryUserEmailAddress :: Text -- ^ 
  , newTemporaryUserLegalConsents :: [LegalConsent] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NewTemporaryUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newTemporaryUser")
instance ToJSON NewTemporaryUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newTemporaryUser")


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


-- | 
data Package = Package
  { packageId :: Text -- ^ Package identifier
  , packageName :: Text -- ^ Package name
  , packageDescription :: [Text] -- ^ Package description
  , packagePaper :: Paper -- ^ 
  , packageDigitalOnly :: Bool -- ^ All products in this package are digital
  , packageProducts :: [Product] -- ^ The Products contained in a package
  , packageOffers :: [PackageOffer] -- ^ Offers for the package
  , packageCampaigns :: [PackageCampaign] -- ^ Active campaigns for the package
  , packageNextDelivery :: Maybe Day -- ^ 
  , packageCanPause :: Bool -- ^ Does the package allow delivery pauses
  , packageCanTempAddr :: Bool -- ^ Does the package allow temporary address changes
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Package where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "package")
instance ToJSON Package where
  toJSON = genericToJSON (removeFieldLabelPrefix False "package")


-- | 
data PackageCampaign = PackageCampaign
  { packageCampaignNo :: Int -- ^ Campaign number
  , packageCampaignId :: Text -- ^ Campaign id
  , packageCampaignName :: Text -- ^ Campaign name
  , packageCampaignPriceEur :: Double -- ^ Price of campaign in euros
  , packageCampaignLength :: Int -- ^ Length of campaign
  , packageCampaignLengthUnit :: Text -- ^ Unit of length (days, weeks, months, years)
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PackageCampaign where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "packageCampaign")
instance ToJSON PackageCampaign where
  toJSON = genericToJSON (removeFieldLabelPrefix False "packageCampaign")


-- | 
data PackageOffer = PackageOffer
  { packageOfferMonths :: Int -- ^ Duration of the offer
  , packageOfferTotalPrice :: Int -- ^ Amount of cents that has to be paid
  , packageOfferMonthlyPrice :: Int -- ^ Amount of cents that has to be paid
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PackageOffer where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "packageOffer")
instance ToJSON PackageOffer where
  toJSON = genericToJSON (removeFieldLabelPrefix False "packageOffer")


-- | 
data Paper = Paper
  { paperCode :: Text -- ^ 
  , paperName :: Text -- ^ The name of the paper
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Paper where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "paper")
instance ToJSON Paper where
  toJSON = genericToJSON (removeFieldLabelPrefix False "paper")


-- | Data for previously used temporary addresses
data PastTemporaryAddress = PastTemporaryAddress
  { pastTemporaryAddressCountryCode :: Text -- ^ Country code
  , pastTemporaryAddressZipcode :: Text -- ^ Zip code
  , pastTemporaryAddressCityName :: Maybe Text -- ^ City
  , pastTemporaryAddressStreet :: Text -- ^ Street
  , pastTemporaryAddressTemporaryName :: Maybe Text -- ^ Temporary name (c/o)
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PastTemporaryAddress where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pastTemporaryAddress")
instance ToJSON PastTemporaryAddress where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pastTemporaryAddress")


-- | 
data PausedSubscription = PausedSubscription
  { pausedSubscriptionStartDate :: Day -- ^ 
  , pausedSubscriptionEndDate :: Day -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PausedSubscription where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pausedSubscription")
instance ToJSON PausedSubscription where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pausedSubscription")


-- | 
data Payment = Payment
  { paymentInvno :: Int -- ^ Payment invoice ID
  , paymentDate :: Day -- ^ 
  , paymentDueDate :: Day -- ^ 
  , paymentExpenses :: Double -- ^ 
  , paymentInterest :: Double -- ^ 
  , paymentVat :: Double -- ^ 
  , paymentAmount :: Double -- ^ 
  , paymentOpenAmount :: Double -- ^ 
  , paymentType :: Text -- ^ 
  , paymentState :: Text -- ^ 
  , paymentDiscPercent :: Maybe Double -- ^ 
  , paymentDiscAmount :: Maybe Double -- ^ 
  , paymentReference :: Maybe Text -- ^ Reference number
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Payment where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "payment")
instance ToJSON Payment where
  toJSON = genericToJSON (removeFieldLabelPrefix False "payment")


-- | 
data PendingAddressChange = PendingAddressChange
  { pendingAddressChangeAddress :: DeliveryAddress -- ^ 
  , pendingAddressChangeStartDate :: Day -- ^ 
  , pendingAddressChangeEndDate :: Maybe Day -- ^ 
  , pendingAddressChangeType :: Text -- ^ Type of address change
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PendingAddressChange where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pendingAddressChange")
instance ToJSON PendingAddressChange where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pendingAddressChange")


-- | 
data Product = Product
  { productId :: Text -- ^ Identifying code of the product
  , productName :: Text -- ^ The name of the product
  , productActive :: ActiveDays -- ^ 
  , productNextDelivery :: Maybe Day -- ^ 
  , productPaper :: Paper -- ^ 
  , productDigital :: Bool -- ^ Is the product digital?
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Product where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "product")
instance ToJSON Product where
  toJSON = genericToJSON (removeFieldLabelPrefix False "product")


-- | 
data SearchQuery = SearchQuery
  { searchQueryFaroLimit :: Int -- ^ 
  , searchQueryQuery :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SearchQuery where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "searchQuery")
instance ToJSON SearchQuery where
  toJSON = genericToJSON (removeFieldLabelPrefix False "searchQuery")


-- | 
data SearchResult = SearchResult
  { searchResultJanrain :: Maybe JanrainUser -- ^ 
  , searchResultFaro :: [FaroUser] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SearchResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "searchResult")
instance ToJSON SearchResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "searchResult")


-- | 
data Subscription = Subscription
  { subscriptionSubsno :: Int -- ^ Subscription Id - primary key together with extno
  , subscriptionExtno :: Int -- ^ Subscription Extension Id - how many times a subscription has been extended
  , subscriptionCusno :: Int -- ^ Customer getting the subscription
  , subscriptionPaycusno :: Int -- ^ Customer paying for the subscription
  , subscriptionKind :: Text -- ^ Subscription kind - what kind of order is it
  , subscriptionState :: Text -- ^ Current state of the Subscription
  , subscriptionPricegroup :: Maybe Text -- ^ Pricegroup of the Subscription
  , subscriptionPackage :: Package -- ^ 
  , subscriptionDates :: SubscriptionDates -- ^ 
  , subscriptionExtsubsexists :: Bool -- ^ If the extension of this subscription exists
  , subscriptionCampaign :: Maybe PackageCampaign -- ^ 
  , subscriptionPaused :: Maybe [PausedSubscription] -- ^ Pause periods of this subscription
  , subscriptionReceiver :: Maybe Text -- ^ The name of subscription receiver
  , subscriptionDeliveryAddress :: Maybe DeliveryAddress -- ^ 
  , subscriptionPendingAddressChanges :: Maybe [PendingAddressChange] -- ^ Pending and ongoing temporary address changes
  , subscriptionOrderNumber :: Maybe Text -- ^ Order number of subscription
  , subscriptionPaymentMethod :: Maybe Text -- ^ Payment method of subscription
  , subscriptionPaymentMethodId :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Subscription where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscription")
instance ToJSON Subscription where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscription")


-- | 
data SubscriptionDates = SubscriptionDates
  { subscriptionDatesLenMonths :: Maybe Int -- ^ Length of Subscription in months
  , subscriptionDatesLenDays :: Maybe Int -- ^ Additional days (on top of months) for Subscription duration
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


-- | 
data SubscriptionPauseDates = SubscriptionPauseDates
  { subscriptionPauseDatesStartDate :: Day -- ^ 
  , subscriptionPauseDatesEndDate :: Day -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPauseDates where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPauseDates")
instance ToJSON SubscriptionPauseDates where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPauseDates")


-- | 
data SubscriptionPauseEdit = SubscriptionPauseEdit
  { subscriptionPauseEditOldStartDate :: Day -- ^ 
  , subscriptionPauseEditOldEndDate :: Day -- ^ 
  , subscriptionPauseEditNewStartDate :: Day -- ^ 
  , subscriptionPauseEditNewEndDate :: Day -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPauseEdit where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPauseEdit")
instance ToJSON SubscriptionPauseEdit where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPauseEdit")


-- | 
data SubscriptionPayments = SubscriptionPayments
  { subscriptionPaymentsSubsno :: Int -- ^ Product subsno
  , subscriptionPaymentsName :: Text -- ^ Package name
  , subscriptionPaymentsStartDate :: Day -- ^ 
  , subscriptionPaymentsLastDate :: Day -- ^ 
  , subscriptionPaymentsPayments :: [Payment] -- ^ Payments
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPayments where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPayments")
instance ToJSON SubscriptionPayments where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPayments")


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


-- | Data for changing a temporary address change.
data TemporaryAddressChangeDates = TemporaryAddressChangeDates
  { temporaryAddressChangeDatesOldStartDate :: Day -- ^ 
  , temporaryAddressChangeDatesNewStartDate :: Day -- ^ 
  , temporaryAddressChangeDatesNewEndDate :: Maybe Day -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemporaryAddressChangeDates where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "temporaryAddressChangeDates")
instance ToJSON TemporaryAddressChangeDates where
  toJSON = genericToJSON (removeFieldLabelPrefix False "temporaryAddressChangeDates")


-- | 
data UpdatePasswordData = UpdatePasswordData
  { updatePasswordDataPassword :: Maybe Text -- ^ 
  , updatePasswordDataConfirmPassword :: Maybe Text -- ^ 
  , updatePasswordDataToken :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdatePasswordData where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updatePasswordData")
instance ToJSON UpdatePasswordData where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updatePasswordData")


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
  , userPastTemporaryAddresses :: [PastTemporaryAddress] -- ^ 
  , userHasCompletedRegistration :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON User where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "user")
instance ToJSON User where
  toJSON = genericToJSON (removeFieldLabelPrefix False "user")


-- | 
data UserUpdate = UserUpdate
  { userUpdateFirstName :: Maybe Text -- ^ 
  , userUpdateLastName :: Maybe Text -- ^ 
  , userUpdateAddress :: Maybe UserUpdateAddress -- ^ 
  , userUpdateEmail :: Maybe Text -- ^ 
  , userUpdatePendingAddressChanges :: Maybe [Value] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userUpdate")
instance ToJSON UserUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userUpdate")


-- | 
data UserUpdateAddress = UserUpdateAddress
  { userUpdateAddressCountryCode :: Text -- ^ 
  , userUpdateAddressZipCode :: Text -- ^ 
  , userUpdateAddressStreetAddress :: Text -- ^ 
  , userUpdateAddressValidFrom :: Maybe Day -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserUpdateAddress where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userUpdateAddress")
instance ToJSON UserUpdateAddress where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userUpdateAddress")


-- | 
data UserUpdatePassword = UserUpdatePassword
  { userUpdatePasswordPassword :: Text -- ^ 
  , userUpdatePasswordConfirmPassword :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserUpdatePassword where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userUpdatePassword")
instance ToJSON UserUpdatePassword where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userUpdatePassword")


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
