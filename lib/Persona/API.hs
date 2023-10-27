{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module Persona.API
  -- * Client and Server
  ( Config(..)
  , PersonaBackend(..)
  , createPersonaClient
  , runPersonaServer
  , runPersonaClient
  , runPersonaClientWithManager
  , callPersona
  , PersonaClient
  , PersonaClientError(..)
  -- ** Servant
  , PersonaAPI
  ) where

import           Persona.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serve)
import           Servant.API
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..))
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for Persona.
type PersonaAPI
    =    "account" :> "password" :> "forgot" :> QueryParam "email" Text :> QueryParam "redir" Bool :> Verb 'GET 200 '[JSON] [Value] -- 'accountPasswordForgotGet' route
    :<|> "account" :> "password" :> "forgot" :> ReqBody '[JSON] ForgotPasswordData :> Verb 'POST 200 '[JSON] [Value] -- 'accountPasswordForgotPost' route
    :<|> "account" :> "password" :> "reset" :> ReqBody '[JSON] UpdatePasswordData :> Verb 'POST 200 '[JSON] [Value] -- 'accountPasswordResetPost' route
    :<|> "admin" :> "search" :> ReqBody '[JSON] SearchQuery :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] [SearchResult] -- 'adminSearchPost' route
    :<|> "admin" :> "transfer-passive-subscribers" :> Capture "listid" Text :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] Value -- 'adminTransferPassiveSubscribersListidPost' route
    :<|> "admin" :> "user" :> ReqBody '[JSON] AdminNewUser :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] LoginResponse -- 'adminUserPost' route
    :<|> "entitlements" :> "allow" :> ReqBody '[JSON] Integer :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'DELETE 200 '[JSON] [Value] -- 'entitlementsAllowDelete' route
    :<|> "entitlements" :> "allow" :> QueryParam "ip" Text :> QueryParam "paper" Text :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'GET 200 '[JSON] [PersistedEntitlementAccess] -- 'entitlementsAllowGet' route
    :<|> "entitlements" :> "allow" :> ReqBody '[JSON] EntitlementAccess :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] [Value] -- 'entitlementsAllowPost' route
    :<|> "entitlements" :> "allow" :> Capture "uuid" UUID :> ReqBody '[JSON] EntitlementAccess :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] [Value] -- 'entitlementsAllowUuidPost' route
    :<|> "entitlements" :> Verb 'GET 200 '[JSON] ((Map.Map String [Text])) -- 'entitlementsGet' route
    :<|> "entitlements" :> "global" :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'GET 200 '[JSON] [PersistedEntitlementAccess] -- 'entitlementsGlobalGet' route
    :<|> "identification" :> "login" :> Verb 'GET 200 '[JSON] () -- 'identificationLoginGet' route
    :<|> "identification" :> "login" :> "monitor" :> Verb 'GET 200 '[JSON] () -- 'identificationLoginMonitorGet' route
    :<|> "identification" :> "login" :> "return" :> QueryParam "code" Text :> QueryParam "state" Text :> Header "cookie" FilePath :> Header "cookie" FilePath :> Verb 'GET 200 '[JSON] Text -- 'identificationLoginReturnGet' route
    :<|> "identification" :> "user" :> "stamp" :> Capture "uuid" UUID :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] Text -- 'identificationUserStampUuidPost' route
    :<|> "login" :> "ip" :> QueryParam "paper" Text :> Header "X-Real-IP" Text :> Verb 'GET 200 '[JSON] LoginResponse -- 'loginIpGet' route
    :<|> "login" :> ReqBody '[JSON] LoginData :> Verb 'POST 200 '[JSON] LoginResponse -- 'loginPost' route
    :<|> "login" :> "some" :> ReqBody '[JSON] LoginDataSoMe :> Verb 'POST 200 '[JSON] LoginResponse -- 'loginSomePost' route
    :<|> "login" :> "sso" :> ReqBody '[JSON] LoginDataSSO :> Verb 'POST 200 '[JSON] LoginResponse -- 'loginSsoPost' route
    :<|> "login" :> Capture "uuid" UUID :> QueryParam "everywhere" Bool :> Header "Authorization" Text :> Verb 'DELETE 200 '[JSON] [Value] -- 'loginUuidDelete' route
    :<|> "users" :> ReqBody '[JSON] NewUser :> Verb 'POST 200 '[JSON] LoginResponse -- 'usersPost' route
    :<|> "users" :> "temporary" :> ReqBody '[JSON] NewTemporaryUser :> Verb 'POST 200 '[JSON] LoginResponse -- 'usersTemporaryPost' route
    :<|> "users" :> Capture "uuid" UUID :> "entitlement" :> Header "AuthUser" UUID :> Header "Authorization" Text :> Header "Cache-Control" Text :> Verb 'GET 200 '[JSON] [Text] -- 'usersUuidEntitlementGet' route
    :<|> "users" :> Capture "uuid" UUID :> "gdpr" :> ReqBody '[JSON] [GdprConsent] :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'PUT 200 '[JSON] User -- 'usersUuidGdprPut' route
    :<|> "users" :> Capture "uuid" UUID :> Header "AuthUser" UUID :> Header "Authorization" Text :> Header "Cache-Control" Text :> Verb 'GET 200 '[JSON] User -- 'usersUuidGet' route
    :<|> "users" :> Capture "uuid" UUID :> "legal" :> ReqBody '[JSON] [LegalConsent] :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'PUT 200 '[JSON] User -- 'usersUuidLegalPut' route
    :<|> "users" :> Capture "uuid" UUID :> "newsletters" :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'GET 200 '[JSON] [Newsletter] -- 'usersUuidNewslettersGet' route
    :<|> "users" :> Capture "uuid" UUID :> "newsletters" :> ReqBody '[JSON] [Newsletter] :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'PUT 200 '[JSON] [Newsletter] -- 'usersUuidNewslettersPut' route
    :<|> "users" :> Capture "uuid" UUID :> "password" :> ReqBody '[JSON] UserUpdatePassword :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'PUT 200 '[JSON] User -- 'usersUuidPasswordPut' route
    :<|> "users" :> Capture "uuid" UUID :> ReqBody '[JSON] UserUpdate :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'PATCH 200 '[JSON] User -- 'usersUuidPatch' route
    :<|> "users" :> Capture "uuid" UUID :> "payments" :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'GET 200 '[JSON] [SubscriptionPayments] -- 'usersUuidPaymentsGet' route
    :<|> "users" :> Capture "uuid" UUID :> "scope" :> QueryParam "scope" Text :> Header "Authorization" Text :> Verb 'GET 200 '[JSON] Int -- 'usersUuidScopeGet' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "addressChange" :> ReqBody '[JSON] DeleteTempAddressChangeDates :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'DELETE 200 '[JSON] Subscription -- 'usersUuidSubscriptionsSubsnoAddressChangeDelete' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "addressChange" :> ReqBody '[JSON] TemporaryAddressChangeDates :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'PATCH 200 '[JSON] Subscription -- 'usersUuidSubscriptionsSubsnoAddressChangePatch' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "addressChange" :> ReqBody '[JSON] TemporaryAddressChange :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] Subscription -- 'usersUuidSubscriptionsSubsnoAddressChangePost' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "cancel" :> ReqBody '[JSON] CancelSubscriptionReason :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'PUT 200 '[JSON] Subscription -- 'usersUuidSubscriptionsSubsnoCancelPut' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "pause" :> ReqBody '[JSON] SubscriptionPauseEdit :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'PATCH 200 '[JSON] Subscription -- 'usersUuidSubscriptionsSubsnoPausePatch' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "pause" :> ReqBody '[JSON] SubscriptionPauseDates :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] Subscription -- 'usersUuidSubscriptionsSubsnoPausePost' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "reclamation" :> ReqBody '[JSON] NewDeliveryReclamation :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] DeliveryReclamation -- 'usersUuidSubscriptionsSubsnoReclamationPost' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "reclamations" :> Capture "reclaimno" Int :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'GET 200 '[JSON] DeliveryReclamation -- 'usersUuidSubscriptionsSubsnoReclamationsReclaimnoGet' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "unpause" :> QueryParam "startDate" Day :> QueryParam "endDate" Day :> Header "AuthUser" UUID :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] Subscription -- 'usersUuidSubscriptionsSubsnoUnpausePost' route


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype PersonaClientError = PersonaClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for Persona.
-- The backend can be used both for the client and the server. The client generated from the Persona OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createPersonaClient@). Alternatively, provided
-- a backend, the API can be served using @runPersonaServer@.
data PersonaBackend m = PersonaBackend
  { accountPasswordForgotGet :: Maybe Text -> Maybe Bool -> m [Value]{- ^  -}
  , accountPasswordForgotPost :: ForgotPasswordData -> m [Value]{- ^  -}
  , accountPasswordResetPost :: UpdatePasswordData -> m [Value]{- ^  -}
  , adminSearchPost :: SearchQuery -> Maybe UUID -> Maybe Text -> m [SearchResult]{- ^  -}
  , adminTransferPassiveSubscribersListidPost :: Text -> Maybe UUID -> Maybe Text -> m Value{- ^ Passive subscribers/members/customers are users who don't have active entitlements and haven't opted out from email marketing. For the given list (audience) ID, this endpoint transfers the list of passive subscribers from Kayak to Mailchimp (via Faro). -}
  , adminUserPost :: AdminNewUser -> Maybe UUID -> Maybe Text -> m LoginResponse{- ^  -}
  , entitlementsAllowDelete :: Integer -> Maybe UUID -> Maybe Text -> m [Value]{- ^  -}
  , entitlementsAllowGet :: Maybe Text -> Maybe Text -> Maybe UUID -> Maybe Text -> m [PersistedEntitlementAccess]{- ^  -}
  , entitlementsAllowPost :: EntitlementAccess -> Maybe UUID -> Maybe Text -> m [Value]{- ^  -}
  , entitlementsAllowUuidPost :: UUID -> EntitlementAccess -> Maybe UUID -> Maybe Text -> m [Value]{- ^  -}
  , entitlementsGet :: m ((Map.Map String [Text])){- ^  -}
  , entitlementsGlobalGet :: Maybe UUID -> Maybe Text -> m [PersistedEntitlementAccess]{- ^  -}
  , identificationLoginGet :: m (){- ^  -}
  , identificationLoginMonitorGet :: m (){- ^  -}
  , identificationLoginReturnGet :: Maybe Text -> Maybe Text -> Maybe FilePath -> Maybe FilePath -> m Text{- ^  -}
  , identificationUserStampUuidPost :: UUID -> Maybe UUID -> Maybe Text -> m Text{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , loginIpGet :: Maybe Text -> Maybe Text -> m LoginResponse{- ^ Returns auth & token for customers with IP based entitlement -}
  , loginPost :: LoginData -> m LoginResponse{- ^  -}
  , loginSomePost :: LoginDataSoMe -> m LoginResponse{- ^  -}
  , loginSsoPost :: LoginDataSSO -> m LoginResponse{- ^  -}
  , loginUuidDelete :: UUID -> Maybe Bool -> Maybe Text -> m [Value]{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersPost :: NewUser -> m LoginResponse{- ^  -}
  , usersTemporaryPost :: NewTemporaryUser -> m LoginResponse{- ^  -}
  , usersUuidEntitlementGet :: UUID -> Maybe UUID -> Maybe Text -> Maybe Text -> m [Text]{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidGdprPut :: UUID -> [GdprConsent] -> Maybe UUID -> Maybe Text -> m User{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidGet :: UUID -> Maybe UUID -> Maybe Text -> Maybe Text -> m User{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidLegalPut :: UUID -> [LegalConsent] -> Maybe UUID -> Maybe Text -> m User{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidNewslettersGet :: UUID -> Maybe UUID -> Maybe Text -> m [Newsletter]{- ^ Get list of newsletter subscriptions from mailchimp -}
  , usersUuidNewslettersPut :: UUID -> [Newsletter] -> Maybe UUID -> Maybe Text -> m [Newsletter]{- ^ Get list of newsletter subscriptions from mailchimp -}
  , usersUuidPasswordPut :: UUID -> UserUpdatePassword -> Maybe UUID -> Maybe Text -> m User{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidPatch :: UUID -> UserUpdate -> Maybe UUID -> Maybe Text -> m User{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidPaymentsGet :: UUID -> Maybe UUID -> Maybe Text -> m [SubscriptionPayments]{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidScopeGet :: UUID -> Maybe Text -> Maybe Text -> m Int{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidSubscriptionsSubsnoAddressChangeDelete :: UUID -> Int -> DeleteTempAddressChangeDates -> Maybe UUID -> Maybe Text -> m Subscription{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidSubscriptionsSubsnoAddressChangePatch :: UUID -> Int -> TemporaryAddressChangeDates -> Maybe UUID -> Maybe Text -> m Subscription{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidSubscriptionsSubsnoAddressChangePost :: UUID -> Int -> TemporaryAddressChange -> Maybe UUID -> Maybe Text -> m Subscription{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidSubscriptionsSubsnoCancelPut :: UUID -> Int -> CancelSubscriptionReason -> Maybe UUID -> Maybe Text -> m Subscription{- ^ The subscription continues to be valid until the end of the billing period. Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidSubscriptionsSubsnoPausePatch :: UUID -> Int -> SubscriptionPauseEdit -> Maybe UUID -> Maybe Text -> m Subscription{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidSubscriptionsSubsnoPausePost :: UUID -> Int -> SubscriptionPauseDates -> Maybe UUID -> Maybe Text -> m Subscription{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidSubscriptionsSubsnoReclamationPost :: UUID -> Int -> NewDeliveryReclamation -> Maybe UUID -> Maybe Text -> m DeliveryReclamation{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidSubscriptionsSubsnoReclamationsReclaimnoGet :: UUID -> Int -> Int -> Maybe UUID -> Maybe Text -> m DeliveryReclamation{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidSubscriptionsSubsnoUnpausePost :: UUID -> Int -> Maybe Day -> Maybe Day -> Maybe UUID -> Maybe Text -> m Subscription{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  }

newtype PersonaClient a = PersonaClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative PersonaClient where
  pure x = PersonaClient (\_ -> pure x)
  (PersonaClient f) <*> (PersonaClient x) =
    PersonaClient (\env -> f env <*> x env)

instance Monad PersonaClient where
  (PersonaClient a) >>= f =
    PersonaClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO PersonaClient where
  liftIO io = PersonaClient (\_ -> liftIO io)

createPersonaClient :: PersonaBackend PersonaClient
createPersonaClient = PersonaBackend{..}
  where
    ((coerce -> accountPasswordForgotGet) :<|>
     (coerce -> accountPasswordForgotPost) :<|>
     (coerce -> accountPasswordResetPost) :<|>
     (coerce -> adminSearchPost) :<|>
     (coerce -> adminTransferPassiveSubscribersListidPost) :<|>
     (coerce -> adminUserPost) :<|>
     (coerce -> entitlementsAllowDelete) :<|>
     (coerce -> entitlementsAllowGet) :<|>
     (coerce -> entitlementsAllowPost) :<|>
     (coerce -> entitlementsAllowUuidPost) :<|>
     (coerce -> entitlementsGet) :<|>
     (coerce -> entitlementsGlobalGet) :<|>
     (coerce -> identificationLoginGet) :<|>
     (coerce -> identificationLoginMonitorGet) :<|>
     (coerce -> identificationLoginReturnGet) :<|>
     (coerce -> identificationUserStampUuidPost) :<|>
     (coerce -> loginIpGet) :<|>
     (coerce -> loginPost) :<|>
     (coerce -> loginSomePost) :<|>
     (coerce -> loginSsoPost) :<|>
     (coerce -> loginUuidDelete) :<|>
     (coerce -> usersPost) :<|>
     (coerce -> usersTemporaryPost) :<|>
     (coerce -> usersUuidEntitlementGet) :<|>
     (coerce -> usersUuidGdprPut) :<|>
     (coerce -> usersUuidGet) :<|>
     (coerce -> usersUuidLegalPut) :<|>
     (coerce -> usersUuidNewslettersGet) :<|>
     (coerce -> usersUuidNewslettersPut) :<|>
     (coerce -> usersUuidPasswordPut) :<|>
     (coerce -> usersUuidPatch) :<|>
     (coerce -> usersUuidPaymentsGet) :<|>
     (coerce -> usersUuidScopeGet) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoAddressChangeDelete) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoAddressChangePatch) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoAddressChangePost) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoCancelPut) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoPausePatch) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoPausePost) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoReclamationPost) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoReclamationsReclaimnoGet) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoUnpausePost)) = client (Proxy :: Proxy PersonaAPI)

-- | Run requests in the PersonaClient monad.
runPersonaClient :: Config -> PersonaClient a -> ExceptT ClientError IO a
runPersonaClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runPersonaClientWithManager manager clientConfig cl

-- | Run requests in the PersonaClient monad using a custom manager.
runPersonaClientWithManager :: Manager -> Config -> PersonaClient a -> ExceptT ClientError IO a
runPersonaClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a PersonaClientError
callPersona
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> PersonaClient a -> m a
callPersona env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (PersonaClientError err)
    Right response -> pure response

-- | Run the Persona server at the provided host and port.
runPersonaServer
  :: (MonadIO m, MonadThrow m)
  => Config -> PersonaBackend (ExceptT ServerError IO) -> m ()
runPersonaServer Config{..} backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy PersonaAPI) (serverFromBackend backend)
  where
    serverFromBackend PersonaBackend{..} =
      (coerce accountPasswordForgotGet :<|>
       coerce accountPasswordForgotPost :<|>
       coerce accountPasswordResetPost :<|>
       coerce adminSearchPost :<|>
       coerce adminTransferPassiveSubscribersListidPost :<|>
       coerce adminUserPost :<|>
       coerce entitlementsAllowDelete :<|>
       coerce entitlementsAllowGet :<|>
       coerce entitlementsAllowPost :<|>
       coerce entitlementsAllowUuidPost :<|>
       coerce entitlementsGet :<|>
       coerce entitlementsGlobalGet :<|>
       coerce identificationLoginGet :<|>
       coerce identificationLoginMonitorGet :<|>
       coerce identificationLoginReturnGet :<|>
       coerce identificationUserStampUuidPost :<|>
       coerce loginIpGet :<|>
       coerce loginPost :<|>
       coerce loginSomePost :<|>
       coerce loginSsoPost :<|>
       coerce loginUuidDelete :<|>
       coerce usersPost :<|>
       coerce usersTemporaryPost :<|>
       coerce usersUuidEntitlementGet :<|>
       coerce usersUuidGdprPut :<|>
       coerce usersUuidGet :<|>
       coerce usersUuidLegalPut :<|>
       coerce usersUuidNewslettersGet :<|>
       coerce usersUuidNewslettersPut :<|>
       coerce usersUuidPasswordPut :<|>
       coerce usersUuidPatch :<|>
       coerce usersUuidPaymentsGet :<|>
       coerce usersUuidScopeGet :<|>
       coerce usersUuidSubscriptionsSubsnoAddressChangeDelete :<|>
       coerce usersUuidSubscriptionsSubsnoAddressChangePatch :<|>
       coerce usersUuidSubscriptionsSubsnoAddressChangePost :<|>
       coerce usersUuidSubscriptionsSubsnoCancelPut :<|>
       coerce usersUuidSubscriptionsSubsnoPausePatch :<|>
       coerce usersUuidSubscriptionsSubsnoPausePost :<|>
       coerce usersUuidSubscriptionsSubsnoReclamationPost :<|>
       coerce usersUuidSubscriptionsSubsnoReclamationsReclaimnoGet :<|>
       coerce usersUuidSubscriptionsSubsnoUnpausePost)
