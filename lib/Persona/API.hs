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
    =    "account" :> "codeForToken" :> ReqBody '[JSON] CodeForTokenData :> Verb 'POST 200 '[JSON] TokenResponse -- 'accountCodeForTokenPost' route
    :<|> "account" :> "forgotPass" :> ReqBody '[JSON] ForgotPasswordData :> Verb 'POST 200 '[JSON] ForgotPasswordResponse -- 'accountForgotPassPost' route
    :<|> "account" :> "resetForgottenPassword" :> ReqBody '[JSON] UpdatePasswordData :> Verb 'POST 200 '[JSON] ForgotPasswordResponse -- 'accountResetForgottenPasswordPost' route
    :<|> "entitlements" :> "allow" :> ReqBody '[JSON] GlobalEntitlementAccess :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] [Value] -- 'entitlementsAllowPost' route
    :<|> "entitlements" :> Verb 'GET 200 '[JSON] ((Map.Map String [Text])) -- 'entitlementsGet' route
    :<|> "login" :> ReqBody '[JSON] LoginData :> Verb 'POST 200 '[JSON] LoginResponse -- 'loginPost' route
    :<|> "login" :> "some" :> ReqBody '[JSON] LoginDataSoMe :> Verb 'POST 200 '[JSON] LoginResponse -- 'loginSomePost' route
    :<|> "login" :> "sso" :> ReqBody '[JSON] LoginDataSSO :> Verb 'POST 200 '[JSON] LoginResponse -- 'loginSsoPost' route
    :<|> "login" :> Capture "uuid" UUID :> QueryParam "everywhere" Bool :> Header "Authorization" Text :> Verb 'DELETE 200 '[JSON] [Value] -- 'loginUuidDelete' route
    :<|> "users" :> ReqBody '[JSON] NewUser :> Verb 'POST 200 '[JSON] LoginResponse -- 'usersPost' route
    :<|> "users" :> Capture "uuid" UUID :> "entitlement" :> Header "Authorization" Text :> Header "Cache-Control" Text :> Verb 'GET 200 '[JSON] [Text] -- 'usersUuidEntitlementGet' route
    :<|> "users" :> Capture "uuid" UUID :> "gdpr" :> ReqBody '[JSON] [GdprConsent] :> Header "Authorization" Text :> Verb 'PUT 200 '[JSON] User -- 'usersUuidGdprPut' route
    :<|> "users" :> Capture "uuid" UUID :> Header "Authorization" Text :> Header "Cache-Control" Text :> Verb 'GET 200 '[JSON] User -- 'usersUuidGet' route
    :<|> "users" :> Capture "uuid" UUID :> "legal" :> ReqBody '[JSON] [LegalConsent] :> Header "Authorization" Text :> Verb 'PUT 200 '[JSON] User -- 'usersUuidLegalPut' route
    :<|> "users" :> Capture "uuid" UUID :> ReqBody '[JSON] UserUpdate :> Header "Authorization" Text :> Verb 'PATCH 200 '[JSON] User -- 'usersUuidPatch' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "addressChange" :> ReqBody '[JSON] TemporaryAddressChange :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] Subscription -- 'usersUuidSubscriptionsSubsnoAddressChangePost' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "pause" :> ReqBody '[JSON] SubscriptionPauseDates :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] Subscription -- 'usersUuidSubscriptionsSubsnoPausePost' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "reclamation" :> ReqBody '[JSON] NewDeliveryReclamation :> Header "Authorization" Text :> Verb 'POST 200 '[JSON] DeliveryReclamation -- 'usersUuidSubscriptionsSubsnoReclamationPost' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "reclamations" :> Capture "reclaimno" Int :> Header "Authorization" Text :> Verb 'GET 200 '[JSON] DeliveryReclamation -- 'usersUuidSubscriptionsSubsnoReclamationsReclaimnoGet' route
    :<|> "users" :> Capture "uuid" UUID :> "subscriptions" :> Capture "subsno" Int :> "reclamations" :> Capture "reclaimno" Int :> ReqBody '[JSON] DeliveryReclamationUpdate :> Header "Authorization" Text :> Verb 'PATCH 200 '[JSON] DeliveryReclamation -- 'usersUuidSubscriptionsSubsnoReclamationsReclaimnoPatch' route


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
  { accountCodeForTokenPost :: CodeForTokenData -> m TokenResponse{- ^  -}
  , accountForgotPassPost :: ForgotPasswordData -> m ForgotPasswordResponse{- ^  -}
  , accountResetForgottenPasswordPost :: UpdatePasswordData -> m ForgotPasswordResponse{- ^  -}
  , entitlementsAllowPost :: GlobalEntitlementAccess -> Maybe Text -> m [Value]{- ^  -}
  , entitlementsGet :: m ((Map.Map String [Text])){- ^  -}
  , loginPost :: LoginData -> m LoginResponse{- ^  -}
  , loginSomePost :: LoginDataSoMe -> m LoginResponse{- ^  -}
  , loginSsoPost :: LoginDataSSO -> m LoginResponse{- ^  -}
  , loginUuidDelete :: UUID -> Maybe Bool -> Maybe Text -> m [Value]{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersPost :: NewUser -> m LoginResponse{- ^  -}
  , usersUuidEntitlementGet :: UUID -> Maybe Text -> Maybe Text -> m [Text]{- ^  -}
  , usersUuidGdprPut :: UUID -> [GdprConsent] -> Maybe Text -> m User{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidGet :: UUID -> Maybe Text -> Maybe Text -> m User{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidLegalPut :: UUID -> [LegalConsent] -> Maybe Text -> m User{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidPatch :: UUID -> UserUpdate -> Maybe Text -> m User{- ^ Authorization header expects the following format ‘OAuth {token}’ -}
  , usersUuidSubscriptionsSubsnoAddressChangePost :: UUID -> Int -> TemporaryAddressChange -> Maybe Text -> m Subscription{- ^  -}
  , usersUuidSubscriptionsSubsnoPausePost :: UUID -> Int -> SubscriptionPauseDates -> Maybe Text -> m Subscription{- ^  -}
  , usersUuidSubscriptionsSubsnoReclamationPost :: UUID -> Int -> NewDeliveryReclamation -> Maybe Text -> m DeliveryReclamation{- ^  -}
  , usersUuidSubscriptionsSubsnoReclamationsReclaimnoGet :: UUID -> Int -> Int -> Maybe Text -> m DeliveryReclamation{- ^  -}
  , usersUuidSubscriptionsSubsnoReclamationsReclaimnoPatch :: UUID -> Int -> Int -> DeliveryReclamationUpdate -> Maybe Text -> m DeliveryReclamation{- ^  -}
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
    ((coerce -> accountCodeForTokenPost) :<|>
     (coerce -> accountForgotPassPost) :<|>
     (coerce -> accountResetForgottenPasswordPost) :<|>
     (coerce -> entitlementsAllowPost) :<|>
     (coerce -> entitlementsGet) :<|>
     (coerce -> loginPost) :<|>
     (coerce -> loginSomePost) :<|>
     (coerce -> loginSsoPost) :<|>
     (coerce -> loginUuidDelete) :<|>
     (coerce -> usersPost) :<|>
     (coerce -> usersUuidEntitlementGet) :<|>
     (coerce -> usersUuidGdprPut) :<|>
     (coerce -> usersUuidGet) :<|>
     (coerce -> usersUuidLegalPut) :<|>
     (coerce -> usersUuidPatch) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoAddressChangePost) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoPausePost) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoReclamationPost) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoReclamationsReclaimnoGet) :<|>
     (coerce -> usersUuidSubscriptionsSubsnoReclamationsReclaimnoPatch)) = client (Proxy :: Proxy PersonaAPI)

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
      (coerce accountCodeForTokenPost :<|>
       coerce accountForgotPassPost :<|>
       coerce accountResetForgottenPasswordPost :<|>
       coerce entitlementsAllowPost :<|>
       coerce entitlementsGet :<|>
       coerce loginPost :<|>
       coerce loginSomePost :<|>
       coerce loginSsoPost :<|>
       coerce loginUuidDelete :<|>
       coerce usersPost :<|>
       coerce usersUuidEntitlementGet :<|>
       coerce usersUuidGdprPut :<|>
       coerce usersUuidGet :<|>
       coerce usersUuidLegalPut :<|>
       coerce usersUuidPatch :<|>
       coerce usersUuidSubscriptionsSubsnoAddressChangePost :<|>
       coerce usersUuidSubscriptionsSubsnoPausePost :<|>
       coerce usersUuidSubscriptionsSubsnoReclamationPost :<|>
       coerce usersUuidSubscriptionsSubsnoReclamationsReclaimnoGet :<|>
       coerce usersUuidSubscriptionsSubsnoReclamationsReclaimnoPatch)
