# Servant NamedRoutes Examples

Complete, working examples for Servant using the record pattern (`NamedRoutes`).

## Example 1: Basic CRUD API

Full API definition with NamedRoutes records:

```haskell
{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}

module Api where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant

-- Domain types
newtype UserId = UserId Int
  deriving (Show, Eq, FromHttpApiData, ToJSON, FromJSON, Generic)

data User = User
  { userId    :: UserId
  , userName  :: Text
  , userEmail :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON User
instance FromJSON User

data CreateUser = CreateUser
  { createUserName  :: Text
  , createUserEmail :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CreateUser

data UpdateUser = UpdateUser
  { updateUserName  :: Maybe Text
  , updateUserEmail :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON UpdateUser

-- API definition using NamedRoutes
data UserRoutes mode = UserRoutes
  { list   :: mode :- "users" :> QueryParam "limit" Int :> Get '[JSON] [User]
  , get    :: mode :- "users" :> Capture "id" UserId :> Get '[JSON] User
  , create :: mode :- "users" :> ReqBody '[JSON] CreateUser :> Post '[JSON] User
  , update :: mode :- "users" :> Capture "id" UserId :> ReqBody '[JSON] UpdateUser :> Put '[JSON] User
  , delete :: mode :- "users" :> Capture "id" UserId :> DeleteNoContent
  } deriving Generic

type API = NamedRoutes UserRoutes
```

## Example 2: Nested Route Composition

Composing multiple resource records into a top-level API:

```haskell
{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}

module Api.Routes where

import GHC.Generics (Generic)
import Servant

-- Per-resource route records
data UserRoutes mode = UserRoutes
  { listUsers  :: mode :- Get '[JSON] [User]
  , getUser    :: mode :- Capture "userId" UserId :> Get '[JSON] User
  , createUser :: mode :- ReqBody '[JSON] CreateUser :> Post '[JSON] User
  } deriving Generic

data ProductRoutes mode = ProductRoutes
  { listProducts  :: mode :- Get '[JSON] [Product]
  , getProduct    :: mode :- Capture "productId" ProductId :> Get '[JSON] Product
  , searchProducts :: mode :- "search" :> QueryParam' '[Required] "q" Text :> Get '[JSON] [Product]
  } deriving Generic

data OrderRoutes mode = OrderRoutes
  { listOrders  :: mode :- Get '[JSON] [Order]
  , getOrder    :: mode :- Capture "orderId" OrderId :> Get '[JSON] Order
  , createOrder :: mode :- ReqBody '[JSON] CreateOrder :> Post '[JSON] Order
  , cancelOrder :: mode :- Capture "orderId" OrderId :> "cancel" :> PostNoContent
  } deriving Generic

-- Top-level API composing all resources
data API mode = API
  { users    :: mode :- "api" :> "v1" :> "users" :> NamedRoutes UserRoutes
  , products :: mode :- "api" :> "v1" :> "products" :> NamedRoutes ProductRoutes
  , orders   :: mode :- "api" :> "v1" :> "orders" :> NamedRoutes OrderRoutes
  , health   :: mode :- "health" :> Get '[JSON] HealthStatus
  } deriving Generic

type AppAPI = NamedRoutes API
```

## Example 3: Handlers with Service Layer

Handler records that delegate to a service layer:

```haskell
{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}

module Api.Handlers where

import Servant

import Api.Routes (UserRoutes(..), API(..))
import qualified Services.User as UserService

-- Handler record mirrors the API record
userHandlers :: Pool Connection -> UserRoutes AsServer
userHandlers pool = UserRoutes
  { listUsers  = listUsersHandler pool
  , getUser    = getUserHandler pool
  , createUser = createUserHandler pool
  }

listUsersHandler :: Pool Connection -> Handler [User]
listUsersHandler pool = do
  result <- liftIO $ UserService.listAll pool
  case result of
    Left err    -> throwError $ toServerError err
    Right users -> pure users

getUserHandler :: Pool Connection -> UserId -> Handler User
getUserHandler pool uid = do
  result <- liftIO $ UserService.findById pool uid
  case result of
    Left err          -> throwError $ toServerError err
    Right Nothing     -> throwError err404 { errBody = "User not found" }
    Right (Just user) -> pure user

createUserHandler :: Pool Connection -> CreateUser -> Handler User
createUserHandler pool body = do
  result <- liftIO $ UserService.create pool body
  case result of
    Left err   -> throwError $ toServerError err
    Right user -> pure user

-- Top-level handler wiring
apiHandlers :: Pool Connection -> API AsServer
apiHandlers pool = API
  { users    = userHandlers pool
  , products = productHandlers pool
  , orders   = orderHandlers pool
  , health   = pure HealthOk
  }

-- Application entry point
app :: Pool Connection -> Application
app pool = serve (Proxy @(NamedRoutes API)) (apiHandlers pool)
```

## Example 4: Structured Error Handling

Domain errors converted to ServerError at the boundary:

```haskell
{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings #-}

module Api.Errors where

import Data.Aeson (ToJSON(..), encode, object, (.=))
import GHC.Generics (Generic)
import Servant (ServerError(..), err400, err404, err409, err500)

-- Domain error types
data AppError
  = NotFound Text
  | ValidationError [Text]
  | Conflict Text
  | InternalError Text
  deriving (Show, Eq)

-- JSON error response
data ErrorResponse = ErrorResponse
  { errorCode    :: Text
  , errorMessage :: Text
  , errorDetails :: Maybe [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON ErrorResponse

-- Convert domain errors to HTTP errors
toServerError :: AppError -> ServerError
toServerError (NotFound msg) = err404
  { errBody = encode $ ErrorResponse "NOT_FOUND" msg Nothing
  , errHeaders = [("Content-Type", "application/json")]
  }
toServerError (ValidationError details) = err400
  { errBody = encode $ ErrorResponse "VALIDATION_ERROR" "Invalid input" (Just details)
  , errHeaders = [("Content-Type", "application/json")]
  }
toServerError (Conflict msg) = err409
  { errBody = encode $ ErrorResponse "CONFLICT" msg Nothing
  , errHeaders = [("Content-Type", "application/json")]
  }
toServerError (InternalError _) = err500
  { errBody = encode $ ErrorResponse "INTERNAL_ERROR" "Internal server error" Nothing
  , errHeaders = [("Content-Type", "application/json")]
  }
```

## Example 5: Authentication with servant-auth

Protected routes using JWT authentication:

```haskell
{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}

module Api.Auth where

import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server

-- Authenticated user claim
data AuthUser = AuthUser
  { authUserId   :: UserId
  , authUserRole :: Role
  } deriving (Show, Eq, Generic)

instance ToJSON AuthUser
instance FromJSON AuthUser
instance ToJWT AuthUser
instance FromJWT AuthUser

data Role = Admin | Member
  deriving (Show, Eq, Generic)

instance ToJSON Role
instance FromJSON Role

-- Public routes (no auth)
data PublicRoutes mode = PublicRoutes
  { login    :: mode :- "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
  , register :: mode :- "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] User
  } deriving Generic

-- Protected routes (require JWT)
data ProtectedRoutes mode = ProtectedRoutes
  { getProfile    :: mode :- "profile" :> Get '[JSON] User
  , updateProfile :: mode :- "profile" :> ReqBody '[JSON] UpdateUser :> Put '[JSON] User
  , adminUsers    :: mode :- "admin" :> "users" :> Get '[JSON] [User]
  } deriving Generic

-- Top-level API combining public and protected
data API mode = API
  { public    :: mode :- "api" :> NamedRoutes PublicRoutes
  , protected :: mode :- "api" :> Auth '[JWT] AuthUser :> NamedRoutes ProtectedRoutes
  } deriving Generic

-- Protected handler: pattern match on auth result
protectedHandlers :: AuthResult AuthUser -> ProtectedRoutes AsServer
protectedHandlers (Authenticated authUser) = ProtectedRoutes
  { getProfile    = getProfileHandler authUser
  , updateProfile = updateProfileHandler authUser
  , adminUsers    = requireAdmin authUser >> listAllUsersHandler
  }
protectedHandlers _ = ProtectedRoutes
  { getProfile    = throwAll err401
  , updateProfile = \_ -> throwAll err401
  , adminUsers    = throwAll err401
  }

requireAdmin :: AuthUser -> Handler ()
requireAdmin au = case authUserRole au of
  Admin  -> pure ()
  Member -> throwError err403 { errBody = "Admin access required" }
```

## Example 6: Client Generation

Deriving type-safe HTTP clients from the same API definition:

```haskell
{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}

module Api.Client where

import Servant
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)

import Api.Routes (UserRoutes(..), API(..))

-- Generate client functions from the API type
apiClient :: API (AsClientT ClientM)
apiClient = client (Proxy @(NamedRoutes API))

-- Use record fields directly as client functions
listUsersClient :: ClientM [User]
listUsersClient = listUsers (users apiClient)

getUserClient :: UserId -> ClientM User
getUserClient = getUser (users apiClient)

createUserClient :: CreateUser -> ClientM User
createUserClient = createUser (users apiClient)

-- Running client calls
runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient = flip runClientM

example :: IO ()
example = do
  manager <- newManager defaultManagerSettings
  let baseUrl' = BaseUrl Http "localhost" 8080 ""
      env = mkClientEnv manager baseUrl'
  result <- runClient env listUsersClient
  case result of
    Left err    -> putStrLn $ "Error: " <> show err
    Right users -> mapM_ print users
```

## Example 7: Testing with Hspec and Warp

Integration tests using `hspec-wai`:

```haskell
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Api.Spec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (json)
import Network.Wai (Application)
import Data.Aeson (encode)
import Servant

import Api.Routes (API)
import Api.Handlers (apiHandlers)

-- Test application with in-memory state
testApp :: IO Application
testApp = do
  pool <- createTestPool
  pure $ serve (Proxy @(NamedRoutes API)) (apiHandlers pool)

spec :: Spec
spec = with testApp $ do
  describe "GET /api/v1/users" $ do
    it "returns empty list initially" $
      get "/api/v1/users" `shouldRespondWith`
        [json|[]|] { matchStatus = 200 }

  describe "POST /api/v1/users" $ do
    it "creates a new user" $ do
      let body = encode $ CreateUser "Alice" "alice@example.com"
      post "/api/v1/users" body `shouldRespondWith` 201

    it "rejects invalid input" $ do
      let body = encode $ object ["name" .= ("" :: Text)]
      post "/api/v1/users" body `shouldRespondWith` 400

  describe "GET /api/v1/users/:id" $ do
    it "returns 404 for missing user" $
      get "/api/v1/users/999" `shouldRespondWith` 404
```

## Example 8: Middleware and Server Setup

Full server setup with logging and CORS:

```haskell
{-# LANGUAGE DataKinds, TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant

import Api.Routes (API)
import Api.Handlers (apiHandlers)

main :: IO ()
main = do
  pool <- initConnectionPool
  let app = simpleCors
          . logStdoutDev
          $ serve (Proxy @(NamedRoutes API)) (apiHandlers pool)
  putStrLn "Server running on port 8080"
  run 8080 app
```

## Example 9: Effectful Integration

Using Servant with the effectful library via `hoistServer`:

```haskell
{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}

module Api.Effectful where

import Effectful
import Effectful.Reader.Static
import Effectful.Error.Static
import GHC.Generics (Generic)
import Servant

data AppEnv = AppEnv
  { appPool   :: Pool Connection
  , appConfig :: Config
  }

type AppEffects = '[Reader AppEnv, Error AppError, IOE]

data UserRoutes mode = UserRoutes
  { listUsers  :: mode :- "users" :> Get '[JSON] [User]
  , getUser    :: mode :- "users" :> Capture "id" UserId :> Get '[JSON] User
  , createUser :: mode :- "users" :> ReqBody '[JSON] CreateUser :> Post '[JSON] User
  } deriving Generic

type API = NamedRoutes UserRoutes

-- Handlers in Eff monad
userHandlersEff :: UserRoutes (AsServerT (Eff AppEffects))
userHandlersEff = UserRoutes
  { listUsers  = listUsersEff
  , getUser    = getUserEff
  , createUser = createUserEff
  }

listUsersEff :: (Reader AppEnv :> es, Error AppError :> es, IOE :> es) => Eff es [User]
listUsersEff = do
  env <- ask
  liftIO $ UserService.listAll (appPool env)

-- Natural transformation: Eff AppEffects ~> Handler
effToHandler :: AppEnv -> Eff AppEffects a -> Handler a
effToHandler env action = do
  result <- liftIO $ runEff . runError @AppError . runReader env $ action
  case result of
    Left (_cs, err) -> throwError $ toServerError err
    Right a         -> pure a

-- Hoist server from Eff to Handler
app :: AppEnv -> Application
app env = serve (Proxy @API)
        $ hoistServer (Proxy @API) (effToHandler env) userHandlersEff
```

## Example 10: OpenAPI/Swagger Documentation

Generating API documentation from NamedRoutes:

```haskell
{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}

module Api.Docs where

import Data.OpenApi (OpenApi, ToSchema)
import GHC.Generics (Generic)
import Servant
import Servant.OpenApi (toOpenApi)

-- Ensure domain types derive ToSchema
data User = User
  { userId    :: UserId
  , userName  :: Text
  , userEmail :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON User
instance FromJSON User
instance ToSchema User

data CreateUser = CreateUser
  { createUserName  :: Text
  , createUserEmail :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CreateUser
instance ToSchema CreateUser

-- Generate OpenAPI spec from the API type
apiDocs :: OpenApi
apiDocs = toOpenApi (Proxy @(NamedRoutes API))

-- Serve docs alongside the API
data DocsAPI mode = DocsAPI
  { api     :: mode :- NamedRoutes API
  , swagger :: mode :- "swagger.json" :> Get '[JSON] OpenApi
  } deriving Generic

docsHandlers :: Pool Connection -> DocsAPI AsServer
docsHandlers pool = DocsAPI
  { api     = apiHandlers pool
  , swagger = pure apiDocs
  }
```

## Quick Reference: Common Servant Types

| Type | Purpose |
|------|---------|
| `Get '[JSON] a` | GET returning JSON |
| `Post '[JSON] a` | POST returning JSON |
| `Put '[JSON] a` | PUT returning JSON |
| `DeleteNoContent` | DELETE with no body |
| `Capture "name" Type` | Path parameter |
| `QueryParam "name" Type` | Optional query param |
| `QueryParam' '[Required] "name" Type` | Required query param |
| `ReqBody '[JSON] Type` | JSON request body |
| `Header "name" Type` | Request header |
| `NamedRoutes RecordType` | Record-based sub-routes |

## Quick Reference: NamedRoutes Essentials

| Pattern | Usage |
|---------|-------|
| `data Routes mode = Routes { ... } deriving Generic` | Define API as record |
| `mode :- "path" :> ...` | Define a route field |
| `NamedRoutes Routes` | Use record in API composition |
| `Routes AsServer` | Handler record type |
| `Routes (AsClientT ClientM)` | Client record type |
| `AsServerT (Eff es)` | Custom monad handler type |
| `hoistServer proxy nt handlers` | Transform handler monad |
