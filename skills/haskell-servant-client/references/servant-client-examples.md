# Servant Client Examples

Complete, working examples for Servant client API wrappers with two-layer error handling.
Based on patterns from [slack-web](https://hackage.haskell.org/package/slack-web).

## Example 1: Two-Layer Error Type

The core pattern -- separating network errors from domain-specific API errors:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Web.Slack.Common where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Data.Aeson (Object)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Client (ClientError)

-- | API-level errors returned by the service.
-- Contrast with 'SlackClientError' which additionally
-- contains errors from the network communication.
data ResponseSlackError = ResponseSlackError
  { errorText :: Text
  , responseMetadata :: Object
  }
  deriving stock (Eq, Show, Generic)

instance NFData ResponseSlackError

-- | Errors that can be triggered by a slack request.
data SlackClientError
  = -- | errors from the network connection
    ServantError ClientError
  | -- | errors returned by the slack API
    SlackError ResponseSlackError
  deriving stock (Eq, Generic, Show)

instance NFData SlackClientError
instance Exception SlackClientError
```

Key design decisions:
- **`ServantError ClientError`**: Wraps connection failures, HTTP status errors, decode failures -- anything from the transport layer
- **`SlackError ResponseSlackError`**: Wraps API-level errors where HTTP 200 was returned but the response body indicates failure (e.g. `"error": "channel_not_found"`)
- **`NFData`**: Enables `deepseq` / `force` evaluation to prevent thunk leaks, especially important in concurrent or long-running client code
- **`Exception`**: Makes the top-level error throwable in IO, required for effectful `adapt` pattern and `catch` handling
- **`StrictData`**: Prevents thunk buildup in error record fields

## Example 2: ResponseJSON Newtype and unnestErrors

The response envelope pattern -- parsing the API's `{ok, error, response_metadata}` structure:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.Internal where

import Data.Aeson (FromJSON(..), Object, (.:), (.:?), withObject)
import Data.Aeson.KeyMap qualified as KM
import Network.HTTP.Client (Manager)
import Servant.API (AuthProtect)
import Servant.Client
  ( BaseUrl(..), ClientError, ClientM, Scheme(..)
  , mkClientEnv, runClientM
  )
import Servant.Client.Core
  ( AuthClientData, AuthenticatedRequest, Request
  , addHeader, mkAuthenticatedRequest
  )
import Web.Slack.Common qualified as Common

type Response a = Either Common.SlackClientError a

-- | Internal response wrapper that parses the API envelope.
-- Servant decodes the HTTP body into this; the custom FromJSON
-- checks the "ok" field and branches accordingly.
newtype ResponseJSON a = ResponseJSON (Either Common.ResponseSlackError a)
  deriving stock (Show)

instance (FromJSON a) => FromJSON (ResponseJSON a) where
  parseJSON = withObject "Response" $ \o -> do
    ok <- o .: "ok"
    ResponseJSON
      <$> if ok
        then Right <$> parseJSON (Object o)
        else do
          err <- o .: "error"
          meta <- o .:? "response_metadata"
          pure $ Left $ Common.ResponseSlackError
            { Common.errorText = err
            , Common.responseMetadata = fromMaybe KM.empty meta
            }

-- | Collapse the two layers of Either into a single Response.
-- Right (Right a)   -> success
-- Right (Left  err) -> domain error (API returned ok=false)
-- Left  clientErr   -> network error (HTTP/connection failure)
unnestErrors :: Either ClientError (ResponseJSON a) -> Response a
unnestErrors (Right (ResponseJSON (Right a))) = Right a
unnestErrors (Right (ResponseJSON (Left err))) = Left (Common.SlackError err)
unnestErrors (Left slackErr) = Left (Common.ServantError slackErr)
```

## Example 3: Authentication and Config

Setting up `AuthProtect`, `SlackConfig`, and the `run` function:

```haskell
-- (continued from Web.Slack.Internal)

data SlackConfig = SlackConfig
  { slackConfigManager :: Manager
  , slackConfigToken :: Text
  }

type instance AuthClientData (AuthProtect "token") = Text

authenticateReq :: Text -> Request -> Request
authenticateReq token = addHeader "Authorization" $ "Bearer " <> token

mkSlackAuthenticateReq :: SlackConfig -> AuthenticatedRequest (AuthProtect "token")
mkSlackAuthenticateReq =
  (`mkAuthenticatedRequest` authenticateReq) . slackConfigToken

-- | Run a ClientM action, converting the result through unnestErrors.
run :: ClientM (ResponseJSON a) -> Manager -> IO (Response a)
run clientAction mgr = do
  let baseUrl = BaseUrl Https "slack.com" 443 "/api"
  unnestErrors <$> runClientM clientAction (mkClientEnv mgr baseUrl)

-- | Create a SlackConfig from a token (initializes TLS manager).
mkSlackConfig :: Text -> IO SlackConfig
mkSlackConfig token =
  SlackConfig <$> newManager tlsManagerSettings <*> pure token
```

## Example 4: Three-Tier API Organization

Raw Servant clients (tier 1) and IO wrappers (tier 2):

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Slack where

import Data.Proxy (Proxy(..))
import Servant.API
import Servant.Client (client)
import Servant.Client.Core (AuthenticatedRequest)
import Web.FormUrlEncoded (ToForm)

import Web.Slack.Chat qualified as Chat
import Web.Slack.Conversation qualified as Conversation
import Web.Slack.Internal

-- API type definition
type Api =
  "conversations.list"
    :> AuthProtect "token"
    :> ReqBody '[FormUrlEncoded] Conversation.ListReq
    :> Post '[JSON] (ResponseJSON Conversation.ListRsp)
  :<|> "chat.postMessage"
    :> AuthProtect "token"
    :> ReqBody '[FormUrlEncoded] Chat.PostMsgReq
    :> Post '[JSON] (ResponseJSON Chat.PostMsgRsp)

-- Tier 1: Raw Servant client functions (internal, suffixed with _)
conversationsList_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Conversation.ListReq
  -> ClientM (ResponseJSON Conversation.ListRsp)
chatPostMessage_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Chat.PostMsgReq
  -> ClientM (ResponseJSON Chat.PostMsgRsp)

conversationsList_ :<|> chatPostMessage_ = client (Proxy :: Proxy Api)

-- Tier 2: IO wrapper functions (public API)
conversationsList
  :: SlackConfig
  -> Conversation.ListReq
  -> IO (Response Conversation.ListRsp)
conversationsList = flip $ \listReq -> do
  authR <- mkSlackAuthenticateReq
  run (conversationsList_ authR listReq) . slackConfigManager

chatPostMessage
  :: SlackConfig
  -> Chat.PostMsgReq
  -> IO (Response Chat.PostMsgRsp)
chatPostMessage = flip $ \postReq -> do
  authR <- mkSlackAuthenticateReq
  run (chatPostMessage_ authR postReq) . slackConfigManager
```

## Example 5: Request/Response Types with FormUrlEncoded

Using `ToForm` for request encoding and TH for JSON derivation:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Slack.Chat where

import Data.Aeson.TH (deriveJSON, deriveFromJSON)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Web.FormUrlEncoded (ToForm(..), genericToForm)

-- | Request type -- uses FormUrlEncoded for Slack API
data PostMsgReq = PostMsgReq
  { postMsgReqChannel :: Text
  , postMsgReqText :: Maybe Text
  , postMsgReqBlocks :: Maybe Text
  , postMsgReqThreadTs :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)

instance NFData PostMsgReq

$(deriveJSON (jsonOpts "postMsgReq") ''PostMsgReq)

instance ToForm PostMsgReq where
  toForm = genericToForm (formOpts "postMsgReq")

-- | Smart constructor with required fields only
mkPostMsgReq :: Text -> Text -> PostMsgReq
mkPostMsgReq channel text = PostMsgReq
  { postMsgReqChannel = channel
  , postMsgReqText = Just text
  , postMsgReqBlocks = Nothing
  , postMsgReqThreadTs = Nothing
  }

-- | Response type -- JSON decoded from API response
data PostMsgRsp = PostMsgRsp
  { postMsgRspTs :: Text
  , postMsgRspMessage :: PostMsg
  }
  deriving stock (Eq, Generic, Show)

instance NFData PostMsgRsp

$(deriveFromJSON (jsonOpts "postMsgRsp") ''PostMsgRsp)
```

## Example 6: Pagination with LoadPage

Cursor-based pagination abstraction:

```haskell
{-# LANGUAGE TypeFamilies #-}

module Web.Slack.Pager where

import Web.Slack.Common qualified as Common

type Response a = Either Common.SlackClientError a

-- | An action which returns a paginated response.
-- Every call performs a request with a new cursor to get the next page.
-- Returns an empty list when there are no more pages.
type LoadPage m a = m (Response [a])

-- | Typeclass for requests that support cursor-based pagination
class PagedRequest a where
  setCursor :: Maybe Cursor -> a -> a

-- | Typeclass for responses that contain pagination metadata
class PagedResponse a where
  type ResponseObject a
  getResponseMetadata :: a -> Maybe ResponseMetadata
  getResponseData :: a -> [ResponseObject a]

-- | Generic paginated fetcher: takes a single-page request function
-- and returns a LoadPage action that iterates through all pages.
fetchAllBy
  :: (MonadIO m, PagedRequest req, PagedResponse resp)
  => (req -> m (Response resp))
  -> req
  -> m (LoadPage m (ResponseObject resp))
fetchAllBy sendRequest initialRequest = do
  cursorRef <- liftIO $ newIORef Nothing
  let requestFromCursor cursor = setCursor cursor initialRequest
      collectAndUpdateCursor resp = do
        let newCursor = responseMetadataNextCursor =<< getResponseMetadata resp
            cursorToSave = if isNothing newCursor then emptyCursor else newCursor
        writeIORef cursorRef cursorToSave
        return $ getResponseData resp
  return $ do
    cursor <- liftIO $ readIORef cursorRef
    if cursor == emptyCursor
      then return $ Right []
      else traverse (liftIO . collectAndUpdateCursor)
             =<< sendRequest (requestFromCursor cursor)
  where
    emptyCursor = Just $ Common.Cursor ""

-- | Usage: conversationsListAll = fetchAllBy . conversationsList
conversationsListAll
  :: SlackConfig -> Conversation.ListReq -> IO (LoadPage IO Conversation.Conversation)
conversationsListAll = fetchAllBy . conversationsList
```

## Example 7: Effectful Integration with GADT-Constrained Errors

Client as a dynamic effect using the GADT-constrained error pattern and `adapt`:

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators #-}

module Web.Slack.Effect where

import qualified Control.Monad.Catch as C

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Static

import Web.Slack.Common (SlackClientError(..), ResponseSlackError(..))
import Web.Slack.Internal (SlackConfig(..), ResponseJSON(..), unnestErrors, mkSlackAuthenticateReq)
import Web.Slack.Chat qualified as Chat
import Web.Slack.Conversation qualified as Conversation
import Servant.Client (BaseUrl(..), ClientM, Scheme(..), mkClientEnv, runClientM)

----------------------------------------
-- Effect definition with GADT-constrained errors

data SlackClient :: Effect where
  ChatPostMessage
    :: Error SlackClientError :> es
    => Chat.PostMsgReq
    -> SlackClient (Eff es) Chat.PostMsgRsp
  ConversationsList
    :: Error SlackClientError :> es
    => Conversation.ListReq
    -> SlackClient (Eff es) Conversation.ListRsp

type instance DispatchOf SlackClient = Dynamic

-- Smart constructors
chatPostMessage
  :: (SlackClient :> es, Error SlackClientError :> es)
  => Chat.PostMsgReq -> Eff es Chat.PostMsgRsp
chatPostMessage = send . ChatPostMessage

conversationsList
  :: (SlackClient :> es, Error SlackClientError :> es)
  => Conversation.ListReq -> Eff es Conversation.ListRsp
conversationsList = send . ConversationsList

----------------------------------------
-- IO interpreter using adapt pattern

runSlackClientIO
  :: (IOE :> es, Reader SlackConfig :> es)
  => Eff (SlackClient : es) a
  -> Eff es a
runSlackClientIO = interpret $ \env -> \case
  ChatPostMessage req   -> adaptClient env (chatPostMessage_ authR req)
  ConversationsList req -> adaptClient env (conversationsList_ authR req)
  where
    -- adapt: run ClientM through unnestErrors, throw in caller's scope
    adaptClient localEnv action = do
      config <- ask @SlackConfig
      let clientEnv = mkClientEnv
            (slackConfigManager config)
            (BaseUrl Https "slack.com" 443 "/api")
          authR = mkSlackAuthenticateReq config
      result <- liftIO $ unnestErrors <$> runClientM action clientEnv
      case result of
        Left err -> localSeqUnlift localEnv $ \unlift ->
          unlift (throwError err)
        Right a -> pure a

----------------------------------------
-- Run the full effect stack

runSlackStack
  :: SlackConfig
  -> Eff '[SlackClient, Reader SlackConfig, Error SlackClientError, IOE] a
  -> IO (Either (CallStack, SlackClientError) a)
runSlackStack config = runEff
                     . runError @SlackClientError
                     . runReader config
                     . runSlackClientIO
```

Usage in application code:

```haskell
-- Caller propagates error
notifyChannel
  :: (SlackClient :> es, Error SlackClientError :> es)
  => Text -> Text -> Eff es Chat.PostMsgRsp
notifyChannel channel text =
  chatPostMessage (Chat.mkPostMsgReq channel text)

-- Caller catches at boundary
notifyChannelSafe
  :: (SlackClient :> es, IOE :> es)
  => Text -> Text -> Eff es (Maybe Chat.PostMsgRsp)
notifyChannelSafe channel text = do
  result <- runErrorNoCallStack @SlackClientError $
    chatPostMessage (Chat.mkPostMsgReq channel text)
  pure $ either (const Nothing) Just result
```

## Example 8: Testing with Mock Handlers

Pure mock interpreter for testing without network calls:

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators #-}

module Web.Slack.Effect.Mock where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local

import Web.Slack.Effect (SlackClient(..))
import Web.Slack.Common (SlackClientError(..))
import Web.Slack.Chat qualified as Chat
import Web.Slack.Conversation qualified as Conversation

-- Mock state for testing
data MockSlackState = MockSlackState
  { mockPosted     :: [Chat.PostMsgReq]
  , mockChannels   :: Conversation.ListRsp
  , mockShouldFail :: Maybe SlackClientError
  }

-- Mock interpreter
runSlackClientMock
  :: State MockSlackState :> es
  => Eff (SlackClient : es) a
  -> Eff es a
runSlackClientMock = interpret $ \env -> \case
  ChatPostMessage req -> do
    st <- get
    case mockShouldFail st of
      Just err -> localSeqUnlift env $ \unlift ->
        unlift (throwError err)
      Nothing -> do
        modify $ \s -> s { mockPosted = req : mockPosted s }
        pure $ Chat.PostMsgRsp "1234.5678" (Chat.PostMsg { .. })
  ConversationsList _req -> do
    st <- get
    case mockShouldFail st of
      Just err -> localSeqUnlift env $ \unlift ->
        unlift (throwError err)
      Nothing -> pure (mockChannels st)

-- Test runner
runMockSlack
  :: MockSlackState
  -> Eff '[SlackClient, State MockSlackState, Error SlackClientError, IOE] a
  -> IO (Either (CallStack, SlackClientError) (a, MockSlackState))
runMockSlack st = runEff
               . runError @SlackClientError
               . runStateLocal st
               . runSlackClientMock
```

Test example with HSpec:

```haskell
module Web.Slack.Effect.Spec where

import Test.Hspec
import Web.Slack.Effect (chatPostMessage)
import Web.Slack.Effect.Mock
import Web.Slack.Common (SlackClientError(..), ResponseSlackError(..))
import Web.Slack.Chat qualified as Chat

spec :: Spec
spec = describe "SlackClient effect" $ do
  it "posts a message and records it" $ do
    let st = MockSlackState [] defaultListRsp Nothing
    result <- runMockSlack st $
      chatPostMessage (Chat.mkPostMsgReq "C123" "hello")
    case result of
      Left _ -> expectationFailure "unexpected error"
      Right (rsp, finalSt) -> do
        Chat.postMsgRspTs rsp `shouldBe` "1234.5678"
        length (mockPosted finalSt) `shouldBe` 1

  it "propagates configured domain errors" $ do
    let err = SlackError (ResponseSlackError "channel_not_found" mempty)
        st = MockSlackState [] defaultListRsp (Just err)
    result <- runMockSlack st $
      chatPostMessage (Chat.mkPostMsgReq "C999" "fail")
    case result of
      Left (_, e) -> e `shouldBe` err
      Right _     -> expectationFailure "expected error"
```

## Quick Reference: Client Architecture

| Layer | Pattern | Example |
|-------|---------|---------|
| Error types | `ResponseDomainError` + `ServiceClientError` | `ResponseSlackError` + `SlackClientError` |
| Response parsing | `newtype ResponseJSON a = ResponseJSON (Either DomainError a)` | Custom `FromJSON` checks `ok` field |
| Error unnesting | `unnestErrors :: Either ClientError (ResponseJSON a) -> Response a` | Collapses two `Either` layers |
| Raw clients | `foo_ :: AuthReq -> Req -> ClientM (ResponseJSON Rsp)` | Generated by `client (Proxy @Api)` |
| IO wrappers | `foo :: Config -> Req -> IO (Response Rsp)` | Calls `run` with auth |
| Effect interface | `data FooClient :: Effect where ...` | GADT-constrained errors |
| Pagination | `type LoadPage m a = m (Response [a])` | `fetchAllBy` combinator |

## Quick Reference: Error Type Checklist

| Requirement | Why |
|-------------|-----|
| `deriving stock (Eq, Show, Generic)` | Standard instances for comparison, debugging, serialization |
| `instance NFData` on all error and response types | Deep evaluation, prevent thunk leaks |
| `instance Exception` on top-level error | IO interop, `catch`/`throw`, effectful `adapt` pattern |
| Separate `ServantError` constructor | Distinguish retryable network failures from domain errors |
| Separate domain error constructor | Typed access to API-specific error details (error code + metadata) |
| `{-# LANGUAGE StrictData #-}` | Prevent thunk buildup in record fields |
