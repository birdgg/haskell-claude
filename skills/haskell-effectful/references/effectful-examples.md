# Effectful Code Examples

Complete, working examples for common effectful patterns.

## Example 1: Application Scaffold

Full application with Reader, State, Error, and IOE:

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators #-}

module App where

import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.Error.Static

data Config = Config
  { configHost :: String
  , configPort :: Int
  , configDb   :: String
  }

data AppState = AppState
  { stateRequestCount :: Int
  , stateCache        :: Map Text Value
  }

data AppError
  = NotFound Text
  | ValidationError Text
  | DatabaseError Text
  deriving (Show, Eq)

type AppEffects = '[Reader Config, State AppState, Error AppError, IOE]

type App a = Eff AppEffects a

runApp :: Config -> AppState -> App a -> IO (Either (CallStack, AppError) (a, AppState))
runApp cfg st = runEff
              . runError
              . runStateLocal st
              . runReader cfg
```

## Example 2: Custom Effect with Dependency Injection

Database effect with production and test implementations:

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}

module Effects.Database where

import Effectful
import Effectful.Dispatch.Dynamic

-- Effect definition
data Database :: Effect where
  Query   :: Text -> [Param] -> Database m [Row]
  Execute :: Text -> [Param] -> Database m Int

type instance DispatchOf Database = Dynamic

-- Smart constructors
query :: Database :> es => Text -> [Param] -> Eff es [Row]
query sql params = send (Query sql params)

execute :: Database :> es => Text -> [Param] -> Eff es Int
execute sql params = send (Execute sql params)

-- Production handler
runDatabasePostgres :: IOE :> es => Connection -> Eff (Database : es) a -> Eff es a
runDatabasePostgres conn = interpret_ $ \case
  Query sql params   -> liftIO $ pgQuery conn sql params
  Execute sql params -> liftIO $ pgExecute conn sql params

-- Mock handler for testing
data MockDB = MockDB
  { mockQueryResults :: Map Text [Row]
  , mockExecutions   :: [Text]
  }

runDatabaseMock :: State MockDB :> es => Eff (Database : es) a -> Eff es a
runDatabaseMock = interpret_ $ \case
  Query sql _params -> do
    db <- get
    pure $ fromMaybe [] (Map.lookup sql (mockQueryResults db))
  Execute sql _params -> do
    modify $ \db -> db { mockExecutions = sql : mockExecutions db }
    pure 1
```

## Example 3: Testing with Mocks

Using the mock handler in HSpec tests:

```haskell
module Effects.DatabaseSpec where

import Test.Hspec
import Effectful
import Effectful.State.Static.Local
import Effectful.Error.Static

import Effects.Database

spec :: Spec
spec = describe "UserService" $ do
  it "finds a user by id" $ do
    let mockDB = MockDB
          { mockQueryResults = Map.fromList
              [("SELECT * FROM users WHERE id = ?", [userRow])]
          , mockExecutions = []
          }
    let result = runPureEff
               . runStateLocal mockDB
               . runDatabaseMock
               $ query "SELECT * FROM users WHERE id = ?" [IntParam 1]
    fst result `shouldBe` [userRow]

  it "records executed statements" $ do
    let mockDB = MockDB { mockQueryResults = Map.empty, mockExecutions = [] }
    let (_, finalDB) = runPureEff
                     . runStateLocal mockDB
                     . runDatabaseMock
                     $ execute "INSERT INTO users (name) VALUES (?)" [TextParam "alice"]
    mockExecutions finalDB `shouldBe` ["INSERT INTO users (name) VALUES (?)"]
```

## Example 4: Constrained Effect Signatures with `adapt` Pattern

Error handling where the caller decides, with IO and pure handlers:

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators,
             DerivingStrategies, DeriveAnyClass #-}

module Effects.FileSystem where

import Control.Exception
import qualified Control.Monad.Catch as C
import qualified Data.Map.Strict as M
import qualified System.IO as IO

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local

----------------------------------------
-- Errors

newtype FsReadError = FsReadError String
  deriving stock Show
  deriving anyclass Exception
newtype FsWriteError = FsWriteError String
  deriving stock Show
  deriving anyclass Exception

----------------------------------------
-- Effect

data FileSystem :: Effect where
  ReadFile  :: Error FsReadError :> es => FilePath -> FileSystem (Eff es) String
  WriteFile :: Error FsWriteError :> es => FilePath -> String -> FileSystem (Eff es) ()
type instance DispatchOf FileSystem = Dynamic

readFile
  :: (Error FsReadError :> es, FileSystem :> es)
  => FilePath
  -> Eff es String
readFile path = send (ReadFile path)

writeFile
  :: (Error FsWriteError :> es, FileSystem :> es)
  => FilePath
  -> String
  -> Eff es ()
writeFile path content = send (WriteFile path content)

----------------------------------------
-- IO Handler (adapt pattern)

runFileSystemIO
  :: IOE :> es
  => Eff (FileSystem : es) a
  -> Eff es a
runFileSystemIO = interpret $ \env -> \case
  ReadFile path           -> adapt env FsReadError  $ IO.readFile path
  WriteFile path contents -> adapt env FsWriteError $ IO.writeFile path contents
  where
    -- adapt: liftIO + catch IOException + localSeqUnlift to throw in caller's scope
    adapt env errCon m = liftIO m `C.catch` \(e :: IOException) ->
      localSeqUnlift env $ \unlift -> unlift . throwError . errCon $ show e

----------------------------------------
-- Pure Handler (reinterpret + evalState)

runFileSystemPure
  :: M.Map FilePath String
  -> Eff (FileSystem : es) a
  -> Eff es a
runFileSystemPure fs0 = reinterpret (evalState fs0) $ \env -> \case
  ReadFile path -> gets (M.lookup path) >>= \case
    Just contents -> pure contents
    Nothing       -> localSeqUnlift env $ \unlift ->
      unlift . throwError . FsReadError $ "File not found: " ++ show path
  WriteFile path contents -> modify $ M.insert path contents
```

Key patterns:
- **`deriving anyclass Exception`**: Error newtypes derive Exception for IO interop
- **`adapt` helper**: Combines `liftIO`, `C.catch`, and `localSeqUnlift` to convert IO exceptions to effect errors in the caller's scope
- **`reinterpret` + `evalState`**: Pure handler introduces private `State` effect for testable in-memory filesystem
- **Constrained GADT constructors**: `Error FsReadError :> es` on individual constructors lets caller decide error scope

Usage:

```haskell
-- Caller propagates error
readConfig :: (FileSystem :> es, Error FsReadError :> es) => Eff es String
readConfig = readFile "/etc/config"

-- Caller catches at boundary
readConfigSafe :: (FileSystem :> es, IOE :> es) => Eff es (Maybe String)
readConfigSafe = do
  result <- runErrorNoCallStack @FsReadError $ readFile "/etc/config"
  pure $ either (const Nothing) Just result

-- Pure test
testRead :: Either (CallStack, FsReadError) String
testRead = runPureEff
         . runError @FsReadError
         . runFileSystemPure (M.singleton "/etc/config" "hello")
         $ readFile "/etc/config"
```

## Example 5: liftEither Helper

Converting Either values to Error effects with Error.Static:

```haskell
module Effects.Utils where

import Effectful
import Effectful.Error.Static (Error, throwError)
import Data.Bifunctor (first)

-- Error.Static does NOT provide MonadError instance,
-- so Control.Monad.Except.liftEither won't work.
-- Define a local helper:
liftEither :: Error e :> es => Either e a -> Eff es a
liftEither = either throwError pure

-- Usage examples:

-- Parse JSON, converting decode errors
parseJson :: Error AppError :> es => ByteString -> Eff es Value
parseJson body = liftEither $ first (ParseError . T.pack) $ eitherDecode body

-- Validate input
validateAge :: Error ValidationError :> es => Int -> Eff es PositiveInt
validateAge n = liftEither $ case mkPositiveInt n of
  Nothing -> Left (ValidationError "Age must be positive")
  Just p  -> Right p

-- Chain multiple Either conversions
processRequest :: (Error AppError :> es, IOE :> es) => Request -> Eff es Response
processRequest req = do
  body   <- liftEither $ first ParseError $ decodeBody req
  params <- liftEither $ first ValidationError $ validateParams body
  result <- liftEither $ first BusinessError $ processParams params
  pure (toResponse result)
```

## Example 6: Higher-Order Effect with Resource Management

Transaction effect that wraps computation in begin/commit/rollback:

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}

module Effects.Transaction where

import Effectful
import Effectful.Dispatch.Dynamic

data Transaction :: Effect where
  WithTransaction :: m a -> Transaction m a

type instance DispatchOf Transaction = Dynamic

withTransaction :: Transaction :> es => Eff es a -> Eff es a
withTransaction action = send (WithTransaction action)

-- Production handler with rollback on exception
runTransactionIO :: IOE :> es => Connection -> Eff (Transaction : es) a -> Eff es a
runTransactionIO conn = interpret $ \env -> \case
  WithTransaction action -> do
    liftIO $ beginTx conn
    result <- localSeqUnlift env $ \unlift -> do
      r <- unlift action `catch` \(e :: SomeException) -> do
        liftIO $ rollbackTx conn
        throwIO e
      pure r
    liftIO $ commitTx conn
    pure result

-- Pure handler for testing (no-op transaction)
runTransactionPure :: Eff (Transaction : es) a -> Eff es a
runTransactionPure = interpret $ \env -> \case
  WithTransaction action ->
    localSeqUnlift env $ \unlift -> unlift action
```

## Example 7: Logging Effect

Structured logging with multiple implementations:

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}

module Effects.Log where

import Effectful
import Effectful.Dispatch.Dynamic

data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord)

data Log :: Effect where
  LogMsg :: LogLevel -> Text -> Log m ()

type instance DispatchOf Log = Dynamic

logDebug, logInfo, logWarn, logError :: Log :> es => Text -> Eff es ()
logDebug = send . LogMsg Debug
logInfo  = send . LogMsg Info
logWarn  = send . LogMsg Warn
logError = send . LogMsg Error

-- Console logging
runLogStdout :: IOE :> es => Eff (Log : es) a -> Eff es a
runLogStdout = interpret_ $ \case
  LogMsg level msg -> liftIO $ T.putStrLn $ "[" <> T.pack (show level) <> "] " <> msg

-- Filtered logging
runLogFiltered :: IOE :> es => LogLevel -> Eff (Log : es) a -> Eff es a
runLogFiltered minLevel = interpret_ $ \case
  LogMsg level msg
    | level >= minLevel -> liftIO $ T.putStrLn $ "[" <> T.pack (show level) <> "] " <> msg
    | otherwise         -> pure ()

-- Collect logs for testing
runLogCollect :: State [Text] :> es => Eff (Log : es) a -> Eff es a
runLogCollect = interpret_ $ \case
  LogMsg level msg -> modify (("[" <> T.pack (show level) <> "] " <> msg) :)
```

## Example 8: Combining Multiple Custom Effects

Service layer using Database, Log, and Error:

```haskell
module Services.User where

import Effectful
import Effectful.Error.Static
import Effects.Database
import Effects.Log

data UserError = UserNotFound UserId | DuplicateEmail Email
  deriving (Show, Eq)

createUser
  :: (Database :> es, Log :> es, Error UserError :> es)
  => Text -> Text -> Eff es User
createUser name email = do
  logInfo $ "Creating user: " <> name
  existing <- query "SELECT id FROM users WHERE email = ?" [TextParam email]
  case existing of
    (_:_) -> do
      logWarn $ "Duplicate email: " <> email
      throwError (DuplicateEmail (Email email))
    [] -> do
      _ <- execute "INSERT INTO users (name, email) VALUES (?, ?)"
                   [TextParam name, TextParam email]
      logInfo $ "User created: " <> name
      rows <- query "SELECT * FROM users WHERE email = ?" [TextParam email]
      case rows of
        [row] -> pure (rowToUser row)
        _     -> throwError (UserNotFound (UserId 0))

-- Wire up in main
main :: IO ()
main = do
  conn <- connectDb
  result <- runEff
          . runLogStdout
          . runError @UserError
          . runDatabasePostgres conn
          $ createUser "Alice" "alice@example.com"
  case result of
    Left (_cs, err) -> putStrLn $ "Error: " <> show err
    Right user      -> putStrLn $ "Created: " <> show user
```

## Example 9: MTL Migration

### Before (MTL)

```haskell
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

type App a = ReaderT Config (StateT AppState (ExceptT AppError IO)) a

runApp :: Config -> AppState -> App a -> IO (Either AppError (a, AppState))
runApp cfg st = runExceptT . flip runStateT st . flip runReaderT cfg

getServerUrl :: App String
getServerUrl = do
  cfg <- ask
  pure $ configHost cfg <> ":" <> show (configPort cfg)
```

### After (Effectful)

```haskell
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.Error.Static

type App a = Eff '[Reader Config, State AppState, Error AppError, IOE] a

runApp :: Config -> AppState -> App a -> IO (Either (CallStack, AppError) (a, AppState))
runApp cfg st = runEff
              . runError
              . runStateLocal st
              . runReader cfg

getServerUrl :: Reader Config :> es => Eff es String
getServerUrl = do
  cfg <- ask
  pure $ configHost cfg <> ":" <> show (configPort cfg)
```

Key migration differences:
- `ReaderT Config ... IO` becomes `Reader Config :> es => Eff es`
- Handlers compose with `.` instead of nesting transformers
- Error includes `CallStack` automatically
- Constraints are explicit per-function, not global via type alias

## Example 10: Concurrency with Effectful Modules

Always use `Effectful.Concurrent.*` instead of IO concurrent libraries.
These require `Concurrent :> es` and keep concurrency in the effect system.

### Async

```haskell
module Effects.ConcurrentExample where

import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Concurrent.Async (mapConcurrently, race, withAsync)

-- Concurrent fetch: use Effectful.Concurrent.Async, not Control.Concurrent.Async
fetchAll :: (Concurrent :> es, IOE :> es) => [Url] -> Eff es [Response]
fetchAll urls = mapConcurrently fetch urls

-- Race with timeout
raceTimeout :: Concurrent :> es => Int -> Eff es a -> Eff es (Maybe a)
raceTimeout microseconds action = do
  result <- race (threadDelay microseconds) action
  case result of
    Left _  -> pure Nothing
    Right a -> pure (Just a)
```

### MVar

```haskell
import Effectful.Concurrent.MVar.Strict (MVar, newMVar, modifyMVar_, readMVar)

newCounter :: Concurrent :> es => Eff es (MVar Int)
newCounter = newMVar 0

increment :: Concurrent :> es => MVar Int -> Eff es ()
increment counter = modifyMVar_ counter (pure . (+ 1))
```

### STM

```haskell
import Effectful.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')

newSharedState :: Concurrent :> es => a -> Eff es (TVar a)
newSharedState = newTVarIO

updateState :: Concurrent :> es => TVar Int -> Eff es ()
updateState var = atomically $ modifyTVar' var (+ 1)
```

### Chan

```haskell
import Effectful.Concurrent.Chan (Chan, newChan, writeChan, readChan)

producer :: Concurrent :> es => Chan Text -> [Text] -> Eff es ()
producer ch = mapM_ (writeChan ch)

consumer :: Concurrent :> es => Chan Text -> Eff es Text
consumer = readChan
```

### Running Concurrent Effects

```haskell
import Effectful
import Effectful.Concurrent (runConcurrent)

main :: IO ()
main = runEff . runConcurrent $ do
  results <- fetchAll urls
  counter <- newCounter
  mapM_ (\_ -> increment counter) results
  count <- readMVar counter
  liftIO $ putStrLn $ "Processed: " <> show count
```

### Import Mapping Reference

| IO Module | Effectful Module |
|-----------|-----------------|
| `Control.Concurrent` | `Effectful.Concurrent` |
| `Control.Concurrent.Async` | `Effectful.Concurrent.Async` |
| `Control.Concurrent.Chan` | `Effectful.Concurrent.Chan` |
| `Control.Concurrent.Chan` (strict) | `Effectful.Concurrent.Chan.Strict` |
| `Control.Concurrent.MVar` | `Effectful.Concurrent.MVar` |
| `Control.Concurrent.MVar` (strict) | `Effectful.Concurrent.MVar.Strict` |
| `Control.Concurrent.MVar` (strict compat) | `Effectful.Concurrent.MVar.Strict.Compat` |
| `Control.Concurrent.QSem` | `Effectful.Concurrent.QSem` |
| `Control.Concurrent.QSemN` | `Effectful.Concurrent.QSemN` |
| `Control.Concurrent.STM` | `Effectful.Concurrent.STM` |

## Example 11: Using Effectful.TH for Boilerplate Reduction

Template Haskell can auto-generate smart constructors and DispatchOf instances.

### Manual Definition (Without TH)

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}

module Effects.KV.Manual where

import Effectful
import Effectful.Dispatch.Dynamic

data KV :: Effect where
  Get :: Text -> KV m (Maybe Text)
  Put :: Text -> Text -> KV m ()
  Delete :: Text -> KV m ()

type instance DispatchOf KV = Dynamic

-- Manual smart constructors
get :: KV :> es => Text -> Eff es (Maybe Text)
get k = send (Get k)

put :: KV :> es => Text -> Text -> Eff es ()
put k v = send (Put k v)

delete :: KV :> es => Text -> Eff es ()
delete k = send (Delete k)
```

### Using Effectful.TH (Recommended)

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TemplateHaskell #-}

module Effects.KV.TH where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH

data KV :: Effect where
  Get :: Text -> KV m (Maybe Text)
  Put :: Text -> Text -> KV m ()
  Delete :: Text -> KV m ()

-- Auto-generates:
-- 1. type instance DispatchOf KV = Dynamic
-- 2. Smart constructors: get, put, delete
makeEffect ''KV
```

The `makeEffect` splice generates the same code as the manual version, following these conventions:
- Constructor name `Get` → smart constructor `get` (lowercase first letter)
- Adds `KV :> es` constraint automatically
- Infers return type from GADT constructor

### Static Dispatch with TH

For static dispatch, use `makeEffect_`:

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TemplateHaskell #-}

module Effects.Counter.Static where

import Effectful
import Effectful.Dispatch.Static
import Effectful.TH

data Counter :: Effect

type instance DispatchOf Counter = Static WithSideEffects

newtype instance StaticRep Counter = Counter Int

-- Auto-generates DispatchOf instance for Static
makeEffect_ ''Counter

increment :: Counter :> es => Eff es ()
increment = stateStaticRep @Counter $ \(Counter n) -> ((), Counter (n + 1))

current :: Counter :> es => Eff es Int
current = stateStaticRep @Counter $ \s@(Counter n) -> (n, s)
```

### Higher-Order Effects with TH

`makeEffect` also handles higher-order effects (with `m` parameter):

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TemplateHaskell #-}

module Effects.Resource where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH

data Resource :: Effect where
  WithResource :: Text -> m a -> Resource m a
  Cleanup :: m () -> Resource m ()

makeEffect ''Resource

-- Generated smart constructors:
-- withResource :: Resource :> es => Text -> Eff es a -> Eff es a
-- cleanup :: Resource :> es => Eff es () -> Eff es ()
```

### When NOT to Use TH

Avoid `makeEffect` when:
1. **Custom naming**: Smart constructor name differs from GADT constructor
2. **Constrained GADTs**: Constructors have effect constraints (e.g., `Error e :> es`)
3. **Selective exports**: Only exporting some operations, not all

Example where manual is better:

```haskell
-- GADT with Error constraints - makeEffect won't work correctly
data FileSystem :: Effect where
  ReadFile :: Error FsReadError :> es => FilePath -> FileSystem (Eff es) String
  WriteFile :: Error FsWriteError :> es => FilePath -> String -> FileSystem (Eff es) ()

type instance DispatchOf FileSystem = Dynamic

-- Manual smart constructors with proper constraints
readFile :: (Error FsReadError :> es, FileSystem :> es) => FilePath -> Eff es String
readFile path = send (ReadFile path)

writeFile :: (Error FsWriteError :> es, FileSystem :> es) => FilePath -> String -> Eff es ()
writeFile path content = send (WriteFile path content)
```

### TH Best Practices

1. **Use `makeEffect` by default** for simple dynamic effects
2. **Use `makeEffect_` for static dispatch** when you want compile-time optimization
3. **Write manual definitions** when GADTs have effect constraints or custom naming
4. **Enable TemplateHaskell extension** in both definition and usage modules

```haskell
-- In module defining the effect
{-# LANGUAGE TemplateHaskell #-}
makeEffect ''MyEffect

-- In module using the effect (no TH needed)
import Effects.MyEffect (myOperation)
```

## Quick Reference: Interpretation Functions

| Function | For | Effect Order |
|----------|-----|-------------|
| `interpret_` | First-order effects | Handles top effect |
| `interpret` | Higher-order effects | Handles top effect, provides LocalEnv |
| `reinterpret_` | First-order + private effects | Adds private handler effects |
| `reinterpret` | Higher-order + private effects | Adds private handler effects + LocalEnv |
| `interpose_` | First-order interception | Modifies existing effect behavior |
| `interpose` | Higher-order interception | Modifies existing effect behavior + LocalEnv |
