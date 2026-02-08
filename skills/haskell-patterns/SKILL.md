---
description: Idiomatic Haskell patterns, best practices, and anti-patterns
triggers:
  - writing Haskell code
  - asking about Haskell patterns
  - designing Haskell data types
  - working with monads or effects
  - Haskell error handling
  - Haskell concurrency
---

# Haskell Idiomatic Patterns

## Type Design

### Newtype Wrappers

Wrap primitive types for domain safety:

```haskell
newtype UserId = UserId { unUserId :: Int }
  deriving (Eq, Ord, Show)

newtype Email = Email { unEmail :: Text }
  deriving (Eq, Show)
```

### Smart Constructors

Validate invariants at construction time:

```haskell
newtype PositiveInt = PositiveInt { unPositiveInt :: Int }
  deriving (Eq, Ord, Show)

mkPositiveInt :: Int -> Maybe PositiveInt
mkPositiveInt n
  | n > 0     = Just (PositiveInt n)
  | otherwise = Nothing
```

### Sum Types for States

Model states explicitly instead of booleans:

```haskell
-- Bad: data User = User { isActive :: Bool, isVerified :: Bool }
-- Good:
data UserStatus = Pending | Active | Suspended | Deleted
  deriving (Eq, Show)
```

## Error Handling

### Use Either for Expected Errors

```haskell
data AppError
  = NotFound Text
  | ValidationError Text
  | Unauthorized
  deriving (Show)

findUser :: UserId -> ExceptT AppError IO User
findUser uid = do
  mUser <- lift $ queryUser uid
  case mUser of
    Nothing   -> throwError (NotFound "User not found")
    Just user -> pure user
```

### Avoid Partial Functions

```haskell
-- Bad
head xs          -- crashes on empty list
fromJust mVal    -- crashes on Nothing
read str         -- crashes on parse failure

-- Good
listToMaybe xs        -- returns Maybe
mVal                  -- keep the Maybe, handle it
readMaybe str         -- returns Maybe
```

## Monad Patterns

### ReaderT Pattern

Structure applications with a shared environment:

```haskell
data AppEnv = AppEnv
  { appLogger   :: LogAction IO Message
  , appDbPool   :: Pool Connection
  , appConfig   :: Config
  }

type App = ReaderT AppEnv IO

runApp :: AppEnv -> App a -> IO a
runApp = flip runReaderT
```

### MTL Style

Use typeclass constraints for effect abstraction:

```haskell
class Monad m => MonadDB m where
  queryUser :: UserId -> m (Maybe User)
  saveUser  :: User -> m ()

class Monad m => MonadLog m where
  logInfo  :: Text -> m ()
  logError :: Text -> m ()

handleRequest :: (MonadDB m, MonadLog m) => UserId -> m Response
handleRequest uid = do
  logInfo $ "Fetching user: " <> show uid
  mUser <- queryUser uid
  case mUser of
    Nothing -> do
      logError "User not found"
      pure notFoundResponse
    Just user ->
      pure (okResponse user)
```

## Lens Patterns

### Basic Lens Usage

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Config = Config
  { _configHost :: Text
  , _configPort :: Int
  , _configDb   :: DbConfig
  }

data DbConfig = DbConfig
  { _dbHost :: Text
  , _dbPort :: Int
  }

makeLenses ''Config
makeLenses ''DbConfig

-- Nested update (immutable)
updateDbPort :: Int -> Config -> Config
updateDbPort port = configDb . dbPort .~ port
```

## Concurrency

### STM for Shared State

```haskell
import Control.Concurrent.STM

type Counter = TVar Int

newCounter :: IO Counter
newCounter = newTVarIO 0

increment :: Counter -> STM ()
increment counter = modifyTVar' counter (+ 1)

readCount :: Counter -> STM Int
readCount = readTVar
```

### Async for Concurrent Tasks

```haskell
import Control.Concurrent.Async

fetchAll :: [Url] -> IO [Response]
fetchAll urls = mapConcurrently fetch urls

raceTimeout :: Int -> IO a -> IO (Maybe a)
raceTimeout microseconds action =
  race (threadDelay microseconds) action >>= \case
    Left _  -> pure Nothing
    Right a -> pure (Just a)
```

## Anti-Patterns

### String for Text Data

```haskell
-- Bad: String is [Char], O(n) for most operations
processName :: String -> String

-- Good: Use Text
processName :: Text -> Text
```

### Lazy IO

```haskell
-- Bad: Lazy IO leads to resource leaks
contents <- readFile "data.txt"

-- Good: Strict IO or streaming
contents <- Data.Text.IO.readFile "data.txt"
-- Or use conduit/pipes for large files
```

### Deep Monad Transformer Stacks

```haskell
-- Bad: Hard to reason about, slow
type App = StateT AppState (ReaderT Config (ExceptT AppError (LoggingT IO)))

-- Good: Use ReaderT + IO, handle errors with ExceptT only where needed
type App = ReaderT AppEnv IO
```
