# Relude Migration Guide

## Step-by-Step Migration from Standard Prelude

### 1. Add Dependency

```cabal
-- project.cabal
build-depends: relude >= 1.2 && < 1.3
```

```yaml
# package.yaml (hpack)
dependencies:
  - relude >= 1.2 && < 1.3
```

### 2. Configure Prelude Replacement

**Option A: Cabal Mixins (recommended)**

```cabal
mixins: base hiding (Prelude)
      , relude (Relude as Prelude)
      , relude
```

**Option B: Per-module pragma**

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
import Relude
```

**Option C: Custom Prelude module**

```haskell
-- src/MyPrelude.hs
module MyPrelude
  ( module Relude
  , module Relude.Extra.Map
  ) where

import Relude
import Relude.Extra.Map
```

Then in `.cabal`:
```cabal
other-modules: MyPrelude
-- or use mixins to alias it as Prelude
```

### 3. HLint Configuration

Add Relude's HLint rules to your project:

```yaml
# .hlint.yaml
- package:
    name: relude
    modules:
      - {name: Relude, as: Prelude}
```

## Common Migration Patterns

### String -> Text

```haskell
-- Before
processName :: String -> String
processName name = map toUpper name

-- After
import qualified Data.Text as T

processName :: Text -> Text
processName name = T.toUpper name
```

### Partial Functions -> Total Functions

```haskell
-- Before
getFirst :: [a] -> a
getFirst xs = head xs  -- crashes on []

-- After (Option 1: NonEmpty)
getFirst :: NonEmpty a -> a
getFirst xs = head xs  -- always safe

-- After (Option 2: Maybe)
getFirst :: [a] -> Maybe a
getFirst xs = viaNonEmpty head xs
```

### read -> readMaybe

```haskell
-- Before
parseAge :: String -> Int
parseAge = read  -- crashes on invalid input

-- After
parseAge :: Text -> Maybe Int
parseAge = readMaybe . toString
```

### putStrLn -> putTextLn

```haskell
-- Before
main :: IO ()
main = putStrLn "Hello"

-- After
main :: IO ()
main = putTextLn "Hello"
```

### show (String) -> show (Text)

```haskell
-- Before (produces String)
logValue :: Show a => a -> IO ()
logValue x = putStrLn ("Value: " ++ show x)

-- After (produces Text)
logValue :: (Show a, MonadIO m) => a -> m ()
logValue x = putTextLn ("Value: " <> show x)
```

### readFile -> readFileText

```haskell
-- Before (lazy String IO)
processFile :: FilePath -> IO String
processFile path = readFile path

-- After (strict Text IO)
processFile :: MonadIO m => FilePath -> m Text
processFile path = readFileText path
```

### nub -> ordNub / hashNub

```haskell
-- Before: O(n^2)
unique :: Eq a => [a] -> [a]
unique = nub

-- After: O(n log n)
unique :: Ord a => [a] -> [a]
unique = ordNub

-- Or O(n) with Hashable constraint
unique :: (Eq a, Hashable a) => [a] -> [a]
unique = hashNub
```

### liftIO . newIORef -> newIORef (lifted)

```haskell
-- Before
initState :: MonadIO m => m (IORef Int)
initState = liftIO (newIORef 0)

-- After (Relude lifts automatically)
initState :: MonadIO m => m (IORef Int)
initState = newIORef 0
```

## Common Compilation Errors After Migration

### "Not in scope: head"

`head` on `[a]` is not reexported. Solutions:

```haskell
-- Use viaNonEmpty
viaNonEmpty head myList

-- Use NonEmpty as input type
processItems :: NonEmpty Item -> Item
processItems items = head items

-- If you really need the partial version (not recommended)
import Relude.Unsafe (head)
```

### "Couldn't match type Text with [Char]"

Functions now expect `Text`:

```haskell
-- Fix: convert at boundaries
legacyFunction :: String -> String
legacyFunction input = toString (processText (toText input))

processText :: Text -> Text
processText = T.toUpper
```

### "Ambiguous occurrence show"

When both `Prelude.show` and `Relude.show` are in scope:

```haskell
-- Fix: ensure only Relude is imported, not both
-- Remove: import Prelude
-- Use the mixins approach to fully replace Prelude
```

### "No instance for (Eq (Set a)) => elem"

`elem` on `Set`/`HashSet` is forbidden:

```haskell
-- Fix: use member
import qualified Data.Set as Set
Set.member x mySet
```

### "error expects Text"

```haskell
-- Before
error "something went wrong"  -- String

-- After (error takes Text in Relude)
error "something went wrong"  -- Text literal works fine
-- But if you have a String variable:
error (toText myStringVar)
```

## Integration with Common Libraries

### With Aeson

```haskell
import Relude
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)

parseConfig :: ByteString -> Either Text Config
parseConfig bs = first toText (eitherDecodeStrict bs)
```

### With Servant

```haskell
import Relude
import Servant

-- Servant uses Text naturally, minimal friction
type API = "users" :> Get '[JSON] [User]
```

### With Effectful

```haskell
import Relude hiding (Reader, State, ask, get, put, modify)
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
```

### With Optparse-Applicative

```haskell
import Relude
import Options.Applicative

-- Text-based option parsing
textOption :: Mod OptionFields Text -> Parser Text
textOption = fmap toText . strOption
```

## Migration Checklist

- [ ] Add `relude` dependency to cabal/package.yaml
- [ ] Configure mixins or NoImplicitPrelude
- [ ] Replace `String` with `Text` in type signatures
- [ ] Replace `head`/`tail`/`last`/`init` with safe alternatives
- [ ] Replace `read` with `readMaybe`/`readEither`
- [ ] Replace `putStrLn` with `putTextLn`
- [ ] Replace `readFile`/`writeFile` with `readFileText`/`writeFileText`
- [ ] Replace `nub` with `ordNub` or `hashNub`
- [ ] Replace `elem` on Sets with `member`
- [ ] Remove explicit `liftIO` on IORef/MVar operations
- [ ] Remove unnecessary imports (many things are in scope via Relude)
- [ ] Add HLint rules for Relude
- [ ] Run full test suite to verify behavior
