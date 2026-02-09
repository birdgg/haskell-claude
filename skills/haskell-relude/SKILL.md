---
name: haskell-relude
description: Relude conventions and safe patterns. Use when setting up relude, migrating from Prelude, or working with relude-based projects.
---

# Relude Conventions

## Setup

Use cabal mixins for project-wide adoption (preferred over NoImplicitPrelude):

```cabal
build-depends:    base >= 4.14 && < 5, relude >= 1.2 && < 1.3
mixins:           base hiding (Prelude)
                , relude (Relude as Prelude)
                , relude
```

## Key Rules

- **Text everywhere** -- never use `String` except for legacy interop
- **NonEmpty for non-empty collections** -- use `viaNonEmpty` to safely bridge from lists
- **`readMaybe`/`readEither`** -- never `read`
- **`ordNub`/`hashNub`** -- never `nub` (O(n^2))
- **`Set.member`/`HashMap.member`** -- never `elem` on Set/HashSet (compile error in relude)
- **`whenM`/`unlessM`** -- for monadic conditionals
- **`one`** -- for singleton container construction
- **Lifted IO** -- use relude's lifted `newIORef`, `readIORef`, etc. directly (no `liftIO` wrapper needed)

## Common Conversions

- `encodeUtf8` / `decodeUtf8` -- Text <-> ByteString
- `toText` / `toLText` / `toString` -- between string types
- `toStrict` / `toLazy` -- strict <-> lazy Text

## Anti-patterns

- Using `String` when `Text` is available
- Using `head`/`tail` on `[a]` instead of `NonEmpty` or `viaNonEmpty`
- Using `liftIO . readIORef` instead of lifted `readIORef`
- Importing `Prelude` alongside `Relude`
- Leaving `trace` calls in production code

## Reference

For migration guide and complete examples:
- `references/relude-migration.md`
