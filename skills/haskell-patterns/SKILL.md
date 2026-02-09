---
name: haskell-patterns
description: Idiomatic Haskell conventions and decision rules. Use when writing Haskell code, designing data types, or handling errors.
---

# Haskell Conventions

## Type Design

- **Newtype wrappers** for domain primitives (`UserId`, `Email`) -- never raw `Int`/`Text`
- **Smart constructors** to enforce invariants at construction time
- **Sum types for states** -- never booleans for mutually exclusive states

## Error Handling

- `Either` / `ExceptT` for expected, recoverable errors
- `throwIO` for unexpected, unrecoverable errors
- Never use partial functions: `head`, `tail`, `fromJust`, `read`

## Application Structure

- **ReaderT pattern** (`ReaderT AppEnv IO`) over deep transformer stacks
- MTL-style typeclasses for effect abstraction when needed
- Avoid stacking more than 2-3 transformers

## Strictness

- Use `Text`, never `String` (unless legacy interop)
- Strict IO (`Data.Text.IO`) over lazy IO
- Use streaming (conduit/pipes) for large data

## Concurrency

- `STM` / `TVar` for shared mutable state
- `async` / `mapConcurrently` for concurrent tasks
- `race` for timeouts

## Anti-patterns

- `String` for text data
- Lazy IO (`Prelude.readFile`)
- Deep monad transformer stacks
- Boolean parameters instead of sum types
- Partial functions
