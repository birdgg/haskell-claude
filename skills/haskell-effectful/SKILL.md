---
name: haskell-effectful
description: Effectful library conventions and decision rules. Use when writing effectful code, designing effects, or migrating from mtl.
---

# Effectful Conventions

## Dispatch Choice

- **Default: Static** (`Effectful.Reader.Static`, `Effectful.State.Static.Local`)
- Only use Dynamic when MonadReader/MonadState instances are needed

## Effect Stack Order

Always consistent: `Reader -> State -> Error -> IOE`

```haskell
runApp :: Config -> AppState -> App a -> IO (Either (CallStack, AppError) (a, AppState))
runApp cfg st = runEff . runError . runStateLocal st . runReader cfg
```

## Custom Effects

### Definition
- **Default: Use `makeEffect`** from `Effectful.TH` to auto-generate smart constructors and DispatchOf instances
- **Manual definition required** when GADTs have effect constraints (e.g., `Error e :> es`) or custom naming

### Handlers
- First-order (no `m` parameter): use `interpret_`
- Higher-order (uses `m` parameter): use `interpret` + `localSeqUnlift`

## Error Handling

- **Put `Error` constraints on GADT constructors, not on handler signatures** -- caller decides error scope
- Error.Static lacks MonadError -- define local `liftEither` helper
- Use `runErrorNoCallStack` or `runErrorWith` at boundaries
- Derive `Exception` on error newtypes for IO interop
- Use `adapt` helper pattern: `liftIO` + `C.catch` + `localSeqUnlift` for IO error conversion
- Pure handlers: `reinterpret` + `evalState` with `Map` for testable filesystem-like effects

```haskell
-- CORRECT: Error constraint on GADT constructor
data FileSystem :: Effect where
  ReadFile  :: Error FsReadError :> es => FilePath -> FileSystem (Eff es) String
  WriteFile :: Error FsWriteError :> es => FilePath -> String -> FileSystem (Eff es) ()

-- WRONG: Error constraint on handler
runFileSystemIO :: (IOE :> es, Error FsReadError :> es) => ...
```

## Concurrency

ALWAYS use `Effectful.Concurrent.*`, NEVER `Control.Concurrent.*` with `liftIO`.

## Anti-patterns

- Adding `Error` constraint to interpreter instead of effect definition
- Returning raw `Either` when errors should propagate via effect
- Mixing Error.Static and Error.Dynamic
- Over-constraining effect signatures
- Using `Control.Concurrent.*` with `liftIO` instead of `Effectful.Concurrent.*`
- Using `makeEffect` on GADTs with effect constraints (manual definition required)

## Reference

For detailed code examples (dependency injection, testing, MTL migration, concurrency):
- `references/effectful-examples.md`
