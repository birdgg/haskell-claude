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

- First-order (no `m` parameter): use `interpret_`
- Higher-order (uses `m` parameter): use `interpret` + `localSeqUnlift`

## Error Handling

- Use constrained effect signatures so caller decides error handling
- Error.Static lacks MonadError -- define local `liftEither` helper
- Use `runErrorNoCallStack` or `runErrorWith` at boundaries

## Concurrency

ALWAYS use `Effectful.Concurrent.*`, NEVER `Control.Concurrent.*` with `liftIO`.

## Anti-patterns

- Adding `Error` constraint to interpreter instead of effect definition
- Returning raw `Either` when errors should propagate via effect
- Mixing Error.Static and Error.Dynamic
- Over-constraining effect signatures
- Using `Control.Concurrent.*` with `liftIO` instead of `Effectful.Concurrent.*`

## Reference

For detailed code examples (dependency injection, testing, MTL migration, concurrency):
- `references/effectful-examples.md`
