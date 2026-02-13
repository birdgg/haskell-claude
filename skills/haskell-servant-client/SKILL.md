---
name: haskell-servant-client
description: Servant client API wrapper conventions with two-layer error handling. Use when generating HTTP clients from Servant APIs, wrapping external service APIs, or integrating servant-client with effectful.
---

# Servant Client Conventions (Two-Layer Error Pattern)

## Two-Layer Error Type

- **Always separate network errors from domain errors** -- never flatten into one type
- Wrap `ClientError` (network/HTTP) and domain-specific error (parsed from response body) as distinct constructors
- Derive `Generic`, `Show`, `Eq` with `deriving stock`
- Add `NFData` on all error types for deep evaluation
- Add `Exception` on the top-level client error for IO interop

```haskell
data SlackClientError
  = ServantError ClientError
  | SlackError ResponseSlackError
  deriving stock (Eq, Generic, Show)

instance NFData SlackClientError
instance Exception SlackClientError
```

## ResponseJSON Newtype

- Use `newtype ResponseJSON a = ResponseJSON (Either DomainError a)` to parse API envelopes
- The custom `FromJSON` instance checks the `ok` field and branches to error or payload parsing
- Use `unnestErrors` to collapse `Either ClientError (ResponseJSON a)` into `Response a`

```haskell
newtype ResponseJSON a = ResponseJSON (Either ResponseSlackError a)

unnestErrors :: Either ClientError (ResponseJSON a) -> Response a
```

## Three-Tier API Organization

1. **Raw Servant clients** (internal, suffixed with `_`): `chatPostMessage_ :: AuthReq -> PostMsgReq -> ClientM (ResponseJSON PostMsgRsp)`
2. **IO wrapper functions** (public API): `chatPostMessage :: SlackConfig -> PostMsgReq -> IO (Response PostMsgRsp)`
3. **Effect-based interface** (effectful or MonadReader): caller decides the monad

## Authentication

- Use `AuthProtect "token"` in the API type with `type instance AuthClientData (AuthProtect "token") = Text`
- Create `authenticateReq` to add `Authorization: Bearer <token>` header
- Bundle `Manager` + token in a config record (`SlackConfig`)

## Pagination

- Define `PagedRequest` / `PagedResponse` typeclasses with cursor-based iteration
- Use `type LoadPage m a = m (Response [a])` for paginated fetching
- Provide `fetchAllBy :: (req -> m (Response resp)) -> req -> m (LoadPage m a)` combinator

## Effectful Integration

- Define a dynamic effect (GADT) for the client operations
- **Put `Error` constraints on GADT constructors** -- caller decides error scope
- Use the `adapt` pattern: `liftIO` + `C.catch` + `localSeqUnlift` to convert `ClientError` to effect errors
- Parse domain errors inside the interpreter, throw via the GADT-constrained error

```haskell
data MyServiceClient :: Effect where
  ListItems :: Error MyServiceError :> es => MyServiceClient (Eff es) [Item]
```

## Anti-patterns

- Flattening `ClientError` and domain errors into a single sum type without distinction
- Missing `NFData` instances (prevents deep evaluation, hides thunk leaks)
- Returning raw `Either ClientError a` to callers instead of domain error types
- Hardcoding `BaseUrl` instead of reading from config
- Skipping `Exception` instance on error types (breaks IO boundary interop)
- Adding `Error` constraint to interpreter signature instead of GADT constructor (effectful)
- Parsing response envelope in each handler instead of centrally in `ResponseJSON`

## Reference

For complete code examples (error types, ResponseJSON, auth, pagination, effectful integration):
- `references/servant-client-examples.md`
