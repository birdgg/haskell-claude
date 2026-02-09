---
name: haskell-servant
description: Servant web framework conventions using NamedRoutes record pattern. Use when building REST APIs, defining endpoints, writing handlers, or structuring Servant applications.
---

# Servant Conventions (NamedRoutes Record Pattern)

## API Definition

- **Always use `NamedRoutes`** with record types -- never raw `:<|>` combinators
- Derive `Generic` for all API record types
- One record per resource/domain (e.g., `UserRoutes`, `ProductRoutes`)

```haskell
data UserRoutes mode = UserRoutes
  { list   :: mode :- "users" :> Get '[JSON] [User]
  , get    :: mode :- "users" :> Capture "id" UserId :> Get '[JSON] User
  , create :: mode :- "users" :> ReqBody '[JSON] CreateUser :> Post '[JSON] User
  } deriving Generic
```

## Composing APIs

- Nest records for sub-routes using `NamedRoutes`
- Top-level API record composes all resource routes

```haskell
data API mode = API
  { users    :: mode :- "api" :> "v1" :> NamedRoutes UserRoutes
  , products :: mode :- "api" :> "v1" :> NamedRoutes ProductRoutes
  , health   :: mode :- "health" :> Get '[JSON] HealthStatus
  } deriving Generic

type AppAPI = NamedRoutes API
```

## Handlers

- Handler records mirror API records exactly
- Use `UserRoutes AsServer` as the handler type
- Keep handlers thin -- delegate to service layer

```haskell
userHandlers :: UserRoutes AsServer
userHandlers = UserRoutes
  { list   = listUsersHandler
  , get    = getUserHandler
  , create = createUserHandler
  }
```

## Error Handling

- Use `throwError` with `ServerError` for HTTP errors
- Define domain-specific error types, convert at boundary
- Never leak internal errors to clients

## Authentication

- Use `AuthProtect` or `Auth` from servant-auth for protected routes
- Apply auth at the record field level, not globally

## Content Types

- Default to `JSON` for API endpoints
- Use `PlainText` for health checks and simple responses
- Define custom content types for special serialization needs

## Anti-patterns

- Using `:<|>` combinators instead of `NamedRoutes` records
- Fat handlers with business logic inline
- Returning raw `Text`/`String` instead of domain types
- Missing `Generic` derivation on API records
- Hardcoded error messages instead of structured error types
- Skipping newtypes for path captures (`Capture "id" Int` vs `Capture "id" UserId`)

## Reference

For complete code examples (CRUD API, auth, error handling, testing, client generation):
- `references/servant-examples.md`
