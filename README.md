# haskell-claude

Haskell development skills, commands, and agents for Claude Code.

## Installation

```bash
/plugin add github:birdgg/haskell-claude
```

## Components

### Skills

| Skill | Description |
|-------|-------------|
| `haskell-patterns` | Idiomatic Haskell conventions: newtypes, smart constructors, ReaderT, error handling, concurrency |
| `haskell-effectful` | Effectful library conventions: dispatch choice, effect stack order, custom effects, concurrency |
| `haskell-relude` | Relude conventions: Text-first, safe alternatives, container types, lifted IO |
| `haskell-servant` | Servant web framework conventions: NamedRoutes record pattern, CRUD APIs, auth, testing |

### Commands

| Command | Description |
|---------|-------------|
| `/haskell-build` | Build with `cabal`/`stack`, parse GHC errors, and auto-fix |
| `/haskell-test` | Run HSpec/QuickCheck/Tasty tests and report results |

### Agents

| Agent | Description |
|-------|-------------|
| `haskell-reviewer` | Code review for idiomatic Haskell, type safety, purity, and performance |

### Hooks

| Hook | Trigger | Description |
|------|---------|-------------|
| HLint | PostToolUse (Edit/Write) | Runs HLint on `.hs` files after editing |

## Prerequisites

- GHC and `cabal-install` or `stack`
- `hlint` (optional): `cabal install hlint`

## Structure

```
haskell-claude/
├── .claude-plugin/
│   └── plugin.json
├── commands/
│   ├── haskell-build.md
│   └── haskell-test.md
├── agents/
│   └── haskell-reviewer.md
├── skills/
│   ├── haskell-patterns/
│   │   └── SKILL.md
│   ├── haskell-effectful/
│   │   ├── SKILL.md
│   │   └── references/
│   │       └── effectful-examples.md
│   ├── haskell-relude/
│   │   ├── SKILL.md
│   │   └── references/
│   │       └── relude-migration.md
│   └── haskell-servant/
│       ├── SKILL.md
│       └── references/
│           └── servant-examples.md
├── hooks/
│   └── hooks.json
└── README.md
```
