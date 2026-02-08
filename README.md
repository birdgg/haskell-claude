# haskell-skills

Haskell development skills, commands, and agents for Claude Code.

## Installation

Add this plugin to your Claude Code configuration:

```bash
claude plugin add /path/to/haskell-skills
```

Or symlink into your plugins directory:

```bash
ln -s /path/to/haskell-skills ~/.claude/plugins/haskell-skills
```

## Components

### Commands

| Command | Description |
|---------|-------------|
| `/haskell-build` | Build a Haskell project with `cabal` or `stack`, parse GHC errors, and auto-fix |
| `/haskell-test` | Run HSpec/QuickCheck/Tasty tests and report results |

### Agents

| Agent | Description |
|-------|-------------|
| `haskell-reviewer` | Code review for idiomatic Haskell, type safety, purity, and performance |

### Skills

| Skill | Description |
|-------|-------------|
| `haskell-patterns` | Idiomatic Haskell patterns: newtypes, smart constructors, ReaderT, MTL, lens, STM |

### Hooks

| Hook | Trigger | Description |
|------|---------|-------------|
| HLint | PostToolUse (Edit/Write) | Automatically runs HLint on `.hs` files after editing |

## Prerequisites

- GHC and `cabal-install` or `stack`
- `hlint` (optional, for automatic linting): `cabal install hlint`

## Structure

```
haskell-skills/
├── .claude-plugin/
│   └── plugin.json
├── commands/
│   ├── haskell-build.md
│   └── haskell-test.md
├── agents/
│   └── haskell-reviewer.md
├── skills/
│   └── haskell-patterns/
│       ├── SKILL.md
│       └── references/
├── hooks/
│   └── hooks.json
└── README.md
```
