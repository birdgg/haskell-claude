---
allowed-tools: Bash, Read, Grep, Glob
description: Run Haskell tests and report results
---

# Haskell Test

Run HSpec, QuickCheck, or Tasty tests for the current Haskell project.

## Steps

1. Detect the build tool (`cabal` or `stack`) by checking for project files.

2. If `$ARGUMENTS` is provided, use it to filter tests:
   - Module name: run only that test module.
   - Pattern: pass as `--match` to the test runner.

3. Run tests:

```
$TOOL test $ARGUMENTS 2>&1
```

4. Parse the test output:
   - Count passed, failed, and pending tests.
   - For each failure, extract:
     - Test name / describe block
     - Expected vs actual values
     - Source location if available

5. Report results in a structured summary:
   - Total: X | Passed: Y | Failed: Z | Pending: W
   - List each failure with context.

6. For failed tests, read the relevant test file and source file to understand the failure. Suggest specific fixes based on the mismatch.

## Arguments

- `$ARGUMENTS`: Optional test filter or flags (e.g., module name, `--match "parser"`, `--quickcheck-tests 1000`).

## Notes

- Do not modify test files unless explicitly asked.
- Report QuickCheck counterexamples with the shrunk minimal failing case.
- If no test suite is found, suggest how to set one up with HSpec.
