---
allowed-tools: Bash, Read, Edit, Grep, Glob
description: Build a Haskell project and auto-fix GHC errors
---

# Haskell Build

Build the current Haskell project, parse GHC errors, and attempt automatic fixes.

## Steps

1. Detect the build tool by checking for `cabal.project` / `stack.yaml` / `*.cabal` files in the workspace.

2. Run the build:

```
$TOOL = cabal or stack (detected in step 1, default to cabal)
```

Execute: `!$TOOL build 2>&1`

3. If the build succeeds, report success and exit.

4. If the build fails, parse the GHC error output:
   - Extract file path, line number, and error message from each diagnostic.
   - Group errors by file.

5. For each error, read the relevant file and surrounding context, then apply a fix:
   - **Type mismatch**: Check expected vs actual types, add conversions or fix signatures.
   - **Not in scope**: Check imports, add missing imports or qualify names.
   - **Parse error**: Fix syntax issues (missing `do`, incorrect indentation, missing `where`).
   - **Redundant constraint**: Remove the unnecessary constraint from the type signature.
   - **Missing instance**: Add a `deriving` clause or manual instance.

6. After applying fixes, re-run the build. Repeat up to 3 iterations.

7. If errors persist after 3 iterations, report remaining errors with file locations and suggested manual fixes.

## Arguments

- `$ARGUMENTS`: Optional flags passed directly to the build tool (e.g., `--ghc-options="-Wall"`).

## Notes

- Always preserve the user's existing code style and indentation.
- Never remove user comments or documentation.
- If unsure about a fix, show the error and ask the user rather than guessing.
