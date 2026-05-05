# DearBindings haskell toolchain

The project is using the `stack` toolchain:
- `stack.yaml` -- project-level configuration: pin deps, set package flags.
- `package.yaml` -- package-level: declare deps, declare and react to package flags, manage GHC options and extensions.
- Commands are typically wrapped in `stack`: `stack build`, `stack clean`, `stack ghci`, `stack haddock`, `stack hoogle`, ...
- Use project's own environment to query for types and docs.
- Use `stack clean` to flush cached artifacts that require force-rebuilding.

When in need for documentation, references, versions:
* Use stack-provided tools to query for types, docs etc.
* Search only in the project-local .stack-work.
* Just ask the user.

In general: just ask the user if you need something, or want to know something about the system, or something is missing.

The temporary directory is `./tmp`.
Prefer relative paths and avoid chaining commands with `cd`.

In general: keep everything inside the project's directory: scratch pads, one-off scripts, test setups.

House codestyle:
- GHC2021
- NoFieldSelectors, DuplicateRecordFields, OverloadedRecordDot
- LambdaCase, BlockArguments
- OverloadedStrings + Text
- ImportQualifiedPost: import Data.Blah (Blah) / import Data.Blah qualified as Blah
- File layout: Entry points and module focus types/functions go first, trailed by implementation details and helpers.
