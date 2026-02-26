# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Does

`elm-street` is a Haskell library that automatically generates Elm type definitions and compatible JSON encoders/decoders from Haskell types, using GHC Generics. It eliminates boilerplate in full-stack Haskell/Elm applications.

## Build & Test Commands

```bash
# Build
cabal build all
stack build --system-ghc

# Run all tests
cabal test all
stack test --system-ghc

# Run a specific test (hspec pattern matching)
cabal test elm-street-test --test-options "--match /golden"

# Format Haskell code (100 char column limit)
stylish-haskell -r src/

# Run the Elm code generator (outputs to frontend/src/Core/)
cabal new-run generate-elm

# Run the example backend (Warp on port 8080)
cabal new-run run-backend

# Run the example frontend (in separate terminal)
cd frontend && elm-app start
```

## Architecture

### Data Flow

```
Haskell type (with Generic + Elm instances)
  → Elm.Generic: extracts type info via GHC Generics
  → Elm.Ast: intermediate AST (ElmDefinition)
  → Elm.Print.*: pretty-printed Elm source code
  → generateElm: writes Types.elm, Encoder.elm, Decoder.elm, ElmStreet.elm
```

### Key Modules

- **`Elm.Generic`** (`src/Elm/Generic.hs`): The `Elm` typeclass. Derive via `DeriveAnyClass` or `DerivingVia ElmStreet`. Includes compile-time type constraint checks (no type variables, max 8 constructor fields, no named sum type fields).
- **`Elm.Ast`** (`src/Elm/Ast.hs`): The `ElmDefinition` ADT representing Records, Sum types, Primitives, and Newtypes independent of formatting.
- **`Elm.Generate`** (`src/Elm/Generate.hs`): `generateElm @Types` entry point that writes all four Elm files to disk.
- **`Elm.Aeson`** (`src/Elm/Aeson.hs`): `elmStreetToJson` / `elmStreetParseJson` — aeson options that strip the type-name prefix from record fields, keeping JSON consistent with generated Elm decoders.
- **`Elm.Print.*`** (`src/Elm/Print/`): Converts `ElmDefinition` AST to `prettyprinter` `Doc` for Types, Encoders, and Decoders.

### Project Layout

- `src/` — library source (the main `elm-street` package)
- `types/` — shared `OneType` test data type used by tests and example app
- `test/` — Hspec + golden tests (`test/golden/` holds expected outputs)
- `generate-elm/` — standalone executable that generates Elm files from `types/`
- `backend/` — example Servant/Warp server
- `frontend/` — example Elm 0.19.1 app consuming generated types

### Naming Conventions

Record fields **must** be prefixed with the type name or abbreviation (e.g., `userName`, `hrUser`). `elmStreetParseJson`/`elmStreetToJson` strips this prefix when serializing to JSON, matching what the generated Elm decoders expect.

### Deriving Patterns

```haskell
-- Standard records and sum types
deriving (Elm, ToJSON, FromJSON) via ElmStreet MyType

-- Newtypes: JSON uses newtype strategy, Elm uses anyclass
deriving newtype (FromJSON, ToJSON)
deriving anyclass (Elm)

-- Phantom type newtypes: implement Elm manually
instance Elm (Id a) where
    toElmDefinition _ = elmNewtype @Text "Id" "unId"
```

### Library Restrictions

- No type variables in derived types (phantom types need a manual `Elm` instance)
- Sum types with record fields are not supported
- Sum types with >8 fields in any constructor are not supported
- Self-referential record types are not supported (generates `type alias`, which Elm disallows recursively)
- Only `UTCTime` is supported for time types (maps to Elm `Posix`)
- Certain Elm reserved words (`type`, `if`, `case`, `module`, `tag`, etc.) cannot be used as field names

### GHCi

The `.ghci` file enables `-XTypeApplications`, `-XDerivingStrategies`, `-XDeriveAnyClass`, `-XDataKinds` by default, which are all required for typical elm-street usage.
