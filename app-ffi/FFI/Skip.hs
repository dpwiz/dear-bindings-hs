{-| v0 skip rules. The FFI generator silently drops a handful of entity
shapes that have no straightforward Haskell FFI representation in v0,
counting each kind so the run can print a one-shot summary at the
end. See plan §B6 for the full list and the rationale per row.
-}
module FFI.Skip
  ( SkipCounters (..)
  , empty
  , bump

    -- * Per-entity decisions
  , SkipReason (..)
  , reasonLabel
  , skipFunction
  , skipDefine
  , skipStruct
  , skipEnum
  , skipTypedef
  , entityHasConditionals
  , conditionalsActiveDefault
  , isNumericLiteral

    -- * Reporting
  , renderReport
  ) where

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Char (isDigit, isHexDigit, toLower)
import Data.Text (Text)
import Data.Text qualified as Text
import DearBindings.JSON
  ( Argument (..)
  , Conditional
  , Define (..)
  , Enum_ (..)
  , Function (..)
  , Struct (..)
  , TypeKind (..)
  , Typedef (..)
  )
import DearBindings.JSON.Conditional qualified
import DearBindings.JSON.Types qualified
import FFI.HType (typeKindContainsInlineAggregate)

data SkipReason
  = ReasonVarargs
  | ReasonConditional
  | ReasonDefaultArgHelper
  | ReasonImstrOrUnformatted
  | ReasonNonNumericDefine
  | ReasonInlineAggregate
  | ReasonAnonymous
  | ReasonByValueStruct
  deriving (Eq, Show)

reasonLabel :: SkipReason -> Text
reasonLabel = \case
  ReasonVarargs -> "varargs functions"
  ReasonConditional -> "conditional entities"
  ReasonDefaultArgHelper -> "default-arg helpers"
  ReasonImstrOrUnformatted -> "imstr / unformatted helpers"
  ReasonNonNumericDefine -> "non-numeric defines"
  ReasonInlineAggregate -> "inline aggregates in signatures"
  ReasonAnonymous -> "anonymous types"
  ReasonByValueStruct -> "by-value struct in signature"

data SkipCounters = SkipCounters
  { varargsFunctions :: Int
  , conditionalEntities :: Int
  , defaultArgHelpers :: Int
  , imstrOrUnformattedHelpers :: Int
  , nonNumericDefines :: Int
  , inlineAggregateInSig :: Int
  , anonymousTypes :: Int
  , byValueStructInSig :: Int
  }
  deriving (Eq, Show)

empty :: SkipCounters
empty = SkipCounters 0 0 0 0 0 0 0 0

bump :: SkipReason -> SkipCounters -> SkipCounters
bump r c = case r of
  ReasonVarargs -> c{varargsFunctions = c.varargsFunctions + 1}
  ReasonConditional -> c{conditionalEntities = c.conditionalEntities + 1}
  ReasonDefaultArgHelper -> c{defaultArgHelpers = c.defaultArgHelpers + 1}
  ReasonImstrOrUnformatted -> c{imstrOrUnformattedHelpers = c.imstrOrUnformattedHelpers + 1}
  ReasonNonNumericDefine -> c{nonNumericDefines = c.nonNumericDefines + 1}
  ReasonInlineAggregate -> c{inlineAggregateInSig = c.inlineAggregateInSig + 1}
  ReasonAnonymous -> c{anonymousTypes = c.anonymousTypes + 1}
  ReasonByValueStruct -> c{byValueStructInSig = c.byValueStructInSig + 1}

-- ---------------------------------------------------------------------------
-- Per-entity decisions

{- | Decide whether a function should be skipped. The 'Set' parameter
is the catalog's set of struct names: any TKUser reference to one
of those names that appears in by-value position (not behind a
@TKPointer@) means the function passes a struct by value, which
@capi@-marshaled opaque @data X@ types can't carry — so we skip.

Conditional entries are skipped only when their guards evaluate to
@false@ under the build's default define set (currently empty). So
@ifdef __EMSCRIPTEN__@ skips on a Linux build (the C symbol isn't
emitted by the impl's .cpp), but @ifndef IMGUI_DISABLE_DEBUG_TOOLS@
keeps (the symbol IS emitted under default flags). This preserves
typedefs like @ImDrawIdx@ (guarded by a default-fallback pattern)
without leaving emscripten-only impl functions unresolved at link.
-}
skipFunction :: Set Text -> Function -> Maybe SkipReason
skipFunction structNames f
  | not (conditionalsActive defaultDefines f.conditionals) = Just ReasonConditional
  | f.isDefaultArgumentHelper = Just ReasonDefaultArgHelper
  | f.isImstrHelper || f.isUnformattedHelper || f.isManualHelper = Just ReasonImstrOrUnformatted
  | any (.isVarargs) f.arguments = Just ReasonVarargs
  | refersToVaList f.returnType.description = Just ReasonVarargs
  | any argRefersToVaList f.arguments = Just ReasonVarargs
  | typeKindContainsInlineAggregate f.returnType.description = Just ReasonInlineAggregate
  | any argHasInlineAggregate f.arguments = Just ReasonInlineAggregate
  | byValueStructIn f.returnType.description = Just ReasonByValueStruct
  | any argHasByValueStruct f.arguments = Just ReasonByValueStruct
  | otherwise = Nothing
  where
    argHasInlineAggregate :: Argument -> Bool
    argHasInlineAggregate a = case a.type_ of
      Just tr -> typeKindContainsInlineAggregate tr.description
      Nothing -> False

    argHasByValueStruct :: Argument -> Bool
    argHasByValueStruct a = case a.type_ of
      Just tr -> byValueStructIn tr.description
      Nothing -> False

    argRefersToVaList :: Argument -> Bool
    argRefersToVaList a = case a.type_ of
      Just tr -> refersToVaList tr.description
      Nothing -> False

    byValueStructIn = byValueStructRef structNames

{- | True iff a 'TypeKind' tree mentions @va_list@. Functions taking
@va_list@ are the @_V@ companions of the C-side varargs forms; like
the varargs originals, they have no Haskell FFI counterpart (no
portable representation of @va_list@), so we count them under
'ReasonVarargs'.
-}
refersToVaList :: TypeKind -> Bool
refersToVaList = go
  where
    go = \case
      TKBuiltin _ _ -> False
      TKUser "va_list" _ -> True
      TKUser _ _ -> False
      TKPointer inner _ _ _ -> go inner
      TKArray inner _ -> go inner
      TKType _ inner -> go inner
      TKFunction ret params -> go ret || any go params
      TKInlineStruct _ -> False
      TKInlineUnion _ -> False

{- | Walk a 'TypeKind' looking for a 'TKUser' name that refers to a
catalog struct, in by-value position (not under a 'TKPointer'). True
if such a reference exists.
-}
byValueStructRef :: Set Text -> TypeKind -> Bool
byValueStructRef names = go
  where
    go = \case
      TKBuiltin _ _ -> False
      TKPointer _ _ _ _ -> False
      TKArray _ _ -> False -- arrays decay to pointers in signatures
      TKUser n _ -> n `Set.member` names
      TKType _ inner -> go inner
      TKFunction _ _ -> False
      TKInlineStruct _ -> False -- handled by ReasonInlineAggregate
      TKInlineUnion _ -> False

skipDefine :: Define -> Maybe SkipReason
skipDefine d
  | not (conditionalsActive defaultDefines d.conditionals) = Just ReasonConditional
  | not (numericContent d.content) = Just ReasonNonNumericDefine
  | otherwise = Nothing
  where
    numericContent Nothing = False
    numericContent (Just t) = isNumericLiteral t

skipStruct :: Struct -> Maybe SkipReason
skipStruct s
  | not (conditionalsActive defaultDefines s.conditionals) = Just ReasonConditional
  | s.isAnonymous = Just ReasonAnonymous
  | otherwise = Nothing

skipEnum :: Enum_ -> Maybe SkipReason
skipEnum e
  | not (conditionalsActive defaultDefines e.conditionals) = Just ReasonConditional
  | otherwise = Nothing

skipTypedef :: Typedef -> Maybe SkipReason
skipTypedef t
  | not (conditionalsActive defaultDefines t.conditionals) = Just ReasonConditional
  | typeKindContainsInlineAggregate t.type_.description = Just ReasonInlineAggregate
  | otherwise = Nothing

{- | True iff the entity's @conditionals@ field carries at least one
preprocessor guard. v0 drops everything guarded.
-}
entityHasConditionals :: Maybe [Conditional] -> Bool
entityHasConditionals = hasConditionals

hasConditionals :: Maybe [Conditional] -> Bool
hasConditionals Nothing = False
hasConditionals (Just []) = False
hasConditionals (Just _) = True

{- | The default define set used to evaluate @conditionals@. Includes
the flavor-flagging macros that the dear_bindings public C-API
header @\#define@s unconditionally — @IMGUI_HAS_DOCK@,
@IMGUI_HAS_VIEWPORT@, @IMGUI_HAS_TABLE@, @IMGUI_HAS_TEXTURES@.
Without these the docking-internal package would skip
@ImGuiDockNode@ et al. (gated by @\#ifdef IMGUI_HAS_DOCK@).

We deliberately do NOT set @IMGUI_DISABLE_DEBUG_TOOLS@,
@IMGUI_DISABLE_OBSOLETE_FUNCTIONS@, @IMGUI_HAS_IMSTR@,
@IMGUI_ENABLE_TEST_ENGINE@, @IMGUI_STB_NAMESPACE@, or
@__EMSCRIPTEN__@ — guards involving those resolve to false here,
which matches the build's compile-time view.
-}
defaultDefines :: Set Text
defaultDefines =
  Set.fromList
    [ "IMGUI_HAS_DOCK"
    , "IMGUI_HAS_VIEWPORT"
    , "IMGUI_HAS_TABLE"
    , "IMGUI_HAS_TEXTURES"
    , -- The C side enables SSE on x86_64 Linux via an
      -- @#if (defined __SSE__ || …)@ block in the public header.
      -- That triggers a @#define IMGUI_ENABLE_SSE@, which gates the
      -- SSE-specialised forms of e.g. @cImRsqrt@. We assume the
      -- build is SSE-enabled (true on every x86_64 Linux target).
      "IMGUI_ENABLE_SSE"
    ]

{- | True iff every conditional in the list evaluates to true under
the given define set (i.e. the entity is INSIDE all the @#ifdef@s
and not gated out). @Nothing@ and @[]@ both mean "no conditionals,
keep".
-}
conditionalsActive :: Set Text -> Maybe [Conditional] -> Bool
conditionalsActive _ Nothing = True
conditionalsActive _ (Just []) = True
conditionalsActive defs (Just cs) = all (evalConditional defs) cs

-- | 'conditionalsActive' specialised to the default (empty) define
-- set. Used to filter sub-entities like enum elements whose own
-- guards may differ from the parent entity's.
conditionalsActiveDefault :: Maybe [Conditional] -> Bool
conditionalsActiveDefault = conditionalsActive defaultDefines

{- | Evaluate one preprocessor guard. Recognises @ifdef@ / @ifndef@
exactly, plus @if@ expressions of the form @defined(X)@ /
@!defined(X)@. Anything else returns 'True' — we'd rather emit a
binding and let GHC's link step surface the truth than silently
drop entries with parse-quirks in their guard expressions.
-}
evalConditional :: Set Text -> Conditional -> Bool
evalConditional defs c = case c.condition of
  "ifdef" -> Set.member (Text.strip c.expression) defs
  "ifndef" -> Set.notMember (Text.strip c.expression) defs
  "if" -> evalIfExpr defs (Text.strip c.expression)
  _ -> True

evalIfExpr :: Set Text -> Text -> Bool
evalIfExpr defs e
  | Just sym <- stripDefined e = Set.member sym defs
  | Just sym <- Text.stripPrefix "!" e
  , Just sym' <- stripDefined (Text.strip sym) =
      Set.notMember sym' defs
  | otherwise = True
  where
    stripDefined t = do
      t1 <- Text.stripPrefix "defined" (Text.strip t)
      t2 <- Text.stripPrefix "(" (Text.stripStart t1)
      t3 <- Text.stripSuffix ")" (Text.stripEnd t2)
      pure (Text.strip t3)

-- ---------------------------------------------------------------------------
-- Numeric-literal detector for #defines.
--
-- Accepts decimal integers, hex (@0x…@), and floating-point with the
-- usual suffixes. Rejects strings, function-like macros, casts,
-- bit-shift expressions in parens, and anything containing
-- whitespace, parens, or operators.

isNumericLiteral :: Text -> Bool
isNumericLiteral raw =
  let t = Text.strip raw
  in not (Text.null t) && go (dropSign t)
  where
    dropSign s = case Text.uncons s of
      Just ('-', rest) -> rest
      Just ('+', rest) -> rest
      _ -> s

    go s
      | Text.isPrefixOf "0x" lower || Text.isPrefixOf "0X" lower =
          isHexBody (dropSuffixes (Text.drop 2 s))
      | otherwise = isDecOrFloat (dropSuffixes s)
      where
        lower = Text.toLower (Text.take 2 s)

    isHexBody body =
      not (Text.null body) && Text.all isHexDigit body

    isDecOrFloat body =
      not (Text.null body)
        && Text.all (\c -> isDigit c || c == '.' || c `elem` ("eE+-" :: String)) body
        && Text.any isDigit body
        -- Reject floats with multiple dots etc. by demanding the
        -- string parses as either Integer or Double.
        && validNumber (Text.unpack body)

    validNumber s =
      case reads @Integer s of
        [(_, "")] -> True
        _ -> case reads @Double s of
          [(_, "")] -> True
          _ -> False

    dropSuffixes :: Text -> Text
    dropSuffixes = Text.dropWhileEnd (\c -> toLower c `elem` ("ulf" :: String))

-- ---------------------------------------------------------------------------
-- Reporting

renderReport :: SkipCounters -> Text
renderReport c =
  Text.unlines $
    "Skipped:" : map renderRow rows
  where
    rows =
      [ ("varargs functions", c.varargsFunctions)
      , ("conditional entities", c.conditionalEntities)
      , ("default-arg helpers", c.defaultArgHelpers)
      , ("imstr / unformatted helpers", c.imstrOrUnformattedHelpers)
      , ("non-numeric defines", c.nonNumericDefines)
      , ("inline aggregates in signatures", c.inlineAggregateInSig)
      , ("anonymous types", c.anonymousTypes)
      , ("by-value struct in signature", c.byValueStructInSig)
      ]
    renderRow (label, n) =
      "  " <> Text.justifyLeft 35 ' ' (label <> ":") <> Text.pack (show n)
