
{-| FFI-specific routing of catalog entities into modules.

Two-layer layout:

* All **type-defining** entities (typedefs, structs, enums, defines)
  go into a single shared @\<root\>.Types@ module. C types span the
  whole API surface — a function declared in the @ImGui@ slice can
  reference @ImTextureRef@ which "naturally" lives elsewhere — so
  trying to scatter them per-slice produces unresolvable
  cross-module references. One central types module is the simplest
  form that compiles.

* **Functions** are partitioned per-slice using the same
  most-specific-qualifier rule the doc generator's symbol table
  uses. Each slice module imports @\<root\>.Types@ for its type
  references.

Slice qualifiers are filtered to legal Haskell module components.
Functions that match no legal qualifier (or only match
illegal ones, like @__anonymous_type0@) land in the synthetic root
group with qualifier @""@.
-}
module FFI.Slicing
  ( EmitGroup (..)
  , RouteOptions (..)
  , routeEverything
  , groupTotal
  , typesQualifier
  ) where

import Data.List (maximumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DearBindings.Catalog (Catalog (..))
import DearBindings.JSON
  ( Define (..)
  , Enum_ (..)
  , Function (..)
  , Struct (..)
  , Typedef (..)
  )
import DearBindings.JSON.Types qualified
import DearBindings.Slice (functionInSlice)
import FFI.Module (isLegalModuleComponent)

data EmitGroup = EmitGroup
  { qualifier :: Text
  -- ^ Empty for the synthetic root group; otherwise a legal Haskell
  -- module component.
  , structs :: [Struct]
  , enums :: [Enum_]
  , functions :: [Function]
  , defines :: [Define]
  , typedefs :: [Typedef]
  }
  deriving (Eq, Show)

-- | Total number of entities in a group.
groupTotal :: EmitGroup -> Int
groupTotal g =
  length g.structs
    + length g.enums
    + length g.functions
    + length g.defines
    + length g.typedefs

{- | Tunables for 'routeEverything'.
-}
data RouteOptions = RouteOptions
  { implMode :: Bool
  -- ^ When true, the generator is producing an impl package:
  -- forward-declared structs are dropped from the local @typesGroup@
  -- (they live in the external core package's Types module). A
  -- typedef whose name shadows a forward-declared struct is dropped
  -- too, since the core's full struct definition supersedes both.
  }
  deriving (Eq, Show)

{- | Route every catalog entity into an 'EmitGroup'. Guarantees:

* Every relevant entity appears in exactly one returned group.
  In core mode (@implMode = False@), every entity of every category
  is routed (callers can sanity-check via @sum (map groupTotal …)
  == Catalog.size@). In impl mode, forward-declared structs and
  shadowing typedefs are intentionally dropped — the coverage check
  has to subtract those.
* Each non-root group's @qualifier@ is a legal Haskell module
  component.
* Empty groups are dropped.
-}
routeEverything :: RouteOptions -> Catalog -> [EmitGroup]
routeEverything ropts c =
  let
    legalQuals = Set.toList (legalQualifiers c)

    -- Functions partition into per-slice groups; types all collapse
    -- into one shared module so cross-slice references resolve.
    routedFunctions = routeBy functionInSlice legalQuals (Map.toAscList c.functions)

    fnQualifiers = Set.toAscList (Set.fromList (map fst routedFunctions))

    fnGroupAt q =
      EmitGroup
        { qualifier = q
        , structs = []
        , enums = []
        , functions = pickAll routedFunctions q
        , defines = []
        , typedefs = []
        }

    -- In impl mode, forward-declared structs (e.g. ImDrawData in
    -- the impl JSON) are placeholders for types defined in the
    -- core package. Drop them to avoid redeclaring the same
    -- @data X@ name. dear_bindings also emits typedef restatements
    -- of core typedefs (e.g. @typedef ImDrawIdx@ inside opengl3) —
    -- treat all impl-side typedefs as redundant for v0.1. (If a
    -- future impl needs a uniquely local typedef, switch to a
    -- name-driven filter loaded from the external JSON.)
    keepStruct s = not (ropts.implMode && s.forwardDeclaration)
    keepTypedef _ = not ropts.implMode

    typesGroup =
      EmitGroup
        { qualifier = typesQualifier
        , structs = filter keepStruct (Map.elems c.structs)
        , enums = Map.elems c.enums
        , functions = []
        , defines = Map.elems c.defines
        , typedefs = filter keepTypedef (Map.elems c.typedefs)
        }
  in
    filter (\g -> groupTotal g > 0) (typesGroup : map fnGroupAt fnQualifiers)

-- | The qualifier (and module-component) used for the shared types
-- module. Slice modules import @\<root\>.\<typesQualifier\>@ to see
-- everything declared here.
typesQualifier :: Text
typesQualifier = "Types"

-- ---------------------------------------------------------------------------
-- Internal

{- | The set of qualifiers we'll consider as candidate slice keys —
every struct name plus every function-name qualifier (the same
sources 'DearBindings.Slice.slices' uses), filtered down to those
that are also legal Haskell module components. Anonymous types
(@__anonymous_type0@) drop out here.
-}
legalQualifiers :: Catalog -> Set.Set Text
legalQualifiers c =
  Set.filter isLegalModuleComponent $
    Set.fromList (Map.keys c.structs)
      <> Set.fromList (mapMaybe splitFirst (Map.keys c.functions))
  where
    splitFirst :: Text -> Maybe Text
    splitFirst n = case Text.breakOnEnd "_" n of
      ("", _) -> Nothing
      (_, "") -> Nothing
      (q, _) -> Just (Text.dropEnd 1 q)

{- | Assign each entity to its longest matching qualifier (or the empty
qualifier — i.e. root — if no candidate matches).
-}
routeBy
  :: (Text -> Text -> Bool)
  -- ^ inSlice predicate (qualifier -> name -> Bool)
  -> [Text]
  -- ^ candidate qualifiers (already filtered to legal ones)
  -> [(Text, a)]
  -- ^ entities (name + value)
  -> [(Text, a)]
  -- ^ pairs of (chosen qualifier, value), with "" meaning root
routeBy p quals = map assign
  where
    assign (n, v) = (chooseQualifier p quals n, v)

chooseQualifier :: (Text -> Text -> Bool) -> [Text] -> Text -> Text
chooseQualifier p quals n =
  case filter (\q -> p q n) quals of
    [] -> ""
    matches -> maximumBy (comparing Text.length) matches

pickAll :: [(Text, a)] -> Text -> [a]
pickAll xs target = [v | (q, v) <- xs, q == target]
