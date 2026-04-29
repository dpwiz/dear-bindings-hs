{-| Generate by-value-struct shim wrappers.

@capi@ rejects user-defined 'Storable' types as foreign-call argument
or return types — even with @CTYPE@. To unlock functions that touch
whiteboxed structs by value, we synthesize a thin C wrapper for each
such function:

* by-value struct argument @T@ becomes @T const* p@; the wrapper
  derefs at the call to the original;
* by-value struct return @T@ is rewritten to @void@ + a trailing
  @T* _out@ output parameter; the wrapper writes @*_out = call(...)@.

Per wrapped function we emit three rendered fragments:

* a C @extern \"C\"@ definition (lands in a single @.cpp@);
* a C declaration (lands in a single @.h@);
* the Haskell side: a raw @foreign import@ with @Ptr@-typed args and
  no by-value return, plus a user-facing function with the
  natural Haskell signature that does the @with@\/@alloca@ dance
  and calls the raw import.

The @WrapperDef@ rendered here is accumulated by 'FFI.Run' and the
collected @cBody@\/@cDecl@ are written to a single
@DearImGuiWrappers.{cpp,h}@ pair next to the @.hsc@ tree.
-}
module FFI.Wrapper
  ( WrapperDef (..)
  , wrapperHeaderName
  , needsWrap
  , renderWrapper
  ) where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DearBindings.JSON
  ( Argument (..)
  , Function (..)
  , TypeKind (..)
  , TypeRef (..)
  )
import DearBindings.JSON.Types qualified
import FFI.HType (TypeAliasMap, renderArgType, renderReturnType)

{- | The header file name we write wrapper declarations into. Both
the @.cpp@ side (@\#include\@) and the Haskell @foreign import\@
header reference use this string.
-}
wrapperHeaderName :: Text
wrapperHeaderName = "DearImGuiWrappers.h"

{- | C symbol suffix appended to the original function name to form
the wrapper's symbol.
-}
wrapSuffix :: Text
wrapSuffix = "_wrap"

data WrapperDef = WrapperDef
  { cBody :: Text
  {- ^ The wrapper's C @extern \"C\"@ definition. Goes into the
  generated @.cpp@.
  -}
  , cDecl :: Text
  {- ^ The wrapper's C declaration line. Goes into the generated
  @.h@.
  -}
  , haskell :: Text
  {- ^ Haskell-side: a raw @foreign import\@ that calls the wrapper
  (using @Ptr T@ for what the wrapper made indirect), plus a
  user-facing function with the original Haskell-natural type
  that does the @with@\/@alloca@ dance.
  -}
  }
  deriving (Eq, Show)

{- | True iff the function's signature includes a by-value reference
to a whitebox-eligible struct in args or return position. These
functions get a generated C shim instead of a direct @foreign
import@.
-}
needsWrap :: Set Text -> Function -> Bool
needsWrap whitebox f =
  byValRef whitebox f.returnType.description
    || any (argByValRef whitebox) f.arguments

{- | Render the full wrapper bundle for one function. Pre-condition:
'needsWrap' is true for the function.
-}
renderWrapper :: TypeAliasMap -> Set Text -> Function -> WrapperDef
renderWrapper aliases whitebox f =
  let
    wrapName = f.name <> wrapSuffix
    hsRaw = "c_" <> lowerFirst f.name
    hsUser = lowerFirst f.name

    -- decide whether we rewrite the return into an out-parameter
    retByValName = userByValName whitebox f.returnType.description
    retIsByVal = isJust retByValName

    -- C-side signatures
    cArgsList = map renderCArg (zip [0 ..] f.arguments)
    cOutParam = case retByValName of
      Just t -> [t <> "* _out"]
      Nothing -> []
    cParamList = Text.intercalate ", " (cArgsList <> cOutParam)
    cRetDecl =
      if retIsByVal
        then "void"
        else f.returnType.declaration
    cCallArgs = Text.intercalate ", " (map cCallArg (zip [0 ..] f.arguments))
    cCallExpr = f.name <> "(" <> cCallArgs <> ")"
    cBodyStmt = case (retIsByVal, Text.strip cRetDecl) of
      (True, _) -> "*_out = " <> cCallExpr <> ";"
      (False, "void") -> cCallExpr <> ";"
      (False, _) -> "return " <> cCallExpr <> ";"
    cBody' =
      Text.unlines
        [ "extern \"C\" " <> cRetDecl <> " " <> wrapName <> "(" <> cParamList <> ") {"
        , "    " <> cBodyStmt
        , "}"
        ]
    cDecl' = cRetDecl <> " " <> wrapName <> "(" <> cParamList <> ");"

    -- Haskell-side: raw import (Ptr-shaped) and user-facing wrapper
    hsRawArgTypes = map (hsRawArgType aliases whitebox) f.arguments
    hsRawTrail = case retByValName of
      Just t -> ["Ptr " <> t]
      Nothing -> []
    hsRawRet =
      if retIsByVal
        then "IO ()"
        else "IO " <> paren (renderReturnType aliases f.returnType)
    hsRawType =
      Text.intercalate
        " -> "
        (map paren (hsRawArgTypes <> hsRawTrail) <> [hsRawRet])

    hsUserArgTypes = map (renderArgType aliases) f.arguments
    hsUserRet = "IO " <> paren (renderReturnType aliases f.returnType)
    hsUserType =
      Text.intercalate
        " -> "
        (map paren hsUserArgTypes <> [hsUserRet])

    argNames = [hsArgName i a | (i, a) <- zip [0 ..] f.arguments]
    -- For each by-value-whitebox arg, we need a `with` binding.
    -- Otherwise the arg passes through unchanged.
    bindings = collectBindings whitebox (zip argNames f.arguments)
    -- Wrapper rendering doesn't need to look at Wrapper's hsRawArgType
    -- callsite directly here; that helper now also takes the alias map.
    callArgs =
      [ case lookup an bindings of
          Just pName -> pName
          Nothing -> an
      | an <- argNames
      ]
    finalCall =
      if retIsByVal
        then
          "alloca $ \\_pOut -> "
            <> hsRaw
            <> " "
            <> Text.unwords (callArgs <> ["_pOut"])
            <> " >> peek _pOut"
        else hsRaw <> Text.concat [" " <> a | a <- callArgs]

    bodyExpr = wrapWithBinds bindings finalCall

    haskellText =
      Text.unlines
        [ "foreign import capi unsafe \""
            <> wrapperHeaderName
            <> " "
            <> wrapName
            <> "\""
        , "  " <> hsRaw <> " :: " <> hsRawType
        , ""
        , hsUser <> " :: " <> hsUserType
        , hsUser <> Text.concat [" " <> n | n <- argNames] <> " ="
        , "  " <> bodyExpr
        ]
  in
    WrapperDef
      { cBody = cBody'
      , cDecl = cDecl'
      , haskell = haskellText
      }
  where
    renderCArg :: (Int, Argument) -> Text
    renderCArg (i, a) =
      let nm = argCName i a
      in case a.type_ of
           Just tr -> case userByValName whitebox tr.description of
             Just t -> t <> " const* " <> nm
             Nothing
               -- Function-pointer args (and the like) already
               -- embed the parameter name in their declaration
               -- string, e.g. @float (*values_getter)(...)@.
               -- Splicing another name would produce invalid C.
               | "(*" `Text.isInfixOf` tr.declaration -> tr.declaration
               | otherwise -> tr.declaration <> " " <> nm
           Nothing -> "/* arg" <> Text.pack (show i) <> ": no type */"

    cCallArg :: (Int, Argument) -> Text
    cCallArg (i, a) =
      let nm = argCName i a
      in case a.type_ of
           Just tr -> case userByValName whitebox tr.description of
             Just _ -> "*" <> nm
             Nothing -> nm
           Nothing -> nm

    -- C identifier for an argument. Uses the JSON-supplied name if
    -- present, otherwise synthesises @arg\<n\>@.
    argCName :: Int -> Argument -> Text
    argCName i a = fromMaybe ("arg" <> Text.pack (show i)) a.name

{- | Haskell identifier for an argument. Same as the C name unless
it collides with a Haskell keyword or shadows a common Prelude
binding — in which case we suffix with @_@.
-}
hsArgName :: Int -> Argument -> Text
hsArgName i a =
  let n = fromMaybe ("arg" <> Text.pack (show i)) a.name
  in if n `Set.member` reservedHsNames
       then n <> "_"
       else n

{- | Names that a function-argument identifier can't safely be in
Haskell. Includes both reserved keywords (parse errors) and
common Prelude bindings whose shadowing GHC warns about.
-}
reservedHsNames :: Set Text
reservedHsNames =
  Set.fromList
    [ "case"
    , "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "forall"
    , "foreign"
    , "hiding"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "module"
    , "newtype"
    , "of"
    , "qualified"
    , "then"
    , "type"
    , "where"
    , -- Prelude bindings that imgui parameter names sometimes use.
      "id"
    , "map"
    , "head"
    , "tail"
    , "init"
    , "last"
    , "read"
    , "show"
    , "fst"
    , "snd"
    , "null"
    , "min"
    , "max"
    , "abs"
    , "log"
    , "lookup"
    , "fail"
    , "either"
    , "maybe"
    , "fromIntegral"
    , "fromMaybe"
    , "print"
    ]

isJust :: Maybe a -> Bool
isJust = \case
  Just _ -> True
  Nothing -> False

{- | Haskell type for the raw foreign import's argument. By-value
whitebox struct args become @Ptr T@; everything else uses the
normal 'renderArgType' rendering.
-}
hsRawArgType :: TypeAliasMap -> Set Text -> Argument -> Text
hsRawArgType aliases whitebox a = case a.type_ of
  Just tr -> case userByValName whitebox tr.description of
    Just t -> "Ptr " <> t
    Nothing -> renderArgType aliases a
  Nothing -> renderArgType aliases a

{- | If the type is a by-value whitebox @TKUser@ reference, return
the struct name. Walks through @TKType@ aliases.
-}
userByValName :: Set Text -> TypeKind -> Maybe Text
userByValName ws = \case
  TKUser n _ | n `Set.member` ws -> Just n
  TKType _ inner -> userByValName ws inner
  _ -> Nothing

byValRef :: Set Text -> TypeKind -> Bool
byValRef ws t = isJust (userByValName ws t)

argByValRef :: Set Text -> Argument -> Bool
argByValRef ws a = case a.type_ of
  Just tr -> byValRef ws tr.description
  Nothing -> False

{- | For each argument that is a by-value whitebox struct, allocate
a fresh local pointer name. Returns @[(argHsName, pName)]@.
-}
collectBindings :: Set Text -> [(Text, Argument)] -> [(Text, Text)]
collectBindings ws pairs =
  [ (an, "_p" <> Text.pack (show i))
  | (i, (an, a)) <- zip [0 :: Int ..] pairs
  , argByValRef ws a
  ]

-- | Wrap the inner expression in nested @with x $ \\p -> @ binders.
wrapWithBinds :: [(Text, Text)] -> Text -> Text
wrapWithBinds [] inner = inner
wrapWithBinds ((arg, p) : rest) inner =
  "with " <> arg <> " $ \\" <> p <> " -> " <> wrapWithBinds rest inner

{- | Lowercase the first character. Mirrors 'FFI.Emit.lowerFirst' so
this module stays self-contained.
-}
lowerFirst :: Text -> Text
lowerFirst t = case Text.uncons t of
  Just (c, rest) -> Text.cons (toLowerChar c) rest
  Nothing -> t
  where
    toLowerChar c =
      if c >= 'A' && c <= 'Z'
        then toEnum (fromEnum c + 32)
        else c

{- | Wrap a rendering in parens unless it's a single token. Used by
the Haskell type splicer to avoid producing @Ptr Foo Bar@.
-}
paren :: Text -> Text
paren t
  | Text.any (== ' ') t && not (Text.isPrefixOf "(" t && Text.isSuffixOf ")" t) =
      "(" <> t <> ")"
  | otherwise = t
