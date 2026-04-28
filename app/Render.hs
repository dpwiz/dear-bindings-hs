{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| One renderer per record type. Each function takes one schema entity
and produces a 'Blocks' fragment with:

  * a level-1 header carrying a stable anchor id
    (@\<category\>-\<name\>@)
  * a body shaped to that record's most useful fields
  * a one-line source footer at the bottom

These functions know nothing about files, format selection, or
linking. The wrapping into a full pandoc document is done by
"Generate" (one entity per file) and "Query" (concatenated to
stdout). That separation is the whole point of this module — anyone
forking this codebase to drive their own bindings generator should
be able to pluck any one of these renderers out and adapt it to
their target language.
-}
module Render
  ( renderDefine
  , renderEnum
  , renderTypedef
  , renderStruct
  , renderFunction
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import DearBindings.JSON
  ( Argument (..)
  , Comments (..)
  , Conditional (..)
  , Define (..)
  , EnumElement (..)
  , Enum_ (..)
  , Function (..)
  , Struct (..)
  , StructField (..)
  , TypeRef (..)
  , Typedef (..)
  )
import Render.Common
  ( Category (..)
  , LinkContext
  , anchor
  , commentBlocks
  , linkifyDecl
  , sourceFooter
  )
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Builder qualified as B

-- ---------------------------------------------------------------------------
-- Define

renderDefine :: LinkContext -> Define -> Blocks
renderDefine _ctx d =
  mconcat
    [ B.headerWith (anchor Defines d.name) 1 (B.code d.name)
    , commentBlocks d.comments
    , maybe mempty (B.codeBlock) d.content
    , conditionalBlocks d.conditionals
    , sourceFooter d.sourceLocation
    ]

-- ---------------------------------------------------------------------------
-- Enum

renderEnum :: LinkContext -> Enum_ -> Blocks
renderEnum ctx e =
  mconcat
    [ B.headerWith (anchor Enums e.name) 1 (B.code e.name)
    , commentBlocks e.comments
    , flagsLine
    , storageLine
    , elementsTable e.elements
    , conditionalBlocks e.conditionals
    , sourceFooter e.sourceLocation
    ]
  where
    flagsLine
      | e.isFlagsEnum = B.para (B.emph (B.text "Flags enum"))
      | otherwise = mempty
    storageLine = case e.storageType of
      Just t -> B.para $ B.text "storage type: " <> linkifyDecl ctx t.declaration
      Nothing -> mempty

elementsTable :: [EnumElement] -> Blocks
elementsTable [] = mempty
elementsTable xs =
  B.simpleTable
    [B.plain (B.text "Name"), B.plain (B.text "Value"), B.plain (B.text "Expression"), B.plain (B.text "Comment")]
    (map row xs)
  where
    row :: EnumElement -> [Blocks]
    row el =
      [ B.plain (B.code el.name)
      , B.plain (B.str (Text.pack (show el.value)))
      , B.plain (maybe mempty B.code el.valueExpression)
      , B.plain (commentInline el.comments)
      ]

-- ---------------------------------------------------------------------------
-- Typedef

renderTypedef :: LinkContext -> Typedef -> Blocks
renderTypedef _ctx t =
  mconcat
    [ B.headerWith (anchor Typedefs t.name) 1 (B.code t.name)
    , commentBlocks t.comments
    , B.codeBlock t.type_.declaration
    , conditionalBlocks t.conditionals
    , sourceFooter t.sourceLocation
    ]

-- ---------------------------------------------------------------------------
-- Struct

renderStruct :: LinkContext -> Struct -> Blocks
renderStruct ctx s =
  mconcat
    [ B.headerWith (anchor Structs s.name) 1 $
        B.text s.kind <> B.space <> B.code s.name
    , commentBlocks s.comments
    , flagsLine
    , fieldsList ctx s.fields
    , conditionalBlocks s.conditionals
    , sourceFooter s.sourceLocation
    ]
  where
    flagsLine
      | s.forwardDeclaration = B.para (B.emph (B.text "Forward declaration"))
      | otherwise = mempty

fieldsList :: LinkContext -> [StructField] -> Blocks
fieldsList _ [] = mempty
fieldsList ctx fields = B.definitionList (map describe fields)
  where
    describe :: StructField -> (B.Inlines, [Blocks])
    describe f =
      ( B.code (f.name <> bitfield f) <> B.text " : " <> linkifyDecl ctx f.type_.declaration
      , filter
          (/= mempty)
          [ commentBlocks f.comments
          , maybe mempty (\b -> B.para (B.text "array bounds: " <> B.code b)) f.arrayBounds
          , maybe mempty (\dv -> B.para (B.text "default: " <> B.code dv)) f.defaultValue
          ]
      )
    bitfield :: StructField -> Text
    bitfield f = maybe "" (\w -> " : " <> Text.pack (show w)) f.width

-- ---------------------------------------------------------------------------
-- Function

renderFunction :: LinkContext -> Function -> Blocks
renderFunction ctx f =
  mconcat
    [ B.headerWith (anchor Functions f.name) 1 (B.code f.name)
    , commentBlocks f.comments
    , B.codeBlock (signature f)
    , argList ctx f.arguments
    , helperBadges f
    , conditionalBlocks f.conditionals
    , sourceFooter f.sourceLocation
    ]

-- | Reconstruct a one-line C signature from the parts.
signature :: Function -> Text
signature f =
  Text.concat
    [ f.returnType.declaration
    , " "
    , f.name
    , "("
    , Text.intercalate ", " (map argDecl f.arguments)
    , ");"
    ]
  where
    argDecl :: Argument -> Text
    argDecl a
      | a.isVarargs = "..."
      | otherwise =
          let
            decl = maybe "" (.declaration) a.type_
            nm = fromMaybe "" a.name
          in
            case (Text.null decl, Text.null nm) of
              (True, True) -> ""
              (True, False) -> nm
              (False, True) -> decl
              (False, False) -> decl <> " " <> nm

argList :: LinkContext -> [Argument] -> Blocks
argList _ [] = mempty
argList ctx args = B.definitionList (map describe (filter (not . isVarargs) args))
  where
    isVarargs :: Argument -> Bool
    isVarargs a = a.isVarargs
    describe :: Argument -> (B.Inlines, [Blocks])
    describe a =
      ( B.code (fromMaybe "(unnamed)" a.name)
          <> B.text " : "
          <> maybe (B.code "...") (linkifyDecl ctx . (.declaration)) a.type_
      , filter
          (/= mempty)
          [ instanceLine a
          , maybe mempty (\dv -> B.para (B.text "default: " <> B.code dv)) a.defaultValue
          , maybe mempty (\b -> B.para (B.text "array bounds: " <> B.code b)) a.arrayBounds
          ]
      )
    instanceLine :: Argument -> Blocks
    instanceLine a
      | a.isInstancePointer = B.para (B.emph (B.text "instance pointer (self)"))
      | otherwise = mempty

helperBadges :: Function -> Blocks
helperBadges f
  | null badges = mempty
  | otherwise = B.para . B.emph . B.text $ Text.intercalate ", " badges
  where
    badges =
      map fst $
        filter
          snd
          [ ("imstr helper", f.isImstrHelper)
          , ("manual helper", f.isManualHelper)
          , ("unformatted helper", f.isUnformattedHelper)
          , ("default-argument helper", f.isDefaultArgumentHelper)
          , ("has imstr helper", f.hasImstrHelper)
          , ("static", f.isStatic)
          ]

-- ---------------------------------------------------------------------------
-- Shared bits

conditionalBlocks :: Maybe [Conditional] -> Blocks
conditionalBlocks Nothing = mempty
conditionalBlocks (Just []) = mempty
conditionalBlocks (Just cs) =
  B.para (B.emph (B.text "Conditionals:"))
    <> B.bulletList (map item cs)
  where
    item :: Conditional -> Blocks
    item c = B.plain $ B.code (c.condition <> " " <> c.expression)

commentInline :: Maybe Comments -> B.Inlines
commentInline Nothing = mempty
commentInline (Just c) = case (c.preceding, c.attached) of
  (Just xs, _) -> B.text (Text.intercalate " " (map Text.strip xs))
  (Nothing, Just a) -> B.text a
  _ -> mempty
