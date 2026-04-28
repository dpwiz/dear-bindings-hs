
{-| Translate a slice qualifier into a Haskell module name and the
on-disk path of the corresponding @.hsc@ file. The synthetic root
slice (qualifier @""@) maps to the module-root itself; every other
qualifier becomes @\<root\>.\<qualifier\>@.
-}
module FFI.Module
  ( qualifierToModule
  , qualifierToPath
  , isLegalModuleComponent
  ) where

import Data.Char (isAlphaNum, isUpper)
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath (joinPath, (</>), (<.>))

-- | @qualifierToModule root qualifier@ produces the dotted Haskell module name.
qualifierToModule :: Text -> Text -> Text
qualifierToModule root q
  | Text.null q = root
  | otherwise = root <> "." <> q

{- | @qualifierToPath outDir root qualifier@ yields the @.hsc@ file path
inside @outDir@ for the given qualifier. Dots in the module name
become path separators.
-}
qualifierToPath :: FilePath -> Text -> Text -> FilePath
qualifierToPath outDir root q =
  let m = qualifierToModule root q
      parts = map Text.unpack (Text.splitOn "." m)
  in outDir </> joinPath parts <.> "hsc"

{- | A Haskell module-name component must start with an uppercase letter
and contain only @[A-Za-z0-9']@ thereafter. C-side qualifiers
extracted by 'splitQualifier' are normally fine (they come from
identifiers like @ImGui@, @ImDrawList@); the failure mode is
anonymous structs (@__anonymous_type0@) which the FFI generator
must route into the root module instead.
-}
isLegalModuleComponent :: Text -> Bool
isLegalModuleComponent c = case Text.uncons c of
  Nothing -> False
  Just (h, t) -> isUpper h && Text.all isComponentChar t
  where
    isComponentChar ch = isAlphaNum ch || ch == '\''
