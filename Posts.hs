{-# LANGUAGE OverloadedStrings #-}

module Posts (postRules, PostMode(..)) where

import Hakyll 
import Compiler
import Text.Read (readMaybe)

data PostMode = Draft | Publish

postRules :: PostMode -> Rules ()
postRules Draft =
  do
    legacyPostRules

    match "posts/*.md" $ do
      route $ setExtension "html"
      compile laTeXPostCompiler

postRules Publish =
  do
    legacyPostRules

    matchMetadata "posts/*.md" postShouldPublish $ do
      route $ setExtension "html"
      compile laTeXPostCompiler


legacyPostRules :: Rules ()
legacyPostRules =
  do
    --- When the Post has a Bibliography, have to do some extra work.
    matchMetadata "posts/*.tex" laTeXPostHasReferences $ do
      route $ setExtension "html"
      compile laTeXPostWithBibCompiler

    -- Otherwise it's quite simple.
    matchMetadata "posts/*.tex" (not . laTeXPostHasReferences) $ do
      route $ setExtension "html"
      compile laTeXPostCompiler


postShouldPublish :: Metadata -> Bool
postShouldPublish metadata = not $ isDraft metadata


isDraft :: Metadata -> Bool
isDraft metadata =
  case lookupString "draft" metadata of
    Just value ->
      case readMaybe value of
        Just True -> True 
        _ -> False
    _ -> False 