{-# LANGUAGE OverloadedStrings #-}

module Posts (postRules, PostMode(..), PostListBound(..), postListContext) where

import Hakyll 
import Compiler
import Text.Read (readMaybe)

data PostMode = Draft | Publish
data PostListBound = BoundedBy Int | Unbounded

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


postListContext :: PostListBound -> Context b

postListContext Unbounded =
  listField "posts" postCtx posts
  where 
    posts = recentFirst =<< loadAll "posts/*"

postListContext (BoundedBy n) =
  listField "posts" postCtx postsBoundedByN
  where 
    posts = recentFirst =<< loadAll "posts/*"
    postsBoundedByN = (return . (take n)) =<< posts


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