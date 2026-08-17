{-# LANGUAGE OverloadedStrings #-}

import Compiler
import Hakyll
import Navigation
import Posts
import System.Environment as Environment

main :: IO ()
main = do
  isInDraft <- draftMode
  hakyll $ do
    postRules isInDraft
    staticRules
    match "contactme.md" $ markdownRules ContactMePage
    match "publications.html" $ htmlRules PublicationsPage
    match "templates/*" $ compile templateBodyCompiler

    create ["archive.html"] $
      let archiveCtx = constField "title" "Archive" <> postListContext Unbounded <> defaultContext
          applyTemplate = loadAndApplyTemplate "templates/archive.html" archiveCtx
          andCompileAs = defaultCompiler archiveCtx
       in do
            route idRoute
            compile $
              makeItem "archive.html"
                >>= applyTemplate
                >>= andCompileAs ArchivePage

    match "index.html" $
      let archiveCtx = postListContext (BoundedBy 4) <> defaultContext
          applyTemplate = applyAsTemplate archiveCtx
          andCompileAs = defaultCompiler archiveCtx
       in do
            route idRoute
            compile $
              getResourceBody
                >>= applyTemplate
                >>= andCompileAs HomePage

markdownRules :: ActivePage -> Rules ()
markdownRules page =
  do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= defaultCompiler defaultContext page

htmlRules :: ActivePage -> Rules ()
htmlRules page =
  do
    route idRoute
    compile $
      getResourceBody
        >>= defaultCompiler defaultContext page

staticRules :: Rules ()
staticRules =
  do
    match anImageForPublishing $ do
      route idRoute
      compile copyFileCompiler

    match "webroot/**" $ do
      route $ gsubRoute "webroot/" (const "")
      compile copyFileCompiler

    match "scripts/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" cssRuleset
    match "css/*" integrityHashRuleset

anImageForPublishing :: Pattern
anImageForPublishing =
  fromGlob "images/**" .&&. fromRegex ".*[.](png|svg|gif|jpg|jpeg|ico)"

draftMode :: IO PostMode
draftMode = do
  env <- Environment.lookupEnv "HAKYLL_DRAFT_MODE"
  return $
    case env of
      Just _ -> Draft
      Nothing -> Publish
