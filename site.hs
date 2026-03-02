{-# LANGUAGE OverloadedStrings #-}

import Compiler
import Data.Version as Version
import Hakyll
import Navigation
import qualified Paths_rollen_academic_site as SitePaths
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

    match "css/*" $ do
      route $ gsubRoute "css/" (\x -> "css/" ++ siteVersionString ++ ".")
      compile compressCssCompiler

    match "css/*" integrityHashRuleset

anImageForPublishing :: Pattern
anImageForPublishing =
  fromGlob "images/**" .&&. fromRegex ".*[.](png|svg|gif|jpg|jpeg|ico)"

siteVersionString = "site." ++ Version.showVersion SitePaths.version

draftMode :: IO PostMode
draftMode = do
  env <- Environment.lookupEnv "HAKYLL_DRAFT_MODE"
  return $
    case env of
      Just _ -> Draft
      Nothing -> Publish
