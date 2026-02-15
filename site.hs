--------------------------------------------------------------------------------
--- EXTERNAL IMPORTS
---
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import System.Environment as Environment
---
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--- LOCAL IMPORTS
import Compiler
import Navigation
import Posts
---
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--- ENTRY POINT FOR SITE COMPILATION
---
main :: IO ()
main = do
  isInDraft <- draftMode
  hakyll $ do
    postRules isInDraft
    staticRules 

    --- BEGIN One off Files
    ---
    match "about.tex" $ do
      route   $ setExtension "html"
      compile $ pandocCompilerWith defaultHakyllReaderOptions laTeXWriterOptions
        >>= defaultCompiler defaultContext AboutMyWorkPage

    match "contactme.md" $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= defaultCompiler defaultContext ContactMePage

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let
          archiveCtx =
            listField "posts" postCtx (return posts)  <>
            constField "title" "Archive"              <>
            defaultContext

        makeItem "archive.html"
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= defaultCompiler archiveCtx ArchivePage

    match "index.html" $
      let
        fourRecentPosts = (return . (take 4)) =<< recentFirst =<< loadAll "posts/*"
        archiveCtx      = navBarContext HomePage <>
                          listField "posts" postCtx fourRecentPosts <>
                          defaultContext
      in do
        route   $ setExtension "html"
        compile $
          getResourceBody
            >>= applyAsTemplate archiveCtx
            >>= defaultCompiler archiveCtx HomePage

    match "publications.html" $ do
      route   $ idRoute
      compile $ do
        getResourceBody
          >>= defaultCompiler defaultContext PublicationsPage
    ---
    --- END One off Files
    
    --- Template Compilation
    match "templates/*" $ compile templateBodyCompiler

staticRules :: Rules ()
staticRules =
  do
    match "images/**" $ do
      route   idRoute
      compile copyFileCompiler

    match "webroot/**" $ do
      route   $ gsubRoute "webroot/" (const "")
      compile $ copyFileCompiler

    match "scripts/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler


draftMode :: IO PostMode
draftMode = do
  env <- Environment.lookupEnv "HAKYLL_DRAFT_MODE"
  return $ 
    case env of
      Just _ -> Draft
      Nothing -> Publish
