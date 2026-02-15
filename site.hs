{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.Version as Version
import System.Environment as Environment

import Compiler
import Navigation
import Posts

import Paths_rollen_academic_site (version)

main :: IO ()
main = do
  isInDraft <- draftMode
  hakyll $ do
    postRules isInDraft
    staticRules 

    --- BEGIN One off Files
    ---
    match "about.tex" $ do
      route $ setExtension "html"
      compile $ pandocCompilerWith defaultHakyllReaderOptions laTeXWriterOptions
        >>= defaultCompiler defaultContext AboutMyWorkPage

    match "contactme.md" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= defaultCompiler defaultContext ContactMePage

    create ["archive.html"] $ 
      let
        archiveCtx = constField "title" "Archive" <> postListContext Unbounded <> defaultContext
      in do
        route idRoute
        compile $
          makeItem "archive.html"
            >>= loadAndApplyTemplate "templates/archive.html" (archiveCtx)
            >>= defaultCompiler (archiveCtx) ArchivePage

    match "index.html" $
      let
        archiveCtx = postListContext (BoundedBy 4) <> defaultContext
      in do
        route idRoute
        compile $
          getResourceBody
            >>= applyAsTemplate (archiveCtx)
            >>= defaultCompiler (archiveCtx) HomePage

    match "publications.html" $ do
      route idRoute
      compile $
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
      route $ gsubRoute "css/" (\x -> "css/" ++ siteVersionString ++ ".")
      compile compressCssCompiler

siteVersionString = "site." ++ (Version.showVersion Paths_rollen_academic_site.version)

draftMode :: IO PostMode
draftMode = do
  env <- Environment.lookupEnv "HAKYLL_DRAFT_MODE"
  return $ 
    case env of
      Just _ -> Draft
      Nothing -> Publish
