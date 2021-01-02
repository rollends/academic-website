--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.Map.Strict (insert)
import Data.Text (pack)
import Text.CSL.Pandoc (processCites')
import Text.Pandoc
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--- ENTRY POINT FOR SITE COMPILATION
---
main :: IO ()
main = hakyll $ do
  --- BEGIN Post Compilation
  ---

  --- When the Post has a Bibliography, have to do some extra work.
  matchMetadata "posts/*.tex" laTeXPostHasReferences $ do
    route $ setExtension "html"
    compile laTeXPostWithBibCompiler

  -- Otherwise it's quite simple.
  matchMetadata "posts/*.tex" (not . laTeXPostHasReferences) $ do
    route $ setExtension "html"
    compile laTeXPostCompiler

  ---
  --- END Post Compilation

  --- BEGIN Static Content
  ---   these are simply copied over (CSS is compressed)
  ---
  match "images/**" $ do
      route   idRoute
      compile copyFileCompiler

  match "scripts/*" $ do
      route   idRoute
      compile copyFileCompiler

  match "css/*" $ do
      route   idRoute
      compile compressCssCompiler
  ---
  --- END Static Content

  --- BEGIN One off Files
  ---
  match (fromList ["about.tex", "contactme.md"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let
        archiveCtx =
          listField "posts" postCtx (return posts) <>
          constField "title" "Archive"             <>
          defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route   $ setExtension "html"
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx = listField "posts" postCtx (return posts) <> defaultContext

      getResourceBody
        >>= applyAsTemplate archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "publications.html" $ do
    route   $ idRoute
    compile $ do
      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
  ---
  --- END One off Files
  
  --- Template Compilation
  match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
--------------------------------------------------------------------------------
loadLaTeXPostBibliography :: String -> Item Pandoc -> Compiler (IO Pandoc)
loadLaTeXPostBibliography filepath (Item id (Pandoc meta docBody)) =
  return $ processCites' $ Pandoc newMetadata docBody
  where
    newMetadata =
      Meta $
        insert "csl" (MetaString $ pack "ieee.csl") $
        insert "bibliography" (MetaString $ pack filepath) $
        unMeta meta

laTeXPostHasReferences :: Metadata -> Bool
laTeXPostHasReferences meta =
  case lookupString "bib" meta of
    Nothing   -> False  -- No Bibliography Referenced by LaTeX Post.
    Just _    -> True   -- Bibliography referenced.

laTeXPostWithBibCompiler :: Compiler (Item String)
laTeXPostWithBibCompiler = do
  bibfilepath <- getUnderlying >>= (\x -> getMetadataField' x "bib")
  identifier <- getUnderlying
  getResourceBody
    >>= readPandoc
    >>= loadLaTeXPostBibliography bibfilepath
    >>= unsafeCompiler
    >>= \document -> return (writePandocWith laTeXWriterOptions (Item identifier document))
    >>= loadAndApplyTemplate "templates/post.html" postCtx 
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= relativizeUrls

laTeXPostCompiler :: Compiler (Item String)
laTeXPostCompiler =
  pandocCompilerWith defaultHakyllReaderOptions laTeXWriterOptions
    >>= loadAndApplyTemplate "templates/post.html" postCtx
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= relativizeUrls

--- The Messy Pandoc Writer Options for reading LaTeX Docs.
laTeXWriterOptions =
  WriterOptions{
    writerTemplate         = writerTemplate defaultHakyllWriterOptions
  , writerVariables        = writerVariables defaultHakyllWriterOptions
  , writerTabStop          = writerTabStop defaultHakyllWriterOptions
  , writerTableOfContents  = writerTableOfContents defaultHakyllWriterOptions
  , writerIncremental      = writerIncremental defaultHakyllWriterOptions
  , writerHTMLMathMethod   = MathJax ""
  , writerNumberSections   = writerNumberSections defaultHakyllWriterOptions
  , writerNumberOffset     = writerNumberOffset defaultHakyllWriterOptions
  , writerSectionDivs      = writerSectionDivs defaultHakyllWriterOptions
  , writerExtensions       = writerExtensions defaultHakyllWriterOptions
  , writerReferenceLinks   = writerReferenceLinks defaultHakyllWriterOptions
  , writerDpi              = writerDpi defaultHakyllWriterOptions
  , writerWrapText         = writerWrapText defaultHakyllWriterOptions
  , writerColumns          = writerColumns defaultHakyllWriterOptions
  , writerEmailObfuscation = writerEmailObfuscation defaultHakyllWriterOptions
  , writerIdentifierPrefix = writerIdentifierPrefix defaultHakyllWriterOptions
  , writerCiteMethod       = Citeproc
  , writerHtmlQTags        = writerHtmlQTags defaultHakyllWriterOptions
  , writerSlideLevel       = writerSlideLevel defaultHakyllWriterOptions
  , writerTopLevelDivision = writerTopLevelDivision defaultHakyllWriterOptions
  , writerListings         = writerListings defaultHakyllWriterOptions
  , writerHighlightStyle   = writerHighlightStyle defaultHakyllWriterOptions
  , writerSetextHeaders    = writerSetextHeaders defaultHakyllWriterOptions
  , writerEpubSubdirectory = writerEpubSubdirectory defaultHakyllWriterOptions
  , writerEpubMetadata     = writerEpubMetadata defaultHakyllWriterOptions
  , writerEpubFonts        = writerEpubFonts defaultHakyllWriterOptions
  , writerEpubChapterLevel = writerEpubChapterLevel defaultHakyllWriterOptions
  , writerTOCDepth         = writerTOCDepth defaultHakyllWriterOptions
  , writerReferenceDoc     = writerReferenceDoc defaultHakyllWriterOptions
  , writerReferenceLocation= writerReferenceLocation defaultHakyllWriterOptions
  , writerSyntaxMap        = writerSyntaxMap defaultHakyllWriterOptions
  , writerPreferAscii      = writerPreferAscii defaultHakyllWriterOptions
  }