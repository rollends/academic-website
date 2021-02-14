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

  match "webroot/**" $ do
      route   $ gsubRoute "webroot/" (const "")
      compile $ copyFileCompiler

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

      makeItem ""
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

--------------------------------------------------------------------------------

defaultCompiler :: Context String -> ActivePage -> Item String -> Compiler (Item String)
defaultCompiler ctx page item =
  loadAndApplyTemplate "templates/default.html" context item
    >>= relativizeUrls
  where
    context = navBarContext page <> ctx

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  navBarContext OtherPage <>
  defaultContext

postCompiler :: Item String -> Compiler (Item String)
postCompiler item =
  loadAndApplyTemplate "templates/post.html" postCtx item
    >>= defaultCompiler postCtx OtherPage

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
    >>= postCompiler

laTeXPostCompiler :: Compiler (Item String)
laTeXPostCompiler =
  pandocCompilerWith defaultHakyllReaderOptions laTeXWriterOptions
    >>= postCompiler

--- The Messy Pandoc Writer Options for reading LaTeX Docs.
laTeXWriterOptions =
  WriterOptions{
    writerTemplate         = writerTemplate defaultHakyllWriterOptions
  , writerVariables        = writerVariables defaultHakyllWriterOptions
  , writerTabStop          = writerTabStop defaultHakyllWriterOptions
  , writerTableOfContents  = writerTableOfContents defaultHakyllWriterOptions
  , writerIncremental      = writerIncremental defaultHakyllWriterOptions
  , writerHTMLMathMethod   = MathJax defaultMathJaxURL
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
  , writerPreferAscii      = False
  }

--------------------------------------------------------------------------------
--- NAVBAR DATATYPES
---   Which navbar buttons are active is dependent on the page we are on.
---   These data types try to easily capture that information.
---
data ActivePage =
    OtherPage
  | HomePage
  | PublicationsPage
  | AboutMyWorkPage
  | ArchivePage
  | ContactMePage

navBarContext :: ActivePage -> Context a
navBarContext OtherPage =
  constField "LinkHomeProperties" classnavlink <>
  constField "LinkPublicationsProperties" classnavlink <>
  constField "LinkAboutProperties" classnavlink <>
  constField "LinkArchiveProperties" classnavlink <>
  constField "LinkContactProperties" classnavlink <>
  constField "IconHomeStyle" style <>
  constField "IconPublicationsStyle" style <>
  constField "IconAboutStyle" style <>
  constField "IconArchiveStyle" style <>
  constField "IconContactStyle" style
  where
    classnavlink = "class=\"nav-link\""
    style = ""

navBarContext HomePage =
  navBarActivePageSetting "LinkHomeProperties" "IconHomeStyle" <>
  navBarContext OtherPage
navBarContext PublicationsPage =
  navBarActivePageSetting "LinkPublicationsProperties" "IconPublicationsStyle" <> 
  navBarContext OtherPage
navBarContext AboutMyWorkPage =
  navBarActivePageSetting "LinkAboutProperties" "IconAboutStyle" <> 
  navBarContext OtherPage
navBarContext ArchivePage =
  navBarActivePageSetting "LinkArchiveProperties" "IconArchiveStyle" <> 
  navBarContext OtherPage
navBarContext ContactMePage =
  navBarActivePageSetting "LinkContactProperties" "IconContactStyle" <> 
  navBarContext OtherPage 

navBarActivePageSetting :: String -> String -> Context a
navBarActivePageSetting v1 v2 =
  constField v1 "class=\"nav-link active\" aria-current=\"page\"" <>
  constField v2 "filter: brightness(2);"

---
--------------------------------------------------------------------------------