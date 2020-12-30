--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Core.Compiler.Internal (compilerThrow)
import           Data.Text (unpack, append, empty, singleton)
import           Text.Pandoc

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "scripts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler


    match (fromList ["about.tex", "contactme.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/*.tex" $ do
        route $ setExtension "html"
        compile laTeXPostCompiler

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archive"             `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.tex" $ do
      route   $ setExtension "html"
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx = listField "posts" postCtx (return posts) `mappend` defaultContext

        getResourceBody
          >>= applyAsTemplate archiveCtx
          >>= renderPandoc
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------
laTeXPostCompiler :: Compiler (Item String)
laTeXPostCompiler =
  let
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
      , writerCiteMethod       = writerCiteMethod defaultHakyllWriterOptions
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
  in do
    pandocCompilerWith defaultHakyllReaderOptions laTeXWriterOptions
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls
