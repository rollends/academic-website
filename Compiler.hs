{-# LANGUAGE OverloadedStrings #-}

module Compiler (defaultCompiler, postCompiler, laTeXPostCompiler, laTeXPostHasReferences, laTeXPostWithBibCompiler, laTeXWriterOptions, postCtx) where

import Hakyll
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256(..))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Base64 as B64
import Data.Map.Strict (insert)
import Data.Text (pack)
import Text.Pandoc.Citeproc (processCitations)
import Text.Pandoc

import Navigation

--------------------------------------------------------------------------------

defaultCompiler :: Context String -> ActivePage -> Item String -> Compiler (Item String)
defaultCompiler ctx page item =
  do
    bootstrapIntegrityHash <- computeIntegrityHashForItem "css/bootstrap.css"
    siteIntegrityHash <- computeIntegrityHashForItem "css/rollends.ca.css"
    loadAndApplyTemplate ("templates/default.html") (context bootstrapIntegrityHash siteIntegrityHash) item
      >>= relativizeUrls
  where
    context bHash sHash = 
      constField "BootstrapStyleIntegrityHash" bHash
        <> constField "SiteStyleIntegrityHash" sHash
        <> navBarContext page 
        <> ctx

--------------------------------------------------------------------------------
computeIntegrityHashForItem :: Identifier -> Compiler String
computeIntegrityHashForItem itemId = 
  do
    cssBody <- loadBody itemId
    return $ BU.toString $ B64.encode $ BS.pack $ BA.unpack $ hashWith SHA256 (BU.fromString cssBody)

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  navBarContext OtherPage <>
  defaultContext

postCompiler :: Item String -> Compiler (Item String)
postCompiler item =
    loadAndApplyTemplate ("templates/post.html") postCtx (fmap demoteHeaders item)
    >>= defaultCompiler postCtx OtherPage

--------------------------------------------------------------------------------

loadLaTeXPostBibliography :: String -> Item Pandoc -> Compiler (IO Pandoc)
loadLaTeXPostBibliography filepath (Item id (Pandoc meta docBody)) =
  return $ runIOorExplode $ processCitations $ Pandoc newMetadata docBody
  where
    newMetadata =
      Meta $
        insert ("csl") (MetaString $ pack "ieee.csl") $
        insert ("bibliography") (MetaString $ pack filepath) $
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
  , writerTOCDepth         = writerTOCDepth defaultHakyllWriterOptions
  , writerReferenceDoc     = writerReferenceDoc defaultHakyllWriterOptions
  , writerReferenceLocation= writerReferenceLocation defaultHakyllWriterOptions
  , writerSyntaxMap        = writerSyntaxMap defaultHakyllWriterOptions
  , writerPreferAscii      = False
  , writerFigureCaptionPosition = CaptionBelow
  , writerTableCaptionPosition  = CaptionBelow
  , writerListOfFigures    = False
  , writerListOfTables     = False
  , writerListTables       = False
  , writerEpubTitlePage    = False
  , writerSplitLevel       = writerSplitLevel defaultHakyllWriterOptions
  , writerChunkTemplate    = writerChunkTemplate defaultHakyllWriterOptions
  , writerLinkImages       = False
  }
