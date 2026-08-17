{-# LANGUAGE OverloadedStrings #-}

module Compiler (defaultCompiler, postCompiler, laTeXPostCompiler, laTeXPostHasReferences, laTeXPostWithBibCompiler, laTeXWriterOptions, postCtx, cssRuleset, integrityHashRuleset) where

import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256 (..))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BU
import Data.Map.Strict (insert)
import qualified Data.Set
import Data.Text (pack)
import Data.Version as Version
import Hakyll
import Hakyll.Core.Compiler.Internal (compilerThrow)
import Navigation
import Text.Pandoc
import Text.Pandoc.Citeproc (processCitations)
import qualified Paths_rollen_academic_site as SitePaths

--------------------------------------------------------------------------------
defaultCompiler :: Context String -> ActivePage -> Item String -> Compiler (Item String)
defaultCompiler ctx page item =
  do
    bootstrapIntegrityHash <- loadBody . setVersion (Just "integrity_hash") $ fromFilePath "css/bootstrap.css"
    siteIntegrityHash <- loadBody . setVersion (Just "integrity_hash") $ fromFilePath "css/rollends.ca.css"
    loadAndApplyTemplate "templates/default.html" (context bootstrapIntegrityHash  siteIntegrityHash) item
      >>= relativizeUrls
  where
    context bHash sHash =
      constField "BootstrapStyleIntegrityHash" bHash
        <> constField "SiteStyleIntegrityHash" sHash
        <> navBarContext page
        <> ctx

--------------------------------------------------------------------------------
cssRuleset :: Rules ()
cssRuleset =
  do
    route $ gsubRoute "css/" (\x -> "css/" ++ siteVersionString ++ ".")
    compile cssCompiler

siteVersionString = "site." ++ Version.showVersion SitePaths.version

cssCompiler :: Compiler (Item String)
cssCompiler =
  do 
    item <- compressCssCompiler
    saveSnapshot "_final" item

integrityHashRuleset :: Rules ()
integrityHashRuleset =
  version "integrity_hash" $ do
    route $ setExtension "integrity_hash"
    compile $ do 
      path <- getResourceFilePath 
      compiledData <- loadBody . setVersion Nothing $ fromFilePath path
      hashedItem <- integrityHashOf compiledData
      saveSnapshot "_final" hashedItem

integrityHashOf :: String -> Compiler (Item String)
integrityHashOf body =
  let
    hashString = BU.toString . B64.encode . BS.pack . BA.unpack . hashWith SHA256
    hashCompiler = makeItem . hashString . BU.fromString
  in
    hashCompiler body
--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> navBarContext OtherPage
    <> defaultContext

postCompiler :: Item String -> Compiler (Item String)
postCompiler item =
  loadAndApplyTemplate "templates/post.html" postCtx (fmap demoteHeaders item)
    >>= defaultCompiler postCtx OtherPage

--------------------------------------------------------------------------------

loadLaTeXPostBibliography :: String -> Item Pandoc -> Compiler (IO Pandoc)
loadLaTeXPostBibliography filepath (Item id (Pandoc meta docBody)) =
  return $ runIOorExplode $ processCitations $ Pandoc newMetadata docBody
  where
    newMetadata =
      Meta $
        insert "csl" (MetaString $ pack "ieee.csl") $
          insert "bibliography" (MetaString $ pack filepath) $
            unMeta meta

laTeXPostHasReferences :: Metadata -> Bool
laTeXPostHasReferences meta =
  case lookupString "bib" meta of
    Nothing -> False -- No Bibliography Referenced by LaTeX Post.
    Just _ -> True -- Bibliography referenced.

laTeXPostWithBibCompiler :: Compiler (Item String)
laTeXPostWithBibCompiler = do
  bibfilepath <- getUnderlying >>= (`getMetadataField'` "bib")
  identifier <- getUnderlying
  getResourceBody
    >>= readPandoc
    >>= loadLaTeXPostBibliography bibfilepath
    >>= unsafeCompiler
    >>= \document ->
      postCompiler (writePandocWith laTeXWriterOptions (Item identifier document))

laTeXPostCompiler :: Compiler (Item String)
laTeXPostCompiler =
  pandocCompilerWith defaultHakyllReaderOptions laTeXWriterOptions
    >>= postCompiler

--- The Messy Pandoc Writer Options for reading LaTeX Docs.
laTeXWriterOptions =
  WriterOptions
    { writerTemplate = writerTemplate defaultHakyllWriterOptions,
      writerVariables = writerVariables defaultHakyllWriterOptions,
      writerTabStop = writerTabStop defaultHakyllWriterOptions,
      writerTableOfContents = writerTableOfContents defaultHakyllWriterOptions,
      writerIncremental = writerIncremental defaultHakyllWriterOptions,
      writerHTMLMathMethod = MathJax defaultMathJaxURL,
      writerNumberSections = writerNumberSections defaultHakyllWriterOptions,
      writerNumberOffset = writerNumberOffset defaultHakyllWriterOptions,
      writerSectionDivs = writerSectionDivs defaultHakyllWriterOptions,
      writerExtensions = writerExtensions defaultHakyllWriterOptions,
      writerReferenceLinks = writerReferenceLinks defaultHakyllWriterOptions,
      writerDpi = writerDpi defaultHakyllWriterOptions,
      writerWrapText = writerWrapText defaultHakyllWriterOptions,
      writerColumns = writerColumns defaultHakyllWriterOptions,
      writerEmailObfuscation = writerEmailObfuscation defaultHakyllWriterOptions,
      writerIdentifierPrefix = writerIdentifierPrefix defaultHakyllWriterOptions,
      writerCiteMethod = Citeproc,
      writerHtmlQTags = writerHtmlQTags defaultHakyllWriterOptions,
      writerSlideLevel = writerSlideLevel defaultHakyllWriterOptions,
      writerTopLevelDivision = writerTopLevelDivision defaultHakyllWriterOptions,
      writerListings = writerListings defaultHakyllWriterOptions,
      writerHighlightStyle = writerHighlightStyle defaultHakyllWriterOptions,
      writerSetextHeaders = writerSetextHeaders defaultHakyllWriterOptions,
      writerEpubSubdirectory = writerEpubSubdirectory defaultHakyllWriterOptions,
      writerEpubMetadata = writerEpubMetadata defaultHakyllWriterOptions,
      writerEpubFonts = writerEpubFonts defaultHakyllWriterOptions,
      writerTOCDepth = writerTOCDepth defaultHakyllWriterOptions,
      writerReferenceDoc = writerReferenceDoc defaultHakyllWriterOptions,
      writerReferenceLocation = writerReferenceLocation defaultHakyllWriterOptions,
      writerSyntaxMap = writerSyntaxMap defaultHakyllWriterOptions,
      writerPreferAscii = False,
      writerFigureCaptionPosition = CaptionBelow,
      writerTableCaptionPosition = CaptionBelow,
      writerListOfFigures = False,
      writerListOfTables = False,
      writerListTables = False,
      writerEpubTitlePage = False,
      writerSplitLevel = writerSplitLevel defaultHakyllWriterOptions,
      writerChunkTemplate = writerChunkTemplate defaultHakyllWriterOptions,
      writerLinkImages = False
    }
