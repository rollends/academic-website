module Navigation (ActivePage(..), navBarContext) where

import Hakyll
 
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
