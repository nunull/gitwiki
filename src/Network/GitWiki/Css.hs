{-# LANGUAGE OverloadedStrings #-}
module Network.GitWiki.Css where

import Clay
import Data.Monoid

css :: Css
css = do
  a ? do
    textDecoration none
    color darkBlue
  ".button" <> button ? do
    cursor pointer
    border solid (px 0) transparent
    borderRadius tinyLength tinyLength tinyLength tinyLength
    padding smallLength smallLength smallLength smallLength
    background darkBlue
    color white
    ":hover" & do
      background $ darken 0.1 darkBlue
  h1 <> h2 ?  fontWeight normal
  pre ? do
    padding smallLength smallLength smallLength smallLength
    borderRadius tinyLength tinyLength tinyLength tinyLength
    background lightGrey
  "input[type=text]" <> "input[type=email]" <> "input[type=password]" <> textarea ? do
    width $ pct 100
    boxSizing borderBox
    border solid (px 0) transparent
    marginBottom smallLength
    padding smallLength smallLength smallLength smallLength
    background lightGrey
    monoFont
  textarea ? do
    minHeight (px 500)
    "resize" -: "vertical"
  ":not(a):focus" ? outline solid (px 3) darkBlue
  body ? do
    display flex
    minHeight (vh 100)
    flexDirection column
    margin zero zero zero zero
    fontFamily [] [sansSerif]
    fontSize (px 14)
  header ? do
    marginTop (px $ - 18)
    display flex
    nav ? do
      background darkGrey
      star ? color white
    ".nav-left" ? do
      width sidebarWidth
    ".nav-right" ? do
      flexGrow 1
  footer ? nav ? background white
  ".content" ? do
    display flex
    flexGrow 1
    ".sidebar" ? do
      display flex
      flexDirection column
      width sidebarWidth
      paddingLeft smallLength
      ul ?  flexGrow 1
    main_ ?  flexGrow 1
  nav <> ".diff" ? do
    marginTop smallLength
    marginBottom smallLength
    padding smallLength smallLength smallLength smallLength
    background lightGrey
  (nav <> Clay.div) |> (star) ?  marginRight smallLength
  main_ ? do
    width $ pct 100
    maxWidth $ pageWidth
  ".diff" ? do
    monoFont
    ".diff-line-added" ? do
      color green
      ":before" & do
        "content" -: "'+'"
        marginRight smallLength
        opacity 0.5
    ".diff-line-removed" ? do
      color red
      ":before" & do
        "content" -: "'-'"
        marginRight smallLength
        opacity 0.5
    ".diff-line-meta" ? do
      color grey
      ":before" & do
        "content" -: "''"
        marginRight smallLength
        opacity 0.5
  where
    zero         = px 0
    tinyLength   = px 2
    smallLength  = px 18
    pageWidth    = px 700
    sidebarWidth = px 300
    darkBlue     = rgb 53 112 206
    lightGrey    = rgb 230 230 230
    darkGrey     = rgb 40 40 40
    monoFont     = fontFamily ["Monaco"] [monospace]
