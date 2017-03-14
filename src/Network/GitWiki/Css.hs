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
    smallPadding
    background darkBlue
    color white
    ":hover" & do
      background $ darken 0.1 darkBlue
  h1 <> h2 ?  fontWeight normal
  pre ? do
    smallPadding
    borderRadius tinyLength tinyLength tinyLength tinyLength
    background lightGrey
  "input[type=text]" <> "input[type=email]" <> "input[type=password]" <> textarea ? do
    width $ pct 100
    boxSizing borderBox
    border solid (px 0) transparent
    marginBottom smallLength
    smallPadding
    background lightGrey
    monoFont
  blockquote ? do
    borderLeft solid (px 3) lightGrey
    marginLeft zero
    paddingLeft smallLength
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
    fontSize (px 15)
    lineHeight (em 1.4)
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
      ":first-child" ? paddingLeft zero
  footer ? nav ? do
    smallPadding
    background white
  ".content" ? do
    display flex
    flexGrow 1
    ".sidebar" ? do
      boxSizing borderBox
      display flex
      flexDirection column
      width sidebarWidth
      paddingLeft smallLength
      paddingRight smallLength
      ul ?  flexGrow 1
    main_ ?  flexGrow 1
  nav <> ".diff" ? do
    boxSizing borderBox
    marginTop smallLength
    marginBottom smallLength
    background lightGrey
  nav |> a ? do
    display inlineBlock
    smallPadding
    paddingRight zero
  ".diff" ? smallPadding
  -- (nav <> Clay.div) |> (star) ?  marginRight smallLength
  main_ ? do
    width $ pct 100
    maxWidth $ pageWidth
  ".diff" ? do
    monoFont
    ".diff-line" ? do
      ":before" & do
        "content" -: "''"
        display inlineBlock
        width smallLength
        opacity 0.3
    ".diff-line-added" ? do
      color green
      ":before" & do
        "content" -: "'+'"
        display inlineBlock
        width smallLength
        opacity 0.3
    ".diff-line-removed" ? do
      color red
      ":before" & do
        "content" -: "'-'"
        display inlineBlock
        width smallLength
        opacity 0.3
    ".diff-line-meta" ? do
      color grey
      ":before" & do
        "content" -: "''"
        display inlineBlock
        width smallLength
        opacity 0.3
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
    smallPadding = padding smallLength smallLength smallLength smallLength
