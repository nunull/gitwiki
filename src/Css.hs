{-# LANGUAGE OverloadedStrings #-}
module Css where

import Clay
import Data.Monoid

css :: Css
css = do
  body ? do
    display flex
    fontFamily [] [sansSerif]
    margin zero zero zero zero
  header ? do
    display flex
    flexDirection column
    width sidebarWidth
    paddingLeft smallLength
    ul ? do
      flexGrow 1
  nav ? do
    marginTop smallLength
    marginBottom smallLength
    padding smallLength smallLength smallLength smallLength
    background lightGrey
  (nav <> Clay.div) |> (star) ? do
    marginRight smallLength
  main_ ? do
    width $ pct 100
    maxWidth $ pageWidth
  textarea ? do
    minHeight $ px 500
  "input[type=text]" <> "input[type=email]" <> "input[type=password]" <> textarea ? do
    width $ pct 100
    boxSizing borderBox
    border solid (px 1) grey
    marginBottom smallLength
    padding smallLength smallLength smallLength smallLength
  ":focus" ? do
    outline solid (px 3) darkBlue
  ul ? do
    listStyleType none
    paddingLeft zero
  where
    zero         = px 0
    smallLength  = px 10
    pageWidth    = px 700
    sidebarWidth = px 300
    darkBlue     = rgb 53 112 206
    lightGrey    = rgb 220 220 220
