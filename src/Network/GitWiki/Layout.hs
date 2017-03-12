{-# LANGUAGE OverloadedStrings #-}
module Network.GitWiki.Layout where

import qualified Clay as C
import           Data.List
import           Data.Time
import           Data.Version
import           Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Markdown

import Network.GitWiki.Css
import Network.GitWiki.Persistence
import Network.GitWiki.Types

skeleton :: Config -> User -> [(String, String)] -> Html -> IO Html
skeleton config user nav page = do
  pageNames <- readPages config
  return $ H.docTypeHtml $ do
    H.head $ do
      H.style $ toHtml $ C.render css
      H.title "gitwiki"
    H.body $ do
      H.header $ do
        H.nav ! A.class_ "nav-left" $ do
          H.a "add page" ! A.href "/p/add" ! A.class_ "button"
          H.a "home" ! A.href "/"
          H.a "users" ! A.href "/users"
          H.a "trash" ! A.href "/trash"
        H.nav ! A.class_ "nav-right" $ mapM_ navLink nav
      H.div ! A.class_ "content" $ do
        H.div ! A.class_ "sidebar" $ do
          H.ul $ mapM_ (H.li . pageLink) pageNames
        H.main page
      H.footer $ do
        H.nav $ do
          H.strong $ toHtml $ name user
          H.span $ do
            H.span " <"
            H.a ! A.href (stringValue ("mailto:" ++ email user)) $ toHtml $ email user
            H.span ">"
          H.span "version"
          H.span $ toHtml $ showVersion $ version config

pageLink :: String -> Html
pageLink pageName = H.a ! A.href (stringValue $ "/p/" ++ pageName) $ toHtml pageName

navLink :: (String, String) -> Html
navLink (name, href) = H.a ! A.href (stringValue href) $ toHtml name

indexView :: Config -> IO Html
indexView config = do
  pageNames <- readPages config
  histories <- mapM (readHistory config) pageNames
  let history = sortBy (\a b -> compare (date b) (date a)) $ concat histories
  return $ do
    H.h1 "History"
    H.ul $ mapM_ (H.li . historyItemView) history

logItemView :: Revision -> Html
logItemView revision = H.span "logitem"

editPageView :: Config -> Page -> IO Html
editPageView config page = return $ do
  H.h1 $ toHtml $ "Edit " ++ slug page
  (H.form ! A.method (stringValue "POST") ! A.action (stringValue $ "/p/" ++ slug page ++ "/edit")) $ do
    H.textarea ! A.name "content" $ toHtml $ content page
    H.br
    H.button ! A.type_ "submit" $ "save"

removePageView :: Config -> Page -> IO Html
removePageView config page = return $ do
  H.h1 $ toHtml $ "Remove " ++ slug page
  H.p $ H.strong $ toHtml ("Are you sure you want to remove " ++ slug page ++ "?")
  (H.form ! A.method (stringValue "POST") ! A.action (stringValue $ "/p/" ++ slug page ++ "/remove")) $ do
    H.button ! A.type_ "submit" $ "ok"

pageHistoryView :: Config -> Page -> IO Html
pageHistoryView config page = do
  revisions <- readHistory config $ slug page
  return $ do
    H.h1 $ toHtml $ "History of " ++ slug page
    H.ul $ mapM_ (H.li . historyItemView) revisions

historyItemView :: Revision -> Html
historyItemView rev = do
  H.h2 $ toHtml (pageSlug rev ++ "@" ++ (shorthash $ hash rev) ++ (if isLatest rev then " (latest)" else ""))
  H.div $ do
    H.strong $ toHtml $ authorName rev
    H.span $ do
      H.span " <"
      H.a ! A.href (stringValue ("mailto:" ++ authorEmail rev)) $ toHtml $ authorEmail rev
      H.span ">"
  H.div $ H.time $ toHtml $ showDate $ date rev
  H.nav $ do
    H.a "view"
      ! A.href (stringValue $ if not $ isLatest rev
        then "/p/" ++ pageSlug rev ++ "/history/" ++ (shorthash $ hash rev)
        else "/p/" ++ pageSlug rev
      )
    if not $ isLatest rev
      then H.a "revert" ! A.href (stringValue $ "/p/" ++ pageSlug rev ++ "/history/" ++ hash rev ++ "/revert")
      else ""

revisionView :: Config -> Page -> IO Html
revisionView config page = return $ do
  H.h1 $ toHtml $ "Revision " ++ slug page ++ "@" ++ (shorthash $ revision page)
  markdown def $ content page

revisionRevertView :: Config -> Page -> IO Html
revisionRevertView config page = return $ do
  H.h1 $ toHtml $ "Revert " ++ slug page
  H.p $ H.strong
    $ toHtml ("Are you sure you want to revert " ++ slug page ++ " to " ++ (shorthash $ revision page) ++ "?")
  H.form
    ! A.method (stringValue "POST")
    ! A.action (stringValue $ "/p/" ++ slug page ++ "/history/" ++ revision page ++ "/revert")
    $ do
      H.button ! A.type_ "submit" $ "ok"

userView :: User -> Html
userView u = do
  H.h1 $ toHtml $ name u
  H.div $ do
    H.strong $ toHtml $ name u
    H.span $ do
      H.span " <"
      H.a ! A.href (stringValue ("mailto:" ++ email u)) $ toHtml $ email u
      H.span "> "
      H.span (if admin u then "(Admin)" else "")
      H.nav $ do
        H.a "edit" ! A.href (stringValue $ "/users/" ++ email u ++ "/edit")
        H.a "delete" ! A.href (stringValue $ "/users/" ++ email u ++ "/delete")

showDate :: UTCTime -> String
showDate d = formatTime defaultTimeLocale "%F %T" d

shorthash :: String -> String
shorthash = take 6
