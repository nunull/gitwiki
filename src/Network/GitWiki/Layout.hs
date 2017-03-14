{-# LANGUAGE OverloadedStrings #-}
module Network.GitWiki.Layout where

import qualified Clay as C
import           Data.List
import           Data.String.Utils
import           Data.Time
import           Data.Version
import           Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Markdown

import Network.GitWiki.Css
import Network.GitWiki.Persistence
import Network.GitWiki.Types

tabi :: Integer -> Attribute
tabi = A.tabindex . stringValue . show

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
          H.a "Home" ! A.href "/"
          H.a "Users" ! A.href "/users"
          H.a "Trash" ! A.href "/trash"
        H.nav ! A.class_ "nav-right" $ do
          H.form ! A.method "GET" ! A.action "/search" $ do
            H.input ! tabi 100 ! A.type_ "search" ! A.name "q" ! A.placeholder "Search"
          mapM_ navLink nav
      H.div ! A.class_ "content" $ do
        H.div ! A.class_ "sidebar" $ do
          H.a "Add Page" ! A.href "/p/add" ! A.class_ "button"
          H.ul ! A.class_ "blank-list" $ mapM_ (H.li . pageLink) pageNames
        H.main page
      H.footer $ do
        H.nav $ do
          H.strong $ toHtml $ name user
          H.span $ do
            H.span " <"
            H.a ! A.href (stringValue ("mailto:" ++ email user)) $ toHtml $ email user
            H.span ">"
          H.span " version "
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
    H.ul ! A.class_ "blank-list" $ mapM_ (H.li . historyItemView) history

logItemView :: Revision -> Html
logItemView revision = H.span "logitem"

addPageView :: Html
addPageView = do
  H.h1 "Add Page"
  (H.form ! A.method (stringValue "POST") ! A.action "/p/add") $ do
    H.input ! tabi 30 ! A.type_ "text" ! A.name "page" ! A.placeholder "Slug"
    H.br
    H.textarea ! tabi 31 ! A.name "content" $ ""
    H.br
    H.button ! tabi 32 ! A.type_ "submit" $ "Save"

editPageView :: Config -> Page -> IO Html
editPageView config page = return $ do
  H.h1 $ toHtml $ "Edit " ++ slug page
  (H.form ! A.method (stringValue "POST") ! A.action (stringValue $ "/p/" ++ slug page ++ "/edit")) $ do
    H.textarea ! tabi 30 ! A.name "content" $ toHtml $ content page
    H.br
    H.button ! tabi 31 ! A.type_ "submit" $ "Save"

removePageView :: Config -> Page -> IO Html
removePageView config page = return $ do
  H.h1 $ toHtml $ "Remove " ++ slug page
  H.p $ H.strong $ toHtml ("Are you sure you want to remove " ++ slug page ++ "?")
  (H.form ! A.method (stringValue "POST") ! A.action (stringValue $ "/p/" ++ slug page ++ "/remove")) $ do
    H.button ! A.type_ "submit" $ "OK"

pageHistoryView :: Config -> Page -> IO Html
pageHistoryView config page = do
  revisions <- readHistory config $ slug page
  return $ do
    H.h1 $ toHtml $ "History of " ++ slug page
    H.ul ! A.class_ "blank-list" $ mapM_ (H.li . historyItemView) revisions

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
    H.a "View"
      ! A.href (stringValue $ if not $ isLatest rev
        then "/p/" ++ pageSlug rev ++ "/history/" ++ (shorthash $ hash rev)
        else "/p/" ++ pageSlug rev
      )
    if not $ isLatest rev
      then H.a "Revert" ! A.href (stringValue $ "/p/" ++ pageSlug rev ++ "/history/" ++ hash rev ++ "/revert")
      else ""
  diffView $ diff rev

diffView :: String -> Html
diffView diff = H.div ! A.class_ "diff" $ do
  mapM_ diffLineView $ drop 6 $ lines diff
  where
    lineDiv c line    = H.div ! A.class_ c $ toHtml $ strip $ drop 1 $ line
    diffLineView line = if startswith "+"  line then lineDiv "diff-line-added" line
                   else if startswith "-"  line then lineDiv "diff-line-removed" line
                   else if startswith "\\" line then lineDiv "diff-line-meta" line
                                                else lineDiv "diff-line" line

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
      H.button ! A.type_ "submit" $ "OK"

trashView :: [String] -> Html
trashView deletedPages = do
  H.h1 "Trash"
  H.ul ! A.class_ "blank-list" $ mapM_ (H.li . deletedPageView) deletedPages

deletedPageView :: String -> Html
deletedPageView name = do
      H.h2 $ toHtml name
      H.nav $ H.form
        ! A.method (stringValue "POST")
        ! A.action (stringValue $ "/p/" ++ name ++ "/restore")
        $ H.button "Restore" ! A.type_ "submit"

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
        H.a "Edit" ! A.class_ "button" ! A.href (stringValue $ "/users/" ++ email u ++ "/edit")
        H.a "Delete" ! A.href (stringValue $ "/users/" ++ email u ++ "/delete")

addUserView :: Html
addUserView = do
  H.h1 "Add User"
  (H.form ! A.method (stringValue "POST") ! A.action "/users/add") $ do
    H.input ! tabi 30 ! A.type_ "text" ! A.name "name" ! A.placeholder "Name"
    H.br
    H.input ! tabi 31 ! A.type_ "email" ! A.name "email" ! A.placeholder "Email"
    H.br
    H.input ! tabi 32 ! A.type_ "password" ! A.name "password" ! A.placeholder "Password"
    H.br
    H.div $ do
      H.label "Admin: "
      H.input ! tabi 33 ! A.type_ "checkbox" ! A.name "admin"
    H.br
    H.button ! tabi 34 ! A.type_ "submit" $ "Save"

editUserView :: String -> Maybe User -> Html
editUserView email_ user = do
  H.h1 "Edit User"
  case user of
    Nothing -> H.strong $ toHtml $ "User with email " ++ email_ ++ " not found."
    Just u  -> H.form
      ! A.method (stringValue "POST")
      ! A.action (stringValue $ "/users/" ++ email u ++ "/edit")
      $ do
        H.h1 $ toHtml $ email u
        H.input ! tabi 30 ! A.type_ "text" ! A.name "name" ! A.placeholder "Name" ! (A.value $ stringValue $ name u)
        H.br
        H.input ! tabi 31 ! A.type_ "password" ! A.name "password" ! A.placeholder "Password"
        H.br
        H.div $ do
          H.label "Admin: "
          H.input ! tabi 32 ! A.type_ "checkbox" ! A.name "admin"
        H.br
        H.button ! tabi 33 ! A.type_ "submit" $ "Save"

deleteUserView :: String -> Maybe User -> Html
deleteUserView email_ user = do
  H.h1 "Delete User"
  case user of
    Nothing -> H.strong $ toHtml $ "User with email " ++ email_ ++ " not found."
    Just u  -> do
      H.p $ H.strong $ toHtml ("Are you sure you want to remove " ++ email u ++ "?")
      H.form
        ! A.method (stringValue "POST")
        ! A.action (stringValue $ "/users/" ++ email u ++ "/delete")
        $ do
          H.button ! A.type_ "submit" $ "OK"

searchResultsView :: String -> [(String, String, String)] -> Html
searchResultsView query results = do
  H.h1 $ toHtml ("Search Results for \"" ++ query ++ "\"")
  H.ul ! A.class_ "blank-list" $ mapM_ (H.li . searchResultItemView) results

searchResultItemView :: (String, String, String) -> Html
searchResultItemView (title, excerpt, link) = do
  H.h2 $ toHtml title
  H.pre $ toHtml excerpt
  H.nav $ H.a "View" ! A.href (stringValue link)

showDate :: UTCTime -> String
showDate d = formatTime defaultTimeLocale "%F %T" d

shorthash :: String -> String
shorthash = take 6
