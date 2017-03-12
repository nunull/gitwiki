{-# LANGUAGE OverloadedStrings #-}
module Network.GitWiki (run) where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe
import           Data.SecureMem
import qualified Data.Text.Lazy as T
import           Network.Wai.Middleware.HttpAuth
import qualified Paths_gitwiki as Package
import           System.Directory
import           System.Environment
import           System.Exit
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Markdown
import           Web.Scotty

import Network.GitWiki.Layout
import Network.GitWiki.Persistence
import Network.GitWiki.Types

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n x xs = a ++ (x:b) where (a, (_:b)) = splitAt n xs

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex n xs = a ++ b where (a, (_:b)) = splitAt n xs

prompt :: String -> IO String
prompt msg = do
  putStrLn msg
  getLine

setup :: Config -> IO ()
setup config = do
  let wikiDirPath    = wikiDir config
      usersFilePath  = wikiDirPath ++ "/users"
      dataDirPath    = wikiDirPath ++ "/data"
      dataGitDirPath = wikiDirPath ++ "/data/.git"

  wikiDirExists <- doesDirectoryExist wikiDirPath
  if not wikiDirExists then die $ wikiDirPath ++ " does not exists, exiting." else return ()

  usersFileExists <- doesFileExist usersFilePath
  if not usersFileExists
    then do
      userName     <- prompt "Enter username:"
      userEmail    <- prompt "Enter email:"
      userPassword <- prompt "Enter password:"
      let users = [ User { name = userName
                         , email = userEmail
                         , password = hashPassword userPassword
                         , admin = True
                         } ]
      writeUsers config users
    else return ()

  dataDirExists <- doesDirectoryExist dataDirPath
  if not dataDirExists then createDirectory dataDirPath else return ()

  dataGitDirExists <- doesDirectoryExist dataGitDirPath
  if not dataGitDirExists
    then do
      _ <- runCommand dataDirPath "git" ["init"]
      return ()
    else return ()

  return ()

authenticate :: Config -> BS.ByteString -> BS.ByteString -> IO Bool
authenticate config email_ password_ = do
  users <- readUsers config
  return $ isJust $ find findUser users
  where
    findUser u = emailMatches && passwordMatches where
      emailMatches    = email u == BS.unpack email_
      password1       = secureMemFromByteString $ BS.pack $ password u
      password2       = secureMemFromByteString $ BS.pack $ hashPassword $ BS.unpack password_
      passwordMatches = password1 == password2

extractUser config = do
  users      <- liftIO $ readUsers config
  authHeader <- header "Authorization"
  let (basicAuthEmail, _) = fromJust $ extractBasicAuth $ BS.pack $ T.unpack $ fromJust authHeader
      user = fromJust $ find (\u -> email u == BS.unpack basicAuthEmail) users
  return user

run :: IO ()
run = do
  args <- getArgs
  portEnv <- lookupEnv "PORT"

  let port     = maybe 3000 read portEnv
      wikiDir_ = if length args > 0 then last args else "./"
      config   = Config { wikiDir = wikiDir_, version = Package.version }

  setup config

  putStrLn $ "Directory: " ++ (wikiDir config)
  scotty port $ do
    let password = secureMemFromByteString ""
    middleware $ basicAuth (authenticate config) "auth"

    get "/" $ do
      user     <- extractUser config
      let nav   = [("add", "/p/add")]
      pageHtml <- liftIO $ indexView config
      p        <- liftIO $ skeleton config user nav pageHtml
      html $ renderHtml p

    get "/p/add" $ do
      user   <- extractUser config
      let nav = [("cancel", "/")]
      p <- liftIO $ skeleton config user nav $ do
        H.h1 "Add page"
        (H.form ! A.method (stringValue "POST") ! A.action "/p/add") $ do
          H.input ! A.type_ "text" ! A.name "page" ! A.placeholder "Slug"
          H.br
          H.textarea ! A.name "content" $ ""
          H.br
          H.button ! A.type_ "submit" $ "save"
      html $ renderHtml $ p

    post "/p/add" $ do
      user      <- extractUser config
      pageParam <- param "page"
      content   <- param "content"
      liftIO $ writeAndCommit config user pageParam content
      redirect $ T.pack $ "/p/" ++ pageParam

    get "/p/:page" $ do
      user      <- extractUser config
      pageParam <- param "page"
      page      <- liftIO $ readPage config pageParam "HEAD"
      let nav    = [ ("add", "/p/add")
                   , ("edit", "/p/" ++ pageParam ++ "/edit")
                   , ("remove", "/p/" ++ pageParam ++ "/remove")
                   , ("history", "/p/" ++ pageParam ++ "/history")
                   ]
      p         <- liftIO $ skeleton config user nav $ markdown def $ content page
      html $ renderHtml $ p

    get "/p/:page/edit" $ do
      user      <- extractUser config
      pageParam <- param "page"
      page      <- liftIO $ readPage config pageParam "HEAD"
      pageHtml  <- liftIO $ editPageView config page
      let nav    = [("cancel", "/p/" ++ pageParam)]
      p         <- liftIO $ skeleton config user nav pageHtml
      html $ renderHtml $ p

    post "/p/:page/edit" $ do
      user     <- extractUser config
      pageParam <- param "page"
      content   <- param "content"
      liftIO $ writeAndCommit config user pageParam content
      redirect $ T.pack $ "/p/" ++ pageParam

    get "/p/:page/remove" $ do
      user      <- extractUser config
      pageParam <- param "page"
      page      <- liftIO $ readPage config pageParam "HEAD"
      pageHtml  <- liftIO $ removePageView config page
      let nav        = [("cancel", "/p/" ++ pageParam)]
      p <- liftIO $ skeleton config user nav pageHtml
      html $ renderHtml $ p

    post "/p/:page/remove" $ do
      user     <- extractUser config
      pageParam <- param "page"
      liftIO $ removeAndCommit config user pageParam
      redirect $ T.pack "/"

    post "/p/:page/restore" $ do
      user      <- extractUser config
      pageParam <- param "page"
      lastHash  <- liftIO $ readDeletedPageHash config pageParam
      page      <- liftIO $ readPage config pageParam (lastHash ++ "^")
      liftIO $ writeAndCommit config user pageParam $ content page
      redirect $ T.pack $ "/p/" ++ pageParam

    get "/p/:page/history" $ do
      user      <- extractUser config
      pageParam <- param "page"
      page      <- liftIO $ readPage config pageParam "HEAD"
      pageHtml  <- liftIO $ pageHistoryView config page
      let nav    = [("cancel", "/p/" ++ pageParam)]
      p         <- liftIO $ skeleton config user nav pageHtml
      html $ renderHtml $ p

    get "/p/:page/history/:hash" $ do
      user      <- extractUser config
      pageParam <- param "page"
      hashParam <- param "hash"
      page      <- liftIO $ readPage config pageParam hashParam
      pageHtml  <- liftIO $ revisionView config page
      let nav    = [ ("cancel", "/p/" ++ pageParam ++ "/history")
                   , ("revert", "/p/" ++ pageParam ++ "/history/" ++ hashParam ++ "/revert")
                   ]
      p         <- liftIO $ skeleton config user nav pageHtml
      html $ renderHtml $ p

    get "/p/:page/history/:hash/revert" $ do
      user      <- extractUser config
      pageParam <- param "page"
      hashParam <- param "hash"
      page      <- liftIO $ readPage config pageParam hashParam
      pageHtml  <- liftIO $ revisionRevertView config page
      let nav    = [("cancel", "/p/" ++ pageParam ++ "/history/" ++ hashParam)]
      p         <- liftIO $ skeleton config user nav pageHtml
      html $ renderHtml $ p

    post "/p/:page/history/:hash/revert" $ do
      user      <- extractUser config
      pageParam <- param "page"
      hashParam <- param "hash"
      page      <- liftIO $ readPage config pageParam hashParam
      liftIO $ writeAndCommit config user pageParam $ content page
      redirect $ T.pack $ "/p/" ++ pageParam

    get "/trash" $ do
      user          <- extractUser config
      deletedPages  <- liftIO $ readTrash config
      let deletedPage name = do
            H.span $ toHtml name
            H.nav $ H.form
              ! A.method (stringValue "POST")
              ! A.action (stringValue $ "/p/" ++ name ++ "/restore")
              $ H.button "restore" ! A.type_ "submit"
          nav = []
      p <- liftIO $ skeleton config user nav $ do
        H.h1 "Trash"
        H.ul $ mapM_ (H.li . deletedPage) deletedPages
      html $ renderHtml $ p

    get "/users" $ do
      user   <- extractUser config
      users  <- liftIO $ readUsers config
      let nav = [("add", "/users/add")]
      p <- liftIO $ skeleton config user nav $ do
        H.h1 "Users"
        H.ul $ mapM_ (H.li . userView) users
      html $ renderHtml $ p

    get "/users/add/" $ do
      user   <- extractUser config
      let nav = [("cancel", "/users")]
      p <- liftIO $ skeleton config user nav $ do
        H.h1 "Add user"
        (H.form ! A.method (stringValue "POST") ! A.action "/users/add") $ do
          H.input ! A.type_ "text" ! A.name "name" ! A.placeholder "Name"
          H.br
          H.input ! A.type_ "email" ! A.name "email" ! A.placeholder "Email"
          H.br
          H.input ! A.type_ "password" ! A.name "password" ! A.placeholder "Password"
          H.br
          H.label "Admin: "
          H.input ! A.type_ "checkbox" ! A.name "admin"
          H.br
          H.button ! A.type_ "submit" $ "save"
      html $ renderHtml $ p

    post "/users/add" $ do
      nameParam     <- param "name"
      emailParam    <- param "email"
      passwordParam <- param "password"
      adminParam    <- param "admin" `rescue` (\msg -> return $ "")
      users         <- liftIO $ readUsers config
      let user       = User { name = nameParam
                            , email = emailParam
                            , password = hashPassword passwordParam
                            , admin = (adminParam :: String) /= ""
                            }
          users'     = users ++ [user]
      liftIO $ writeUsers config users'
      redirect $ T.pack $ "/users"

    get "/users/:email/edit" $ do
      user       <- extractUser config
      emailParam <- param "email"
      users      <- liftIO $ readUsers config
      let user_   = find (\u -> email u == emailParam) users
          nav     = [("cancel", "/users")]
      p <- liftIO $ skeleton config user nav $ do
        H.h1 "Edit user"
        case user_ of
          Nothing -> H.strong $ toHtml $ "User with email " ++ emailParam ++ " not found."
          Just u  -> H.form
            ! A.method (stringValue "POST")
            ! A.action (stringValue $ "/users/" ++ email u ++ "/edit")
            $ do
              H.h1 $ toHtml $ email u
              H.input ! A.type_ "text" ! A.name "name" ! A.placeholder "Name" ! (A.value $ stringValue $ name u)
              H.br
              H.input ! A.type_ "password" ! A.name "password" ! A.placeholder "Password"
              H.br
              H.label "Admin: "
              H.input ! A.type_ "checkbox" ! A.name "admin"
              H.br
              H.button ! A.type_ "submit" $ "save"
      html $ renderHtml $ p

    post "/users/:email/edit" $ do
      emailParam    <- param "email"
      nameParam     <- param "name"
      passwordParam <- param "password"
      adminParam    <- param "admin" `rescue` (\_ -> return $ "")
      users         <- liftIO $ readUsers config
      let oldUser    = find (\u -> email u == emailParam) users
          newUser    = User { name = nameParam
                            , email = emailParam
                            , password = hashPassword passwordParam
                            , admin = (adminParam :: String) /= ""
                            }
          users'     = case oldUser of
            Nothing -> users
            Just u  -> case elemIndex u users of
              Nothing -> users
              Just n  -> replaceAtIndex n newUser users
      liftIO $ writeUsers config users'
      redirect $ T.pack $ "/users"

    get "/users/:email/delete" $ do
      user       <- extractUser config
      emailParam <- param "email"
      users      <- liftIO $ readUsers config
      let user_   = find (\u -> email u == emailParam) users
          nav     = [("cancel", "/users")]
      p <- liftIO $ skeleton config user nav $ do
        H.h1 "Delete user"
        case user_ of
          Nothing -> H.strong $ toHtml $ "User with email " ++ emailParam ++ " not found."
          Just u  -> do
            H.p $ H.strong $ toHtml ("Are you sure you want to remove " ++ email u ++ "?")
            H.form
              ! A.method (stringValue "POST")
              ! A.action (stringValue $ "/users/" ++ email u ++ "/delete")
              $ do
                H.button ! A.type_ "submit" $ "ok"
      html $ renderHtml $ p

    post "/users/:email/delete" $ do
      emailParam    <- param "email"
      users         <- liftIO $ readUsers config
      let index      = findIndex (\u -> email u == emailParam) users
          users'     = case index of
                         Nothing -> users
                         Just n  -> removeAtIndex n users
      liftIO $ writeUsers config users'
      redirect $ T.pack $ "/users"
