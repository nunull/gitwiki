{-# LANGUAGE OverloadedStrings #-}
module Network.GitWiki.Persistence ( readPages
                                   , readPage
                                   , readHistory
                                   , readDeletedPageHash
                                   , readTrash
                                   , readUsers
                                   , writeAndCommit
                                   , writeUsers
                                   , removeAndCommit
                                   , runCommand
                                   , hashPassword
                                   ) where

import           Control.Monad
import qualified Data.ByteString.Lazy.UTF8 as BS
import           Data.Digest.Pure.SHA
import           Data.List
import           Data.List.Split
import           Data.String.Utils
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified Data.Text.IO as TIO
import           Data.Time
import           Data.Time.Format
import           System.Directory
import           System.FilePath.Posix
import qualified System.Process as P

import Network.GitWiki.Types

runCommand :: String -> String -> [String] -> IO String
runCommand cwd cmd args = P.readCreateProcess (P.proc cmd args) { P.cwd = Just cwd } ""

readUsers :: Config -> IO [User]
readUsers config = do
  users <- TIO.readFile $ wikiDir config ++ "/users"
  return $ parseUsers $ lines $ TS.unpack users

writeUsers :: Config -> [User] -> IO ()
writeUsers config users = TIO.writeFile (wikiDir config ++ "/users") $ TS.pack users' where
  users' = unlines $ map user users
  user (User name email password admin) = name ++ ","
                                       ++ email ++ ","
                                       ++ password ++ ","
                                       ++ show admin

parseUsers :: [String] -> [User]
parseUsers [] = []
parseUsers (ln:tl) = User { name = name_
                          , email = email_
                          , password = password_
                          , admin = admin_
                          } : parseUsers tl
  where
    parts     = splitOn "," ln
    name_     = parts !! 0
    email_    = parts !! 1
    password_ = parts !! 2
    admin_    = parts !! 3 == "True"

pagePath :: Config -> String -> String
pagePath config name = wikiDir config ++ "/data/" ++ name ++ ".md"

readPages :: Config -> IO [String]
readPages config = do
  pageFilenames <- listDirectory $ wikiDir config ++ "/data/"
  return $ filter (/= "") $ filter (not . startswith ".") $ map takeBaseName pageFilenames

readPage :: Config -> String -> String -> IO Page
readPage config slug revision = do
  let cwd = wikiDir config ++ "/data"
  content <- runCommand cwd "git" ["show", revision ++ ":" ++ slug ++ ".md"]
  return $ Page { slug = slug, content = T.pack content, revision = revision }

readTrash :: Config -> IO [String]
readTrash config = do
  let cwd                = wikiDir config ++ "/data"
      doesPageNotExist p = doesFileExist (cwd ++ "/" ++ p) >>= (return . not)
  deletionLog           <- runCommand cwd "git" ["log", "--diff-filter=D", "--summary"]
  filenames             <- filterM doesPageNotExist
                         $ map (last . words)
                         $ filter (startswith " delete mode ")
                         $ lines deletionLog
  return $ nub $ map (replace ".md" "") filenames

writePage :: Config -> String -> T.Text -> IO ()
writePage config name = TIO.writeFile (pagePath config name) . TS.pack . T.unpack

removePage :: Config -> String -> IO ()
removePage config = removeFile . pagePath config

writeAndCommit :: Config -> User -> String -> T.Text -> IO ()
writeAndCommit config user slug content = do
  let cwd = wikiDir config ++ "/data"
  writePage config slug content
  _ <- runCommand cwd "git" ["config", "user.name", name user]
  _ <- runCommand cwd "git" ["config", "user.email", email user]
  _ <- runCommand cwd "git" ["add", slug ++ ".md"]
  _ <- runCommand cwd "git" ["commit", "-m", "Update"]
  return ()

removeAndCommit :: Config -> User -> String -> IO ()
removeAndCommit config user slug = do
  let cwd = wikiDir config ++ "/data"
  removePage config slug
  _ <- runCommand cwd "git" ["config", "user.name", name user]
  _ <- runCommand cwd "git" ["config", "user.email", email user]
  _ <- runCommand cwd "git" ["add", slug ++ ".md"]
  _ <- runCommand cwd "git" ["commit", "-m", "Update"]
  return ()

readHistory :: Config -> String -> IO [Revision]
readHistory config slug = do
  let cwd  = wikiDir config ++ "/data"
  result  <- runCommand cwd "git" ["log", "--", slug ++ ".md"]
  parseHistory config slug $ lines result

readDiff :: Config -> String -> String -> IO String
readDiff config slug hash = do
  let cwd = wikiDir config ++ "/data"
  runCommand cwd "git" ["diff", hash ++ "~1", hash, "--", slug ++ ".md"]

parseHistory :: Config -> String -> [String] -> IO [Revision]
parseHistory config slug lines = parseHistory' slug lines True where
  parseHistory' _ [] _                                     = return []
  parseHistory' slug (hashLn:authorLn:dateLn:tl) isLatest_ = do
    diff_       <- readDiff config slug hash_
    historyTail <- parseHistory' slug tl' False
    return $ Revision { hash = hash_
                      , authorName = authorName_
                      , authorEmail = authorEmail_
                      , date = date_
                      , message = message_
                      , pageSlug = slug
                      , isLatest = isLatest_
                      , diff = diff_
                      }
                      : historyTail
    where
      (messageLns, tl')       = span (not . startswith "commit ") tl
      hash_                   = replace "commit " "" hashLn
      message_                = unlines messageLns
      (authorName_:authorLn') = splitOn " <" $ strip $ replace "Author: " "" authorLn
      authorEmail_            = replace ">" "" $ intercalate " <" authorLn'
      dateStr                 = strip $ replace "Date: " "" dateLn
      date_                   = readTime defaultTimeLocale "%a %b %d %T %0Y %z" dateStr

readDeletedPageHash :: Config -> String -> IO String
readDeletedPageHash config name = do
  let cwd = wikiDir config ++ "/data"
  hash   <- runCommand cwd "git" ["rev-list", "-n", "1", "HEAD", "--", name ++ ".md"]
  return $ strip hash

hashPassword :: String -> String
hashPassword password = showDigest $ sha512 $ BS.fromString password
