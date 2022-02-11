module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery, entryDecoder
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude
import Entry.DB (SnippetDB, empty)
import qualified Data.ByteString.Char8 as DB

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  DB.save empty  
  return ()
            

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  db <- DB.load
  case db of
    Error err  -> putStrLn "Failed to load DB"
    Success dab -> let
                  entry = DB.findFirst (\en -> entryId en == getOptId getOpts) <$> db
                  in
                    case entry of
                      Success ans -> case ans of
                                        Just anAnswer -> putStrLn $ entrySnippet anAnswer
                                        _ -> putStrLn "Faild to find"
                      _ -> putStrLn "Faild to find"

showAllEntries :: TestableMonadIO m => [Entry] ->m()
showAllEntries ls = 
  case ls of
    [] -> return ()
    x:xs -> putStrLn (show(FmtEntry x)) >> showAllEntries xs
-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  db <- DB.load
  case db of
    Error err  -> putStrLn "Failed to load DB"
    Success dab -> let
                  entry = DB.findAll (\en -> Entry.Entry.matchedByAllQueries (searchOptTerms searchOpts) en) <$> db
                  in
                    case entry of
                      Success ans -> case ans of
                                        [] -> putStrLn "No entries found"
                                        _ -> showAllEntries ans
                      _ -> putStrLn "No entries found"

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts =do
  db <- DB.load
  case db of
    Error err  -> putStrLn "Failed to load DB"
    Success dab -> putStrLn "Entry with this content already exists: "
  return ()
  where
    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
