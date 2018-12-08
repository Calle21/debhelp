module Main where

import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as C
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Process (callProcess)

type Dictionary = [([C.ByteString],[C.ByteString])]

main :: IO ()
main = do args <- map C.pack `fmap` getArgs
          if C.pack "update" `elem` args
          then callProcess "/usr/local/bin/debhelp-update" []
          else return ()
          if C.pack "help" `elem` args
          then C.putStrLn (C.pack "Search 'help' to get help")
          else return ()
          homeDir <- getHomeDirectory
          loop =<< (order . C.lines) `fmap` C.readFile (homeDir </> ".debhelp.text")
  where
  order :: [C.ByteString] -> Dictionary
  order []     = []
  order (x:xs) = let (_:tags)   = C.split ' ' x
                     (help,xs') = break isTagLine xs
                 in (tags,help) : order xs'
    where
    isTagLine :: C.ByteString -> Bool
    isTagLine s = C.take 2 s == C.pack "# "
  loop help = do C.putStr (C.pack ">")
                 tagline <- C.getLine
                 if C.null tagline then return ()
                 else do let result = search (C.words tagline) help
                         if null result then C.putStrLn (C.pack "Nothing found")
                                        else putHelp result
                         loop help
    where
    search :: [C.ByteString] -> Dictionary -> Dictionary
    search _    []     = []
    search tags (x:xs) = (if all (`elem` fst x) tags then [x] else []) ++ search tags xs
    putHelp :: Dictionary -> IO ()
    putHelp []     = return ()
    putHelp (x:xs) = do putChar '#'
                        putTags (fst x)
                        mapM_ C.putStrLn (snd x)
                        putHelp xs
      where
      putTags []     = putChar '\n'
      putTags (x:xs) = putChar ' ' >> C.putStr x >> putTags xs

