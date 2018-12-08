module Main where

import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as C
import System.Environment (getArgs)

type Dictionary = [([C.ByteString],[C.ByteString])]

main :: IO ()
main = do args <- getArgs
          if C.pack "update" `elem` map C.pack args
          then undefined
          else return ()
          loop =<< (order . C.lines) `fmap` C.readFile "/usr/share/debhelp.text"
  where
  order :: [C.ByteString] -> Dictionary
  order []     = []
  order (x:xs) = let (_:tags)   = C.split ' ' x
                     (help,xs') = break isTagLine xs
                 in (tags,help) : order xs'
    where
    isTagLine :: C.ByteString -> Bool
    isTagLine s = C.take 2 s == C.pack "$ "
  loop help = do C.putStr (C.pack "Search: ")
                 tagline <- C.getLine
                 if tagline == C.pack "quit" then return ()
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
    putHelp (x:xs) = do C.putStr (C.pack "$ ")
                        putTags (fst x)
                        mapM_ C.putStrLn (snd x)
                        putHelp xs
      where
      putTags []     = putChar '\n'
      putTags (x:xs) = C.putStr x >> putChar ' ' >> putTags xs

