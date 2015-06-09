module Main (main) where

import HSH
import Data.List
import Control.Applicative
import Control.Monad
import System.IO
import System.Exit
import System.Environment
import System.FilePath

eprint = hPutStrLn stderr

-- cabal unpack command line
cabal_unpack p = ("cabal", ["unpack", p])

-- Finds *hs in current dir, recursively
findSources :: [String] -> IO [String]
findSources [] = return []
findSources d = run $ ("find", d ++ words "-type f -and ( -name *\\.hs -or -name *\\.lhs )")

-- Produces list of imported modules for file.hs given
findImports :: [String] -> IO [String]
findImports s = run $ catFrom s -|- extractImports

-- Search for 'import' declarations
extractImports = nub . sort . filter (/=[]) . map (grepImports . words)

grepImports ("import":"qualified":x:_) = x
grepImports ("import":x:_) = x
grepImports _ = []

-- Maps import name to haskell package name
iname2module :: String -> IO String
iname2module m = run $ ghc_pkg m -|- map (head . words) -|- highver
    where highver [] = []
          highver s = last (lines s)
          ghc_pkg m = ("ghc-pkg", ["--simple-output", "find-module", m])

inames2modules :: [String] -> IO [String]
inames2modules is = forM is (iname2module) >>= return . nub . sort . filter (/=[])

testdir dir fyes fno = do
    ret <- run ("test",["-d", dir])
    case ret of
        ExitSuccess -> fyes
        _ -> fno

-- Unapcks haskel package to the sourcedir
unpackModule p = do
    srcdir <- sourcedir
    let fullpath = srcdir </> p
    testdir fullpath
        (do
            eprint $ "Already unpacked " ++ p
            return fullpath
        )
        (do
            cd srcdir
            ec <- tryEC (runIO (cabal_unpack p))
            case ec of
                Left _ -> eprint ("Can't unpack " ++ p) >> return ""
                Right _ -> return fullpath
        )

unpackModules ms = filter (/="") <$> mapM unpackModule ms

-- Run GNU which tool
which :: String -> IO (String, IO (String,ExitCode))
which n = run ("which", [n])

checkapp appname = do
    (_,ec) <- which appname >>= return . snd >>= id
    case ec of
        ExitSuccess -> return ()
        _ -> do
            eprint $ "Please Install \"" ++ appname ++ "\" application"
            exitWith ec

-- Directory to unpack sources into
sourcedir = glob "~" >>= return . (</> ".haskdogs") . head

data Input = Dirs [FilePath] | Files [FilePath]
  deriving (Show)

gentags inp flags = do
    putStrLn (show inp)
    checkapp "cabal"
    checkapp "ghc-pkg"
    checkapp "hasktags"
    d <- sourcedir
    testdir d (return ()) (run ("mkdir",["-p",d]))
    files <- bracketCD "." $ do
      ss_local <- (case inp of
                    Dirs dirs -> findSources dirs
                    Files files -> return files)
      when (null ss_local) $ do
        fail $ "haskdogs were not able to find any sources in " ++ (show inp)
      ss_l1deps <- findImports ss_local >>= inames2modules >>= unpackModules >>= findSources
      return $ ss_local ++ ss_l1deps
    runIO $ echo (unlines files) -|- ("hasktags", flags ++ ["STDIN"])
    -- runIO $ ("hasktags", flags ++ files)

help = do
    eprint "haskdogs: generates tags file for haskell project directory"
    eprint "Usage:"
    eprint "    haskdogs [-d (FILE|'-') | -f (FILE|'-')] [FLAGS]"
    eprint "        FLAGS will be passed to hasktags as-is followed by"
    eprint "        a list of files. Defaults to -c -x."
    eprint "        -f sets the list of FILES to search for sources"
    eprint "        -d sets the list of DIRS to search for sources"
    return ()

defflags = ["-c", "-x"]

amain [] = gentags (Dirs ["."]) defflags
amain ("-f" : filefile : flags) = do
    file <- if (filefile=="-") then return stdin else openFile filefile ReadMode
    files <- lines <$> hGetContents file
    gentags (Files files) (if null flags then defflags else flags)
amain ("-d" : dirfile : flags) = do
    file <- if (dirfile=="-") then return stdin else openFile dirfile ReadMode
    dirs <- lines <$> hGetContents file
    gentags (Dirs dirs) (if null flags then defflags else flags)
amain flags 
  | "-h"     `elem` flags = help
  | "--help" `elem` flags = help
  | "-?"     `elem` flags = help
  | otherwise = gentags (Dirs ["."]) flags

main :: IO()
main = getArgs >>= amain

