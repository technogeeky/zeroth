module Language.Haskell.TH.ZeroTH.Config where

data Config
    = Config
    { ghcPath    :: FilePath
    , cpphsPath  :: FilePath
    , inputFile  :: FilePath
    , outputFile :: FilePath
    , ghcArgs    :: [String]
    , cpphsArgs  :: [String]
    , dropImport :: [String]
    , wholeFile  :: Bool
    } deriving Show