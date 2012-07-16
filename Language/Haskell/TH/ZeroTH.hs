{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.ZeroTH
    ( prettyPrintAll, zeroTH, zeroTHInternal
    ) where

import Language.Haskell.Exts
import System.Process        ( runInteractiveProcess, waitForProcess )
import System.IO             ( hClose, hGetContents, hPutStr, hPutStrLn, openTempFile, stderr )
import System.Directory      ( removeFile, getTemporaryDirectory )
import System.Exit           ( ExitCode (..) )
import Control.Applicative   ( (<$>), (<*>) )
import Control.Monad         ( guard, when )
import Data.Foldable         ( fold )
import Data.Generics.Aliases ( mkT )
import Data.Generics.Schemes ( everywhere, listify )
import Data.List             ( (\\), delete, intersperse, isInfixOf, isPrefixOf, nub, stripPrefix )
import Data.Maybe            ( catMaybes, fromMaybe, mapMaybe )

import Language.Haskell.TH.ZeroTH.Config   ( Config(..) )
import Language.Haskell.TH.ZeroTH.Comments ( Location, parseComments, mixComments )
import Language.Haskell.TH.ZeroTH.Helper   ( idPrefix )
import ListUtils                           ( replaceAll )

readFromFile :: FilePath -> IO String
readFromFile "-"  = getContents
readFromFile path = readFile path

writeToFile :: FilePath -> String -> IO ()
writeToFile "-" d = putStr d
writeToFile path d = writeFile path d

zeroTH :: Config -> IO ()
zeroTH = (=<<) . writeToFile . outputFile <*> (prettyPrintAll <$>) . zeroTHInternal

data ZeroTHOutput
    = ZeroTHOutput { 
                     originalSource :: String
                   , combinedOutput :: Module
                   , thOutput :: [(Location, String)]
                   }

zeroTHInternal :: Config -> IO ZeroTHOutput
zeroTHInternal c
    = do input       <- readFromFile $ inputFile c
         tmpDir      <- getTemporaryDirectory
         (inputFile2, tmpHandle) <- case inputFile c of
                                       "-" -> openTempFile tmpDir "TH.cpphs.zeroth"
                                       _   -> return (inputFile c, undefined)
         when (inputFile c == "-") $ hPutStr tmpHandle input >> hClose tmpHandle
         let exts = readExtensions input
         hPutStrLn stderr $ "extensions: " ++ show exts
         let firstLine      = head $ lines input
             shouldRunCpphs = "-cpp" `elem` ghcArgs c || " -cpp " `isInfixOf` firstLine || CPP `elem` (fold exts)
         thInput     <- if shouldRunCpphs then preprocessCpphs (cpphsPath c) (["--noline","-DHASTH"]++cpphsArgs c) inputFile2
                                          else return input
         zerothInput <- if shouldRunCpphs then preprocessCpphs (cpphsPath c) ("--noline" : cpphsArgs c) inputFile2
                                          else return input
         (thData, qualImports) <- case parseFileContents thInput of
                                     ParseOk m -> unzip <$> runTH (ghcPath c) ((if wholeFile c then id else onlySplices) m) (ghcArgs c)
                                     e -> error $ "error near case thInput" ++ show e ++ '\n' : thInput
         let reattach :: [Decl] -> [Decl]
             reattach (SpliceDecl sLoc _ : t) = (parseDecls . fromMaybe err $ lookup (location sLoc) thData) ++ t
                 where
                     err = error $ "Could not find splice at " ++ show (location sLoc) ++ " in " ++ show thData
             reattach x                       = x
         combinedData <- case parseFileContents zerothInput of
                           ParseOk (Module loc m pragmas mWarn exports im decls)
                             -> return (Module loc m pragmas mWarn exports (postProcessImports (dropImport c) im $ concat qualImports)
                                        (everywhere (mkT reattach) decls))
                           e -> error $ "error near case zerothInput\n" ++ show e ++ '\n' : zerothInput
         when (inputFile c == "-") $ removeFile inputFile2
         return ZeroTHOutput { originalSource = input
                             , combinedOutput = combinedData
                             , thOutput = thData
                             }
    where parseDecls s = case parseFileContents s of
                           ParseOk (Module _ _ _ _ _ _ decls) -> decls
                           e -> error $ "error near parse.. s \n" ++ show e ++ '\n' : s
          onlySplices (Module loc m pragmas mWarn exports im decls) = Module loc m pragmas mWarn exports im $ listify isSplice decls
          isSplice (SpliceDecl _ _) = True
          isSplice _ = False

prettyPrintAll :: ZeroTHOutput -> String
prettyPrintAll out = unlines . mixComments (parseComments $ originalSource out) . numberAndPrettyPrint $ combinedOutput out

location :: SrcLoc -> Location
location sLoc = (srcLine sLoc, srcColumn sLoc)

-- | IncludePragma and CFilesPragma no longer exist, and "ModulePragma" now contains "LanguagePragma"
-- "OptionsPragma" and "AnnModulePragma"
--
numberAndPrettyPrint :: Module -> [(Location, String)]
numberAndPrettyPrint (Module mLoc m prags mbWarn exports imp decls)
    = (nAndPPrag =<< prags)
      ++ (location mLoc, concat $ "module "
                                 : prettyPrint m
                                 : catMaybes [ ppWarnText <$> mbWarn
                                             , (\es -> " (" ++ concat (intersperse ", " $ prettyPrint <$> es) ++ ")") <$> exports
                                             ]
                                 ++ [" where"])
         : (((\i -> (location (importLoc i), prettyPrint i)) <$> imp) ++ (nAndPDec =<< decls))
    where nAndPDec  d@(TypeDecl       loc _ _ _         ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(DataDecl       loc _ _ _ _ _ _   ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(GDataDecl      loc _ _ _ _ _ _ _ ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(InfixDecl      loc _ _ _         ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(ClassDecl      loc _ _ _ _ _     ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(InstDecl       loc _ _ _ _       ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(DefaultDecl    loc _             ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(SpliceDecl     loc _             ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(TypeSig        loc _ _           ) = [(location loc, prettyPrint d)]
          nAndPDec    (FunBind matches)                   = (\match@(Match loc _ _ _ _ _) -> (location loc, prettyPrint match)) <$> matches
          nAndPDec  d@(PatBind        loc _ _ _ _       ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(ForImp         loc _ _ _ _ _     ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(ForExp         loc _ _ _ _       ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(DataFamDecl    loc _ _ _ _       ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(DataInsDecl    loc _ _ _ _       ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(DeprPragmaDecl loc _             ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(DerivDecl      loc _ _ _         ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(GDataInsDecl   loc _ _ _ _ _     ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(InlineSig      loc _ _ _         ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(InstSig        loc _ _ _         ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(RulePragmaDecl loc _             ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(SpecInlineSig  loc _ _ _ _       ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(SpecSig        loc _ _           ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(TypeFamDecl    loc _ _ _         ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(TypeInsDecl    loc _ _           ) = [(location loc, prettyPrint d)]
          nAndPDec  d@(WarnPragmaDecl loc _             ) = [(location loc, prettyPrint d)]
--        nAndPPrag p@(IncludePragma  loc _             ) = [(location loc, prettyPrint p)]
--        nAndPPrag p@(CFilesPragma   loc _             ) = [(location loc, prettyPrint p)]
          nAndPPrag   (OptionsPragma  loc mt s          ) = [(location loc, prettyPrint . OptionsPragma loc mt $ filterOptions s)]
          nAndPPrag   (LanguagePragma loc names         )
              | null filteredNames = []
              | otherwise          = [(location loc, prettyPrint $ LanguagePragma loc filteredNames)]
              where
                  filteredNames = names \\ (Ident <$> unwantedLanguageOptions)
          filterOptions optStr = foldr (\opt -> replaceAll (" -" ++ opt ++ " ") " ") optStr $ "cpp" : "fth" : (('X' :) <$> unwantedLanguageOptions)
          unwantedLanguageOptions = ["CPP", "TemplateHaskell"]

ppWarnText :: WarningText -> String
ppWarnText (DeprText s) = "{-# DEPRECATED" ++ s ++ "#-}"
ppWarnText (WarnText s) = "{-# WARNING" ++ s ++ "#-}"

-- Removes TH imports, and adds any qualified imports needed by generated TH code
postProcessImports :: [String] -> [ImportDecl] -> [String] -> [ImportDecl]
postProcessImports dropPrefixes oldImports qNames
    = nub $ removeTH
            ++ mapMaybe (\q -> do guard . not $ any (maybe False (\(ModuleName m) -> m == q) . importAs) removeTH
                                  return ImportDecl { importLoc = emptySrcLoc
                                                    , importModule = ModuleName q
                                                    , importQualified = True
                                                    , importSrc = False
                                                    , importAs = Nothing
                                                    , importPkg = Nothing
                                                    , importSpecs = Nothing })
                        qNames
    where
        removeTH = filter (not . (\(ModuleName m) -> any (`isPrefixOf` m) dropPrefixes) . importModule) oldImports

preprocessCpphs :: FilePath -- ^ Path to cpphs
                -> [String]
                -> String
                -> IO String
preprocessCpphs cpphs args inputFilename
    = do (inH,outH,_,pid) <- runInteractiveProcess cpphs (inputFilename:args) Nothing Nothing
         hClose inH
         output <- hGetContents outH
         length output `seq` hClose outH
         eCode <- waitForProcess pid
         case eCode of
           ExitFailure err -> error $ "Failed to run cpphs: " ++ show err
           ExitSuccess -> return output

runTH :: FilePath -- ^ Path to GHC
      -> Module 
      -> [String]
      -> IO ([((Location,String),[String])])
runTH ghc (Module _ _ pragmas _ _ imports decls) ghcOpts
    = do tmpDir <- getTemporaryDirectory
         (tmpInPath,tmpInHandle) <- openTempFile tmpDir "TH.source.zeroth.hs"
         hPutStr tmpInHandle realM
         hClose tmpInHandle
         let args = [tmpInPath]++ghcOpts++extraOpts
         --putStrLn $ "Module:\n" ++ realM
         --putStrLn $ "Running: " ++ unwords (ghc:args)
         (inH,outH,errH,pid) <- runInteractiveProcess ghc args Nothing Nothing
         hClose inH
         output <- hGetContents outH
         --putStrLn $ "TH Data:\n" ++ output
         length output `seq` hClose outH
         errMsg <- hGetContents errH
         length errMsg `seq` hClose errH
         eCode <- waitForProcess pid
         -- removeFile tmpInPath
         let check :: [(((Location,String),[String]), String)] -> ((Location,String),[String])
             check [(ret,_)] = ret
             check _         = error $ "Failed to parse result:\n" ++ output
         case eCode of
           ExitFailure err -> error (unwords (ghc:args) ++ ": failure: " ++ show err ++ ":\n" ++ errMsg)
           ExitSuccess -> return . mapMaybe (fmap (check . reads) . stripPrefix idPrefix) $ lines output
    where pp :: (Pretty a) => a -> String
          pp = prettyPrintWithMode (defaultMode{layout = PPInLine})
          realM = unlines $ (pp . disableWarnings <$> pragmas)
                            ++ ["{-# LANGUAGE TypeOperators #-}"]
                            ++ ["module ZerothTemp where"]
                            ++ (pp <$> imports)
                            ++ ["import qualified " ++ helperModule]
                            ++ (prettyPrint <$> everywhere (mkT editSplice) decls)
          extraOpts = ["-w"]
          extraOpts' = (' ' :) =<< extraOpts
          disableWarnings (OptionsPragma  loc Nothing    s) = OptionsPragma  loc Nothing $ s ++ extraOpts' -- Turn off all warnings (works for GHC)
          disableWarnings (OptionsPragma  loc (Just GHC) s) = OptionsPragma  loc (Just GHC) $ s ++ extraOpts'
          disableWarnings (LanguagePragma loc           xs) = LanguagePragma loc $ delete (Ident "CPP") xs
          disableWarnings x = x

spliceToExp (ParenSplice e) =  e
spliceToExp _______________ = error "ZeroTH: FIXME!"

helperModule = "Language.Haskell.TH.ZeroTH.Helper"

helperName = App (Var . Qual (ModuleName helperModule) $ Ident "helper")

editSplice :: Decl -> Decl
editSplice (SpliceDecl loc splice)
     = SpliceDecl loc
          . App (helperName (Paren splice))
          . Tuple
          $ Lit . Int . fromIntegral <$> [ srcLine loc, srcColumn loc ]
editSplice x = x

emptySrcLoc :: SrcLoc
emptySrcLoc = SrcLoc "" 0 0

{-

module Test where
$(test) -- line 2
$(jalla) -- line 3
svend = svend


-------------------------------

module Main where
import Language.Haskell.TH
main = do decs <- sequence [runQ test
                           ,runQ jalla]
          mapM_ (putStrLn.pprint) (zip decs [2,3])


-}
