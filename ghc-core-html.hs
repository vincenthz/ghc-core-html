{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Parsec.String
import System.Environment
import System.Console.GetOpt
import Control.Applicative
import Control.Monad
import Data.Maybe    (fromMaybe, isJust, listToMaybe)
import Data.Monoid
import Data.List
import qualified Data.Foldable as F

import Text.Blaze.Html5 (toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Utf8

import qualified Data.ByteString.Lazy.Char8 as LC
import System.IO
import System.Exit
import System.Process
import Text.Printf

import qualified Data.Map as M

import GhcCore.Parser
import Paths_ghc_core_html

-- To suppress warnings in ghc 7.10
import Prelude


-- | Print raw result of parse
printRaw :: [Atom] -> IO ()
printRaw xs = do
    mapM_ print xs
    let (nJ,nF,nO) = foldl acc (0,0,0) xs
    printf "Parsed %i / %i (%i junks)" nO (nO+nF) nJ
  where
    acc :: (Int,Int,Int) -> Atom -> (Int,Int,Int)
    acc (nJ,nF,nO) (Junk _) = (nJ+1,nF,nO)
    acc (nJ,nF,nO) (RawBinding {}) = (nJ,nF+1,nO)
    acc (nJ,nF,nO) (BindingP {}) = (nJ,nF,nO+1)

runGhc :: [Flag] -> String -> IO (Either String [Atom])
runGhc opts filename = do
    (xc, out, err) <- readProcessWithExitCode ghcProgram args []
    case xc of
        ExitFailure ec -> return (Left (printf "dumping ghc core failed with exit code %d: %s" ec err))
        ExitSuccess    ->
            case runCoreParser core () "core" out of
                Left parseErr -> return (Left (show parseErr))
                Right atoms   -> return (Right atoms)
  where
    withCasts  = if WithCast `elem` opts then [] else ["-dsuppress-coercions"]
    baseArgs   = withCasts ++ ["-O2", "-ddump-simpl", "-fforce-recomp", "--make", filename]
    args       = foldr applyGhcOptionFlag baseArgs opts
    ghcProgram = fromMaybe "ghc" $ listToMaybe [ p | Ghc p <- opts ]
    applyGhcOptionFlag f acc =
        case f of
            GhcOption s -> s : acc
            _           -> acc

go :: [Flag] -> [String] -> IO ()
go _ [] = error "no file specified"
go opts (f:_) = do
    -- Read CSS file
    css <- readFile =<< getDataFileName "css/default.css"
    js  <- readFile =<< getDataFileName "js/page.js"
    -- read the core file, either directly if it's specified as a file, or
    -- by running ghc on the source file.
    result <-
        if CoreFile `elem` opts
            then either (Left . show) Right <$> parseFromFile core f
            else runGhc opts f
    case result of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right xs
            | Raw `elem` opts -> printRaw xs
            | otherwise       -> do
                -- default HTML output
                let table = allSyms xs
                LC.hPutStrLn stdout $ renderHtml $ onPage css js $ do

                H.header $ do
                    H.a ! HA.id "buttonToggleBody" $ "toggle bodies"
                    _ <- " - "
                    indexify table
                F.foldMap (atomToHtml table) xs
  where
    onPage css js p =
        H.html $ do
            H.head $ do
                H.title "core-2-html"
                H.style $ toHtml css
                H.script ! HA.src "http://code.jquery.com/jquery-1.9.1.min.js" $ ""
                H.script ! HA.type_ "text/javascript" $ toHtml js
            H.body p

    allSyms = foldl i M.empty
        where i a (Junk _)             = a
              i a (RawBinding sym _ _) = M.insert sym () a
              i a (BindingP bind)      = M.insert (bindSymbol bind) () a

    indexify table = do
        --let allToplevelSyms = foldl i [] atoms
        let t = foldl prefixify pempty $ map fst $ M.toList table
        H.ul $
            H.li ! HA.class_ "idxdir" $ do
                treeToHtml "Index" t
        
      where prefixify a s =
                case wordsWhen (== '.') s of
                  l@[_] -> pinsert s ("$ANONYMOUS":l) a
                  l     -> pinsert s l a
            treeToHtml n (Leaf m) = toAnchor m $ toHtml n
            treeToHtml n (PTree m) = do
                H.span $ toHtml n
                H.ul $ do
                    forM_ (M.toList m) $ \(k,v) -> do
                        H.li ! HA.class_ "idxdir" $ do
                            treeToHtml k v


atomToHtml :: M.Map String a -> Atom -> H.Html
atomToHtml _ (Junk s) = H.section $
    H.pre (toHtml s)
atomToHtml table (BindingP bind) = H.section ! HA.class_ "binding" $ do
    anchor (bindSymbol bind)
    let (first:after) = lines $ bindBody bind
    H.pre ! HA.class_ "header" ! HA.title (H.toValue $ signatureRaw $ bindSignature bind) $ colorify table first
    H.pre ! HA.class_ "body" $ colorify table $ unlines after
atomToHtml _ (RawBinding sym t errs) = H.section $ do
    anchor sym
    H.p (toHtml errs)
    H.pre (toHtml t)

    
tokenToHtml :: M.Map String a -> Token -> H.Html
tokenToHtml table (Symbol s)  =
    let mn = case s of
                _ | "GHC.Types."   `isPrefixOf` s -> Just $ drop 10 s
                  | "GHC.CString." `isPrefixOf` s -> Just $ drop 12 s
                  | "GHC.Prim."    `isPrefixOf` s -> Just $ drop 9 s
                  | "GHC.Base."    `isPrefixOf` s -> Just $ drop 9 s
                  | "GHC.Word."    `isPrefixOf` s -> Just $ drop 9 s
                  | otherwise -> Nothing
        found   = isJust $ M.lookup s table
        manchor = if found then toAnchor s else id
     in case mn of
        Nothing -> manchor (H.span ! HA.class_ "sym" $ toHtml s)
        Just n  -> manchor (H.span ! HA.class_ "sym" ! HA.title (H.toValue s) $ toHtml n)
        
tokenToHtml _ (Number n)  = H.span ! HA.class_ "nu" ! HA.title hexVal $ toHtml n
                              where hexVal = H.toValue ( printf "hex: 0x%x" (read n :: Integer) :: String)
tokenToHtml _ (Spaces s)  = toHtml s
tokenToHtml _ (StringT s) = H.span ! HA.class_ "str" $ toHtml ("\"" ++ s ++ "\"")
tokenToHtml _ (CharT c)   = H.span ! HA.class_ "str" $ toHtml ("\'" ++ [c] ++ "\'")
tokenToHtml _ (TypeDef s) = ":: " `mappend` (H.span ! HA.class_ "ty" $ toHtml s)
tokenToHtml _ Arrow       = "->"
tokenToHtml _ Dot         = "."
tokenToHtml _ BSlash      = "\\"
tokenToHtml _ Equal       = "="
tokenToHtml _ LBrace      = "{"
tokenToHtml _ RBrace      = "}"
tokenToHtml _ LBrack      = "["
tokenToHtml _ RBrack      = "]"
tokenToHtml _ LParen      = "("
tokenToHtml _ RParen      = ")"
tokenToHtml _ Unit        = "()"
tokenToHtml _ LParenHash  = "(#"
tokenToHtml _ RParenHash  = "#)"
tokenToHtml _ Case        = H.span ! HA.class_ "kw" $ "case"
tokenToHtml _ Of          = H.span ! HA.class_ "kw" $ "of"
tokenToHtml _ Forall      = H.span ! HA.class_ "kw" $ "forall"
tokenToHtml _ Underscore  = "_"
tokenToHtml _ (Unknown s) = toHtml s


colorify :: M.Map String a -> String -> H.Html
colorify table = F.foldMap (tokenToHtml table) . tokenify

anchor :: H.ToValue a => a -> H.Html
anchor sym = H.a ! HA.name (H.toValue sym) $ ""

toAnchor :: String -> H.Html -> H.Html
toAnchor sym c = H.a ! HA.href (H.toValue ('#' : sym)) $ c



----------------------------------------------------------------
-- Main
----------------------------------------------------------------

data Flag = Raw | CoreFile | WithCast | Help | Ghc String | GhcOption String
    deriving (Show,Eq)

options :: [OptDescr Flag]
options =
    [ Option ['r']  ["raw"]   (NoArg Raw)      "output raw instead of html"
    , Option ['c']  ["core"]  (NoArg CoreFile) "argument is already a core file"
    , Option []     ["cast"]  (NoArg WithCast) "don't hide cast / coercions"
    , Option []     ["ghc-option"] (ReqArg GhcOption "FLAG") "pass an argument to ghc"
    , Option ['h']  ["help"]  (NoArg Help)     "show help"
    , Option []     ["ghc"]   (ReqArg (\x -> Ghc x) "PROGRAM") "ghc executable to use (default ghc)"
    ]

help :: IO b
help = do
    putStrLn $ usageInfo "ghc-core-html" options
    exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        (o,n,[]) | Help `elem` o -> help
                 | otherwise     -> go o n
        (_,_,err) -> error (show err)


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Split string into words using predicate
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
