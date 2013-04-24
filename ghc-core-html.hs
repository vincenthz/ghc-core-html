{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Parsec
import Text.Parsec.String
import System.Environment
import System.Console.GetOpt
import Control.Monad
import Data.Maybe    (isJust)
import Data.Monoid
import Data.List
import qualified Data.Foldable as F

import Text.Blaze.Html5 (toHtml)
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


-- | Print raw result of parse
printRaw :: [Atom] -> IO ()
printRaw xs = do
    mapM_ print xs
    let (nJ,nF,nO) = foldl acc (0,0,0) xs
    putStrLn ("Parsed " ++ show nO ++ "/" ++ show (nO+nF) ++ " (" ++ show nJ ++ " junks)")
  where
    acc :: (Int,Int,Int) -> Atom -> (Int,Int,Int)
    acc (nJ,nF,nO) (Junk _) = (nJ+1,nF,nO)
    acc (nJ,nF,nO) (RawBinding {}) = (nJ,nF+1,nO)
    acc (nJ,nF,nO) (BindingP {}) = (nJ,nF,nO+1)

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
            then parseFromFile core f
            else do
                let args = "-O2":"-ddump-simpl":"-fforce-recomp":"--make":
                         (if WithCast `elem` opts then [] else ["-dsuppress-coercions"])
                let ghcProgram = case [ p | Ghc p <- opts ] of
                                    p:_ -> p
                                    _   -> "ghc"
                (x,out,err) <- readProcessWithExitCode ghcProgram (args ++ [f]) []
                case x of
                    ExitFailure _ -> error ("dumping ghc core failed: " ++ err)
                    ExitSuccess   -> return $ runP core () "core" out
    case result of
        Left err -> print err
        Right xs
          | Raw `elem` opts -> printRaw xs >> exitSuccess
          | otherwise       -> do
              -- default HTML output
              let table = allSyms xs
              LC.hPutStrLn stdout $ renderHtml $ onPage css js $ do
                  H.header $ do
                      H.a H.! HA.id "buttonToggleBody" $ "toggle bodies"
                      _ <- " - "
                      indexify table
                  F.foldMap (atomToHtml table) xs
              exitSuccess
  where
    onPage css js p =
        H.html $ do
            H.head $ do
                H.title "core-2-html"
                H.style $ toHtml css
                H.script H.! HA.src "http://code.jquery.com/jquery-1.9.1.min.js" $ ""
                H.script H.! HA.type_ "text/javascript" $ toHtml js
            H.body p

    allSyms = foldl i M.empty
        where i a (Junk _)             = a
              i a (RawBinding sym _ _) = M.insert sym () a
              i a (BindingP bind)      = M.insert (bindSymbol bind) () a

    indexify table = do
        --let allToplevelSyms = foldl i [] atoms
        let t = foldl prefixify pempty $ map fst $ M.toList table
        H.ul $
            H.li H.! HA.class_ "idxdir" $ do
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
                        H.li H.! HA.class_ "idxdir" $ do
                            treeToHtml k v


atomToHtml :: M.Map String a -> Atom -> H.Html
atomToHtml _ (Junk s) = H.section $
    H.pre (toHtml s)
atomToHtml table (BindingP bind) = H.section H.! HA.class_ "binding" $ do
    anchor (bindSymbol bind)
    let (first:after) = lines $ bindBody bind
    H.pre H.! HA.class_ "header" H.! HA.title (H.toValue $ signatureRaw $ bindSignature bind) $ colorify table first
    H.pre H.! HA.class_ "body" $ colorify table $ unlines after
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
        Nothing -> manchor (H.span H.! HA.class_ "sym" $ toHtml s)
        Just n  -> manchor (H.span H.! HA.class_ "sym" H.! HA.title (H.toValue s) $ toHtml n)
        
tokenToHtml _ (Number n)  = H.span H.! HA.class_ "nu" H.! HA.title hexVal $ toHtml n
                              where hexVal = H.toValue ( printf "hex: 0x%x" (read n :: Integer) :: String)
tokenToHtml _ (Spaces s)  = toHtml s
tokenToHtml _ (StringT s) = H.span H.! HA.class_ "str" $ toHtml ("\"" ++ s ++ "\"")
tokenToHtml _ (CharT c)   = H.span H.! HA.class_ "str" $ toHtml ("\'" ++ [c] ++ "\'")
tokenToHtml _ (TypeDef s) = ":: " `mappend` (H.span H.! HA.class_ "ty" $ toHtml s)
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
tokenToHtml _ Case        = H.span H.! HA.class_ "kw" $ "case"
tokenToHtml _ Of          = H.span H.! HA.class_ "kw" $ "of"
tokenToHtml _ Forall      = H.span H.! HA.class_ "kw" $ "forall"
tokenToHtml _ Underscore  = "_"
tokenToHtml _ (Unknown s) = toHtml s


colorify :: M.Map String a -> String -> H.Html
colorify table = F.foldMap (tokenToHtml table) . tokenify

anchor :: H.ToValue a => a -> H.Html
anchor sym = H.a H.! HA.name (H.toValue sym) $ H.span ""

toAnchor :: String -> H.Html -> H.Html
toAnchor sym c = H.a H.! HA.href (H.toValue ('#' : sym)) $ c



----------------------------------------------------------------
-- Main
----------------------------------------------------------------

data Flag = Raw | CoreFile | WithCast | Help | Ghc String
    deriving (Show,Eq)

options :: [OptDescr Flag]
options =
    [ Option ['r']  ["raw"]   (NoArg Raw)      "output raw instead of html"
    , Option ['c']  ["core"]  (NoArg CoreFile) "argument is already a core file"
    , Option []     ["cast"]  (NoArg WithCast) "don't hide cast / coercions"
    , Option ['h']  ["help"]  (NoArg Help)     "show help"
    , Option []     ["ghc"]   (ReqArg (\x -> Ghc x) "PROGRAM") "ghc executable to use (default ghc)"
    ]

help :: IO b
help = do
    putStrLn "usage: ghc-core-html [-r|--raw] [-c|--core] [--ghc program] [--cast] <file>"
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
