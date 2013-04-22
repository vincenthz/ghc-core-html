{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Parsec
import Text.Parsec.String
import System.Environment
import System.Console.GetOpt
import Control.Applicative ((<$>), (<*), (*>))
import Control.Monad
import Data.Monoid
import Data.List

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

data Token =
      Symbol String
    | Number String
    | Spaces String
    | StringT String
    | CharT Char
    | TypeDef String
    | Arrow
    | Dot
    | BSlash
    | Equal
    | LBrace
    | RBrace
    | LBrack
    | RBrack
    | LParen
    | RParen
    | Unit
    | LParenHash
    | RParenHash
    | Case
    | Of
    | Forall
    | Underscore
    | Unknown String
    deriving (Show,Eq)

tokenTable =
    [ ("->", Arrow)
    , ("=" , Equal)
    , ("." , Dot)
    , ("\\", BSlash)
    , ("{" , LBrace)
    , ("}" , RBrace)
    , ("(" , LParen)
    , (")" , RParen)
    , ("()", Unit)
    , ("[" , LBrack)
    , ("]" , RBrack)
    , ("(#" , LParenHash)
    , ("#)" , RParenHash)
    , ("_", Underscore)
    ]

keywordTable =
    [ ("case", Case)
    , ("of", Of)
    , ("forall", Forall)
    ]

symbolBind = do
    h <- alphaNum
    r <- many symb
    return (h:r)
  where
    symb = oneOf symChars
    symChars = ['A'..'Z']++['a'..'z']++['0'..'9']++"_'"

symbol = do
    c <- option "" (string ":")
    h <- alphaNum <|> char '$'
    r <- many symb
    return (c++(h:r))
  where
    symb = oneOf symChars
    symChars = ['A'..'Z']++['a'..'z']++['0'..'9']++"_'$<=/."

spaces1 = space >> spaces

syntax = op <$> many1 (oneOf opChars)
  where
    opChars =  "!@#$%^&*(){}[]_-.,+:;\\/?<>|~`"
    op s = case lookup s tokenTable of
                Just t  -> t
                Nothing -> Unknown s

tokenify :: String -> [Token]
tokenify = either (error . show) id . runP (manyTill tok eof) () ""
  where
    tok     = (choice [spaceTok,stringTok,charTok,typeDef,sym,number,syntax,unknown] <?> "token")
    spaceTok = Spaces <$> many1 (oneOf " \n\t")
    number  = Number <$> many1 digit
    sym     = keyOrIdent <$> (oneOf symFirstChars >>= \s -> many symb >>= \r -> return (s:r))
    symb = oneOf symChars
    typeDef = TypeDef <$> try (string "::" >> spaces >> manyTill anyChar (lookAhead (oneOf ")=")))
    symFirstChars = ['A'..'Z']++['a'..'z']
    symChars = ['A'..'Z']++['a'..'z']++['0'..'9']++"_'$/<=.#"
    stringTok = StringT <$> try (char '"' *> many (noneOf ['"']) <* char '"')
    charTok = CharT <$> try (char '\'' *> anyChar <* char '\'')
    unknown = Unknown . (:[]) <$> anyToken
    keyOrIdent s = case lookup s keywordTable of
                        Nothing -> Symbol s
                        Just k  -> k

data Signature = Signature
    { signatureQualifiers :: Maybe [String]
    , signatureRaw        :: String
    }
    deriving (Show,Eq)

data Binding = Binding
    { bindSymbol     :: String
    , bindRecursive  :: Bool
    , bindSignature  :: Signature
    , bindAttributes :: String
    , bindBody       :: String
    }
    deriving (Show,Eq)

data Atom = RawBinding String String String
          | BindingP Binding
          | Junk String
    deriving (Show,Eq)

data PTree = PTree (M.Map String PTree)
           | Leaf String
           deriving (Show,Eq)

pempty = PTree M.empty

pinsert :: String -> [String] -> PTree -> PTree
pinsert v [x]    (PTree l) = PTree $ M.insert x (Leaf v) l
pinsert v (x:xs) (PTree l) =
    PTree $ M.alter (Just . pinsert v xs . maybe (PTree M.empty) id) x l
pinsert _ _      _ = error "pinsert"

parseSignature = do
    mforall <- optionMaybe (try (string "forall") *> spaces1 *> many1 (symbolBind <* spaces) <* char '.' <* spaces)
    sig     <- parseTypes
    return $ Signature mforall sig
  where
    parseTypes = manyTill anyChar (try (string "\n["))

parseBinding name r = do
    signature <- parseSignature
    attrs     <- parseAttributes
    body      <- manyTill anyChar eof
    return $ Binding name r signature attrs body
  where
    parseAttributes = manyTill anyChar (try (string "]\n" >> lookAhead symbol))

core = many (try binding <|> junk)
  where junk    = Junk <$> (anyChar >>= \c -> liftM (c :) (manyTill anyChar (eof <|> try eoBinding)))
        binding = do recursive <- option False (string "Rec" >> spaces >> string "{" >> spaces >> return True)
                     s <- try symbol
                     spaces
                     optional (manyTill anyChar (lookAhead (string "::")))
                     _ <- string "::" *> spaces
                     z <- manyTill anyChar (try (spaces >> eof) <|> try eoBinding)
                     case runP (parseBinding s recursive) () ("binding " ++ s) z of
                        Left err -> return $ RawBinding s z (show err)
                        Right b  -> return $ BindingP b
        eoBinding = string "\n\n" >> return ()

go _ [] = error "no file specified"
go opts (f:_) = do
    -- read the core file, either directly if it's specified as a file, or
    -- by running ghc on the source file.
    result <-
        if CoreFile `elem` opts
            then parseFromFile core f
            else do
                let args = "-O2":"-ddump-simpl":"-fforce-recomp":"--make":
                         (if WithCast `elem` opts then [] else ["-dsuppress-coercions"])
                let ghcProgram = case filter isGhcFlag opts of
                                    (Ghc p):_ -> p
                                    _         -> "ghc"
                (x,out,err) <- readProcessWithExitCode ghcProgram (args ++ [f]) []
                case x of
                    ExitFailure _ -> error ("dumping ghc core failed: " ++ err)
                    ExitSuccess   -> return $ runP core () "core" out
    case result of
        Left err -> print err
        Right xs -> do
            -- raw output
            when (Raw `elem` opts) $ do
                mapM_ (putStrLn . show) xs
                let (nJ,nF,nO) = foldl acc (0,0,0) xs
                putStrLn ("Parsed " ++ show nO ++ "/" ++ show (nO+nF) ++ " (" ++ show nJ ++ " junks)")
                exitSuccess
            -- default HTML output
            let table = allSyms xs
            LC.hPutStrLn stdout $ renderHtml $ onPage $ do
                H.header $ do
                    H.a H.! HA.id "buttonToggleBody" $ "toggle bodies"
                    _ <- " - "
                    indexify table
                mconcat $ map (atomToHtml table) xs
            exitSuccess
  where
    acc :: (Int,Int,Int) -> Atom -> (Int,Int,Int)
    acc (nJ,nF,nO) (Junk _) = (nJ+1,nF,nO)
    acc (nJ,nF,nO) (RawBinding {}) = (nJ,nF+1,nO)
    acc (nJ,nF,nO) (BindingP {}) = (nJ,nF,nO+1)

    onPage p =
        H.html $ do
            H.head $ do
                H.title "core-2-html"
                H.style $ toHtml css
                H.script H.! HA.src "http://code.jquery.com/jquery-1.9.1.min.js" $ ""
                H.script H.! HA.type_ "text/javascript" $ toHtml collapseIndexScript

            H.body p
      where
        collapseIndexScript = unlines
                [ "jQuery(document).ready(function() {"
                , " jQuery(\".idxdir > ul\").hide();"
                , " jQuery(\".idxdir > span\").click(function()"
                , " {"
                , " jQuery(this).next(\"ul\").slideToggle(200);"
                , " });"
                , " jQuery(\"#buttonToggleBody\").click(function()"
                , " {"
                , " jQuery(\"pre.body\").slideToggle(200);"
                , " });"
                , " jQuery(\".binding > .header\").click(function()"
                , " {"
                , " jQuery(this).next(\".body\").slideToggle(200);"
                , " });"
                , "});"
                ]

    css = unlines
        [ ".kw { color: #800080 }"
        , ".nu { color: #dda500 }"
        , ".sym { color: #0033aa }"
        , "a span.sym { font-weight: bold; }"
        , ".str { color: #dd2200 }"
        , ".ty { color: #347C17 }"
        , ".binding { background-color: #ddd }"
        , "pre a { text-decoration: none }"
        , "header { background-color: #646D7E; border-bottom: 1px thin #000; padding: 20px 20px 20px 20px }"
        , "header a { color: #88aaff; }"
        , "body { margin: 0; padding: 0; }"
        , "section { margin-left: 50px; }"
        , "#buttonToggleBody { border: 1px thin #000; background-color: #eee; color: #000; padding: 5px 5px 5px 5px }"
        ]

    allSyms atoms = foldl i M.empty atoms
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
                let l = wordsWhen (== '.') s in
                if length l == 1
                    then pinsert s ("$ANONYMOUS":l) a
                    else pinsert s l a
            wordsWhen     :: (Char -> Bool) -> String -> [String]
            wordsWhen p s =  case dropWhile p s of
                                  "" -> []
                                  s' -> w : wordsWhen p s''
                                        where (w, s'') = break p s'
            treeToHtml n (Leaf m) = toAnchor m $ toHtml n
            treeToHtml n (PTree m) = do
                H.span $ toHtml n
                H.ul $ do
                    forM_ (M.toList m) $ \(k,v) -> do
                        H.li H.! HA.class_ "idxdir" $ do
                            treeToHtml k v

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

    colorify table = mconcat . map (tokenToHtml table) . tokenify
    
    anchor sym = H.a H.! HA.name (H.toValue sym) $ H.span ""
    toAnchor sym c = H.a H.! HA.href (H.toValue ("#" ++ sym)) $ c

    tokenToHtml table (Symbol s)  =
        let mn = case s of
                    _ | "GHC.Types." `isPrefixOf` s -> (Just $ drop 10 s)
                      | "GHC.CString." `isPrefixOf` s  -> (Just $ drop 12 s)
                      | "GHC.Prim." `isPrefixOf` s  -> (Just $ drop 9 s)
                      | "GHC.Base." `isPrefixOf` s  -> (Just $ drop 9 s)
                      | "GHC.Word." `isPrefixOf` s  -> (Just $ drop 9 s)
                      | otherwise -> Nothing
            found = maybe False (const True) $ M.lookup s table
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

data Flag = Raw | CoreFile | WithCast | Help | Ghc String
    deriving (Show,Eq)

isGhcFlag :: Flag -> Bool
isGhcFlag (Ghc _) = True
isGhcFlag _       = False

options =
    [ Option ['r']  ["raw"]   (NoArg Raw)      "output raw instead of html"
    , Option ['c']  ["core"]  (NoArg CoreFile) "argument is already a core file"
    , Option []     ["cast"]  (NoArg WithCast) "don't hide cast / coercions"
    , Option ['h']  ["help"]  (NoArg Help)     "show help"
    , Option []     ["ghc"]   (ReqArg (\x -> Ghc x) "PROGRAM") "ghc executable to use (default ghc)"
    ]

help = do
    putStrLn "usage: ghc-core-html [-r|--raw] [-c|--core] [--ghc program] [--cast] <file>"
    exitSuccess

main = do
    args <- getArgs
    case getOpt Permute options args of
        (o,n,[]) | Help `elem` o -> help
                 | otherwise     -> go o n
        (_,_,err) -> error (show err)
