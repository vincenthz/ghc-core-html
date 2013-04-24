-- |
-- Parser for output of GHC core dump using @-ddump-simpl@.
module GhcCore.Parser where

import Data.Maybe          (fromMaybe)
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>), (<*), (*>))
import Control.Monad

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

tokenTable :: [(String, Token)]
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

keywordTable :: [(String, Token)]
keywordTable =
    [ ("case", Case)
    , ("of", Of)
    , ("forall", Forall)
    ]

symbolBind :: Parser String
symbolBind = do
    h <- alphaNum
    r <- many symb
    return (h:r)
  where
    symb = oneOf symChars
    symChars = ['A'..'Z']++['a'..'z']++['0'..'9']++"_'"

symbol :: Parser String
symbol = do
    c <- option "" (string ":")
    h <- alphaNum <|> char '$'
    r <- many symb
    return (c++(h:r))
  where
    symb = oneOf symChars
    symChars = ['A'..'Z']++['a'..'z']++['0'..'9']++"_'$<=/."

spaces1 :: Parser ()
spaces1 = space >> spaces

syntax :: Parser Token
syntax = op <$> many1 (oneOf opChars)
  where
    opChars =  "!@#$%^&*(){}[]_-.,+:;\\/?<>|~`"
    op s = fromMaybe (Unknown s) $ lookup s tokenTable

tokenify :: String -> [Token]
tokenify = either (error . show) id . runP (manyTill tok eof) () ""
  where
    tok      = choice [spaceTok,stringTok,charTok,typeDef,sym,number,syntax,unknown] <?> "token"
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
    keyOrIdent s = fromMaybe (Symbol s) $ lookup s keywordTable

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

pempty :: PTree
pempty = PTree M.empty

pinsert :: String -> [String] -> PTree -> PTree
pinsert v [x]    (PTree l) = PTree $ M.insert x (Leaf v) l
pinsert v (x:xs) (PTree l) =
    PTree $ M.alter (Just . pinsert v xs . fromMaybe (PTree M.empty)) x l
pinsert _ _      _ = error "pinsert"

parseSignature :: Parser Signature
parseSignature = do
    mforall <- optionMaybe (try (string "forall") *> spaces1 *> many1 (symbolBind <* spaces) <* char '.' <* spaces)
    sig     <- parseTypes
    return $ Signature mforall sig
  where
    parseTypes = manyTill anyChar (try (string "\n["))

parseBinding :: String -> Bool -> Parser Binding
parseBinding name r = do
    signature <- parseSignature
    attrs     <- parseAttributes
    body      <- manyTill anyChar eof
    return $ Binding name r signature attrs body
  where
    parseAttributes = manyTill anyChar (try (string "]\n" >> lookAhead symbol))

core :: Parser [Atom]
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
