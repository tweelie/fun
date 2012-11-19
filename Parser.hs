-- | Written by Morten Winther Olsson, (c) 2012

module Parser
       ( p_prog
       , p_parse
       , testprog
       , testex
       , testex2
       ) where


import Ast
import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Char
import Data.String

data Precedence = PLeft Int
                | PRight Int
                | PNone Int

getPre :: String -> Precedence
getPre "+" = PLeft 3
getPre "=<" = PLeft 2
getPre "app" = PLeft 6
getPre "if" = PNone 1
getPre "fst" = PNone 7
getPre "snd" = PNone 7
getPre "lam" = PNone 0
getPre "let" = PNone 0
getPre "rec" = PNone 0
getPre "case" = PNone 1

filterBOps :: Int -> Precedence -> Bool
filterBOps p (PLeft n) | n>=p = True
filterBOps p (PRight n) | n>=p = True
filterBOps _ _ = False

calcQ :: Precedence -> Int
calcQ (PLeft n) = n+1
calcQ (PRight n) = n
calcQ (PNone n) = n

getBPar :: String -> Parser (Term -> Term)
getBPar "+" = (flip TPlus <$> (p_char '+' >> (p_term $ calcQ (getPre "+"))))
getBPar "=<" = (flip TLeq <$> (p_word "=<" >> (p_term $ calcQ (getPre "=<"))))
getBPar "app" = (flip TApp <$> (p_term $ calcQ (getPre "app")))

getUPar :: String -> Parser Term
getUPar "if" = TIf <$> (p_word "if" >> p_terms) <*> (p_word "then" >> p_terms) <*> (p_word "else" >> (p_term $ calcQ (getPre "if")))
getUPar "fst" = TFst <$> (p_word "fst" >> (p_term $ calcQ (getPre "fst")))
getUPar "snd" = TSnd <$> (p_word "snd" >> (p_term $ calcQ (getPre "snd")))
getUPar "lam" = TLam <$> (char '\\' >> p_name) <*> (p_char '.' >> (p_term $ calcQ (getPre "lam")))
getUPar "let" = TLet <$> (p_word "let" >> p_name) <*> (p_word "<=" >> p_terms) <*> (p_word "in" >> (p_term $ calcQ (getPre "let")))
getUPar "rec" = TRec <$> (p_word "rec" >> p_name) <*> (p_char '.' >> (p_term $ calcQ (getPre "rec")))
getUPar "case" = TCase <$> (p_word "case" >> p_terms) <*> (p_word "of" >> p_word "inl" >> p_char '(' >> p_name) <*> (p_char ')' >> p_char '.' >> p_terms) <*> (p_char ',' >> p_word "inr" >> p_char '(' >> p_name) <*> (p_char ')' >> p_char '.' >> (p_term $ calcQ (getPre "case")))

getBParsers :: Int -> [Parser (Term -> Term)]
getBParsers p = map getBPar $ filter ((filterBOps p).getPre) ["+", "=<", "app"]

getUParsers = map getUPar ["if", "fst", "snd", "lam", "let", "rec", "case"]

a :: Parser a -> Parser [a] -> Parser [a]
a = liftA2 (:)

ch :: Parser (a -> b) -> Parser ((b -> c) -> a -> c)
ch = liftA $ flip (.)

p_word :: String -> Parser ()
p_word s = try $ string s >> notFollowedBy alphaNum >> spaces

p_char :: Char -> Parser ()
p_char c = char c >> spaces

keywords = ["true", "false", "if", "then", "else", "fst", "snd", "let", "in", "rec", "case", "of", "inl", "inr"]

p_keywords = [p_word s | s <- keywords]

p_name :: Parser String
p_name = notFollowedBy (choice p_keywords) >> letter `a` many alphaNum <* spaces

p_num :: Parser Term
p_num = TNum . foldl1 ((+).(*10)) . map ((-48+).ord)
        <$> (many1 digit <* spaces)

p_var :: Parser Term
p_var = TVar <$> p_name

p_true :: Parser Term
p_true = TTrue <$ p_word "true"

p_false :: Parser Term
p_false = TFalse <$ p_word "false"

p_inl :: Parser Term
p_inl = TInl <$> (p_word "inl" >> p_char '(' >> p_terms <* p_char ')')

p_inr :: Parser Term
p_inr = TInr <$> (p_word "inr" >> p_char '(' >> p_terms <* p_char ')')

p_pair :: Parser (Term -> Term)
p_pair = flip TPair <$> (p_char ',' >> p_terms)

p_paran :: Parser Term
p_paran = p_char '(' >> (p_terms <**> (option id p_pair)) <* p_char ')'

p_unary :: Parser Term
p_unary = choice getUParsers

p_p :: Parser Term
p_p = choice [p_paran, p_unary, p_var, p_num, p_true, p_false, p_inl, p_inr]

p_termLoop :: Int -> Parser (Term -> Term)
p_termLoop p = option id $ ch (choice (getBParsers p)) <*> (p_termLoop p)

p_term :: Int -> Parser Term
p_term p = p_p <**> p_termLoop p

p_terms = p_term 0

p_prog :: Parser Term
p_prog = spaces >> p_terms <* eof

p_parse :: String -> Term
p_parse s = case (parse p_prog "" s) of
  Left _ -> error "Parse error!"
  Right ast  -> ast

testprog = "let acd <= \\x.if x then 10 else 12 in (\\ifxCd. if ifxCd =< 23 + (if true then 12 else 14) + 976 then acd (12 =< 14) else acd false ) "

testex2 = "\\x.case x of inl(y).x, inr(z).inl(z)"

testex = "rec f.\\x.\\y.if 0 =< x then y else f (x + 1) y"
