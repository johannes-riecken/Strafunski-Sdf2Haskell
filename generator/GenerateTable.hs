import Sdf
import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Pretty
import qualified Data.Map as M
import Data.Map (Map(..))
import Text.Parsec.Language (haskellDef)
import Text.Parsec
import Control.Monad.Combinators.Expr
import qualified Text.Parsec.Token as P

x = [AssocGroup
       LeftAssoc
       [Prod
          [Sort "Exp", Lit (Quoted "*"), Sort "Exp"]
          (Sort "Exp")
          NoAttrs,
        Prod
          [Sort "Exp", Lit (Quoted "/"), Sort "Exp"]
          (Sort "Exp")
          NoAttrs,
        Prod
          [Sort "Exp", Lit (Quoted "%"), Sort "Exp"]
          (Sort "Exp")
          NoAttrs],
     AssocGroup
       LeftAssoc
       [Prod
          [Sort "Exp", Lit (Quoted "+"), Sort "Exp"]
          (Sort "Exp")
          NoAttrs,
        Prod
          [Sort "Exp", Lit (Quoted "-"), Sort "Exp"]
          (Sort "Exp")
          NoAttrs]]

data Exp = Var String
    | IntLit Int
    | Times Exp Exp
    | Div Exp Exp
    | Mod Exp Exp
    | Plus Exp Exp
    | Minus Exp Exp
    deriving (Eq, Ord, Show)

ctors :: Map String String
ctors = M.fromList [("*", "Times"), ("/", "Div"), ("%", "Mod"), ("+", "Plus"), ("-", "Minus")]

assocGroupToTableEntry (AssocGroup LeftAssoc prods) = listE $ fmap (\(Prod [_, Lit (Quoted opStr), _] _ _) -> appFun (var . name $ "binaryL") [strE opStr, var $ name (ctors M.! opStr)]) prods

assocGroupsToTable = listE . fmap assocGroupToTableEntry

main :: IO ()
main = do
    putStrLn . prettyPrint . assocGroupsToTable $ x

binaryL, binaryN, binaryR :: Monad m => String -> (a -> a -> a) -> Operator (ParsecT String u m) a
binaryL  name fun = InfixL (do{ reservedOp name; pure fun })
binaryN  name fun = InfixN (do{ reservedOp name; pure fun })
binaryR  name fun = InfixR (do{ reservedOp name; pure fun })

prefix, postfix :: Monad m => String -> (a -> a) -> Operator (ParsecT String u m) a
prefix  name fun       = Prefix (do{ reservedOp name; pure fun })
postfix name fun       = Postfix (do{ reservedOp name; pure fun })

lexer :: Monad m => P.GenTokenParser String u m
lexer       = P.makeTokenParser haskellStyle

parens :: Monad m => ParsecT String u m a -> ParsecT String u m a
parens      = P.parens lexer
comma :: Monad m => ParsecT String u m String
comma      = P.comma lexer
braces :: Monad m => ParsecT String u m a -> ParsecT String u m a
braces      = P.braces lexer
identifier :: Monad m => ParsecT String u m String
identifier  = P.identifier lexer
reserved :: Monad m => String -> ParsecT String u m ()
reserved    = P.reserved lexer
reservedOp :: Monad m => String -> ParsecT String u m ()
reservedOp    = P.reservedOp lexer
natural :: Monad m => ParsecT String u m Int
natural    = fromInteger <$> P.natural lexer
commaSep :: Monad m => ParsecT String u m a -> ParsecT String u m [a]
commaSep    = P.commaSep lexer
commaSep1 :: Monad m => ParsecT String u m a -> ParsecT String u m [a]
commaSep1    = P.commaSep1 lexer
lexeme :: Monad m => ParsecT String u m a -> ParsecT String u m a
lexeme    = P.lexeme lexer

haskellStyle :: Monad m => P.GenLanguageDef String st m
haskellStyle = P.LanguageDef
                    {
                P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                , P.nestedComments = True
                , P.identStart     = letter
                , P.identLetter    = alphaNum <|> oneOf "_'"
                , P.opStart        = P.opLetter haskellStyle
                , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , P.reservedOpNames= []
                , P.reservedNames  = []
                , P.caseSensitive  = True
                }
