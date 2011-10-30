{-
LazyKQ interpreter
   -}

import Control.Applicative
import Data.Char
import System

data Expr = App Expr Expr
        | I
        | K | K' Expr
        | S | S' Expr | S'' Expr Expr
        | Inc
        | Export Int

instance Show Expr where
    show (App a b) = "ダァ" ++ show a ++ show b 
    show S = "シエリ"
    show K = "イェ"
    show I = "ス"
    show (S' a) = "ダァシエリ" ++ show a
    show (K' a) = "ダァイェ" ++ show a
    show (S'' a b) = "ダァダァシエリ" ++ show a ++ show b
    show Inc = "+"
    show (Export x) = show x

apply :: Expr -> Expr -> Expr
apply I x = x
apply K x = K' x
apply (K' x) y = x
apply S x = S' x
apply (S' x) y = S'' x y
apply (S'' x y) z = apply x z `apply` apply y z
apply Inc (Export x) = Export $ x + 1
apply f x = error $ "Can't apply " ++ show f ++ " to " ++ show x

eval :: Expr -> Expr
eval (App x y) = eval x `apply` eval y
eval x = x

export :: Expr -> Maybe Int
export (Export x) = Just x
export _ = Nothing

parseKQ :: String -> (Expr, String)

parseKQ ('ダ':'ァ':xs) = let (a0, xs') = parseKQ xs in
                         let (a1, xs'') = parseKQ xs' in
                             (App a0 a1, xs'')
parseKQ ('ダ':'ア':xs) = parseKQ ('ダ':'ァ':xs)
parseKQ ('シ':'エ':'リ':xs) = (S, xs)
parseKQ ('シ':'ェ':'リ':xs) = (S, xs)
parseKQ ('イ':'ェ':xs) = (K, xs)
parseKQ ('イ':'エ':xs) = (K, xs)
parseKQ ('ス':xs) = (I, xs)
parseKQ ('#':'\n':xs) = parseKQ xs
parseKQ ('#':_:xs) = parseKQ ('#':xs)
parseKQ (_:xs) = parseKQ xs
parseKQ "" = (I, "")

parse = fst.parseKQ

cons :: Expr -> Expr -> Expr
cons a b = App (App S (App (App S I) (App K a))) (App K b)

church :: Int -> Expr
church 0 = App K I
church n = App (App S (App (App S (App K S)) K)) (church $ n - 1)

encode :: String -> Expr
encode = foldr cons (church 256) . map (church.ord)

realize :: Expr -> Maybe Int
realize expr = export $ eval $ App (App expr Inc) $ Export 0

decode :: Expr -> (String, Int)
decode expr = case realize $ App expr K of
     Nothing -> ("", 1)
     Just x -> if x >= 256
                  then ("", x - 256)
                  else (chr x : a, b) where
                      (a, b) = decode $ eval $ App expr (App K I)

output :: (String, Int) -> IO ()
output (a, b) = putStr a >> exitWithCode b

exitWithCode 0 = exitWith $ ExitSuccess
exitWithCode n = exitWith $ ExitFailure n

interpret r d xs = (if r then print else output.decode).eval =<< if d
    then parse <$> getContents
    else App <$> parse <$> readFile (head xs) <*> (encode <$> getContents)

main = do args <- getArgs
          if "-h" `elem` args
             then help
             else interpret ("-r" `elem` args) ("-d" `elem` args)
                 $ filter (('-'/=).head) args

help = do
    putStrLn "Usage: kq [options] [filename]"
    putStrLn "-r: show as an expression"
    putStrLn "-d: read a source from stdin"
    putStrLn "-h: show this help"