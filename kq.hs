
import Control.Applicative
import Data.Char
import System

data Expr = Apply Expr Expr
        | I
        | K | K' Expr
        | S | S' Expr | S'' Expr Expr
        | Increment
        | Export Int

instance Show Expr where
    show (Apply a b) = "ダァ" ++ show a ++ show b 
    show S = "シエリ"
    show K = "イェ"
    show I = "ス"
    show (S' a) = "ダァシエリ" ++ show a
    show (K' a) = "ダァイェ" ++ show a
    show (S'' a b) = "ダァダァシエリ" ++ show a ++ show b

apply :: Expr -> Expr -> Expr
apply I x = x
apply K x = K' x
apply (K' x) y = x
apply S x = S' x
apply (S' x) y = S'' x y
apply (S'' x y) z = apply (apply x z) (apply y z)
apply Increment (Export x) = Export $ x + 1
apply f x = error $ "Can't apply " ++ show f ++ " to " ++ show x
--apply f x = apply (eval f) (eval x)

eval :: Expr -> Expr
eval (Apply x y) = apply (eval x) (eval y)
eval x = x

export (Export x) = Just x
export _ = Nothing

parse :: String -> (Expr, String)

parse ('ダ':'ァ':xs) = let (a0, xs') = parse xs in
                         let (a1, xs'') = parse xs' in
                             (Apply a0 a1, xs'')
parse ('ダ':'ア':xs) = parse ('ダ':'ァ':xs)

parse ('シ':'エ':'リ':xs) = (S, xs)
parse ('シ':'ェ':'リ':xs) = (S, xs)

parse ('イ':'ェ':xs) = (K, xs)
parse ('イ':'エ':xs) = (K, xs)

parse ('ス':xs) = (I, xs)

parse x = error $ x ++ ": Syntax Error"

ignoreCharacters = " \n!！"

church 0 = Apply K I
church n = Apply (Apply S (Apply (Apply S (Apply K S)) K)) (church $ n - 1)

cons a b = Apply (Apply S (Apply (Apply S I) (Apply K a))) (Apply K b)

realize expr = export $ eval $ Apply (Apply expr Increment) (Export 0)

formCode = filter (not.(flip elem) ignoreCharacters)

output expr = case realize (Apply expr K) of
        Nothing -> return ()
        Just 256 -> exitWith ExitSuccess
        Just x -> do
            if x >= 256
               then exitWith $ ExitFailure $ x - 256
               else do
                   putChar $ chr x
                   output $ eval $ Apply expr (Apply K I)

main = do
    args <- getArgs
    opts <- return $ filter (('-'==).head) args
    if "-h" `elem` opts
       then do
           putStrLn "Usage: kq [options] [filename]"
           putStrLn "-r: show as an expression"
           putStrLn "-d: read the source from stdin"
           putStrLn "-h: show this help"
       else do
           printer <- return (if "-r" `elem` opts then print else output)
           if  "-d" `elem` opts
              then do
                code <- formCode <$> getContents
                printer $ eval $ fst $ parse code
              else do
                input <- foldr cons (church 256) <$> map (eval.church.ord) <$>  getContents
                code <- formCode <$> (readFile $ head $ filter (('-'/=).head) args)
                printer $ eval $ Apply (fst $ parse code) $ input