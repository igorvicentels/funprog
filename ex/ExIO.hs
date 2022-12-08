module ExIO where

import System.IO (hGetEcho, hSetEcho, stdin)
import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    , interact
    , (>>)
    , (>>=)
    )

-- read through the whole module first, to get an idea
-- of what's required and to decide where to start

getLine :: IO String
getLine = do 
    c <- getChar
    if (c == '\n')
        then return ""
        else do l <- getLine
                return (c : l)

getInt :: IO Int
getInt = do l <- getLine
            return (read l :: Int)

getSafeInt :: IO (Maybe Int)
getSafeInt = do l <- getLine
                let parsed = (reads l :: [(Int, String)])
                case parsed of
                    [(h,"")] -> return $ Just h
                    _        -> return Nothing
-- sequencing: first do f ignoring its result, then do g and keep its result
infixl 1 >>

(>>) :: IO a -> IO b -> IO b
ax >> ay = do ax
              ay

(<<) = flip (>>)

-- pauses till the user presses any normal key
pause :: IO ()
pause = void $ echoless getChar

echoless :: IO a -> IO a
echoless ax =
    do oldEcho <- hGetEcho stdin
       hSetEcho stdin False
       x <- ax
       hSetEcho stdin oldEcho
       return x

skip :: IO ()
skip = return ()

newline :: IO ()
newline = putChar '\n'

-- define it as a foldr
putStr :: String -> IO ()
putStr ""     = skip
putStr (x:xs) = do putChar x
                   putStr xs

-- putStrFold :: String -> IO ()
-- putStrFold = foldr (>>) skip

-- transform f into one "just like f" except that it prints a newline
-- after any side-effects f may had
lnize :: (a -> IO b) -> a -> IO b
lnize f x = do s <- f x
               newline
               return s

putStrLn :: String -> IO ()
putStrLn = lnize putStr

putCharLn :: Char -> IO ()
putCharLn = lnize putChar

-- reads the entire user input as a single string, transforms it, and prints it
interact :: (String -> String) -> IO ()
interact f = do l <- getLine
                putStr (f l) 
                

perlineize :: (String -> String) -> (String -> String)
perlineize f = unlines . map f . lines

interactPerLine :: (String -> String) -> IO ()
interactPerLine = interact . perlineize

when :: Bool -> IO () -> IO ()
when True  ax = ax 
when False _  = skip

unless :: Bool -> IO () -> IO ()
unless = when . not

guard :: Bool -> IO ()
guard = undefined

forever :: IO a -> IO b
forever ax = do ax 
                forever ax

-- transforms the action given to an equivalent one that has no result
void :: IO a -> IO ()
void ax = ax >> skip
-- void = (>> skip)


-- Kleisli compositions
infixr 1 >=>, <=<

-- diagrammatic order
(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
(>=>) f g x = do x' <- f x
                 g x' 

-- traditional order
-- comparison of types:
-- (.)   :: (b ->    c) -> (a ->    b) -> a ->    c
-- (<=<) :: (b -> IO c) -> (a -> IO b) -> a -> IO c
(<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)
(<=<) = flip (>=>)


-- Bind
infixl 1 >>=

(>>=) :: IO a -> (a -> IO b) -> IO b
ax >>= f = do x <- ax
              f x


infixl 4 $>, <$

-- make an action that has the side effects of the action on the left
-- but with result the value on the right
($>) :: IO a -> b -> IO b
ax $> y = ax >> return y

-- vice-versa
(<$) :: a -> IO b -> IO a
(<$) = flip ($>)

ap :: IO (a -> b) -> IO a -> IO b
af `ap` ax = do f <- af
                iomap f ax

filterIO :: (a -> IO Bool) -> [a] -> IO [a]
filterIO ap []     = return []
filterIO ap (x:xs) = do b <- ap x
                        xs' <- filterIO ap xs
                        return (if b then x : xs' else xs')

iomap :: (a -> b) -> IO a -> IO b
iomap f ax = do x <- ax
                return $ f x

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f = sequenceIO . map f

zipWithIO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]
zipWithIO f []     _      = return []
zipWithIO f _      []     = return []
zipWithIO f (x:xs) (y:ys) = do z <- f x y
                               tl <- zipWithIO f xs ys
                               return $ z : tl 

zipWithIO_ :: (a -> b -> IO c) -> [a] -> [b] -> IO ()
zipWithIO_ f xs ys = zipWithIO f xs ys >> skip

sequenceIO :: [IO a] -> IO [a]
sequenceIO []       = return []
sequenceIO (ax:axs) = do x <- ax
                         tl <- sequenceIO axs
                         return $ x : tl

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ = void . sequenceIO

replicateIO :: Integral i => i -> IO a -> IO [a]
replicateIO 0 ax = return []
replicateIO n ax = do x <- ax
                      xs <- replicateIO (n - 1) ax
                      return $ x : xs

replicateIO_ :: Integral i => i -> IO a -> IO ()
replicateIO_ n xs = replicateIO n xs >> skip

forIO :: [a] -> (a -> IO b) -> IO [b]
forIO []     f = return []
forIO (x:xs) f = do y <- f x
                    xs' <- forIO xs f
                    return $ y : xs'

forIO_ :: [a] -> (a -> IO b) -> IO ()
forIO_ xs f = forIO xs f >> skip 

joinIO :: IO (IO a) -> IO a
joinIO ax = do x <- ax
               x

-- TODO: Probably wrong
foldlIO :: (b -> a -> IO b) -> b -> [a] -> IO b
foldlIO f v []     = return v
foldlIO f v (x:xs) = do x' <- f v x
                        xs' <- foldlIO f x' xs
                        return xs'

foldlIO_ :: (b -> a -> IO b) -> b -> [a] -> IO ()
foldlIO_ f v xs = foldlIO f v xs >> skip


