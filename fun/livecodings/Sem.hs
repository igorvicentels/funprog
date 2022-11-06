module Sem where

-- putChar
-- getChar
-- return
-- do
import Prelude hiding ( putStr )

putStr :: String -> IO ()
putStr xs =
    case xs of
        []     -> return ()
        (y:ys) -> do putChar y 
                     putStr ys