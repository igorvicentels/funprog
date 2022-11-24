module SequenceIO where

sequenceIO :: [IO a] -> IO [a]
sequenceIO []       = return []
sequenceIO (ax:axs) = do x <- ax
                         tl <- sequenceIO axs
                         return $ x : tl

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ = void . sequenceIO