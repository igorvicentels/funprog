module Main where

main
    = do {putStrLn "Take text from where:";
          infile <- getLine;
          putStrLn "How many words:";
          n <- getLine;
          putStrLn "Put results where:";
          outfile <- getLine;
          text <- readFile infile;
          writeFile outfile text;
          putStrLn "cwords done!" }