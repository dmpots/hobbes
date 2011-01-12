module InputFile where

clean :: [String] -> [String]
clean []             = []
clean ("":rest)      = clean rest
clean (('#':_):rest) = clean rest
clean (line:rest)    = line : clean rest

