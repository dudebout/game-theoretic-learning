module GTL.Data.History where

class History history where
    toHistory :: [a] -> history a
    fromHistory :: history a -> [a]
    lastElement :: history a -> a
    lastElement = last . fromHistory
    addToHistory :: history a -> a -> history a
    addToHistory hist elt | length list == 0 = hist
                          | otherwise = toHistory $ (tail $ fromHistory hist) ++ [elt]
                            where list = fromHistory hist
