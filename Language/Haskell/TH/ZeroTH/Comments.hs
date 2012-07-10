module Language.Haskell.TH.ZeroTH.Comments where

type Location = (Int, Int)

type Comment = (Location,String)

mixComments :: [Comment] -> [(Location,String)] -> [String]
mixComments c [] = map (showComment.lines.snd) c
mixComments [] i = map snd i
mixComments c@(((cl, _),cc):cs) i@(((il, _),ic):is)
    | cl <= il = showComment (lines cc):mixComments cs i
    | otherwise = ic:mixComments c is

showComment :: [String] -> String
showComment [x] = '-':'-':' ':x
showComment c = "{- " ++ unlines c ++ " -}"

-- FIXME!
parseComments :: String -> [Comment]
parseComments input = parseComments' (lines input) 1

parseComments' :: [String] -> Int -> [Comment]
parseComments' [] _ = []
parseComments' (('-':'-':c):xs) l = ((l, 1), c):parseComments' xs (l+1)
parseComments' (_:xs) l = parseComments' xs (l+1)
